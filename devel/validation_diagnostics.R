# validation_diagnostics.R
#
# Reproducers for the count-cumulative (Skellam / SkNB) validation model.
#
# Everything here simulates from the article's own generative model
# (main_journal_revised.tex, "Count-cumulative data") at a KNOWN (p, g_C), so any
# disagreement is the package's, not the data's:
#
#   M_t     ~ Poisson(mu_t)                      gross events at event time t
#   D_rpt   ~ discretised LogNormal              event -> report delay
#   Y       ~ Bernoulli(p)                       1 = true case
#   D_val   ~ 1 + Poisson(SIM_LAG)   if Y = 0    report -> retraction lag, >= 1
#   C_t(d)  = #{Y=1, D_rpt <= d} +
#             #{Y=0, D_rpt <= d, D_rpt + D_val > d}
#
# Three blocks, each answering one question:
#
#   A  Does the fit converge, and does it recover (p, g_C)?  Sweeps the retraction
#      rate, which is what used to decide convergence: everything at p >= 0.95
#      aborted with "NA/NaN gradient evaluation" before the saddlepoint rewrite.
#   B  How much does the `p` prior concentration matter?  This is the evidence that
#      the old strongly-concentrated default was unnecessary ON SIMULATED DATA.
#   C  Is the Skellam log-pmf accurate?  Scores `.log_skellam_increment()` against
#      a high-precision reference over the (alpha, beta, m) grid AND over the cells
#      a real fit actually visits.
#
# Flat, top-to-bottom, run it and read the tables.

rm(list = ls())

library(dplyr)
library(tbl.now)
pkgload::load_all(".", quiet = TRUE)   # the WORKING TREE, not the installed package

# ---- run controls -----------------------------------------------------------
SEED       <- 1
SIM_T      <- 60          # event weeks
SIM_LAG    <- 2           # retraction lag C ~ 1 + Poisson(SIM_LAG), median 3
DELAY_MU   <- log(1.2)    # lognormal appearance delay, log scale
DELAY_SD   <- 0.6
N_DRAWS    <- 500

# One simulated count-cumulative tbl_now at a given true confirmation probability.
simulate_cumulative <- function(p_true, seed = SEED) {
  set.seed(seed)
  t_index <- seq_len(SIM_T)
  mu_t    <- 300 * exp(-((t_index - 32) / 14)^2) + 25

  events <- do.call(rbind, lapply(t_index, function(t) {
    m <- rpois(1, mu_t[t])
    data.frame(t = t, d_rpt = floor(rlnorm(m, DELAY_MU, DELAY_SD)),
               y = rbinom(m, 1, p_true))
  }))
  events$d_val <- ifelse(events$y == 1, NA_integer_, 1L + rpois(nrow(events), SIM_LAG))

  cum <- do.call(rbind, lapply(t_index, function(t) {
    rows   <- events[events$t == t, , drop = FALSE]
    d_star <- SIM_T - t
    data.frame(t = t, d = 0:d_star, n = vapply(0:d_star, function(d) {
      sum((rows$y == 1 & rows$d_rpt <= d) |
          (rows$y == 0 & rows$d_rpt <= d & (rows$d_rpt + rows$d_val) > d))
    }, numeric(1)))
  }))

  origin <- as.Date("2024-01-06")
  cum$event_date  <- origin + (cum$t - 1) * 7
  cum$report_date <- cum$event_date + cum$d * 7

  list(
    tn = tbl_now(cum[, c("event_date", "report_date", "n")],
                 event_date = event_date, report_date = report_date, case_count = n,
                 data_type = "count-cumulative", verbose = FALSE),
    truth = events |> group_by(t) |> summarise(truth = sum(y), .groups = "drop") |>
      arrange(t) |> pull(truth),
    increments = cum |> arrange(t, d) |> group_by(t) |>
      mutate(delta = n - lag(n, default = 0)) |> ungroup()
  )
}

# =============================================================================
# A -- convergence and recovery vs the retraction rate
# =============================================================================
# Convergence used to track the number of NEGATIVE increments with a sharp
# threshold: 0/25/45/103 negatives all failed, 168/245 succeeded.  The cause was a
# NaN gradient in the Bessel branch of the increment density, so fits failed at
# exactly the retraction rates real surveillance data shows (p ~ 0.98) and worked
# only at implausible ones.  Both likelihoods should now converge on every row.

cat("\n=== A: convergence and recovery vs retraction rate ===\n")
cat("(true retraction lag median = 3)\n\n")
cat(sprintf("%7s %8s %10s %9s %9s %10s\n",
            "p_true", "lik", "n_neg_inc", "converged", "p_hat", "lag_med"))

for (p_true in c(0.999, 0.99, 0.98, 0.95, 0.90, 0.80)) {
  sim   <- simulate_cumulative(p_true)
  n_neg <- sum(sim$increments$delta < 0)

  for (lik_name in c("poisson", "nb")) {
    mdl <- model(if (lik_name == "nb") nb_likelihood() else poisson_likelihood(),
                 ar1_epidemic(), lognormal_delay(),
                 confirmation = confirmation_process())
    fitted <- tryCatch(
      suppressWarnings(nowcast(sim$tn, mdl, type = "one_stage", n_draws = N_DRAWS,
                               temporal_effects = "none", seed = SEED)),
      error = function(e) NULL)

    if (is.null(fitted)) {
      cat(sprintf("%7.3f %8s %10d %9s %9s %10s\n", p_true, lik_name, n_neg, "FALSE", "-", "-"))
    } else {
      fitted_pars <- fitted@fits[[1]]$parList     # coef() does not surface these
      cat(sprintf("%7.3f %8s %10d %9s %9.4f %10.2f\n", p_true, lik_name, n_neg, "TRUE",
                  plogis(as.numeric(fitted_pars$logit_confirm_p)),
                  exp(as.numeric(fitted_pars$retract_mu))))
    }
  }
}

# =============================================================================
# B -- how much does the `p` prior concentration matter?
# =============================================================================
# The default used to be a Beta with concentration 300 floored at p = 0.9, on the
# grounds that a weaker prior let `p` collapse.  That was an artefact of the broken
# increment density.  On data simulated FROM the model the estimate now moves <0.01
# across a 300x change in concentration, and the weakest prior is the most accurate,
# because the strong one was dragging `p` toward a centre that is biased upward by
# construction (a cumulative stream shows "not retracted YET").
#
# CAVEAT, and it matters: this is simulated data, so the model is correctly
# specified.  On FluSight -- where it is not -- the weak prior does NOT hold the fit
# and `p` runs to 0.07.  See the handoff.

cat("\n=== B: sensitivity of p_hat to the prior concentration ===\n\n")
cat(sprintf("%7s %6s %10s %9s %9s\n", "p_true", "conc", "p_centre", "p_hat", "lag_med"))

for (p_true in c(0.90, 0.80)) {
  sim <- simulate_cumulative(p_true)
  # The centre the package itself would compute, so the sweep isolates concentration.
  p_centre <- min(max(1 - sum(pmax(-sim$increments$delta, 0)) /
                        max(sum(pmax(sim$increments$delta, 0)), 1), 0.01), 0.995)

  for (concentration in c(300, 100, 30, 10, 3, 1)) {
    mdl <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
                 confirmation = confirmation_process(
                   p = beta_prior(p_centre * concentration, (1 - p_centre) * concentration)))
    fitted <- tryCatch(
      suppressWarnings(nowcast(sim$tn, mdl, type = "one_stage", n_draws = 50,
                               temporal_effects = "none", seed = SEED)),
      error = function(e) NULL)
    if (is.null(fitted)) next
    fitted_pars <- fitted@fits[[1]]$parList
    cat(sprintf("%7.2f %6g %10.4f %9.4f %9.2f\n", p_true, concentration, p_centre,
                plogis(as.numeric(fitted_pars$logit_confirm_p)),
                exp(as.numeric(fitted_pars$retract_mu))))
  }
}

# =============================================================================
# C -- accuracy of the Skellam increment log-density
# =============================================================================
# `.log_skellam_increment()` is a closed-form saddlepoint plus the exact ascending
# series for the small-(alpha + beta) corner.  Reference: the same series with far
# more terms than any peak needs -- it runs off the AD tape so cost does not matter,
# and it is in log space so it never underflows.  `besselI()` cannot serve as the
# reference: it returns 0 across the bulk of an asymmetric Skellam.

reference_log_skellam <- function(m, alpha, beta, n_terms = 20000L) {
  alpha <- alpha + 1e-8
  beta  <- beta + 1e-8
  order <- abs(m)
  argument <- 2 * sqrt(alpha * beta)
  k <- 0:n_terms
  log_terms <- (2 * k + order) * log(argument / 2) - lgamma(k + 1) - lgamma(k + order + 1)
  largest <- max(log_terms)
  -(alpha + beta) + (m / 2) * log(alpha / beta) + largest + log(sum(exp(log_terms - largest)))
}

skellam_increment <- diseasenowcasting:::.log_skellam_increment

cat("\n=== C: accuracy of the Skellam increment log-density ===\n\n")

# The (alpha, beta) pairs that broke each historical implementation: a fixed-length
# ascending series was 5729 nats low at (20000, 500), and besselI returns 0 across
# the bulk of the asymmetric pairs.
grid_errors <- c()
for (rates in list(c(20, 3), c(100, 5), c(150, 140), c(1000, 20),
                   c(5000, 100), c(20000, 500), c(1e5, 1e4))) {
  alpha <- rates[1]; beta <- rates[2]
  for (sd_offset in c(-6, -4, -2, -1, 0, 1, 2, 4, 6)) {
    increment <- round((alpha - beta) + sd_offset * sqrt(alpha + beta))
    grid_errors <- c(grid_errors, abs(
      as.numeric(skellam_increment(increment, alpha, beta, 1L)) -
        reference_log_skellam(increment, alpha, beta)))
  }
}
cat("worst error over the (alpha, beta, m) grid :",
    format(max(grid_errors), digits = 4), "nats\n")

# The cells a real fit visits, taken from the event time that used to abort it.
# Small rates and small increments -- the corner the saddlepoint is weakest in.
real_cells <- list(
  c(81, 78.31, 1e-9), c(101, 85.49, 2.577e-5), c(26, 26.87, 0.0684),
  c(8, 8.174, 0.5655), c(3, 2.725, 1.03), c(0, 0.9986, 0.8686),
  c(-1, 0.1701, 0.1878), c(0, 0.07725, 0.06962), c(0, 0.001641, 0.000346))
real_errors <- vapply(real_cells, function(cell) abs(
  as.numeric(skellam_increment(cell[1], cell[2], cell[3], 1L)) -
    reference_log_skellam(cell[1], cell[2], cell[3])), numeric(1))
cat("worst error over the cells a real fit visits:",
    format(max(real_errors), digits = 4), "nats\n")

# A pmf that does not sum to 1 is the signature of a truncated series (falls short)
# or a floored Bessel (diverges).
cat("\ntotal mass over the support:\n")
for (rates in list(c(20, 3), c(1000, 20), c(5000, 100))) {
  alpha <- rates[1]; beta <- rates[2]
  spread  <- ceiling(8 * sqrt(alpha + beta))
  support <- seq(round(alpha - beta) - spread, round(alpha - beta) + spread)
  total   <- sum(exp(vapply(support, function(m)
    as.numeric(skellam_increment(m, alpha, beta, 1L)), numeric(1))))
  cat(sprintf("  Skellam(%6g, %5g) = %.6f\n", alpha, beta, total))
}
