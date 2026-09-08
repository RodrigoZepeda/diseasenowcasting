# benchmark_revision_flusight.R
#
# Does the count-cumulative revision model match the article, and does the
# correction pay for itself?
#
# The article (main_journal_revised.tex, "Count-cumulative data") gives the
# signed update at delay d as Skellam(alpha_t^d, omega_t^d) with
#
#   alpha_t^d = mu_t g_D(d)                                 eq. alphasimplified
#   omega_t^d = mu_t (1 - p) sum_{d1<d} g_D(d1) g_C(d - d1)  eq. omegadef
#
# and mu_t = lambda_t / p the GROSS report rate.  The package used to compute
#
#   omega_t^d = lambda_t (1 - p) (g_D * g_C)(d)
#
# i.e. a factor of p too small.  That is not a reparametrisation: matching both
# moments needs p' solving p'^2 - p' + (1 - p) = 0 together with
# lambda' = lambda p'/p, so the reported epidemic mean -- the nowcast target --
# comes out biased low, and for p < 0.75 the discriminant is negative and the old
# form cannot represent the model at all.
#
#   true p   p'       lambda'/lambda   bias
#   0.98     0.9796   0.9996           -0.04%
#   0.90     0.8873   0.9859           -1.41%
#   0.80     0.7236   0.9045           -9.55%
#   0.70     --       --               unrepresentable
#
# Two arms, switched by an option that reproduces the old intensity:
#
#   legacy   options(diseasenowcasting.legacy_retraction_rate = TRUE)
#   fixed    the article's formula (the package default)
#
# Part A simulates from the article's generative model at a KNOWN (p, g_C) and
# asks whether either arm recovers them.  It is an IDENTIFIABILITY check only --
# analysing at the last event week leaves almost nothing unobserved, so its
# predictions are not a fair test of either arm and are not scored.
# Part B runs both arms on the FluSight hospitalisation stream, which revises
# down as well as up, and scores them against the settled counts.
#
# Flat, top-to-bottom, run it and read the tables at the bottom.

rm(list = ls())

library(dplyr)
library(tbl.now)
pkgload::load_all(".", quiet = TRUE)   # the WORKING TREE, not the installed package

# ---- run controls -----------------------------------------------------------
SEED        <- 20260901
N_DRAWS     <- 1000
N_DATES     <- 6            # as-of dates per state in Part B
STATES      <- c("Texas", "California", "New York", "Florida")
SIM_T       <- 60           # event weeks simulated in Part A
SIM_P       <- c(0.98, 0.90, 0.80)   # true confirmation probabilities to try
SIM_DELAY_MU <- log(1.2)    # lognormal appearance delay, on the log scale
SIM_DELAY_SD <- 0.6
SIM_LAG     <- 2            # retraction lag C ~ 1 + Poisson(SIM_LAG)
FIT_TYPE    <- "one_stage"

set.seed(SEED)

# =============================================================================
# Part A -- simulation at known truth: is (p, g_C) identifiable?
# =============================================================================
# Generative model, straight from the article:
#   M_t     ~ Poisson(mu_t)                      gross events at event time t
#   D_rpt   ~ discretised LogNormal              event -> report delay
#   Y       ~ Bernoulli(p)                       1 = true case
#   D_val   ~ 1 + Poisson(SIM_LAG)   if Y = 0    report -> retraction lag, >= 1
#   C_t(d)  = #{Y=1, D_rpt <= d} +
#             #{Y=0, D_rpt <= d, D_rpt + D_val > d}
# and the nowcast target is N_t^+ = #{Y = 1} at event time t.

sim_rows <- list()

for (p_true in SIM_P) {
  # A single epidemic wave on the GROSS scale, so lambda_t = p_true * mu_t.
  t_index <- seq_len(SIM_T)
  mu_t    <- 300 * exp(-((t_index - 32) / 14)^2) + 25

  events <- lapply(t_index, function(t) {
    m <- rpois(1, mu_t[t])
    if (m == 0) return(NULL)
    # Discretised lognormal appearance delay: floor of the continuous draw.
    d_rpt <- floor(rlnorm(m, SIM_DELAY_MU, SIM_DELAY_SD))
    y     <- rbinom(m, 1, p_true)
    d_val <- ifelse(y == 1, NA_integer_, 1L + rpois(m, SIM_LAG))
    data.frame(t = t, d_rpt = d_rpt, y = y, d_val = d_val)
  })
  events <- do.call(rbind, events)

  now_t <- SIM_T                       # analysis at the last event week
  # C_t(d) for every event time and every observable delay.
  cum <- lapply(t_index, function(t) {
    rows <- events[events$t == t, , drop = FALSE]
    d_star <- now_t - t
    if (d_star < 0) return(NULL)
    counts <- vapply(0:d_star, function(d) {
      kept <- (rows$y == 1 & rows$d_rpt <= d) |
              (rows$y == 0 & rows$d_rpt <= d & (rows$d_rpt + rows$d_val) > d)
      sum(kept)
    }, numeric(1))
    data.frame(t = t, d = 0:d_star, n = counts)
  })
  cum <- do.call(rbind, cum)

  # Calendar dates on a weekly grid, so the tbl_now delay is a whole number.
  origin <- as.Date("2024-01-06")
  cum$event_date  <- origin + (cum$t - 1) * 7
  cum$report_date <- cum$event_date + cum$d * 7
  cum$p_true      <- p_true

  sim_rows[[as.character(p_true)]] <- cum
}

# ---- fit both arms on each simulated scenario --------------------------------
sim_results <- list()

for (p_true in SIM_P) {
  key <- as.character(p_true)
  cum <- sim_rows[[key]]

  tn <- tbl_now(
    cum[, c("event_date", "report_date", "n")],
    event_date = event_date, report_date = report_date, case_count = n,
    data_type = "count-cumulative", verbose = FALSE
  )

  for (arm in c("legacy", "fixed")) {
    options(diseasenowcasting.legacy_retraction_rate = (arm == "legacy"))

    fitted <- tryCatch(
      nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
                        confirmation = confirmation_process()),
              type = FIT_TYPE, n_draws = N_DRAWS, temporal_effects = "none",
              seed = SEED),
      error = function(e) { message("  [", arm, " p=", p_true, "] ", conditionMessage(e)); NULL }
    )
    if (is.null(fitted)) next

    # `coef()` does not surface the revision parameters -- read them off parList.
    fitted_pars <- fitted@fits[[1]]$parList

    sim_results[[length(sim_results) + 1L]] <- data.frame(
      p_true      = p_true,
      arm         = arm,
      p_hat       = plogis(as.numeric(fitted_pars$logit_confirm_p)),
      p_bias      = plogis(as.numeric(fitted_pars$logit_confirm_p)) - p_true,
      # The retraction lag is lognormal, so `retract_mu` is its log-median; the
      # truth is 1 + Poisson(SIM_LAG), median SIM_LAG + 1.
      lag_median_hat  = exp(as.numeric(fitted_pars$retract_mu)),
      lag_median_true = SIM_LAG + 1
    )
  }
}

options(diseasenowcasting.legacy_retraction_rate = FALSE)
sim_table <- do.call(rbind, sim_results)

cat("\n=== Part A: is (p, g_C) identifiable from a cumulative stream? ===\n")
print(sim_table, row.names = FALSE, digits = 4)

# =============================================================================
# Part B -- FluSight: does the correction score better on a real revising stream?
# =============================================================================
# FluSight publishes a snapshot per `as_of`, restating the whole history, and it
# revises DOWN as well as up (684 negative revisions in the Texas series alone).
# `align_weeks()` first: 8.8% of the raw delays are not whole weeks, and the
# signed-increment model is indexed by integer delay.

flusight_results <- list()

for (state in STATES) {
  rows <- tbl.now::flusight[tbl.now::flusight$location_name == state, ]

  tn <- tbl_now(rows, event_date = target_end_date, report_date = as_of,
                case_count = observation, data_type = "count-cumulative",
                verbose = FALSE)
  tn <- align_weeks(tn, date_col = "report_date")

  for (arm in c("legacy", "fixed")) {
    options(diseasenowcasting.legacy_retraction_rate = (arm == "legacy"))

    bt <- tryCatch(
      backtest(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
                         confirmation = confirmation_process()),
               n_dates = N_DATES, type = FIT_TYPE, n_draws = N_DRAWS,
               temporal_effects = "none", seed = SEED),
      error = function(e) { message("  [", arm, " ", state, "] ", conditionMessage(e)); NULL }
    )
    if (is.null(bt)) next

    scored <- score(bt, report = FALSE)
    scored$state <- state
    scored$arm   <- arm
    flusight_results[[length(flusight_results) + 1L]] <- scored
  }
}

options(diseasenowcasting.legacy_retraction_rate = FALSE)
flusight_table <- do.call(rbind, flusight_results)

cat("\n=== Part B: FluSight, legacy vs article-correct retraction rate ===\n")
print(flusight_table, row.names = FALSE, digits = 4)

cat("\n=== Part B: mean over states ===\n")
print(
  flusight_table |>
    group_by(arm) |>
    summarise(wis = mean(wis), ape = mean(ape),
              cov50 = mean(coverage_50), cov90 = mean(coverage_90),
              .groups = "drop"),
  digits = 4
)

saveRDS(list(simulation = sim_table, flusight = flusight_table),
        file.path("devel", "benchmark_revision_flusight.rds"))
