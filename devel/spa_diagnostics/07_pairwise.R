# =============================================================================
# Phase 3, step 2: the ADJACENT-PAIR composite likelihood (their section 19).
# =============================================================================
# Decompose the reports contributing to the pair (Delta_d, Delta_{d+1}) by type.
# Poisson thinning makes the type counts independent, and only five types touch
# the pair (r = appearance delay, s = retraction delay, c = s - r >= 1):
#
#   N1  false, r = d,   s = d+1      -> (+1, -1)   lam1 = mu(1-p) g_D(d) g_C(1)
#   N2  anything r = d, not retracted at d+1
#                                    -> (+1,  0)   lam2 = mu g_D(d)[1 - (1-p) g_C(1)]
#   N3  anything r = d+1             -> ( 0, +1)   lam3 = mu g_D(d+1)
#   N4  false, r < d,   s = d        -> (-1,  0)   lam4 = mu(1-p) g_W(d)
#   N5  false, r < d,   s = d+1      -> ( 0, -1)   lam5 = mu(1-p)[g_W(d+1) - g_D(d) g_C(1)]
#
#   Delta_d = N1 + N2 - N4,   Delta_{d+1} = -N1 + N3 - N5
#
# so, conditioning on N1 (the ONLY shared term, and the entire source of
# dependence):
#
#   P(u, v) = sum_{n >= 0} Pois(n; lam1) Skellam(u - n; lam2, lam4)
#                                         Skellam(v + n; lam3, lam5)
#
# Marginals, variances and Cov = -lam1 = -mu(1-p) g_D(d) g_C(1) all fall out, which
# is the identity being tested -- so this is the same model, with the adjacent
# dependence RETAINED rather than integrated away.
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

# The mixed Skellam branch must not be fed a structurally-zero rate: the
# production likelihood picks the branch from `bin_type` for exactly that reason.
# Here the rates are built per pair, and lam4 / lam5 are proportional to (1 - p),
# so they vanish as p -> 1.  Dispatch on the rates, as the objective does on the
# delay index.
skel <- function(k, a, b) {
  if (b <= 1e-9) return(if (k < 0) -Inf else stats::dpois(k, a + 1e-8, log = TRUE))
  if (a <= 1e-9) return(if (k > 0) -Inf else stats::dpois(-k, b + 1e-8, log = TRUE))
  diseasenowcasting:::.log_skellam_increment(k, a, b, 1L)
}

# log P(Delta_d = u, Delta_{d+1} = v)
log_pair <- function(u, v, lam1, lam2, lam3, lam4, lam5) {
  if (lam1 <= 1e-12) return(skel(u, lam2, lam4) + skel(v, lam3, lam5))

  # The summation window must be driven by FEASIBILITY, not by lam1's spread.
  # When a rate is structurally zero the corresponding Skellam becomes a Poisson,
  # which is zero for a negative argument, so n is constrained:
  #   lam5 = 0  =>  v + n >= 0  =>  n >= -v
  #   lam4 = 0  =>  u - n >= 0  =>  n <= u
  # At d = 0 BOTH are zero (g_W(0) = 0, and g_W(1) = g_D(0) g_C(1) exactly), so a
  # down-revision at delay 1 can only come from N1 and forces n >= -v.  A window of
  # width ~lam1 starting at 0 misses that entirely and returns -Inf for a cell whose
  # true log-probability is large but finite -- which is the pairwise likelihood
  # legitimately punishing a large down-revision the parameters can barely produce.
  width <- ceiling(lam1 + 8 * sqrt(lam1 + 1) + 10)
  n_lo  <- if (lam5 <= 1e-9) max(0, -v) else 0
  n_hi  <- n_lo + width
  if (lam4 <= 1e-9) {
    if (n_lo > u) return(-Inf)          # genuinely infeasible, not truncated
    n_hi <- min(n_hi, u)
  }
  n <- n_lo:n_hi
  lt <- stats::dpois(n, lam1, log = TRUE) +
        vapply(n, function(k) skel(u - k, lam2, lam4), numeric(1)) +
        vapply(n, function(k) skel(v + k, lam3, lam5), numeric(1))
  m <- max(lt); if (!is.finite(m)) return(-Inf)
  m + log(sum(exp(lt - m)))
}

# ---- sanity: does the construction reproduce the marginals and the covariance? --
cat("=== construction check (moments from the 5-type decomposition) ===\n")
set.seed(1)
for (tc in list(c(mu=200, p=0.14, gDd=0.05, gDd1=0.04, gC1=0.99, gWd=0.03, gWd1=0.05),
                c(mu=200, p=0.96, gDd=0.50, gDd1=0.20, gC1=0.73, gWd=0.10, gWd1=0.30))) {
  mu<-tc["mu"]; p<-tc["p"]; gDd<-tc["gDd"]; gDd1<-tc["gDd1"]; gC1<-tc["gC1"]
  gWd<-tc["gWd"]; gWd1<-tc["gWd1"]
  lam1 <- mu*(1-p)*gDd*gC1
  lam2 <- mu*gDd*(1-(1-p)*gC1)
  lam3 <- mu*gDd1
  lam4 <- mu*(1-p)*gWd
  lam5 <- mu*(1-p)*max(gWd1 - gDd*gC1, 0)
  n <- 2e6
  N1<-rpois(n,lam1); N2<-rpois(n,lam2); N3<-rpois(n,lam3); N4<-rpois(n,lam4); N5<-rpois(n,lam5)
  dd <- N1+N2-N4; dd1 <- -N1+N3-N5
  cat(sprintf("  p=%.2f  E[d] %8.3f (want %8.3f)  E[d+1] %8.3f (want %8.3f)  Cov %9.3f (want %9.3f)\n",
      p, mean(dd), mu*gDd - mu*(1-p)*gWd, mean(dd1), mu*gDd1 - mu*(1-p)*gWd1,
      cov(dd, dd1), -lam1))
}

# =============================================================================
# Profile p under the pairwise composite likelihood.
# =============================================================================
# Evaluated along the SAME theta(p) path the marginal profile produced, so the two
# curves differ ONLY in whether adjacent-delay dependence is retained.  Each Delta
# appears in two pairs, so the pairwise sum is a composite likelihood, not a
# likelihood -- comparing its SHAPE across p is the valid use of it.
#
# The like-for-like control is the marginal likelihood summed over the SAME pairs:
#   L_marg_pairs = sum_d [ log P(Delta_d) + log P(Delta_{d+1}) ]
# so any difference in shape is attributable to the dependence and nothing else.

START <- as.Date("2023-09-23"); STATE <- "Texas"
raw <- tbl.now::flusight |>
  filter(location_name == STATE, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))
tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
              case_count = observation, data_type = "count-cumulative",
              verbose = FALSE) |> align_weeks(date_col = "report_date")

P_GRID <- c(0.05, 0.10, 0.20, 0.40, 0.60, 0.80, 0.90, 0.96, 0.99)
rows <- list()

for (p_fixed in P_GRID) {
  fitted <- suppressMessages(suppressWarnings(nowcast(
    tn, model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
              validation = validation_process(p = p_fixed)),
    type = "one_stage", n_draws = 20, temporal_effects = "none", seed = 1)))
  fit <- fitted@fits[[1]]; engine <- fitted@engine; priors <- fitted@priors
  conf_D <- min(as.integer(engine$max_conf_delay) - 1L, 15L)
  dfns <- diseasenowcasting:::.delay_distribution_functions(
    as.integer(engine$delay_family), as.numeric(fit$delay_mu), as.numeric(fit$delay_sigma))
  acdf <- as.numeric(dfns$cdf(seq_len(conf_D + 1L))); g_D <- c(acdf[1], diff(acdf))
  rmu <- as.numeric(fit$parList$retract_mu)
  rsd <- 0.01 + exp(as.numeric(fit$parList$log_retract_sd_exc))
  rfns <- diseasenowcasting:::.delay_distribution_functions(as.integer(priors$retract_family), rmu, rsd)
  g_C <- c(0, diff(c(0, as.numeric(rfns$cdf(seq_len(conf_D))))))
  g_W <- diseasenowcasting:::.convolve_delays(g_D, g_C)
  mu_t <- as.numeric(fit$lambda) / p_fixed
  inc <- engine$increment_array; d_star <- engine$d_star

  L_pair <- L_marg <- 0
  for (t in seq_len(engine$max_time)) {
    h <- min(as.integer(d_star[t, 1]), conf_D); if (h < 1L) next
    mu <- mu_t[t]
    for (d in 0:(h - 1L)) {
      u <- inc[t, d + 1L, 1]; v <- inc[t, d + 2L, 1]
      lam1 <- mu * (1 - p_fixed) * g_D[d + 1L] * g_C[2]
      lam2 <- mu * g_D[d + 1L] * (1 - (1 - p_fixed) * g_C[2])
      lam3 <- mu * g_D[d + 2L]
      lam4 <- mu * (1 - p_fixed) * g_W[d + 1L]
      lam5 <- mu * (1 - p_fixed) * max(g_W[d + 2L] - g_D[d + 1L] * g_C[2], 0)
      L_pair <- L_pair + log_pair(u, v, lam1, lam2, lam3, lam4, lam5)
      a_d  <- mu * g_D[d + 1L]; b_d  <- (1 - p_fixed) * mu * g_W[d + 1L]
      a_d1 <- mu * g_D[d + 2L]; b_d1 <- (1 - p_fixed) * mu * g_W[d + 2L]
      L_marg <- L_marg +
        (if (d == 0L) dpois(u, a_d + 1e-8, log = TRUE) else skel(u, a_d, b_d)) +
        skel(v, a_d1, b_d1)
    }
  }
  cat(sprintf("p=%.2f  L_pair=%12.2f  L_marg_pairs=%12.2f  gC(1)=%.4f\n",
              p_fixed, L_pair, L_marg, g_C[2]))
  rows[[length(rows) + 1L]] <- data.frame(p = p_fixed, L_pair = L_pair,
                                          L_marg = L_marg, gC1 = g_C[2])
}

tab <- do.call(rbind, rows)
tab$rel_pair <- tab$L_pair - max(tab$L_pair)
tab$rel_marg <- tab$L_marg - max(tab$L_marg)
cat("\n=== F. p-profile: marginal vs adjacent-pair composite (Texas) ===\n")
print(tab[, c("p", "rel_marg", "rel_pair", "gC1")], row.names = FALSE, digits = 5)
cat(sprintf("\n  marginal   optimum at p = %.2f\n", tab$p[which.max(tab$L_marg)]))
cat(sprintf("  pairwise   optimum at p = %.2f\n", tab$p[which.max(tab$L_pair)]))
saveRDS(tab, "devel/spa_diagnostics/pairwise_profile.rds")
