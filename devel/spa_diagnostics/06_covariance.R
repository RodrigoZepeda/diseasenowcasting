# =============================================================================
# Phase 3, step 1: does FluSight show the cross-delay dependence the low-p
# solution requires?
# =============================================================================
# The user's result (proved, not re-derived here):
#
#   Cov(Delta_t^d, Delta_t^e) = -mu_t (1 - p) g_D(min{d,e}) g_C(|e - d|),   d != e
#
# so for ADJACENT delays
#
#   Cov(Delta_t^d, Delta_t^{d+1}) = -mu_t (1 - p) g_D(d) g_C(1).
#
# The pathological fit puts 99.4% of g_C on lag 1 and inflates mu_t roughly
# tenfold, so it predicts an enormous negative adjacent-delay covariance.  The
# one-delay marginal likelihood cannot see that prediction.  The data can.
#
# Var(Delta_t^d) = alpha_t^d + beta_t^d, so the implied CORRELATION is
#   Cov / sqrt(Var_d Var_{d+1}), which is the scale-free thing to compare.
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

START <- as.Date("2023-09-23"); STATE <- "Texas"
raw <- tbl.now::flusight |>
  filter(location_name == STATE, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))
tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
              case_count = observation, data_type = "count-cumulative",
              verbose = FALSE) |> align_weeks(date_col = "report_date")

pieces <- function(p_spec) {
  fitted <- suppressMessages(suppressWarnings(nowcast(
    tn, model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
              validation = validation_process(p = p_spec)),
    type = "one_stage", n_draws = 20, temporal_effects = "none", seed = 1)))
  fit <- fitted@fits[[1]]; engine <- fitted@engine; priors <- fitted@priors
  conf_D <- min(as.integer(engine$max_conf_delay) - 1L, 15L)
  p <- if (isTRUE(priors$confirm_p$is_constant == 1L)) priors$confirm_p$fixed
       else stats::plogis(as.numeric(fit$parList$logit_confirm_p))
  dfns <- diseasenowcasting:::.delay_distribution_functions(
    as.integer(engine$delay_family), as.numeric(fit$delay_mu), as.numeric(fit$delay_sigma))
  acdf <- as.numeric(dfns$cdf(seq_len(conf_D + 1L))); g_D <- c(acdf[1], diff(acdf))
  rmu <- as.numeric(fit$parList$retract_mu)
  rsd <- 0.01 + exp(as.numeric(fit$parList$log_retract_sd_exc))
  rfns <- diseasenowcasting:::.delay_distribution_functions(as.integer(priors$retract_family), rmu, rsd)
  g_C <- c(0, diff(c(0, as.numeric(rfns$cdf(seq_len(conf_D))))))
  g_W <- diseasenowcasting:::.convolve_delays(g_D, g_C)
  lambda <- as.numeric(fit$lambda)
  list(p = p, g_D = g_D, g_C = g_C, g_W = g_W, mu = lambda / p,
       eta = (1 - p) * lambda / p, engine = engine, conf_D = conf_D,
       gD_med = exp(as.numeric(fit$delay_mu)), gC_med = exp(rmu))
}

low  <- pieces(beta_prior(9.6, 0.4))
high <- pieces(0.9572)

compare <- function(par, label) {
  engine <- par$engine; conf_D <- par$conf_D
  inc <- engine$increment_array; d_star <- engine$d_star
  cat(sprintf("\n=== %s : p = %.4f, g_D median %.2f wk, g_C median %.3f wk, g_C(1) = %.4f ===\n",
              label, par$p, par$gD_med, par$gC_med, par$g_C[2]))
  cat(sprintf("%4s %6s %14s %14s %12s %12s\n",
              "d", "n_t", "emp cov", "implied cov", "emp corr", "implied corr"))
  out <- list()
  for (d in 0:(conf_D - 1L)) {
    # Event times where BOTH delay d and d+1 are inside the observable horizon.
    ts <- which(pmin(as.integer(d_star[, 1]), conf_D) >= d + 1L)
    if (length(ts) < 8L) next
    z_d  <- inc[ts, d + 1L, 1]; z_d1 <- inc[ts, d + 2L, 1]
    a_d  <- par$mu[ts] * par$g_D[d + 1L]; b_d  <- par$eta[ts] * par$g_W[d + 1L]
    a_d1 <- par$mu[ts] * par$g_D[d + 2L]; b_d1 <- par$eta[ts] * par$g_W[d + 2L]
    # Residuals against each cell's own fitted mean alpha - beta.
    r_d  <- z_d  - (a_d  - b_d)
    r_d1 <- z_d1 - (a_d1 - b_d1)
    emp_cov  <- mean(r_d * r_d1)
    emp_corr <- if (sd(r_d) > 0 && sd(r_d1) > 0) cor(r_d, r_d1) else NA_real_
    # Model-implied, averaged over the same event times.
    implied_cov  <- mean(-par$mu[ts] * (1 - par$p) * par$g_D[d + 1L] * par$g_C[2])
    implied_corr <- mean(-par$mu[ts] * (1 - par$p) * par$g_D[d + 1L] * par$g_C[2] /
                           sqrt((a_d + b_d) * (a_d1 + b_d1)))
    cat(sprintf("%4d %6d %14.2f %14.2f %12.3f %12.3f\n",
                d, length(ts), emp_cov, implied_cov, emp_corr, implied_corr))
    out[[length(out) + 1L]] <- data.frame(d = d, n = length(ts), emp_cov = emp_cov,
      implied_cov = implied_cov, emp_corr = emp_corr, implied_corr = implied_corr)
  }
  do.call(rbind, out)
}

low_tab  <- compare(low,  "LOW-p (free)")
high_tab <- compare(high, "HIGH-p (fixed at the empirical rate)")

cat("\n=== summary over delays with at least 8 event times ===\n")
for (tab in list(list("low p", low_tab), list("high p", high_tab))) {
  t2 <- tab[[2]]
  cat(sprintf("%-8s  mean emp corr %+.3f   mean implied corr %+.3f   ratio implied/emp cov %.1f\n",
              tab[[1]], mean(t2$emp_corr, na.rm = TRUE), mean(t2$implied_corr),
              sum(t2$implied_cov) / sum(t2$emp_cov)))
}
saveRDS(list(low = low_tab, high = high_tab), "devel/spa_diagnostics/covariance.rds")
