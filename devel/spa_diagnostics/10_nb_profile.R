# =============================================================================
# Is the churn solution an OVERDISPERSION device?
# =============================================================================
# Skellam variance is alpha + omega, so under a Poisson observation model the only
# way to buy variance beyond the net mean is to inflate BOTH flows -- which is
# exactly what the low-p solution does (108x too many retractions, at the wrong
# delays, see 09_gw_fit.R).  A negative-binomial likelihood supplies overdispersion
# through the gamma frailty instead.  If that is the mechanism, the NB profile
# should not prefer low p.
#
# Every earlier profile in this investigation used poisson_likelihood().
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

P_GRID <- c(0.10, 0.20, 0.40, 0.60, 0.80, 0.90, 0.96, 0.99)
rows <- list()

for (lik_name in c("poisson", "nb")) {
  lik <- if (lik_name == "poisson") poisson_likelihood() else nb_likelihood()
  for (p_fixed in P_GRID) {
    f <- tryCatch(suppressMessages(suppressWarnings(nowcast(
      tn, model(lik, ar1_epidemic(), lognormal_delay(),
                validation = validation_process(p = p_fixed)),
      type = "one_stage", n_draws = 20, temporal_effects = "none", seed = 1))),
      error = function(e) { message("[", lik_name, " p=", p_fixed, "] ", conditionMessage(e)); NULL })
    if (is.null(f)) next
    fit <- f@fits[[1]]; e <- f@engine; pr <- f@priors
    cD <- min(as.integer(e$max_conf_delay) - 1L, 15L)
    dfns <- diseasenowcasting:::.delay_distribution_functions(
      as.integer(e$delay_family), as.numeric(fit$delay_mu), as.numeric(fit$delay_sigma))
    acdf <- as.numeric(dfns$cdf(seq_len(cD + 1L))); gD <- c(acdf[1], diff(acdf))
    rmu <- as.numeric(fit$parList$retract_mu)
    rsd <- 0.01 + exp(as.numeric(fit$parList$log_retract_sd_exc))
    rfns <- diseasenowcasting:::.delay_distribution_functions(as.integer(pr$retract_family), rmu, rsd)
    gC <- c(0, diff(c(0, as.numeric(rfns$cdf(seq_len(cD))))))
    gW <- diseasenowcasting:::.convolve_delays(gD, gC)
    mu <- as.numeric(fit$lambda) / p_fixed
    ds <- e$d_star; inc <- e$increment_array
    obs_down <- exp_down <- 0
    for (t in seq_len(e$max_time)) {
      h <- min(as.integer(ds[t, 1]), cD); if (h < 0L) next
      obs_down <- obs_down + sum(pmax(-inc[t, seq_len(h + 1L), 1], 0))
      exp_down <- exp_down + mu[t] * (1 - p_fixed) * sum(gW[seq_len(h + 1L)])
    }
    rows[[length(rows) + 1L]] <- data.frame(
      likelihood = lik_name, p = p_fixed, nll = fit$nll,
      gD_median = exp(as.numeric(fit$delay_mu)), gC1 = gC[2],
      phi = if (lik_name == "nb") exp(as.numeric(fit$parList$log_phi_nb)) else NA_real_,
      obs_down = obs_down, exp_down = exp_down, ratio = exp_down / max(obs_down, 1),
      conv = fit$convergence)
    cat(sprintf("%-8s p=%.2f  nll=%12.2f  gD_med=%6.2f  gC1=%.3f  exp/obs down = %8.1f\n",
                lik_name, p_fixed, fit$nll, exp(as.numeric(fit$delay_mu)), gC[2],
                exp_down / max(obs_down, 1)))
  }
}

tab <- do.call(rbind, rows)
cat("\n=== profile in p, Poisson vs NB (relative to each likelihood's own best) ===\n")
for (ln in unique(tab$likelihood)) {
  sub <- tab[tab$likelihood == ln, ]
  sub$rel <- -(sub$nll - min(sub$nll))
  cat(sprintf("\n-- %s --  optimum at p = %.2f\n", ln, sub$p[which.min(sub$nll)]))
  print(sub[, c("p", "rel", "gD_median", "gC1", "phi", "ratio", "conv")],
        row.names = FALSE, digits = 4)
}
saveRDS(tab, "devel/spa_diagnostics/nb_profile.rds")
