# =============================================================================
# When do retractions actually land?  g_W = g_D * g_C is DIRECTLY observable from
# the down-revisions, with no residuals, no covariance and no summation.
# =============================================================================
# Expected down-revision mass at delay d is sum_t omega_t^d = sum_t mu_t (1-p) g_W(d).
# Observed is sum_t max(-Delta_t^d, 0).  A solution that puts g_D 25 weeks out
# cannot produce retractions at delay 1, whatever g_C does, because a report must
# appear before it can be withdrawn.
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
  f <- suppressMessages(suppressWarnings(nowcast(
    tn, model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
              validation = validation_process(p = p_spec)),
    type = "one_stage", n_draws = 20, temporal_effects = "none", seed = 1)))
  fit <- f@fits[[1]]; e <- f@engine; pr <- f@priors
  cD <- min(as.integer(e$max_conf_delay) - 1L, 15L)
  p <- if (isTRUE(pr$confirm_p$is_constant == 1L)) pr$confirm_p$fixed
       else stats::plogis(as.numeric(fit$parList$logit_confirm_p))
  dfns <- diseasenowcasting:::.delay_distribution_functions(
    as.integer(e$delay_family), as.numeric(fit$delay_mu), as.numeric(fit$delay_sigma))
  acdf <- as.numeric(dfns$cdf(seq_len(cD + 1L))); gD <- c(acdf[1], diff(acdf))
  rmu <- as.numeric(fit$parList$retract_mu)
  rsd <- 0.01 + exp(as.numeric(fit$parList$log_retract_sd_exc))
  rfns <- diseasenowcasting:::.delay_distribution_functions(as.integer(pr$retract_family), rmu, rsd)
  gC <- c(0, diff(c(0, as.numeric(rfns$cdf(seq_len(cD))))))
  list(p = p, gD = gD, gC = gC, gW = diseasenowcasting:::.convolve_delays(gD, gC),
       mu = as.numeric(fit$lambda) / p, e = e, cD = cD,
       gD_med = exp(as.numeric(fit$delay_mu)), gC1 = gC[2])
}

low  <- pieces(beta_prior(9.6, 0.4))
high <- pieces(0.9572)

e <- low$e; cD <- low$cD; inc <- e$increment_array; ds <- e$d_star
cat(sprintf("\nlow-p : p=%.4f  g_D median %.2f wk  g_C(1)=%.4f\n", low$p, low$gD_med, low$gC1))
cat(sprintf("high-p: p=%.4f  g_D median %.2f wk  g_C(1)=%.4f\n\n", high$p, high$gD_med, high$gC1))
cat(sprintf("%4s %14s %16s %16s\n", "d", "observed down", "expected low-p", "expected high-p"))
obs_tot <- lo_tot <- hi_tot <- 0
for (d in 0:cD) {
  ts <- which(pmin(as.integer(ds[, 1]), cD) >= d)
  if (!length(ts)) next
  observed <- sum(pmax(-inc[ts, d + 1L, 1], 0))
  exp_lo <- sum(low$mu[ts])  * (1 - low$p)  * low$gW[d + 1L]
  exp_hi <- sum(high$mu[ts]) * (1 - high$p) * high$gW[d + 1L]
  cat(sprintf("%4d %14.0f %16.1f %16.1f\n", d, observed, exp_lo, exp_hi))
  obs_tot <- obs_tot + observed; lo_tot <- lo_tot + exp_lo; hi_tot <- hi_tot + exp_hi
}
cat(sprintf("%4s %14.0f %16.1f %16.1f\n", "tot", obs_tot, lo_tot, hi_tot))
cat(sprintf("\nshare of retractions landing by delay 1:  observed %.3f   low-p %.3f   high-p %.3f\n",
    sum(sapply(0:1, function(d) sum(pmax(-inc[, d+1L, 1], 0)))) / obs_tot,
    sum(low$gW[1:2]) / sum(low$gW[1:(cD+1)]), sum(high$gW[1:2]) / sum(high$gW[1:(cD+1)])))
