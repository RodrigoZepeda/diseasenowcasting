# =============================================================================
# Phase 1 step 2 + deliverable C: SPA error on the ACTUAL FluSight cells, under
# both the pathological low-p and the plausible high-p parameter vectors.
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
invisible(capture.output(source("devel/spa_diagnostics/01_exact_reference.R")))

START <- as.Date("2023-09-23")
STATE <- "Texas"

raw <- tbl.now::flusight |>
  filter(location_name == STATE, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))
tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
              case_count = observation, data_type = "count-cumulative",
              verbose = FALSE) |> align_weeks(date_col = "report_date")

fit_with <- function(p_spec) {
  suppressMessages(suppressWarnings(nowcast(
    tn, model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
              validation = validation_process(p = p_spec)),
    type = "one_stage", n_draws = 50, temporal_effects = "none", seed = 1)))
}

# Rebuild the (z, alpha, omega) cells exactly as the objective forms them.
cells_of <- function(fitted) {
  fit    <- fitted@fits[[1]]
  engine <- fitted@engine
  priors <- fitted@priors
  conf_D <- min(as.integer(engine$max_conf_delay) - 1L, 15L)

  p <- if (isTRUE(priors$confirm_p$is_constant == 1L)) priors$confirm_p$fixed
       else stats::plogis(as.numeric(fit$parList$logit_confirm_p))

  delay_fns <- diseasenowcasting:::.delay_distribution_functions(
    as.integer(engine$delay_family), as.numeric(fit$delay_mu), as.numeric(fit$delay_sigma))
  appearance_cdf <- as.numeric(delay_fns$cdf(seq_len(conf_D + 1L)))
  g_D <- c(appearance_cdf[1], diff(appearance_cdf))

  retract_mu <- if (isTRUE(priors$retract_mu$is_constant == 1L)) priors$retract_mu$fixed
                else as.numeric(fit$parList$retract_mu)
  retract_sd <- if (isTRUE(priors$retract_sigma$is_constant == 1L)) priors$retract_sigma$fixed
                else 0.01 + exp(as.numeric(fit$parList$log_retract_sd_exc))
  retract_fns <- diseasenowcasting:::.delay_distribution_functions(
    as.integer(priors$retract_family), retract_mu, retract_sd)
  retract_cdf <- c(0, as.numeric(retract_fns$cdf(seq_len(conf_D))))
  g_C <- c(0, diff(retract_cdf))
  g_W <- diseasenowcasting:::.convolve_delays(g_D, g_C)

  lambda <- as.numeric(fit$lambda)
  mu_stream  <- lambda / p
  eta_stream <- (1 - p) * mu_stream
  d_star <- engine$d_star
  increments <- engine$increment_array

  out <- list()
  for (t in seq_len(engine$max_time)) {
    horizon <- min(as.integer(d_star[t, 1]), conf_D)
    if (horizon < 0L) next
    for (d in 0:horizon) {
      out[[length(out) + 1L]] <- c(t = t, d = d,
        z = increments[t, d + 1L, 1],
        alpha = mu_stream[t]  * g_D[d + 1L],
        omega = eta_stream[t] * g_W[d + 1L])
    }
  }
  frame <- as.data.frame(do.call(rbind, out))
  frame$p <- p
  frame$delay_median  <- exp(as.numeric(fit$delay_mu))
  frame$retract_median <- exp(retract_mu)
  frame
}

report <- function(label, frame) {
  # d = 0 is bin_type 0 -- an exact dpois, no approximation at all.  Everything
  # with d >= 1 is bin_type 1, the saddlepoint/series blend.
  mixed <- frame[frame$d >= 1L, , drop = FALSE]
  e <- vapply(seq_len(nrow(mixed)), function(i) {
    diseasenowcasting:::.log_skellam_increment(mixed$z[i], mixed$alpha[i], mixed$omega[i], 1L) -
      log_skellam_exact(mixed$z[i] , mixed$alpha[i] + 1e-8, mixed$omega[i] + 1e-8)
  }, numeric(1))
  mixed$e <- e

  cat(sprintf("\n--- %s  (p_hat = %.4f, g_D median = %.2f wk, g_C median = %.3f wk) ---\n",
              label, frame$p[1], frame$delay_median[1], frame$retract_median[1]))
  cat(sprintf("  cells: %d total, %d at d=0 (exact Poisson), %d mixed (approximated)\n",
              nrow(frame), sum(frame$d == 0L), nrow(mixed)))
  cat(sprintf("  alpha: median %.2f  p90 %.2f  max %.1f\n",
              median(mixed$alpha), quantile(mixed$alpha, .9), max(mixed$alpha)))
  cat(sprintf("  omega: median %.2f  p90 %.2f  max %.1f\n",
              median(mixed$omega), quantile(mixed$omega, .9), max(mixed$omega)))
  cat(sprintf("  alpha+omega > 35 (the penalised band) in %.1f%% of mixed cells\n",
              100 * mean(mixed$alpha + mixed$omega > 35)))
  cat(sprintf("  |e|: max %.3e  median %.3e  p90 %.3e  p99 %.3e\n",
              max(abs(e)), median(abs(e)), quantile(abs(e), .9), quantile(abs(e), .99)))
  cat(sprintf("  signed: mean %.3e   TOTAL %.4f nats\n", mean(e), sum(e)))
  for (grp in list(list("z = 0", mixed$z == 0), list("z > 0", mixed$z > 0),
                   list("z < 0", mixed$z < 0))) {
    idx <- grp[[2]]
    if (any(idx)) cat(sprintf("    %-6s n=%4d  total e = %+.4f  mean e = %+.3e\n",
                              grp[[1]], sum(idx), sum(e[idx]), mean(e[idx])))
  }
  list(frame = frame, mixed = mixed,
       L_spa = sum(vapply(seq_len(nrow(frame)), function(i)
         diseasenowcasting:::.log_skellam_increment(
           frame$z[i], frame$alpha[i], frame$omega[i], if (frame$d[i] == 0L) 0L else 1L),
         numeric(1))),
       L_exact = sum(vapply(seq_len(nrow(frame)), function(i)
         log_skellam_exact(frame$z[i], frame$alpha[i] + 1e-8, frame$omega[i] + 1e-8),
         numeric(1))),
       worst = mixed[which.max(abs(e)), , drop = FALSE])
}

low  <- fit_with(beta_prior(9.6, 0.4))   # weak Beta -> the free fit runs to p ~ 0.1
high <- fit_with(0.9572)                 # the empirical rate, held fixed

low_res  <- report("LOW-p  (free, weak Beta)", cells_of(low))
high_res <- report("HIGH-p (fixed at 0.9572)", cells_of(high))

cat("\n=== C. Existing-solution comparison ===\n")
cat(sprintf("%-24s %14s %14s %12s\n", "solution", "SPA logL", "exact logL", "SPA - exact"))
cat(sprintf("%-24s %14.4f %14.4f %12.4f\n", "low p",
            low_res$L_spa, low_res$L_exact, low_res$L_spa - low_res$L_exact))
cat(sprintf("%-24s %14.4f %14.4f %12.4f\n", "high p",
            high_res$L_spa, high_res$L_exact, high_res$L_spa - high_res$L_exact))
cat(sprintf("\n  delta_SPA   (low - high) = %+.4f\n", low_res$L_spa   - high_res$L_spa))
cat(sprintf("  delta_exact (low - high) = %+.4f\n", low_res$L_exact - high_res$L_exact))
cat(sprintf("  SPA bonus to the low-p solution = %+.4f nats\n",
            (low_res$L_spa - high_res$L_spa) - (low_res$L_exact - high_res$L_exact)))
saveRDS(list(low = low_res, high = high_res), "devel/spa_diagnostics/flusight_cells.rds")
