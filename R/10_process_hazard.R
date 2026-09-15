# =============================================================================
# Discrete-hazard regression helpers
# =============================================================================

#' Stable softplus used by the process-hazard regressions
#' @keywords internal
#' @noRd
.process_softplus <- function(x) .logspace_add(x, x * 0)

#' Tilt a baseline discrete distribution by a log-hazard design path
#'
#' @param baseline_cdf CDF after each successive discrete bin.
#' @param eta Linear predictor for the same bins.
#' @return Log PMF, log survival after each bin, and the CDF.
#' @keywords internal
#' @noRd
.process_hazard_path <- function(baseline_cdf, eta) {
  "[<-" <- RTMB::ADoverload("[<-")
  n <- length(baseline_cdf)
  zero <- baseline_cdf[1L] * 0
  baseline_pmf <- c(baseline_cdf[1L],
                    baseline_cdf[-1L] - baseline_cdf[-n])
  survival_before <- c(zero + 1, zero + 1 - baseline_cdf[-n])

  # The epsilon only protects finite-support terminal bins. Away from exact
  # endpoints it changes the zero-effect distribution below machine precision.
  eps <- 1e-12
  baseline_hazard <- (baseline_pmf + eps) / (survival_before + 2 * eps)
  linear <- log(baseline_hazard) - log1p(-baseline_hazard) + eta
  log_hazard <- -.process_softplus(-linear)
  log_one_minus_hazard <- -.process_softplus(linear)

  log_pmf <- log_hazard * 0
  log_survival <- log_hazard * 0
  accumulated <- zero
  for (index in seq_len(n)) {
    log_pmf[index] <- accumulated + log_hazard[index]
    accumulated <- accumulated + log_one_minus_hazard[index]
    log_survival[index] <- accumulated
  }
  list(
    log_pmf = log_pmf,
    log_survival = log_survival,
    cdf = -expm1(log_survival)
  )
}

#' Build reporting-hazard paths for every event-time by stratum cohort
#' @keywords internal
#' @noRd
.report_hazard_paths <- function(cdf_fn, n_time, n_strata, report_calendar,
                                 report_cohort, delay_beta,
                                 n_delay_calendar, n_delay_cohort) {
  baseline_cdf <- cdf_fn(seq_len(n_time))
  lapply(seq_len(n_strata), function(stratum) {
    lapply(seq_len(n_time), function(time) {
      destination <- time:n_time
      eta <- baseline_cdf[seq_along(destination)] * 0
      if (n_delay_calendar > 0L) {
        eta <- eta + as.vector(
          report_calendar[destination, , drop = FALSE] %*%
            delay_beta[seq_len(n_delay_calendar)]
        )
      }
      if (n_delay_cohort > 0L) {
        cohort_index <- n_delay_calendar + seq_len(n_delay_cohort)
        eta <- eta + sum(report_cohort[time, stratum, ] *
                           delay_beta[cohort_index])
      }
      .process_hazard_path(baseline_cdf[seq_along(destination)], eta)
    })
  })
}

#' Conditional reporting-delay likelihood under a hazard regression
#' @keywords internal
#' @noRd
.report_hazard_loglik <- function(paths, exact_rows, censored_rows, d_star) {
  loglik <- paths[[1L]][[1L]]$log_pmf[1L] * 0
  if (nrow(exact_rows) > 0L) for (row in seq_len(nrow(exact_rows))) {
    time <- as.integer(exact_rows[row, 1L])
    delay <- as.integer(exact_rows[row, 3L])
    stratum <- if (ncol(exact_rows) >= 4L) as.integer(exact_rows[row, 4L]) else 1L
    path <- paths[[stratum]][[time]]
    horizon <- as.integer(d_star[time, stratum]) + 1L
    loglik <- loglik + exact_rows[row, 2L] *
      (path$log_pmf[delay] - log(path$cdf[horizon]))
  }
  if (nrow(censored_rows) > 0L) for (row in seq_len(nrow(censored_rows))) {
    time <- as.integer(censored_rows[row, 1L])
    delay <- as.integer(censored_rows[row, 3L])
    stratum <- if (ncol(censored_rows) >= 4L) as.integer(censored_rows[row, 4L]) else 1L
    path <- paths[[stratum]][[time]]
    horizon <- as.integer(d_star[time, stratum]) + 1L
    loglik <- loglik + censored_rows[row, 2L] *
      (log(path$cdf[delay]) - log(path$cdf[horizon]))
  }
  loglik
}
