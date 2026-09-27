# =============================================================================
# Discrete-hazard regressions for the reporting and revision processes
# =============================================================================
# A calendar effect cannot be pushed into the location parameter of a delay law,
# because the destination date is the OUTCOME (`r = t + d`).  Each process
# therefore keeps its delay family as a BASELINE HAZARD and the regression tilts
# that hazard on the log-odds scale:
#
#   logit h(k) = logit h0(k) + eta(k)
#   log g(k)   = log h(k) + sum_{u < k} log(1 - h(u))
#   log S(k)   = sum_{u <= k} log(1 - h(u))
#
# At `eta = 0` this reproduces the baseline law exactly; test-process-hazard.R
# asserts that against the family bundles and against the objectives.
#
# The baseline hazard is read off the log-SURVIVAL, never off differences of the
# CDF.  Writing `h0(k) = 1 - S(k)/S(k-1)` gives
#
#   log(1 - h0(k)) = log S(k) - log S(k-1)      (S(0) = 1)
#   logit h0(k)    = log(-expm1(log(1 - h0(k)))) - log(1 - h0(k))
#
# and both stay accurate arbitrarily deep into the tail.  Differencing the CDF
# instead loses every bin past the point where `F` saturates to 1 in double
# precision -- delay 16 for a LogNormal with mean 3 and SD 0.6 -- which used to
# leave the tail hazards pinned at a floor and made long delays tens of
# log-units too cheap.
# =============================================================================

#' Stable softplus used by the process-hazard regressions
#' @keywords internal
#' @noRd
.process_softplus <- function(x) .logspace_add(x, x * 0)

#' Baseline log-survival evaluated the way the delay likelihood evaluates it
#'
#' Mirrors the split in [.discretised_delay_loglik()].  Below `split_delay` the
#' survival is taken from the log-CDF, which is where a family's `log_cdf` is the
#' accurate one (the generalized Gamma's `log_survival` is a Wilson-Hilferty
#' approximation that is only good in the tail).  Above it the family's own
#' `log_survival` is used, which is where `log1p(-exp(log F))` would cancel.
#' `split_delay` is data, so the partition is AD-safe.
#'
#' @param delay_fns Family bundle with `log_cdf` and `log_survival`.
#' @param delays Delay values (data) to evaluate, in increasing order.
#' @param split_delay Delay separating the two stable expressions.
#' @returns `log S(delay)` for each requested delay.
#' @keywords internal
#' @noRd
.stable_log_survival <- function(delay_fns, delays, split_delay) {
  # Families whose log-survival is exact everywhere (LogNormal, Gamma, the
  # Dirichlet simplex, and any custom delay that supplied its own) need no
  # head branch at all: taking log S from the log-CDF there would only throw
  # away the tail they can represent.
  if (!isTRUE(delay_fns$survival_is_approximate)) split_delay <- 0
  head_delays <- delays <= split_delay
  result <- delay_fns$log_survival(delays) * 0
  if (any(head_delays)) {
    result[head_delays] <-
      log(-expm1(delay_fns$log_cdf(delays[head_delays])))
  }
  if (any(!head_delays)) {
    result[!head_delays] <- delay_fns$log_survival(delays[!head_delays])
  }
  .floor_log_survival(result)
}

#' Keep a log-survival finite without clipping anything representable
#'
#' A family can return `-Inf` where its own tail underflows: `pgamma2` gives up
#' around `S = 1e-17`, and the log-CDF branch as soon as `F` rounds to 1.  The
#' hazard recursion differences consecutive entries, so two `-Inf` in a row would
#' become `NaN` and poison the whole path.
#'
#' This is `max(log S, floor)` to machine precision, written smoothly so it is
#' AD-safe (`pmax()` and `.logspace_add()` are both `Inf - Inf` at `-Inf`).  Its
#' derivative is 1 above the floor and 0 below, which is what we want: nothing
#' below the floor is recoverable.  Dividing by `scale` before exponentiating is
#' what lets the floor sit at `-20000` instead of the `-745` that `exp(log S)`
#' would impose on its own, so a tail the family *can* still represent -- a tight
#' delay law evaluated far out, say -- is never flattened.
#' @keywords internal
#' @noRd
.floor_log_survival <- function(log_survival, floor = -20000, scale = 32) {
  log(exp(log_survival / scale) + exp(floor / scale)) * scale
}

#' Tilt a baseline discrete law by a log-hazard-odds design path
#'
#' @param baseline_log_survival `log S(k)` after each successive bin `k`.
#' @param eta Linear predictor for the same bins.
#' @returns Log PMF, log survival, log CDF and CDF after each bin.
#' @keywords internal
#' @noRd
.process_hazard_path <- function(baseline_log_survival, eta) {
  n <- length(baseline_log_survival)
  zero <- baseline_log_survival[1L] * 0
  # log(1 - h0(k)) = log S(k) - log S(k - 1), with S(0) = 1.
  log_retention <- baseline_log_survival - c(zero, baseline_log_survival[-n])
  # The addend keeps log(0) off the tape for a bin that carries no mass at all
  # (the head of a delay law whose support starts later).  Unlike a
  # hazard-scale floor it cancels out of the derivative: the chain rule pairs
  # 1 / (h0 + addend) with a hazard that is itself h0 + addend.
  linear <- log(-expm1(log_retention) + 1e-300) - log_retention + eta
  log_hazard <- -.process_softplus(-linear)
  log_one_minus_hazard <- -.process_softplus(linear)

  # log S(k) = sum_{u <= k} log(1 - h(u)), and the mass at bin k is what survived
  # to the START of it, log S(k - 1) = log S(k) - log(1 - h(k)).  Written as one
  # cumsum rather than an indexed loop: the loop put two scalar nodes plus an
  # `[<-` on the tape for every bin of every cohort, which is O(strata x time^2)
  # nodes on a daily series and dominated the tape build.
  log_survival <- cumsum(log_one_minus_hazard)
  log_pmf <- log_survival - log_one_minus_hazard + log_hazard
  log_cdf <- log(-expm1(log_survival) + 1e-300)
  list(
    log_pmf = log_pmf,
    log_survival = log_survival,
    log_cdf = log_cdf,
    cdf = -expm1(log_survival)
  )
}

#' Linear predictor for one hazard path
#'
#' `calendar` is indexed by destination date and varies along the path;
#' `row_values` is a single design row whose effect is constant along it.
#'
#' @param template An AD vector of the path's length, used only for its shape.
#' @param calendar Destination-date design matrix (may have zero columns).
#' @param destination Calendar row index for each bin of the path.
#' @param row_values Design row held constant along the path, or `NULL`.
#' @param coefficients The process's coefficient vector: calendar block first,
#'   row block second.
#' @keywords internal
#' @noRd
.process_hazard_eta <- function(template, calendar, destination, row_values,
                                coefficients, n_calendar, n_row) {
  eta <- template * 0
  if (n_calendar > 0L) {
    eta <- eta + as.vector(
      calendar[destination, , drop = FALSE] %*% coefficients[seq_len(n_calendar)]
    )
  }
  if (n_row > 0L) {
    eta <- eta + sum(row_values * coefficients[n_calendar + seq_len(n_row)])
  }
  eta
}

#' Reporting-hazard paths for every event-time by stratum cohort
#'
#' The cohort at event time `t` can only be reported on dates `t .. n_time`, so
#' its path has `n_time - t + 1` bins and bin `k` lands on calendar row
#' `t + k - 1`.  Every cohort tilts the SAME baseline, so the baseline survival
#' is evaluated once and sliced.
#' @keywords internal
#' @noRd
.report_hazard_paths <- function(baseline_log_survival, n_time, n_strata,
                                 report_calendar, report_cohort, delay_beta,
                                 n_delay_calendar, n_delay_cohort) {
  lapply(seq_len(n_strata), function(stratum) {
    lapply(seq_len(n_time), function(time) {
      destination <- time:n_time
      baseline <- baseline_log_survival[seq_along(destination)]
      eta <- .process_hazard_eta(
        baseline, report_calendar, destination,
        if (n_delay_cohort > 0L) report_cohort[time, stratum, ] else NULL,
        delay_beta, n_delay_calendar, n_delay_cohort
      )
      .process_hazard_path(baseline, eta)
    })
  })
}

#' Revision-hazard path for one report row
#'
#' `lag_offset` is 0 when the lag support starts at 1 (retraction) and 1 when it
#' starts at 0 (confirmation), so bin `k` lands on calendar row
#' `report_time + k - lag_offset`.
#' @keywords internal
#' @noRd
.revision_hazard_path <- function(baseline_log_survival, report_time,
                                  path_length, lag_offset, revision_calendar,
                                  row_values, revision_beta,
                                  n_revision_calendar, n_revision_row) {
  baseline <- baseline_log_survival[seq_len(path_length)]
  destination <- report_time + seq_len(path_length) - lag_offset
  eta <- .process_hazard_eta(
    baseline, revision_calendar, destination,
    if (n_revision_row > 0L) row_values else NULL,
    revision_beta, n_revision_calendar, n_revision_row
  )
  .process_hazard_path(baseline, eta)
}

#' Conditional reporting-delay likelihood under a hazard regression
#'
#' The delay-only objective conditions on being reported by the horizon, so each
#' row divides by `G(d_t^star)`; the joint objective does not (its `S_k` factor
#' carries the truncation) and calls the paths directly.
#' @keywords internal
#' @noRd
.report_hazard_loglik <- function(paths, exact_rows, censored_rows, d_star) {
  zero <- paths[[1L]][[1L]]$log_pmf[1L] * 0
  zero +
    .report_hazard_row_loglik(paths, exact_rows, FALSE, d_star) +
    .report_hazard_row_loglik(paths, censored_rows, TRUE, d_star)
}

#' Sum a block of observation rows against their cohorts' hazard paths
#'
#' Rows are grouped by `(event time, stratum)` so each cohort's path is indexed
#' once with a vector of delays rather than once per row.  Row-at-a-time
#' accumulation put one node on the tape for every observation, which on a
#' stratified daily series is tens of thousands of nodes that carry no extra
#' information.
#'
#' @param use_cdf `TRUE` for right-censored rows, whose delay is an upper bound.
#' @param d_star Per-cohort horizon; `NULL` to skip the conditioning term (the
#'   joint objective's `S_k` factor already carries the truncation).
#' @keywords internal
#' @noRd
.report_hazard_row_loglik <- function(paths, rows, use_cdf, d_star = NULL) {
  if (is.null(rows) || !nrow(rows)) return(0)
  times <- as.integer(rows[, 1L])
  strata <- if (ncol(rows) >= 4L) as.integer(rows[, 4L]) else rep(1L, nrow(rows))
  loglik <- 0
  for (group in split(seq_len(nrow(rows)), list(times, strata), drop = TRUE)) {
    time <- times[group[1L]]
    stratum <- strata[group[1L]]
    path <- paths[[stratum]][[time]]
    counts <- rows[group, 2L]
    delays <- as.integer(rows[group, 3L])
    observed <- if (use_cdf) path$log_cdf[delays] else path$log_pmf[delays]
    loglik <- loglik + sum(counts * observed)
    if (!is.null(d_star)) {
      horizon <- as.integer(d_star[time, stratum]) + 1L
      loglik <- loglik - sum(counts) * path$log_cdf[horizon]
    }
  }
  loglik
}

#' Finite-horizon count-cumulative kernel for one cohort
#'
#' Builds the report and revision timing laws this `(event time, stratum)`
#' cohort actually faces and feeds them to
#' [.count_cumulative_components_varying()].  When neither process carries a
#' regression the caller should use the stationary components instead; this
#' helper still reproduces them exactly, but at cohort-by-cohort cost.
#'
#' `report` and `revision` are the process descriptions assembled by
#' [.cumulative_hazard_spec()].
#' @keywords internal
#' @noRd
.cumulative_cohort_components <- function(time, stratum, settlement,
                                          report, revision) {
  report_pmf <- report$pmf
  if (report$active) {
    baseline <- report$log_survival[seq_len(settlement + 1L)]
    eta <- .process_hazard_eta(
      baseline, report$calendar, time + 0:settlement,
      if (report$n_row > 0L) report$cohort[time, stratum, ] else NULL,
      report$beta, report$n_calendar, report$n_row
    )
    report_pmf <- exp(.process_hazard_path(baseline, eta)$log_pmf)
    report_pmf <- report_pmf / sum(report_pmf)
  }
  revision_by_report <- lapply(
    seq_len(settlement + 1L), function(index) revision$pmf
  )
  if (revision$active) {
    baseline <- revision$log_survival[seq_len(settlement)]
    for (report_delay in 0:settlement) {
      eta <- .process_hazard_eta(
        baseline, revision$calendar,
        time + report_delay + seq_len(settlement), NULL,
        revision$beta, revision$n_calendar, 0L
      )
      pmf <- exp(.process_hazard_path(baseline, eta)$log_pmf)
      revision_by_report[[report_delay + 1L]] <- pmf / sum(pmf)
    }
  }
  .count_cumulative_components_varying(
    report_pmf, revision_by_report, revision$mass, settlement
  )
}

#' Assemble the process descriptions [.cumulative_cohort_components()] consumes
#' @keywords internal
#' @noRd
.cumulative_hazard_spec <- function(pmf, log_survival, active, calendar, beta,
                                    n_calendar, cohort = NULL, n_row = 0L,
                                    mass = NULL) {
  list(pmf = pmf, log_survival = log_survival, active = isTRUE(active),
       calendar = calendar, beta = beta, n_calendar = n_calendar,
       cohort = cohort, n_row = n_row, mass = mass)
}

#' Split delays the objectives use to pick the stable log-survival expression
#'
#' The tape derives these from the observed delay/lag tables; the plain-R mirror
#' in `.joint_reconstruct()` must use exactly the same values, or its baseline
#' hazards would differ from the ones the likelihood was maximised under.
#' @keywords internal
#' @noRd
.delay_split <- function(data) {
  if (is.null(data$m) || !nrow(data$m)) return(2)
  max(2, .wtd_median(data$m[, 3], data$m[, 2]))
}

#' @rdname dot-delay_split
#' @keywords internal
#' @noRd
.retract_split <- function(data) {
  table <- data$retract_table
  if (is.null(table) || !nrow(table)) return(2)
  max(2, .wtd_median(table[, "lag"], table[, "count"]))
}
