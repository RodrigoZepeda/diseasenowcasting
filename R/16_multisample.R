# =============================================================================
# Two-stage multiple-imputation nowcast (the COVID-winning cascade)
# =============================================================================
# Mirrors devel/covid_multisample_lognormal.R from diseasenowcast2:
#   Stage A : warm one-stage joint fit (free delay) -> warm epidemic inits.
#   Stage 1 : delay-only fit on a recent censored window -> (mu_hat, sigma_hat)
#             plus Laplace SEs (floored, since the delay-only Laplace is
#             over-confident).
#   Rung 1  : K delay imputations around the Stage-1 estimate, each HARD-FIXED
#             in a warm Stage-2 joint fit; pool the newest-event nowcast draws.
#   Rung 2  : anchored prior (delay free, default families recentred).
#   Rung 3  : plain one-stage fit.
# Pooling over the delay spread re-injects the right-skewed (1/G*) delay
# uncertainty that hard-fixing alone loses, restoring nowcast coverage while
# keeping every Stage-2 fit well-conditioned.
# =============================================================================

#' Window a full observation matrix to its most recent `W` event-times
#' (re-indexed to 1..W) for the delay-only Stage 1.
#' @keywords internal
#' @noRd
.window_delay_m <- function(m, max_time, W) {
  since <- max(1L, max_time - W + 1L)
  keep <- m[, 1] >= since
  mw <- m[keep, , drop = FALSE]
  mw[, 1] <- mw[, 1] - since + 1L
  # `since` is returned because a reporting regression has to be windowed too:
  # its calendar and cohort designs are indexed on the FULL event grid, and the
  # window re-indexes time, so the caller must slice rows `since..max_time` to
  # keep destination dates lined up with the re-indexed cohorts.
  list(m = mw, max_time = as.integer(max_time - since + 1L), since = since)
}

#' Slice a reporting regression's designs onto a Stage-1 window
#'
#' Window cohort `t_w` is global event time `since + t_w - 1`, and its bin `k`
#' lands on global destination `since + t_w + k - 2`.  Dropping the first
#' `since - 1` calendar rows makes that the same arithmetic in window indices.
#' @keywords internal
#' @noRd
.window_report_designs <- function(engine, since) {
  rows <- since:engine$max_time
  list(
    report_calendar = if (ncol(engine$report_calendar) > 0L)
      engine$report_calendar[rows, , drop = FALSE] else NULL,
    report_cohort = if (dim(engine$report_cohort)[3L] > 0L)
      engine$report_cohort[rows, , , drop = FALSE] else NULL
  )
}

#' Two-stage multiple-imputation nowcast
#'
#' @param model A [model()] object (LogNormal delay).
#' @param m Observation matrix `[event_time, count, delay, strata...]`.
#' @param X Optional covariate matrix (`max_time` rows).
#' @param d_star Optional max-observable-delay vector.
#' @param max_time Time-window length; defaults to `max(m[, 1])`.
#' @param target Event-time to nowcast (default newest).
#' @param delay_window Recent window length for the Stage-1 delay fit.
#' @param K Number of delay imputations.
#' @param floor_mu Floor on the log-mean imputation SD (parametric families).
#' @param floor_sig_frac Floor on the delay-SD imputation SD (fraction of sigma).
#' @param np_spread Dirichlet only: covariance-inflation factor for the simplex
#'   imputation (samples `delay_logits` from the Stage-1 Laplace posterior with
#'   covariance scaled by `np_spread`).  Default 1 (the raw, well-informed
#'   full-series posterior); values > 1 widen the simplex spread.
#' @param n_draws_per Posterior nowcast draws per imputation.
#' @param phi NB overdispersion prior (default `lognormal_prior(log(20), 0.5)`).
#' @param probs Quantile probabilities to report.
#' @param seed Optional RNG seed.
#' @returns A list with `quantiles`, `median`, pooled `draws`, the `rung` used
#'   (`"multi"`, `"anchored"`, or `"onestage"`), `n_samp` (imputations pooled),
#'   and `fit_diagnostics`. The latter records the requested and retained
#'   imputation counts plus warm, Stage-1, fallback, exclusion, and retained-fit
#'   diagnostics.
#' @export
nowcast_twostage <- function(model, m, X = NULL, d_star = NULL, max_time = NULL,
                             target = NULL, delay_window = 120L, K = 25L,
                             floor_mu = 0.08, floor_sig_frac = 0.08,
                             np_spread = 1,
                             n_draws_per = 200L,
                             phi = lognormal_prior(log(20), 0.5),
                             probs = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975),
                             seed = sample.int(.Machine$integer.max, 1)) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(max_time)) max_time <- max(m[, 1])
  target <- target %||% max_time

  prepared_data <- prepare_data(model, m, X = X, d_star = d_star, max_time = max_time, delay_only = FALSE)
  priors_full   <- default_priors(model, prepared_data, phi = phi)

  # Keep this legacy matrix interface on exactly the same fitting cascade and
  # adequacy policy as `nowcast(tbl_now, type = "two_stage")`.  In particular,
  # warm and Stage-1 fits are diagnostic inputs, only adequate Stage-2 fits are
  # pooled, and any warning describes the final retained/excluded collection.
  collected <- .collect_nowcast_fits(
    model, prepared_data, priors_full,
    type = "two_stage", K = K,
    floor_mu = floor_mu, floor_sig_frac = floor_sig_frac,
    np_spread = np_spread, delay_window = delay_window
  )

  draws_per_fit <- if (identical(collected$rung, "multi")) {
    n_draws_per
  } else {
    n_draws_per * K
  }
  pooled <- .pool_fit_draws(
    collected$fits, target = target, n_draws = draws_per_fit
  )
  collected$diagnostics$laplace_sampling <- pooled$laplace_sampling
  .warn_laplace_sampling(collected$diagnostics)
  target_draws <- pooled$M[, target]
  case_counts <- prepared_data$case_counts

  list(
    nowcast = summarise_nowcast_matrix(pooled$M),
    M = pooled$M,
    quantiles = quantile(target_draws, probs = probs, na.rm = TRUE),
    median = stats::median(target_draws, na.rm = TRUE),
    rung = collected$rung,
    n_samp = if (identical(collected$rung, "multi")) length(collected$fits) else 1L,
    target = target,
    observed = if (is.matrix(case_counts)) rowSums(case_counts)[target] else case_counts[target],
    fit_diagnostics = collected$diagnostics
  )
}

# =============================================================================
# Stage-1 imputation draws
# =============================================================================
# Stage 2 hard-fixes the reporting process at K imputed values, so the draws ARE
# the delay uncertainty the pooled nowcast carries.  There are two ways to make
# them, and which one is used depends only on whether a reporting regression is
# active:
#
#   stationary  independent normals on (delay_mu, delay_sigma) with the tuned
#               `floor_mu` / `floor_sig_frac` spreads.  Left exactly as it was:
#               the published benchmark and the convergence behaviour on the
#               real datasets were tuned against this, and it is not this
#               feature's business to move them.
#
#   regression  one draw from the Stage-1 joint Laplace over the whole free
#               parameter vector.  `delay_beta` and the baseline are strongly
#               correlated a posteriori -- a weekend effect and a wider sigma
#               explain overlapping variation in the same delays -- so drawing
#               them independently would misstate the propagated uncertainty.
#               The floors survive as a MINIMUM marginal spread.
# =============================================================================

#' Draw the whole Stage-1 parameter vector from its joint Laplace
#'
#' Draws are taken on the UNCONSTRAINED scale the objective optimises, which is
#' also what retires the `pmax(0.05, .)` truncation the natural-scale draws
#' needed: `delay_sigma = 0.01 + exp(log_delay_sigma_excess)` is positive by
#' construction.
#' @param delay_fit A Stage-1 `fit()` result (needs `$obj`).
#' @param K Number of imputations.
#' @param spread Divides the precision, widening the draws (as `np_spread` does).
#' @returns A `[n_parameter x K]` matrix with parameter names as rownames.
#' @keywords internal
#' @noRd
.stage1_joint_draws <- function(delay_fit, K, spread = 1) {
  mode <- delay_fit$obj$env$last.par.best
  precision <- methods::as(delay_fit$obj$he(mode), "sparseMatrix") / spread
  draws <- .sample_mvnorm_precision(as.numeric(mode), precision, K)
  rownames(draws) <- names(mode)
  draws
}

#' Widen selected coordinates to a minimum marginal spread
#'
#' Scaling one coordinate's deviations from the mode multiplies its marginal SD
#' and leaves the correlation matrix alone, so the tuned floors can be imposed
#' without discarding the joint structure that made the draw worth taking.
#' @param minimum_sd Named numeric: parameter name -> smallest acceptable SD.
#' @keywords internal
#' @noRd
.inflate_marginal_spread <- function(draws, minimum_sd) {
  centre <- rowMeans(draws)
  for (parameter in names(minimum_sd)) {
    rows <- which(rownames(draws) == parameter)
    if (!length(rows) || !is.finite(minimum_sd[[parameter]])) next
    observed <- apply(draws[rows, , drop = FALSE], 1L, stats::sd)
    widen <- pmax(1, minimum_sd[[parameter]] / pmax(observed, 1e-12))
    draws[rows, ] <- centre[rows] +
      (draws[rows, , drop = FALSE] - centre[rows]) * widen
  }
  draws
}

#' Turn Stage-1 draws into the per-imputation values Stage 2 fixes
#'
#' Parameters Stage 1 held fixed never appear in its draw, so they fall back to
#' the fitted value.
#' @keywords internal
#' @noRd
.stage1_imputations <- function(delay_fit, K, n_delay_covariates, is_gengamma,
                                floor_mu, floor_sig_frac) {
  fitted_sigma <- delay_fit$delay_sigma
  # A floor stated on the natural sigma scale becomes, by the delta method,
  # floor / (sigma - 0.01) on the log-excess scale the objective uses.
  minimum_sd <- c(
    delay_mu = floor_mu,
    log_delay_sigma_excess = if (is.finite(fitted_sigma) && fitted_sigma > 0.02)
      floor_sig_frac * fitted_sigma / (fitted_sigma - 0.01) else NA_real_
  )
  draws <- .inflate_marginal_spread(.stage1_joint_draws(delay_fit, K), minimum_sd)
  value <- function(parameter, column, fallback) {
    rows <- which(rownames(draws) == parameter)
    if (!length(rows)) return(fallback)
    draws[rows, column]
  }
  lapply(seq_len(K), function(k) list(
    delay_mu = value("delay_mu", k, delay_fit$delay_mu),
    delay_sigma = {
      excess <- value("log_delay_sigma_excess", k, NA_real_)
      if (is.na(excess)) fitted_sigma else 0.01 + exp(excess)
    },
    delay_Q = if (is_gengamma)
      .gengamma_shape_transform(value("delay_Q", k, NA_real_))$shape_Q else NA_real_,
    delay_beta = if (n_delay_covariates > 0L)
      as.numeric(value("delay_beta", k, rep(0, n_delay_covariates))) else numeric(0)
  ))
}
