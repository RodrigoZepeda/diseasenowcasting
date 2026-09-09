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
  list(m = mw, max_time = as.integer(max_time - since + 1L))
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
