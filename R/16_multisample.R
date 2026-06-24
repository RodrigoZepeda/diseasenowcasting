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
#'   (`"multi"`, `"anchored"`, or `"onestage"`), and `n_samp` (imputations pooled).
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

  # Pool a list of [draws x max_time] matrices into the harness nowcast format.
  pool_and_summarise <- function(draws_list, rung, n_imputations) {
    pooled_draws <- if (is.list(draws_list)) do.call(rbind, draws_list) else draws_list
    target_draws <- pooled_draws[, target]
    list(nowcast = summarise_nowcast_matrix(pooled_draws), M = pooled_draws,
         quantiles = quantile(target_draws, probs = probs, na.rm = TRUE),
         median = stats::median(target_draws, na.rm = TRUE),
         rung = rung, n_samp = n_imputations, target = target,
         observed = { cc <- prepared_data$case_counts
                      if (is.matrix(cc)) rowSums(cc)[target] else cc[target] })
  }

  # -- Stage A: warm one-stage fit (free delay) --------------------------------
  warm_inits <- tryCatch({
    warm_fit <- fit(model, prepared_data, priors = priors_full)
    if (warm_fit$convergence == 0) warm_fit$parList else NULL
  }, error = function(e) NULL)

  is_nonparametric <- model@delay@num_id == 4L
  is_gengamma      <- model@delay@num_id == 3L

  # -- Two-stage DIRICHLET (non-parametric) ------------------------------------
  # Stage 1: NP delay-only fit on the FULL series (so the simplex dimension
  # n_bins matches Stage-2 -- the Dirichlet simplex cannot be windowed).  Impute
  # K simplices by sampling delay_logits from the Stage-1 Laplace posterior with
  # the covariance inflated by `np_spread` (the delay-only Laplace is
  # over-confident).  HARD-FIX each simplex in a warm Stage-2 fit (fast: Gstar
  # precomputed, PMF dropped) and pool the newest-event nowcast draws.
  if (is_nonparametric && !is.null(warm_inits)) {
    np_pool <- tryCatch({
      delay_data   <- prepare_data(model, m, X = X, d_star = d_star, max_time = max_time, delay_only = TRUE)
      stage1       <- fit(model, delay_data, priors = default_priors(model, delay_data))
      logits_mode  <- as.numeric(stage1$delay_logits)
      hessian      <- methods::as(stage1$obj$he(logits_mode), "sparseMatrix")
      precision    <- hessian / np_spread                       # inflate covariance by np_spread
      logit_draws  <- .sample_mvnorm_precision(logits_mode, precision, K)
      warm_epidemic_inits <- warm_inits[setdiff(names(warm_inits), "delay_logits")]
      pooled_draws <- list(); n_converged <- 0L
      for (k in seq_len(K)) {
        exp_logits <- exp(logit_draws[, k])
        imputed_simplex <- c(exp_logits, 1) / (sum(exp_logits) + 1)
        imputation_priors <- fix_param(priors_full, "delay_probs", imputed_simplex)
        imputation_draws <- tryCatch({
          imputation_fit <- fit(model, prepared_data, priors = imputation_priors, init = warm_epidemic_inits)
          if (imputation_fit$convergence != 0) NULL
          else .nowcast_draws(imputation_fit, target = target, n_draws = n_draws_per)$M
        }, error = function(e) NULL)
        # Keep only imputations whose Stage-2 fit converged.
      if (!is.null(imputation_draws)) {
        pooled_draws[[length(pooled_draws) + 1]] <- imputation_draws
        n_converged <- n_converged + 1L
      }
      }
      if (n_converged >= 1L) pool_and_summarise(pooled_draws, "multi", n_converged) else NULL
    }, error = function(e) NULL)
    if (!is.null(np_pool)) return(np_pool)
  }

  # -- Stage 1 (parametric): windowed delay-only estimate + SEs -----------------
  delay_estimate <- if (is_nonparametric) NULL else tryCatch({
    window <- .window_delay_m(m, max_time, delay_window)
    delay_data   <- prepare_data(model, window$m, max_time = window$max_time, delay_only = TRUE)
    delay_priors <- default_priors(model, delay_data)
    delay_fit    <- fit(model, delay_data, priors = delay_priors)
    list(mu = delay_fit$delay_mu, sigma = delay_fit$delay_sigma,
         mu_sd = delay_fit$delay_mu_sd, sigma_sd = delay_fit$delay_sigma_sd, shape_Q = delay_fit$delay_Q)
  }, error = function(e) NULL)

  # -- Rung 1: multiple imputation ---------------------------------------------
  if (!is.null(delay_estimate) && !is.null(warm_inits)) {
    spread_mu    <- max(floor_mu, if (is.finite(delay_estimate$mu_sd)) delay_estimate$mu_sd else 0)
    spread_sigma <- max(floor_sig_frac * delay_estimate$sigma,
                        if (is.finite(delay_estimate$sigma_sd)) delay_estimate$sigma_sd else 0)
    imputed_mu    <- rnorm(K, delay_estimate$mu, spread_mu)
    imputed_sigma <- pmax(0.05, rnorm(K, delay_estimate$sigma, spread_sigma))
    warm_epidemic_inits <- warm_inits[setdiff(names(warm_inits),
                                              c("delay_mu", "log_delay_sigma_excess", "delay_Q"))]
    pooled_draws <- list(); n_converged <- 0L
    for (k in seq_len(K)) {
      imputation_priors <- fix_param(fix_param(priors_full, "delay_mu", imputed_mu[k]),
                                     "delay_sigma", imputed_sigma[k])
      # GenGamma: also fix the shape Q (at its Stage-1 estimate) so the delay is
      # FULLY fixed -> Gstar is precomputed as data and the expensive pgamma CDF
      # is not re-evaluated each optimiser step (huge speedup for GG).
      if (is_gengamma && is.finite(delay_estimate$shape_Q %||% NA))
        imputation_priors <- fix_param(imputation_priors, "delay_Q", delay_estimate$shape_Q)
      imputation_draws <- tryCatch({
        imputation_fit <- fit(model, prepared_data, priors = imputation_priors, init = warm_epidemic_inits)
        if (imputation_fit$convergence != 0) NULL
        else .nowcast_draws(imputation_fit, target = target, n_draws = n_draws_per)$M
      }, error = function(e) NULL)
      # Keep only imputations whose Stage-2 fit converged.
      if (!is.null(imputation_draws)) {
        pooled_draws[[length(pooled_draws) + 1]] <- imputation_draws
        n_converged <- n_converged + 1L
      }
    }
    if (n_converged >= 1L) return(pool_and_summarise(pooled_draws, "multi", n_converged))
  }

  # -- Rung 2: anchored prior (delay free, recentred) --------------------------
  if (!is.null(delay_estimate)) {
    anchored_priors <- default_priors(model, prepared_data, phi = phi,
      delay_mu    = normal_prior(delay_estimate$mu, max(0.10, delay_estimate$mu_sd %||% 0.10)),
      delay_sigma = gamma_prior(4, 4 / max(0.5, delay_estimate$sigma)))
    anchored_draws <- tryCatch({
      anchored_fit <- fit(model, prepared_data, priors = anchored_priors, init = warm_inits)
      if (anchored_fit$convergence != 0) NULL
      else .nowcast_draws(anchored_fit, target = target, n_draws = n_draws_per * K)$M
    }, error = function(e) NULL)
    if (!is.null(anchored_draws)) return(pool_and_summarise(anchored_draws, "anchored", 1L))
  }

  # -- Rung 3: plain one-stage -------------------------------------------------
  onestage_fit   <- fit(model, prepared_data, priors = priors_full)
  onestage_draws <- .nowcast_draws(onestage_fit, target = target, n_draws = n_draws_per * K)$M
  pool_and_summarise(onestage_draws, "onestage", 1L)
}
