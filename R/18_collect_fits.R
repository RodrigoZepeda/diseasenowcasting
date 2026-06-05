# =============================================================================
# Collect the fitted objects for a nowcast (one- or two-stage), WITHOUT drawing
# =============================================================================
# This is the "fit only" core behind the lazy S7 nowcast(): it returns the
# underlying RTMB fit object(s) so prediction (Laplace sampling + count draws)
# can be deferred to predict()/mean()/median()/etc.  The two-stage path returns
# the K converged delay-imputed Stage-2 fits (pooling over them re-injects the
# right-skewed delay uncertainty); the one-stage path returns a single joint fit.
# =============================================================================

#' Collect nowcast fit object(s) from prepared engine data (no prediction)
#'
#' @param model A [model()] object.
#' @param engine Prepared-data list from [prepare_data()] (`delay_only = FALSE`).
#' @param priors Prior bundle from [default_priors()].
#' @param type `"two_stage"` (default) or `"one_stage"`.
#' @param K Delay imputations for the two-stage path.
#' @param floor_mu,floor_sig_frac Imputation-spread floors (parametric families).
#' @param np_spread Dirichlet simplex imputation covariance inflation.
#' @param delay_window Recent window length for the parametric Stage-1 delay fit.
#' @returns list(`fits` = list of fit objects, `rung`, `target`).
#' @keywords internal
#' @noRd
.collect_nowcast_fits <- function(model, engine, priors, type = "two_stage",
                                  K = 25L, floor_mu = 0.15, floor_sig_frac = 0.25,
                                  np_spread = 1, delay_window = 120L, warm_inits = NULL) {
  target <- engine$max_time

  if (type == "one_stage") {
    return(list(fits = list(fit(model, engine, priors = priors, init = warm_inits)),
                rung = "onestage", target = target))
  }

  m <- engine$m; max_time <- engine$max_time
  is_nonparametric <- model@delay@num_id == 4L

  # Stage A: warm one-stage fit (free delay) -> warm epidemic inits.  `update()`
  # supplies `warm_inits` from the previous fit to skip this cold fit.
  if (is.null(warm_inits)) warm_inits <- tryCatch({
    warm_fit <- fit(model, engine, priors = priors)
    if (warm_fit$convergence == 0) warm_fit$parList else NULL
  }, error = function(e) NULL)

  # -- Two-stage DIRICHLET (simplex imputation) --------------------------------
  if (is_nonparametric && !is.null(warm_inits)) {
    np_fits <- tryCatch({
      delay_engine <- prepare_data(model, m, X = engine$X, d_star = matrix(engine$d_star, ncol = 1),
                                   max_time = max_time, delay_only = TRUE)
      stage1 <- fit(model, delay_engine, priors = default_priors(model, delay_engine))
      logits_mode <- as.numeric(stage1$delay_logits)
      precision   <- methods::as(stage1$obj$he(logits_mode), "sparseMatrix") / np_spread
      logit_draws <- .sample_mvnorm_precision(logits_mode, precision, K)
      warm_epidemic_inits <- warm_inits[setdiff(names(warm_inits), "delay_logits")]
      collected <- list()
      for (k in seq_len(K)) {
        exp_logits <- exp(logit_draws[, k])
        imputed_simplex <- c(exp_logits, 1) / (sum(exp_logits) + 1)
        imputation_priors <- fix_param(priors, "delay_probs", imputed_simplex)
        imputation_fit <- tryCatch(fit(model, engine, priors = imputation_priors, init = warm_epidemic_inits),
                                   error = function(e) NULL)
        if (!is.null(imputation_fit) && imputation_fit$convergence == 0)
          collected[[length(collected) + 1]] <- imputation_fit
      }
      collected
    }, error = function(e) list())
    if (length(np_fits) > 0) return(list(fits = np_fits, rung = "multi", target = target))
  }

  # -- Two-stage PARAMETRIC (windowed Stage-1, impute mu/sigma) -----------------
  delay_estimate <- if (is_nonparametric) NULL else tryCatch({
    window       <- .window_delay_m(m, max_time, delay_window)
    delay_engine <- prepare_data(model, window$m, max_time = window$max_time, delay_only = TRUE)
    delay_fit    <- fit(model, delay_engine, priors = default_priors(model, delay_engine))
    list(mu = delay_fit$delay_mu, sigma = delay_fit$delay_sigma, mu_sd = delay_fit$delay_mu_sd,
         sigma_sd = delay_fit$delay_sigma_sd, shape_Q = delay_fit$delay_Q)
  }, error = function(e) NULL)
  is_gengamma <- model@delay@num_id == 3L

  if (!is.null(delay_estimate) && !is.null(warm_inits)) {
    spread_mu    <- max(floor_mu, if (is.finite(delay_estimate$mu_sd)) delay_estimate$mu_sd else 0)
    spread_sigma <- max(floor_sig_frac * delay_estimate$sigma,
                        if (is.finite(delay_estimate$sigma_sd)) delay_estimate$sigma_sd else 0)
    imputed_mu    <- rnorm(K, delay_estimate$mu, spread_mu)
    imputed_sigma <- pmax(0.05, rnorm(K, delay_estimate$sigma, spread_sigma))
    warm_epidemic_inits <- warm_inits[setdiff(names(warm_inits),
                                              c("delay_mu", "log_delay_sigma_excess", "delay_Q"))]
    collected <- list()
    for (k in seq_len(K)) {
      imputation_priors <- fix_param(fix_param(priors, "delay_mu", imputed_mu[k]),
                                     "delay_sigma", imputed_sigma[k])
      if (is_gengamma && is.finite(delay_estimate$shape_Q %||% NA))
        imputation_priors <- fix_param(imputation_priors, "delay_Q", delay_estimate$shape_Q)
      imputation_fit <- tryCatch(fit(model, engine, priors = imputation_priors, init = warm_epidemic_inits),
                                 error = function(e) NULL)
      if (!is.null(imputation_fit) && imputation_fit$convergence == 0)
        collected[[length(collected) + 1]] <- imputation_fit
    }
    if (length(collected) > 0) return(list(fits = collected, rung = "multi", target = target))
  }

  # -- Fallback: anchored prior (parametric) then plain one-stage ---------------
  if (!is.null(delay_estimate)) {
    anchored_priors <- default_priors(model, engine, phi = priors$phi_nb_prior %||% lognormal_prior(log(20), 0.5),
      delay_mu    = normal_prior(delay_estimate$mu, max(0.10, delay_estimate$mu_sd %||% 0.10)),
      delay_sigma = gamma_prior(4, 4 / max(0.5, delay_estimate$sigma)))
    anchored_fit <- tryCatch(fit(model, engine, priors = anchored_priors, init = warm_inits),
                             error = function(e) NULL)
    if (!is.null(anchored_fit) && anchored_fit$convergence == 0)
      return(list(fits = list(anchored_fit), rung = "anchored", target = target))
  }
  list(fits = list(fit(model, engine, priors = priors)), rung = "onestage", target = target)
}

#' Pool the posterior-predictive nowcast draws across a list of fits.
#' @param fits list of fit objects.
#' @param target event-time index.
#' @param n_draws draws per fit.
#' @returns list(M = pooled total `[Sigma n_draws x max_time]` matrix,
#'   lambda = pooled latent total, M_strata = pooled `[Sigma n_draws x max_time
#'   x n_strata]` array (or NULL when unstratified)).
#' @keywords internal
#' @noRd
.pool_fit_draws <- function(fits, target, n_draws = 200L) {
  # Draw from each fit separately, then stack the draws.  For a one-stage fit
  # there is a single block; for two-stage there is one block per imputation, and
  # stacking them pools the delay uncertainty across imputations.
  nowcast_blocks <- vector("list", length(fits))
  lambda_blocks  <- vector("list", length(fits))
  strata_blocks  <- vector("list", length(fits))
  n_strata <- 1L
  for (fit_index in seq_along(fits)) {
    fit_draws <- .nowcast_draws(fits[[fit_index]], target = target, n_draws = n_draws)
    nowcast_blocks[[fit_index]] <- fit_draws$M
    lambda_blocks[[fit_index]]  <- fit_draws$lambda_draws
    strata_blocks[[fit_index]]  <- fit_draws$M_strata
    n_strata <- fit_draws$n_strata %||% 1L
  }

  # Per-stratum draws are a 3-D array [draws x time x strata]; only pool them when
  # the fit is actually stratified and every block produced one.
  pooled_strata <- NULL
  if (n_strata > 1L && all(!vapply(strata_blocks, is.null, logical(1)))) {
    n_time <- dim(strata_blocks[[1]])[2]
    pooled_strata <- array(NA_real_, c(0L, n_time, n_strata))   # empty; grown below
    for (stratum_block in strata_blocks)
      pooled_strata <- abind_draws(pooled_strata, stratum_block)
  }

  list(M = do.call(rbind, nowcast_blocks),
       lambda = do.call(rbind, lambda_blocks),
       M_strata = pooled_strata, n_strata = n_strata)
}

#' Bind two `[draws x time x strata]` arrays along the draws (first) dimension
#'
#' A small base-R stand-in for `abind::abind(..., along = 1)`, used to pool
#' per-stratum draw arrays without taking on an extra dependency.
#'
#' @param first,second Numeric 3-D arrays sharing the same time and strata
#'   dimensions (either may be `NULL` or have zero draws, in which case the other
#'   is returned unchanged).
#' @returns The two arrays stacked along the first (draws) dimension.
#' @keywords internal
#' @noRd
abind_draws <- function(first, second) {
  if (is.null(first)  || dim(first)[1]  == 0L) return(second)
  if (is.null(second) || dim(second)[1] == 0L) return(first)

  n_draws_first  <- dim(first)[1]
  n_draws_second <- dim(second)[1]
  n_time         <- dim(first)[2]
  n_strata       <- dim(first)[3]

  combined <- array(NA_real_, c(n_draws_first + n_draws_second, n_time, n_strata))
  combined[seq_len(n_draws_first), , ] <- first
  combined[n_draws_first + seq_len(n_draws_second), , ] <- second
  combined
}
