# =============================================================================
# RTMB objective builders -- delay-only (Phase 1)
# =============================================================================
# Reproduces the FIXED Stan delay-only log-posterior under
# (delay_only = 1, num_delay_seasons = 1, delay_is_censored = 1):
#
#   log p = log p_prior(delay params)
#         + sum_r row_sums_exact[r] * log_pmf(delay_r) - sum_t col_sums_exact[t]*log F(censor_t)
#         + sum_r row_sums_cens[r]  * log F(delay_r)   - sum_t col_sums_cens[t] *log F(censor_t)
#
# censor_t = censoring_col[t] = max_time - t + 1 (per-event censoring, the fix).
# Parametric families 1 (LogNormal), 2 (Gamma), 3 (Generalized Gamma) are
# supported via the family-generic closures in .delay_distribution_functions().
# =============================================================================

#' Build the delay-only RTMB objective (all families)
#'
#' Dispatches to the parametric builder (families 1/2/3) or the non-parametric
#' Dirichlet-simplex builder (family 4).  No random block in either case.
#'
#' @param data Prepared-data list from [prepare_data()] (`delay_only = TRUE`).
#' @param priors Prior bundle from [default_priors()].
#' @param init Optional init list (`delay_mu`/`delay_sigma`/`delay_Q`, or
#'   `delay_logits` for the non-parametric family).
#' @returns An RTMB `obj` (from `MakeADFun`); no random block.
#' @keywords internal
#' @noRd
build_delay_only_obj <- function(data, priors, init = NULL) {
  family <- data$delay_family
  if (!family %in% c(1L, 2L, 3L, 4L))
    cli::cli_abort("build_delay_only_obj supports families 1/2/3/4; family {family} given.")
  if (family == 4L) return(.build_delay_only_nonparametric(data, priors, init))
  is_gengamma <- family == 3L

  delay_mu_is_fixed    <- isTRUE(priors$delay_mu$is_constant == 1L)
  delay_sigma_is_fixed <- isTRUE(priors$delay_sigma$is_constant == 1L)
  shape_Q_is_fixed     <- is_gengamma && isTRUE(priors$delay_Q$is_constant == 1L)

  objective_data <- list(
    family = family, is_gengamma = as.integer(is_gengamma),
    obs_delays      = data$obs_delays,
    row_sums_exact  = data$row_sums_exact,
    col_sums_exact  = data$col_sums_exact,
    obs_delays_cens = data$obs_delays_cens,
    row_sums_cens   = data$row_sums_cens,
    col_sums_cens   = data$col_sums_cens,
    censoring_col   = data$censoring_col,
    split_delay     = max(2, .wtd_median(data$m[, 3], data$m[, 2])),
    prior_mu_dist    = priors$delay_mu$dist,    prior_mu_params    = .pad3(priors$delay_mu$params),
    prior_sigma_dist = priors$delay_sigma$dist, prior_sigma_params = .pad3(priors$delay_sigma$params),
    prior_shape_dist = if (is_gengamma) priors$delay_Q$dist else 0L,
    prior_shape_params = if (is_gengamma) .pad3(priors$delay_Q$params) else c(0, 0, 0),
    delay_mu_fixed    = if (delay_mu_is_fixed)    priors$delay_mu$fixed    else NA_real_,
    delay_sigma_fixed = if (delay_sigma_is_fixed) priors$delay_sigma$fixed else NA_real_,
    shape_Q_fixed     = if (shape_Q_is_fixed)     priors$delay_Q$fixed     else NA_real_
  )

  if (is.null(init)) {
    total_count   <- sum(data$row_sums_exact)
    log_mean_seed <- if (total_count > 0 && length(data$obs_delays) > 0)
      sum(log(data$obs_delays) * data$row_sums_exact) / total_count else log(3)
    delay_sd_seed <- { empirical_sd <- sqrt(.wtd_var(data$m[, 3], data$m[, 2]))
                       if (is.finite(empirical_sd) && empirical_sd > 0) max(2, min(empirical_sd, 60)) else 5 }
    init <- list(delay_mu = log_mean_seed,
                 delay_sigma = if (is_gengamma) 0.6 else delay_sd_seed, delay_Q = -2)
  }

  parameters <- list(
    delay_mu               = if (is.na(objective_data$delay_mu_fixed)) init$delay_mu else 0,
    log_delay_sigma_excess = if (is.na(objective_data$delay_sigma_fixed))
      log(max(init$delay_sigma - 0.01, 1e-6)) else 0
  )
  # delay_Q is the UNCONSTRAINED raw value (Q = .gengamma_shape_transform()); init raw -2 ~ Q 0.4.
  if (is_gengamma) parameters$delay_Q <- if (is.na(objective_data$shape_Q_fixed)) (init$delay_Q %||% -2) else 0
  map <- list()
  if (!is.na(objective_data$delay_mu_fixed))    map$delay_mu <- factor(NA)
  if (!is.na(objective_data$delay_sigma_fixed)) map$log_delay_sigma_excess <- factor(NA)
  if (is_gengamma && !is.na(objective_data$shape_Q_fixed)) map$delay_Q <- factor(NA)

  negative_log_posterior <- function(params) {
    RTMB::getAll(params, objective_data)
    delay_log_mean <- if (is.na(delay_mu_fixed)) delay_mu else delay_mu_fixed
    delay_sd       <- if (is.na(delay_sigma_fixed)) 0.01 + exp(log_delay_sigma_excess) else delay_sigma_fixed
    log_jacobian   <- if (is.na(delay_sigma_fixed)) log_delay_sigma_excess else 0
    shape_Q <- 0
    if (is_gengamma == 1L) {
      if (is.na(shape_Q_fixed)) {
        shape_transform <- .gengamma_shape_transform(delay_Q)
        shape_Q <- shape_transform$shape_Q
        log_jacobian <- log_jacobian + shape_transform$log_jacobian
      } else shape_Q <- shape_Q_fixed
    }

    delay_fns <- if (is_gengamma == 1L)
        .delay_distribution_functions(3L, delay_log_mean, shape_Q, delay_sd)
      else
        .delay_distribution_functions(family, delay_log_mean, delay_sd)

    log_cdf_censoring <- delay_fns$log_cdf(censoring_col)
    loglik <- 0
    if (length(obs_delays) > 0)
      loglik <- loglik +
        .discretised_delay_loglik(obs_delays, row_sums_exact, split_delay,
                                  delay_fns$log_cdf, delay_fns$log_survival) -
        sum(col_sums_exact * log_cdf_censoring)
    if (length(obs_delays_cens) > 0)
      loglik <- loglik + sum(row_sums_cens * delay_fns$log_cdf(obs_delays_cens)) -
        sum(col_sums_cens * log_cdf_censoring)

    log_prior <- 0
    if (is.na(delay_mu_fixed))    log_prior <- log_prior + prior_lpdf(delay_log_mean, prior_mu_dist, prior_mu_params)
    if (is.na(delay_sigma_fixed)) log_prior <- log_prior + prior_lpdf(delay_sd, prior_sigma_dist, prior_sigma_params)
    if (is_gengamma == 1L && is.na(shape_Q_fixed))
      log_prior <- log_prior + prior_lpdf(shape_Q, prior_shape_dist, prior_shape_params)

    RTMB::REPORT(delay_sd)
    if (is.na(delay_sigma_fixed)) RTMB::ADREPORT(delay_sd)
    if (is.na(delay_mu_fixed))    RTMB::ADREPORT(delay_mu)
    -(loglik + log_prior + log_jacobian)
  }

  RTMB::MakeADFun(negative_log_posterior, parameters, map = map, silent = TRUE)
}

#' Build the delay-only RTMB objective for the non-parametric Dirichlet family
#'
#' Stage-1 of the two-stage Dirichlet nowcast.  The reporting-delay simplex is
#' parameterised by `delay_logits` (length `n_bins`) via a softmax with a fixed
#' reference category: `p = c(exp(logits), 1) / (sum(exp(logits)) + 1)`.  The
#' censored delay-only log-likelihood (per-time censoring) plus the Dirichlet
#' prior and the softmax change-of-variables Jacobian are accumulated; no
#' epidemic process.  Returns the simplex-fitting `obj` (the joint Hessian over
#' `delay_logits` then drives the simplex imputation in the multisample).
#' @keywords internal
#' @noRd
.build_delay_only_nonparametric <- function(data, priors, init = NULL) {
  n_bins          <- as.integer(data$np_model_length)
  dirichlet_alpha <- priors$delay_probs$params

  objective_data <- list(
    obs_delays      = data$obs_delays,
    row_sums_exact  = data$row_sums_exact,
    col_sums_exact  = data$col_sums_exact,
    obs_delays_cens = data$obs_delays_cens,
    row_sums_cens   = data$row_sums_cens,
    col_sums_cens   = data$col_sums_cens,
    censoring_col   = data$censoring_col,
    n_bins          = n_bins,
    dirichlet_alpha = dirichlet_alpha
  )

  logits_init <- if (!is.null(init$delay_logits)) init$delay_logits else {
    delay_binned  <- pmin(as.integer(data$m[, 3]), n_bins + 1L)
    bin_counts    <- tapply(data$m[, 2], factor(delay_binned, levels = 1:(n_bins + 1L)), sum)
    bin_counts[is.na(bin_counts)] <- 0
    empirical_pmf <- (as.numeric(bin_counts) + 0.5) / sum(bin_counts + 0.5)
    log(empirical_pmf[1:n_bins]) - log(empirical_pmf[n_bins + 1])
  }
  parameters <- list(delay_logits = logits_init)

  negative_log_posterior <- function(params) {
    RTMB::getAll(params, objective_data)
    exp_logits    <- exp(delay_logits)
    simplex_probs <- c(exp_logits, exp(0 * delay_logits[1])) / (sum(exp_logits) + 1)
    np_fns        <- .nonparametric_delay_functions(simplex_probs, n_bins)

    log_cdf_censoring <- np_fns$log_cdf(censoring_col)
    loglik <- 0
    if (length(obs_delays) > 0)
      loglik <- loglik + sum(row_sums_exact * np_fns$log_pmf_raw(obs_delays)) -
        sum(col_sums_exact * log_cdf_censoring)
    if (length(obs_delays_cens) > 0)
      loglik <- loglik + sum(row_sums_cens * np_fns$log_cdf(obs_delays_cens)) -
        sum(col_sums_cens * log_cdf_censoring)

    log_prior <- dirichlet_lpdf(simplex_probs, dirichlet_alpha) + sum(log(simplex_probs))  # + softmax Jacobian
    RTMB::REPORT(simplex_probs)
    -(loglik + log_prior)
  }

  RTMB::MakeADFun(negative_log_posterior, parameters, silent = TRUE)
}
