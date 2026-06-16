# =============================================================================
# default_priors() -- build the per-parameter prior bundle for the RTMB engine
# =============================================================================
# Returns a flat named list keyed by parameter name.  Each entry is
#   list(dist = <num_id>, params = <length-3>, is_constant = 0/1, fixed = <val>)
# mirroring the resolution logic of diseasenowcast2::default_priors() but
# flattened for direct use inside the RTMB objective (no Stan data layout).
#
# Keys produced (only the ones relevant to the model are consumed downstream):
#   delay_mu, delay_sigma, delay_Q, delay_sigma_gengamma, delay_probs(alpha)
#   mu_intercept, phi_nb, gamma_cov
#   gp_alpha, gp_ell                           (HSGP)
#   ar_phi, ar_sigma                           (AR1 / SIR-RW)
#   R0, gamma_sir, N_eff                       (SIR)
# =============================================================================

#' Resolve a per-parameter prior list for a custom component (delay or process)
#'
#' Both `custom_delay()` and `custom_epidemic()` accept a `priors` list whose
#' elements are each *either* a `prior_class` object (a free parameter to be
#' estimated) *or* a single numeric (a fixed parameter held constant).  This
#' helper flattens that list into the four parallel vectors the RTMB objective
#' consumes, so the two component branches in [default_priors()] stay short and
#' identical in behaviour.
#'
#' @param priors_list The user's `priors` list (length `n_params`).  `NULL`
#'   elements (from an empty `list()`) fall back to `std_normal_prior()`.
#' @param n_params Integer number of parameters.
#' @returns A list with four length-`n_params` slots:
#'   `dists` (prior `num_id`, `0L` when fixed), `params_mat` (`n_params x 3`
#'   matrix of prior parameters), `is_free` (`1L` free / `0L` fixed), and
#'   `fixed_vals` (the fixed value, `0` when free).
#' @noRd
#' @keywords internal
.resolve_custom_param_priors <- function(priors_list, n_params) {
  dists      <- integer(n_params)
  params_mat <- matrix(0.0, n_params, 3L)
  is_free    <- integer(n_params)
  fixed_vals <- numeric(n_params)
  for (i in seq_len(n_params)) {
    this_prior <- priors_list[[i]]
    if (S7::S7_inherits(this_prior, prior_class)) {            # free parameter
      dists[i]        <- this_prior@num_id
      params_mat[i, ] <- .pad3(this_prior@stan_params)
      is_free[i]      <- 1L
    } else if (is.numeric(this_prior) && length(this_prior) == 1L) {  # fixed value
      dists[i]        <- 0L
      is_free[i]      <- 0L
      fixed_vals[i]   <- as.numeric(this_prior)
    } else {                                                   # default: std normal
      default_prior   <- std_normal_prior()
      dists[i]        <- default_prior@num_id
      params_mat[i, ] <- .pad3(default_prior@stan_params)
      is_free[i]      <- 1L
    }
  }
  list(dists = dists, params_mat = params_mat, is_free = is_free, fixed_vals = fixed_vals)
}

#' Build the default prior bundle for an RTMB nowcast model
#'
#' @param mod A [model()] object.
#' @param data Optional prepared-data list from [prepare_data()] (used for the
#'   data-informed location/scale defaults).  May also be a bare list with an
#'   `m` matrix.
#' @param ... Per-key overrides (e.g. `phi = lognormal_prior(log(20), 0.5)`,
#'   `delay_mu = normal_prior(log(5), 0.3)`).
#' @returns A named list of prior specs.
#'
#' @details
#' The default prior for each parameter is documented on the component
#' constructor: see the **Default priors** section of [epidemic_process]
#' (HSGP / AR(1) / SIR), [delay_process] (LogNormal / Gamma / GenGamma /
#' Dirichlet) and [likelihood] (the NB overdispersion `phi`).  To *see* what a
#' set of priors implies for the epidemic curve before fitting, use
#' [nowcast(prior_only = TRUE)][nowcast].
#'
#' @seealso [epidemic_process], [delay_process], [likelihood]
#' @export
default_priors <- function(mod, data = NULL, ...) {
  overrides <- list(...)
  lik <- mod@likelihood
  epi <- mod@epidemic
  dly <- mod@delay

  m_mat <- if (!is.null(data) && !is.null(data$m) && nrow(data$m) > 0) data$m else NULL

  .res <- function(slot_val, default_prior, key = NULL) {
    value <- if (!is.null(key) && !is.null(overrides[[key]])) overrides[[key]] else slot_val
    if (S7::S7_inherits(value, prior_class)) {
      list(dist = value@num_id, params = .pad3(value@stan_params),
           is_constant = 0L, fixed = numeric(0))
    } else if (is.numeric(value) && length(value) > 0) {
      list(dist = 0L, params = c(0, 0, 0), is_constant = 1L, fixed = as.numeric(value))
    } else {
      list(dist = default_prior@num_id, params = .pad3(default_prior@stan_params),
           is_constant = 0L, fixed = numeric(0))
    }
  }

  # -- Data-informed log-scale defaults (mirror diseasenowcast2) --------------
  if (!is.null(m_mat)) {
    daily <- tapply(m_mat[, 2], m_mat[, 1], sum)
    daily <- daily[is.finite(daily) & daily > 0]
    log_mu_center <- if (length(daily) > 0) log(stats::median(daily)) else 0
    log_mu_center <- max(-5, min(log_mu_center, 10))
    mu_log_sd <- if (length(daily) > 2) stats::sd(log(daily)) else 1
    mu_log_sd <- if (is.finite(mu_log_sd) && mu_log_sd > 0) mu_log_sd else 1
    mu_log_sd <- max(0.5, min(mu_log_sd, 2.5))

    med_delay <- .wtd_median(m_mat[, 3], m_mat[, 2])
    log_delay_center <- if (!is.na(med_delay) && med_delay > 0) log(med_delay) else log(3)
    log_delay_center <- max(log(0.5), min(log_delay_center, log(60)))
    delay_log_sd <- sqrt(.wtd_var(log(m_mat[, 3]), m_mat[, 2]))
    delay_log_sd <- if (is.finite(delay_log_sd) && delay_log_sd > 0) delay_log_sd else 1
    delay_log_sd <- max(0.3, min(delay_log_sd, 2))

    default_mu_prior     <- normal_prior(log_mu_center, mu_log_sd)
    default_delay1_prior <- normal_prior(log_delay_center, delay_log_sd)
  } else {
    default_mu_prior     <- std_normal_prior()
    default_delay1_prior <- normal_prior(log(7), 1)
  }

  pr <- list()

  # -- Likelihood: epidemic-mean intercept + NB overdispersion ----------------
  mu_slot <- if (S7::S7_inherits(lik, poisson_likelihood_class) ||
                 S7::S7_inherits(lik, nb_likelihood_class)) lik@mu else numeric(0)
  pr$mu_intercept <- .res(mu_slot, default_mu_prior, key = "mu")

  if (S7::S7_inherits(lik, nb_likelihood_class)) {
    pr$phi_nb <- .res(lik@phi, exponential_prior(1), key = "phi")
  }

  # -- Covariate coefficients ------------------------------------------------
  cov_val <- overrides[["gamma_coef"]] %||% mod@covariate_prior
  pr$gamma_cov <- if (S7::S7_inherits(cov_val, prior_class)) {
    list(dist = cov_val@num_id, params = .pad3(cov_val@stan_params), is_constant = 0L, fixed = numeric(0))
  } else {
    list(dist = normal_prior(0, 1)@num_id, params = .pad3(c(0, 1)), is_constant = 0L, fixed = numeric(0))
  }

  # -- Epidemic process priors ------------------------------------------------
  if (S7::S7_inherits(epi, hsgp_epidemic_class)) {
    # The HSGP basis coefficients use a non-centred parameterisation: they are
    # fixed at N(0, 1) inside the objective (the trend amplitude is carried by
    # `gp_alpha`), so there is no user-settable prior for them here.
    pr$gp_alpha <- .res(epi@alpha, half_normal_prior(0, 1), key = "gp_alpha")
    pr$gp_ell   <- .res(epi@ell,   inv_gamma_prior(3, 1),   key = "gp_ell")
  } else if (S7::S7_inherits(epi, ar1_epidemic_class)) {
    pr$ar_phi   <- .res(epi@phi,   std_normal_prior(),     key = "ar_phi")
    pr$ar_sigma <- .res(epi@sigma, exponential_prior(100), key = "ar_sigma")
  } else if (S7::S7_inherits(epi, sir_epidemic_class)) {
    pr$R0        <- .res(epi@R0,    lognormal_prior(log(2),   0.5), key = "R0")
    pr$gamma_sir <- .res(epi@gamma, lognormal_prior(log(1/5), 0.5), key = "gamma_sir")
    pr$ar_phi    <- .res(numeric(0), std_normal_prior(),     key = "ar_phi")
    pr$ar_sigma  <- .res(numeric(0), exponential_prior(100), key = "ar_sigma")
    pr$N_eff     <- .res(epi@N_eff, beta_prior(2, 5),        key = "N_eff")
  } else if (S7::S7_inherits(epi, custom_epidemic_class)) {
    n_custom_epi <- as.integer(epi@n_params)
    resolved      <- .resolve_custom_param_priors(epi@priors, n_custom_epi)
    pr$intensity_fn                     <- epi@intensity_fn
    pr$custom_epidemic_n_params          <- n_custom_epi
    pr$custom_epidemic_prior_dists       <- resolved$dists
    pr$custom_epidemic_prior_params_mat  <- resolved$params_mat
    pr$custom_epidemic_is_free           <- resolved$is_free
    pr$custom_epidemic_fixed_vals        <- resolved$fixed_vals
    pr$custom_epidemic_inits             <- epi@inits
  }

  # -- Delay process priors ----------------------------------------------------
  if (S7::S7_inherits(dly, lognormal_delay_class)) {
    pr$delay_mu    <- .res(dly@mu,    default_delay1_prior, key = "delay_mu")
    pr$delay_sigma <- .res(dly@sigma, gamma_prior(2, 2),    key = "delay_sigma")
  } else if (S7::S7_inherits(dly, gamma_delay_class)) {
    gamma_sd_center <- if (!is.null(m_mat)) {
      sdv <- sqrt(.wtd_var(m_mat[, 3], m_mat[, 2]))
      if (is.finite(sdv) && sdv > 0) max(0.5, min(sdv, 30)) else 2
    } else 2
    pr$delay_mu    <- .res(dly@shape, default_delay1_prior,              key = "delay_mu")
    pr$delay_sigma <- .res(dly@rate,  gamma_prior(2, 2 / gamma_sd_center), key = "delay_sigma")
  } else if (S7::S7_inherits(dly, generalized_gamma_delay_class)) {
    pr$delay_mu    <- .res(dly@mu,    default_delay1_prior, key = "delay_mu")
    pr$delay_Q     <- .res(dly@Q,     normal_prior(0, 0.5), key = "delay_Q")
    pr$delay_sigma <- .res(dly@sigma, gamma_prior(2, 0.1),  key = "delay_sigma")
  } else if (S7::S7_inherits(dly, dirichlet_delay_class)) {
    bins <- if (length(dly@bins) == 0 || is.na(dly@bins)) (data$np_model_length %||% 14L) else dly@bins
    alpha_val <- overrides[["delay_alpha"]] %||% dly@alpha
    if (is.numeric(alpha_val) && length(alpha_val) > 0) {
      alpha_vec <- if (length(alpha_val) == 1) rep(alpha_val, bins + 1) else alpha_val
    } else if (!is.null(m_mat)) {
      dl  <- pmin(as.integer(m_mat[, 3]), bins + 1L)
      cnt <- tapply(m_mat[, 2], factor(dl, levels = 1:(bins + 1L)), sum)
      cnt[is.na(cnt)] <- 0
      pmf <- as.numeric(cnt) / sum(cnt)
      alpha_vec <- 0.05 + (bins + 1) * pmf
    } else {
      alpha_vec <- rep(1, bins + 1)
    }
    pr$delay_probs <- list(dist = 4L, params = alpha_vec, is_constant = 0L, fixed = numeric(0), bins = bins)
  } else if (S7::S7_inherits(dly, custom_delay_class)) {
    n_custom <- as.integer(dly@n_params)
    resolved <- .resolve_custom_param_priors(dly@priors, n_custom)
    pr$cdf_factory                   <- dly@cdf_factory
    pr$custom_delay_n_params         <- n_custom
    pr$custom_delay_prior_dists      <- resolved$dists
    pr$custom_delay_prior_params_mat <- resolved$params_mat
    pr$custom_delay_is_free          <- resolved$is_free
    pr$custom_delay_fixed_vals       <- resolved$fixed_vals
    pr$custom_delay_inits            <- dly@inits
  }

  pr
}

#' Hard-fix a parameter in a prior bundle (treat as data, drop from estimation)
#'
#' @param priors A prior bundle from [default_priors()].
#' @param key Parameter key to fix.
#' @param value Fixed numeric value.
#' @returns The modified prior bundle.
#' @export
fix_param <- function(priors, key, value) {
  priors[[key]]$is_constant <- 1L
  priors[[key]]$fixed       <- as.numeric(value)
  priors
}
