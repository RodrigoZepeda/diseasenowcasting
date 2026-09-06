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
  is_count_cumulative <- !is.null(data) &&
    isTRUE(data$is_count_cumulative == 1L)
  delay_m_mat <- m_mat
  if (is_count_cumulative && !is.null(delay_m_mat)) {
    # Signed withdrawals are not frequency weights.  The appearance-delay
    # defaults use positive additions only; the epidemic intercept uses the
    # latest cumulative level carried by `case_counts` below.
    delay_m_mat[, 2L] <- pmax(delay_m_mat[, 2L], 0)
  }

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
    daily <- if (is_count_cumulative && !is.null(data$case_counts)) {
      rowSums(as.matrix(data$case_counts))
    } else {
      tapply(m_mat[, 2], m_mat[, 1], sum)
    }
    daily <- daily[is.finite(daily) & daily > 0]
    log_mu_center <- if (length(daily) > 0) log(stats::median(daily)) else 0
    log_mu_center <- max(-5, min(log_mu_center, 10))
    mu_log_sd <- if (length(daily) > 2) stats::sd(log(daily)) else 1
    mu_log_sd <- if (is.finite(mu_log_sd) && mu_log_sd > 0) mu_log_sd else 1
    mu_log_sd <- max(0.5, min(mu_log_sd, 2.5))

    delay_weights <- delay_m_mat[, 2L]
    med_delay <- if (sum(delay_weights) > 0)
      .wtd_median(delay_m_mat[, 3L], delay_weights) else NA_real_
    log_delay_center <- if (!is.na(med_delay) && med_delay > 0) log(med_delay) else log(3)
    log_delay_center <- max(log(0.5), min(log_delay_center, log(60)))
    delay_log_sd <- if (sum(delay_weights) > 0)
      sqrt(.wtd_var(log(delay_m_mat[, 3L]), delay_weights)) else NA_real_
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
    gamma_sd_center <- if (!is.null(delay_m_mat) &&
                           sum(delay_m_mat[, 2L]) > 0) {
      sdv <- sqrt(.wtd_var(delay_m_mat[, 3], delay_m_mat[, 2]))
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
      dl  <- pmin(as.integer(delay_m_mat[, 3]), bins + 1L)
      cnt <- tapply(delay_m_mat[, 2], factor(dl, levels = 1:(bins + 1L)), sum)
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

  # -- Count-cumulative collapsed retraction and hurdle priors ----------------
  cumulative <- tryCatch(mod@count_cumulative, error = function(e) NULL)
  if (!is.null(cumulative) && isTRUE(cumulative@active)) {
    pr$count_cumulative_observation <- switch(
      cumulative@observation,
      cumulative = 1L,
      hurdle_ztnb = 2L,
      hurdle_ztpoisson = 3L
    )
    pr$count_cumulative_settlement <- as.integer(cumulative@settlement)
    pr$retraction_mass <- .res(
      cumulative@retraction_mass, beta_prior(1.5, 20),
      key = "retraction_mass"
    )
    pr$movement_intercept <- .res(
      cumulative@movement_intercept, normal_prior(-1, 2),
      key = "movement_intercept"
    )
    pr$movement_age <- .res(
      cumulative@movement_age, normal_prior(0, 1),
      key = "movement_age"
    )
    pr$movement_previous <- .res(
      cumulative@movement_previous, normal_prior(0, 1),
      key = "movement_previous"
    )
    if (identical(cumulative@observation, "hurdle_ztnb")) {
      pr$magnitude_size <- .res(
        cumulative@magnitude_size, lognormal_prior(0, 1.5),
        key = "magnitude_size"
      )
    }

    retraction_delay <- cumulative@retraction_delay
    pr$count_cumulative_retraction_family <-
      as.integer(retraction_delay@num_id)
    default_retraction_location <- normal_prior(log(2), 1)
    if (S7::S7_inherits(retraction_delay, lognormal_delay_class)) {
      pr$count_cumulative_retraction_mu <- .res(
        retraction_delay@mu, default_retraction_location,
        key = "count_cumulative_retraction_mu"
      )
      pr$count_cumulative_retraction_sigma <- .res(
        retraction_delay@sigma, gamma_prior(2, 2),
        key = "count_cumulative_retraction_sigma"
      )
    } else if (S7::S7_inherits(retraction_delay, gamma_delay_class)) {
      pr$count_cumulative_retraction_mu <- .res(
        retraction_delay@shape, default_retraction_location,
        key = "count_cumulative_retraction_mu"
      )
      pr$count_cumulative_retraction_sigma <- .res(
        retraction_delay@rate, gamma_prior(2, 2),
        key = "count_cumulative_retraction_sigma"
      )
    } else if (S7::S7_inherits(retraction_delay,
                               generalized_gamma_delay_class)) {
      pr$count_cumulative_retraction_mu <- .res(
        retraction_delay@mu, default_retraction_location,
        key = "count_cumulative_retraction_mu"
      )
      pr$count_cumulative_retraction_sigma <- .res(
        retraction_delay@sigma, gamma_prior(2, 0.1),
        key = "count_cumulative_retraction_sigma"
      )
      pr$count_cumulative_retraction_Q <- .res(
        retraction_delay@Q, normal_prior(0, 0.5),
        key = "count_cumulative_retraction_Q"
      )
    }
  }

  # -- Report-level validation priors ------------------------------------------
  # Active only when a linelist/count-incidence model carries
  # validation_process(). Count-cumulative data use the collapsed kernel above.
  confirmation <- tryCatch(mod@validation, error = function(e) NULL)
  if (is_count_cumulative && !is.null(confirmation) &&
      isTRUE(confirmation@active)) {
    cli::cli_abort(c(
      "A {.fn validation_process} cannot supply count-cumulative priors.",
      "i" = "Use {.fn count_cumulative_process}; cumulative data identify {.code h_R}, not a separate {.code p}."
    ))
  }
  if (!is.null(confirmation) && isTRUE(confirmation@active)) {
    is_linelist_retraction <- isTRUE(data$is_linelist_retraction == 1L)

    if (is_linelist_retraction) {
      # LINELIST: `p` is identified by the cure block -- standing rows with long
      # follow-up pin the cure fraction directly -- and in the count block it is
      # exactly aliased with the epidemic intercept, so the count likelihood
      # cannot pull it around.  A WEAK data-informed Beta is therefore enough.
      retract_table  <- data$retract_table
      standing_table <- data$standing_table
      p_hat <- .empirical_confirmation_probability(
        if (is.null(retract_table))  numeric(0) else retract_table[, "lag"],
        if (is.null(retract_table))  numeric(0) else retract_table[, "count"],
        if (is.null(standing_table)) numeric(0) else standing_table[, "age"],
        if (is.null(standing_table)) numeric(0) else standing_table[, "count"])
      concentration <- 10
    } else {
      # COUNT-CUMULATIVE: centre a WEAK Beta on the empirical retraction rate
      # (1 - retracted/appeared from the signed increments).
      #
      # This prior used to be floored at 0.9 and carry a concentration of 300, on
      # the grounds that a weak prior let the Skellam variance abuse the retraction
      # stream as an overdispersion knob until `p` collapsed.  That pathology was an
      # artefact of the Bessel-series truncation in the increment density (see
      # `.log_skellam_increment()`), which was 62-5729 nats wrong at realistic
      # counts.  With the density fixed, sweeping the concentration from 300 down to
      # 1 on data simulated at known `p` moves the estimate by <0.01 and the WEAKEST
      # prior is the most accurate one, because the strong prior was simply dragging
      # `p` toward the centre.
      #
      # The centre has to be weak, because the estimator is biased upward by
      # construction and cannot be fixed: a cumulative stream shows "not retracted
      # YET", which is `p + (1 - p) P(not yet retracted)`, so it overstates `p` by
      # +0.007 at p = 0.99 rising to +0.10 at p = 0.7.  (The alternative
      # peak-vs-final estimator is biased by the same amount.)  At a concentration
      # of 10 that bias sits inside one prior standard deviation, so the likelihood
      # dominates -- which is the honest arrangement given the article's point that
      # a cumulative stream cannot separate `p` from "never retracted" on its own.
      appeared  <- if (!is.null(m_mat)) sum(pmax(m_mat[, 2], 0)) else 1
      retracted <- if (!is.null(m_mat)) sum(pmax(-m_mat[, 2], 0)) else 0
      p_hat <- min(max(1 - retracted / max(appeared, 1), 0.01), 0.995)
      concentration <- 10
    }
    default_p_prior <- beta_prior(p_hat * concentration, (1 - p_hat) * concentration)
    pr$confirm_p <- if (is_linelist_retraction) {
      .res(confirmation@p, default_p_prior, key = "confirm_p")
    } else {
      # COUNT-CUMULATIVE: `p` is FIXED at the empirical rate unless the user says
      # otherwise.  This is the article's "model constraint", and it is needed
      # because `p` is not merely weakly identified here -- it is near-unidentified
      # against the appearance delay, through a degenerate solution the likelihood
      # actively prefers:
      #
      #   gross reports ~10x the truth arrive, and ~90% of them are retracted at
      #   the shortest lag `g_C` allows.
      #
      # Instant churn cancels itself out of the observed increments, so it costs
      # the likelihood nothing, and it buys the freedom to stretch `g_D`.  On the
      # FluSight hospitalisation stream (Texas and California, windowed to the
      # snapshot era) every arm that estimated `p` landed there:
      #
      #   prior concentration   10      100     300     fixed
      #   p_hat (TX, emp 0.958) 0.111   0.120   0.142   --
      #   p_hat (CA, emp 0.990) 0.094   0.103   0.124   --
      #   g_C median (periods)  0.04    0.04    0.04    0.98 / 1.55
      #   delay_mu              3.08    2.97    2.71    -0.16 / -0.12
      #
      # Thirty times more prior information buys 0.03 in `p_hat` against a gap of
      # 0.85, and only the fixed arm recovers a credible appearance delay (a
      # ~0.9-week median rather than 15-25 weeks).  A prior cannot fix this,
      # because the problem is not that the prior is too weak -- it is that the
      # likelihood surface has a second, wrong optimum that no amount of
      # concentration removes.
      #
      # The estimate is biased upward by construction (a cumulative stream shows
      # "not retracted YET", so it overstates `p` by +0.007 at p = 0.99 rising to
      # +0.10 at p = 0.7), which is a far smaller error than the one it prevents.
      #
      # Pass `p = beta_prior(...)` to estimate it anyway, or `p = <number>` to fix
      # it somewhere else.  Linelist / count-incidence data keep the weak Beta:
      # there the cure block identifies `p` directly from the standing rows.
      .res(if (length(confirmation@p) > 0) confirmation@p else p_hat,
           default_p_prior, key = "confirm_p")
    }
    # One p per stratum, each under the same prior.  Only honoured for linelist
    # retractions -- the count-cumulative model has no per-stratum increment split.
    pr$confirm_p_stratified <- as.integer(is_linelist_retraction &&
                                          isTRUE(confirmation@stratified_p))

    retract_delay     <- confirmation@validation_delay
    pr$retract_family <- as.integer(retract_delay@num_id)

    # Retraction-delay priors, one branch per family.  A linelist observes the
    # retraction lags directly, so the location prior is centred on their mean
    # rather than on the count-cumulative fallback of ~1.5 periods.
    retract_table   <- data$retract_table
    observed_lags   <- if (is.null(retract_table)) numeric(0) else retract_table[, "lag"]
    observed_counts <- if (is.null(retract_table)) numeric(0) else retract_table[, "count"]
    mean_observed_lag <- if (sum(observed_counts) > 0)
      sum(observed_lags * observed_counts) / sum(observed_counts) else 1.5
    default_retract_mu <- normal_prior(log(max(mean_observed_lag, 1)), 0.5)

    if (S7::S7_inherits(retract_delay, dirichlet_delay_class)) {
      bins <- if (length(retract_delay@bins) == 0 || is.na(retract_delay@bins))
        max(2L, min(as.integer(data$max_report_age %||% 7L), 14L)) else as.integer(retract_delay@bins)
      alpha_val <- retract_delay@alpha
      alpha_vec <- if (is.numeric(alpha_val) && length(alpha_val) > 0) {
        if (length(alpha_val) == 1) rep(alpha_val, bins + 1) else alpha_val
      } else if (sum(observed_counts) > 0) {
        binned    <- pmin(as.integer(observed_lags), bins + 1L)
        bin_total <- tapply(observed_counts, factor(binned, levels = 1:(bins + 1L)), sum)
        bin_total[is.na(bin_total)] <- 0
        0.05 + (bins + 1) * as.numeric(bin_total) / sum(bin_total)
      } else rep(1, bins + 1)
      pr$retract_probs <- list(dist = 4L, params = alpha_vec, is_constant = 0L,
                               fixed = numeric(0), bins = bins)
    } else if (S7::S7_inherits(retract_delay, gamma_delay_class)) {
      pr$retract_mu    <- .res(retract_delay@shape, default_retract_mu, key = "retract_mu")
      pr$retract_sigma <- .res(retract_delay@rate,  gamma_prior(2, 2),  key = "retract_sigma")
    } else if (S7::S7_inherits(retract_delay, generalized_gamma_delay_class)) {
      pr$retract_mu    <- .res(retract_delay@mu,    default_retract_mu,   key = "retract_mu")
      pr$retract_Q     <- .res(retract_delay@Q,     normal_prior(0, 0.5), key = "retract_Q")
      pr$retract_sigma <- .res(retract_delay@sigma, gamma_prior(2, 0.1),  key = "retract_sigma")
    } else if (S7::S7_inherits(retract_delay, lognormal_delay_class)) {
      pr$retract_mu    <- .res(retract_delay@mu,    default_retract_mu, key = "retract_mu")
      pr$retract_sigma <- .res(retract_delay@sigma, gamma_prior(2, 2),  key = "retract_sigma")
    } else {
      cli::cli_abort(c("Unsupported retraction delay family {.val {retract_delay@name}}.",
                       "i" = "`validation_delay` must be lognormal, gamma, generalized-gamma or Dirichlet."))
    }

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
