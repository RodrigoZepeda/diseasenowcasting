# =============================================================================
# Joint RTMB objective: latent epidemic (Laplace) + delay + S_k likelihood
# =============================================================================
# Stratified (S7 model + tbl.now strata): the log-likelihood is a sum over
# (time t, stratum s) cells.  The reporting delay G_D is SHARED across strata
# (estimated from all observed delays pooled); the epidemic mean is per-stratum:
#   log_mean[t, s] = gamma0[s] + (X gamma[, s])[t] + epidemic_trend[s](t).
# Per-stratum: intercept, covariate coefficients, HSGP basis_coefs / AR1
# innovations + ar_phi/ar_sigma.  Shared: NB overdispersion phi, HSGP kernel
# (alpha, ell), the delay.  SIR couples strata through a shared force of
# infection (beta^(s) * sum_s' I^(s')).  At num_strata = 1 everything reduces
# exactly to the single-stratum model.
#
# Latent epidemic coefficients are `random` only when use_random = TRUE (the
# marginal Laplace); the default joint-mode keeps them as fixed effects.
# =============================================================================

#' Build the joint RTMB objective (stratified)
#' @keywords internal
#' @noRd
build_joint_obj <- function(data, priors, init = NULL, use_random = TRUE,
                            hierarchical_strata = FALSE) {
  family <- data$delay_family
  if (!family %in% c(1L, 2L, 3L, 4L, 5L))
    cli::cli_abort("build_joint_obj supports delay families 1/2/3/4/5; family {family} given.")
  is_gengamma      <- family == 3L
  is_nonparametric <- family == 4L
  is_custom_delay  <- family == 5L
  n_bins           <- if (is_nonparametric) as.integer(data$np_model_length) else 0L
  epidemic_model   <- data$epidemic_model
  if (!epidemic_model %in% c(1L, 2L, 3L))
    cli::cli_abort("build_joint_obj supports HSGP (1), AR1 (2), SIR (3) epidemic.")
  is_sir    <- epidemic_model == 3L
  is_negbin <- data$is_negative_binomial == 1L
  n_covariates <- data$P
  n_time       <- data$max_time
  n_strata     <- as.integer(data$num_strata)

  if (epidemic_model == 1L) {
    time_scaled       <- hsgp_time_scaled(n_time, data$tmax_model)
    hsgp_basis_matrix <- hsgp_basis(time_scaled, data$gp_L_left, data$gp_L_right,
                                    data$num_basis, data$gp_basis)
    hsgp_frequencies  <- seq_len(data$num_basis) * pi / (data$gp_L_left + data$gp_L_right)
  } else { hsgp_basis_matrix <- matrix(0.0, n_time, 0L); hsgp_frequencies <- numeric(0) }

  delay_mu_is_fixed    <- !is_nonparametric && !is_custom_delay && isTRUE(priors$delay_mu$is_constant == 1L)
  delay_sigma_is_fixed <- !is_nonparametric && !is_custom_delay && isTRUE(priors$delay_sigma$is_constant == 1L)
  shape_Q_is_fixed     <- is_gengamma && isTRUE(priors$delay_Q$is_constant == 1L)
  dirichlet_alpha      <- if (is_nonparametric) priors$delay_probs$params else numeric(0)
  delay_probs_fixed    <- is_nonparametric && isTRUE(priors$delay_probs$is_constant == 1L)

  # Custom delay (family 5) data extracted from priors
  cdf_factory           <- if (is_custom_delay) priors$cdf_factory else NULL
  n_params_custom       <- if (is_custom_delay) as.integer(priors$custom_delay_n_params) else 0L
  custom_prior_dists    <- if (is_custom_delay) priors$custom_delay_prior_dists  else integer(0)
  custom_prior_params   <- if (is_custom_delay) priors$custom_delay_prior_params_mat else matrix(0.0, 0L, 3L)
  custom_is_free        <- if (is_custom_delay) priors$custom_delay_is_free      else integer(0)
  custom_fixed_vals     <- if (is_custom_delay) priors$custom_delay_fixed_vals   else numeric(0)
  custom_fully_fixed    <- is_custom_delay && n_params_custom > 0L && all(custom_is_free == 0L)

  # Precompute the shared-delay Gstar [n_time x n_strata] when the delay is fully
  # fixed (multisample Stage-2): the CDF is then data, not re-taped per step.
  delay_fully_fixed <- (!is_nonparametric && !is_custom_delay && delay_mu_is_fixed && delay_sigma_is_fixed &&
                        (!is_gengamma || shape_Q_is_fixed)) || delay_probs_fixed || custom_fully_fixed
  gstar_precomputed <- matrix(0.0, 0L, 0L)
  if (delay_fully_fixed) {
    fixed_delay_fns <- if (is_nonparametric)
        .nonparametric_delay_functions(priors$delay_probs$fixed, n_bins)
      else if (is_gengamma)
        .delay_distribution_functions(3L, priors$delay_mu$fixed, priors$delay_Q$fixed, priors$delay_sigma$fixed)
      else if (is_custom_delay)
        cdf_factory(custom_fixed_vals)
      else
        .delay_distribution_functions(family, priors$delay_mu$fixed, priors$delay_sigma$fixed)
    gstar_precomputed <- matrix(as.numeric(fixed_delay_fns$cdf(as.numeric(data$d_star) + 1)),
                                n_time, n_strata)
  }

  is_hierarchical <- isTRUE(hierarchical_strata) && n_strata > 1L

  objective_data <- list(
    family = family, is_gengamma = as.integer(is_gengamma),
    is_nonparametric = as.integer(is_nonparametric), n_bins = n_bins,
    is_custom_delay = as.integer(is_custom_delay),
    n_params_custom = n_params_custom,
    custom_prior_dists = custom_prior_dists, custom_prior_params = custom_prior_params,
    custom_is_free = custom_is_free,
    dirichlet_alpha = dirichlet_alpha,
    delay_fully_fixed = as.integer(delay_fully_fixed), gstar_precomputed = gstar_precomputed,
    case_counts = data$case_counts, d_star = data$d_star,           # [n_time x n_strata] matrices
    n_time = n_time, n_strata = n_strata, is_hierarchical = as.integer(is_hierarchical),
    obs_delays = data$obs_delays, row_sums = data$row_sums_exact,
    obs_delays_cens = data$obs_delays_cens %||% numeric(0),
    row_sums_cens   = data$row_sums_cens   %||% numeric(0),
    split_delay = max(2, .wtd_median(data$m[, 3], data$m[, 2])),
    X = data$X, hsgp_basis_matrix = hsgp_basis_matrix, hsgp_frequencies = hsgp_frequencies,
    gp_kernel = data$gp_kernel,
    epidemic_model = epidemic_model, is_negbin = as.integer(is_negbin), n_covariates = n_covariates,
    mu_log_upper_bound = data$mu_log_upper_bound, ar_sigma_max = data$ar_sigma_max,
    prior_mu_dist     = priors$delay_mu$dist,    prior_mu_params     = .pad3(priors$delay_mu$params),
    prior_sigma_dist  = priors$delay_sigma$dist, prior_sigma_params  = .pad3(priors$delay_sigma$params),
    prior_shape_dist  = if (is_gengamma) priors$delay_Q$dist else 0L,
    prior_shape_params = if (is_gengamma) .pad3(priors$delay_Q$params) else c(0, 0, 0),
    prior_intercept_dist = priors$mu_intercept$dist, prior_intercept_params = .pad3(priors$mu_intercept$params),
    prior_gamma_dist  = priors$gamma_cov$dist,   prior_gamma_params  = .pad3(priors$gamma_cov$params),
    prior_phi_dist    = if (is_negbin) priors$phi_nb$dist else 0L,
    prior_phi_params  = if (is_negbin) .pad3(priors$phi_nb$params) else c(0, 0, 0),
    prior_gp_alpha_dist = if (epidemic_model == 1L) priors$gp_alpha$dist else 0L,
    prior_gp_alpha_params = if (epidemic_model == 1L) .pad3(priors$gp_alpha$params) else c(0, 0, 0),
    prior_gp_ell_dist = if (epidemic_model == 1L) priors$gp_ell$dist else 0L,
    prior_gp_ell_params = if (epidemic_model == 1L) .pad3(priors$gp_ell$params) else c(0, 0, 0),
    prior_ar_phi_dist = if (epidemic_model == 2L) priors$ar_phi$dist else 0L,
    prior_ar_phi_params = if (epidemic_model == 2L) .pad3(priors$ar_phi$params) else c(0, 0, 0),
    prior_ar_sigma_dist = if (epidemic_model %in% c(2L, 3L)) priors$ar_sigma$dist else 0L,
    prior_ar_sigma_params = if (epidemic_model %in% c(2L, 3L)) .pad3(priors$ar_sigma$params) else c(0, 0, 0),
    prior_ar_phi_sir_dist = if (is_sir) priors$ar_phi$dist else 0L,
    prior_ar_phi_sir_params = if (is_sir) .pad3(priors$ar_phi$params) else c(0, 0, 0),
    is_sir = as.integer(is_sir), N_pop = data$N_pop,
    initial_infected = if (is_sir) data$case_counts[1, ] else numeric(n_strata),
    prior_R0_dist = if (is_sir) priors$R0$dist else 0L, prior_R0_params = if (is_sir) .pad3(priors$R0$params) else c(0, 0, 0),
    prior_gamma_sir_dist = if (is_sir) priors$gamma_sir$dist else 0L,
    prior_gamma_sir_params = if (is_sir) .pad3(priors$gamma_sir$params) else c(0, 0, 0),
    prior_n_eff_dist = if (is_sir) priors$N_eff$dist else 0L, prior_n_eff_params = if (is_sir) .pad3(priors$N_eff$params) else c(0, 0, 0),
    delay_mu_is_fixed = as.integer(delay_mu_is_fixed), delay_mu_fixed = if (delay_mu_is_fixed) priors$delay_mu$fixed else 0,
    delay_sigma_is_fixed = as.integer(delay_sigma_is_fixed), delay_sigma_fixed = if (delay_sigma_is_fixed) priors$delay_sigma$fixed else 0,
    shape_Q_is_fixed = as.integer(shape_Q_is_fixed), shape_Q_fixed = if (shape_Q_is_fixed) priors$delay_Q$fixed else 0
  )

  # -- parameter initial values (per-stratum where applicable) ------------------
  init <- init %||% list()
  positive_col_log_median <- function(col) { positive <- col[col > 0]
    if (length(positive)) log(stats::median(positive)) else 0 }
  intercept_init <- init$mu_intercept %||% apply(data$case_counts, 2, positive_col_log_median)
  if (length(intercept_init) != n_strata) intercept_init <- rep_len(intercept_init, n_strata)
  delay_mu_init  <- init$delay_mu %||% log(max(.wtd_median(data$m[, 3], data$m[, 2]), 1.5))
  delay_sigma_init <- init$delay_sigma %||% {
    if (is_gengamma) 0.6 else { empirical_sd <- sqrt(.wtd_var(data$m[, 3], data$m[, 2]))
      if (is.finite(empirical_sd) && empirical_sd > 0) max(2, min(empirical_sd, 60)) else 5 } }

  parameters <- if (is_sir) list()
                else if (is_hierarchical) list(
                  # Non-centred hierarchical intercept: mu[s] = mu_global + tau * delta[s]
                  mu_global          = init$mu_global %||% mean(intercept_init),
                  log_tau_intercept  = init$log_tau_intercept %||% 0,
                  delta_intercept    = init$delta_intercept %||% rep(0, n_strata),
                  gamma = if (n_covariates > 0) (init$gamma %||% matrix(0, n_covariates, n_strata)) else matrix(0, 0, 0)
                ) else list(
                  mu_intercept = intercept_init,
                  gamma = if (n_covariates > 0) (init$gamma %||% matrix(0, n_covariates, n_strata)) else matrix(0, 0, 0)
                )
  if (is_nonparametric) {
    if (!delay_probs_fixed) {
      logits_init <- if (!is.null(init$delay_logits)) init$delay_logits else {
        delay_binned <- pmin(as.integer(data$m[, 3]), n_bins + 1L)
        bin_counts   <- tapply(data$m[, 2], factor(delay_binned, levels = 1:(n_bins + 1L)), sum)
        bin_counts[is.na(bin_counts)] <- 0
        empirical_pmf <- (as.numeric(bin_counts) + 0.5) / sum(bin_counts + 0.5)
        log(empirical_pmf[1:n_bins]) - log(empirical_pmf[n_bins + 1])
      }
      parameters$delay_logits <- logits_init
    }
  } else if (is_custom_delay) {
    user_inits <- priors$custom_delay_inits %||% rep(0.0, n_params_custom)
    init_vals  <- numeric(n_params_custom)
    for (i in seq_len(n_params_custom)) {
      init_vals[i] <- if (custom_is_free[i] == 0L) custom_fixed_vals[i]
                      else (init$custom_delay_params[i] %||% user_inits[i])
    }
    parameters$custom_delay_params <- init_vals
  } else {
    parameters$delay_mu               <- if (delay_mu_is_fixed) 0 else delay_mu_init
    parameters$log_delay_sigma_excess <- if (delay_sigma_is_fixed) 0 else log(max(delay_sigma_init - 0.01, 1e-6))
    if (is_gengamma) parameters$delay_Q <- if (shape_Q_is_fixed) 0 else (init$delay_Q %||% -2)
  }
  if (is_negbin) parameters$log_phi_nb <- init$log_phi_nb %||% log(20)
  if (epidemic_model == 1L) {
    parameters$log_gp_alpha <- init$log_gp_alpha %||% log(1)
    parameters$log_gp_ell   <- init$log_gp_ell   %||% log(1)
    parameters$basis_coefs  <- init$basis_coefs  %||% matrix(0, data$num_basis, n_strata)
    random <- "basis_coefs"
  } else if (epidemic_model == 2L) {
    parameters$ar_phi_unc       <- init$ar_phi_unc %||% rep(0, n_strata)
    parameters$log_ar_sigma_unc <- init$log_ar_sigma_unc %||% rep(-2, n_strata)
    parameters$ar_innov         <- init$ar_innov %||% matrix(0, n_time, n_strata)
    random <- "ar_innov"
  } else {  # SIR (coupled): per-stratum R0/gamma/N_eff + shared-FOI beta random walk
    parameters$log_R0  <- init$log_R0  %||% rep(log(2), n_strata)
    parameters$u_gamma <- init$u_gamma %||% rep(stats::qlogis(1/5), n_strata)
    parameters$u_neff  <- init$u_neff  %||% rep(stats::qlogis(0.5), n_strata)
    parameters$ar_phi_unc       <- init$ar_phi_unc %||% rep(0, n_strata)
    parameters$log_ar_sigma_unc <- init$log_ar_sigma_unc %||% rep(-2, n_strata)
    parameters$ar_innov         <- init$ar_innov %||% matrix(0, n_time, n_strata)
    random <- "ar_innov"
  }

  # Defensive: per-stratum vector params must have length num_strata even if a
  # warm-start / ladder seed supplied a scalar.
  for (nm in intersect(c("mu_intercept", "ar_phi_unc", "log_ar_sigma_unc", "log_R0", "u_gamma", "u_neff"),
                       names(parameters)))
    if (length(parameters[[nm]]) != n_strata) parameters[[nm]] <- rep_len(parameters[[nm]], n_strata)

  map <- list()
  if (!is_nonparametric && !is_custom_delay) {
    if (delay_mu_is_fixed)    map$delay_mu <- factor(NA)
    if (delay_sigma_is_fixed) map$log_delay_sigma_excess <- factor(NA)
    if (is_gengamma && shape_Q_is_fixed) map$delay_Q <- factor(NA)
  } else if (is_custom_delay && any(custom_is_free == 0L)) {
    map_vals <- rep(NA_integer_, n_params_custom)
    free_idx <- 0L
    for (i in seq_len(n_params_custom)) {
      if (custom_is_free[i] == 1L) {
        free_idx <- free_idx + 1L
        map_vals[i] <- free_idx
      }
    }
    map$custom_delay_params <- factor(map_vals)
  }

  negative_log_posterior <- function(params) {
    RTMB::getAll(params, objective_data)
    "[<-" <- RTMB::ADoverload("[<-")
    log_jacobian <- 0

    # -- shared delay distribution ------------------------------------------
    if (delay_fully_fixed == 1L) {
      delay_fns <- NULL
    } else if (is_nonparametric == 1L) {
      exp_logits    <- exp(delay_logits)
      simplex_probs <- c(exp_logits, exp(0 * delay_logits[1])) / (sum(exp_logits) + 1)
      np_fns        <- .nonparametric_delay_functions(simplex_probs, n_bins)
      delay_fns     <- list(cdf = np_fns$cdf)
    } else if (is_custom_delay == 1L) {
      delay_fns <- cdf_factory(custom_delay_params)
    } else {
      delay_log_mean <- if (delay_mu_is_fixed == 1L) delay_mu_fixed else delay_mu
      delay_sd       <- if (delay_sigma_is_fixed == 1L) delay_sigma_fixed else 0.01 + exp(log_delay_sigma_excess)
      if (delay_sigma_is_fixed == 0L) log_jacobian <- log_jacobian + log_delay_sigma_excess
      shape_Q <- 0
      if (is_gengamma == 1L) {
        if (shape_Q_is_fixed == 1L) shape_Q <- shape_Q_fixed
        else { shape_transform <- .gengamma_shape_transform(delay_Q)
               shape_Q <- shape_transform$shape_Q; log_jacobian <- log_jacobian + shape_transform$log_jacobian }
      }
      delay_fns <- if (is_gengamma == 1L) .delay_distribution_functions(3L, delay_log_mean, shape_Q, delay_sd)
                   else                   .delay_distribution_functions(family, delay_log_mean, delay_sd)
    }
    cdf_fn <- if (delay_fully_fixed == 1L) NULL else delay_fns$cdf
    upper_bound <- mu_log_upper_bound
    nb_size <- if (is_negbin == 1L) 1.0 / exp(log_phi_nb) else 0

    # -- per-stratum epidemic mean + S_k accumulation -----------------------
    # Accumulate the count log-likelihood cell-by-cell.  For HSGP/AR1 each
    # stratum is independent (column loop); SIR couples strata via sum(I).
    log_mean_matrix <- NULL                       # built only for SIR (coupled)
    if (is_sir == 1L) {
      R0 <- exp(log_R0); recovery_rate <- plogis(u_gamma); susceptible_frac <- plogis(u_neff)
      effective_pop <- susceptible_frac * N_pop
      ar_phi   <- -0.999 + 1.998 * plogis(ar_phi_unc)
      ar_sigma <- ar_sigma_max * plogis(log_ar_sigma_unc)
      log_beta_baseline <- log(R0 * recovery_rate)
      incidence  <- matrix(0.0, n_time, n_strata)
      susceptible <- 1 - initial_infected / effective_pop
      infected    <- initial_infected / effective_pop
      trend_cols  <- vector("list", n_strata)
      for (s in seq_len(n_strata)) trend_cols[[s]] <- ar1_trend(ar_innov[, s], ar_phi[s], ar_sigma[s])
      for (t in seq_len(n_time)) {
        total_infectious <- sum(infected)                       # coupled force of infection
        for (s in seq_len(n_strata)) {
          beta_ts <- exp(log_beta_baseline[s] + trend_cols[[s]][t])
          new_infections <- susceptible[s] * (1 - exp(-beta_ts * total_infectious))
          incidence[t, s] <- new_infections * effective_pop[s]
          susceptible[s]  <- susceptible[s] * exp(-beta_ts * total_infectious)
          infected[s]     <- new_infections + (1 - recovery_rate[s]) * infected[s]
        }
      }
      # Guard the SIR incidence: the discrete recursion can drive `incidence`
      # negative for extreme parameter values the optimizer explores on sparse
      # data, turning log() into NaN and killing the fit.  (incidence+|incidence|)/2
      # is pmax(incidence, 0) -- IDENTICAL to `incidence` whenever it is >= 0 (every
      # valid fit), so successful fits are unchanged; it only replaces the NaN with
      # a finite log(1e-8) penalty in the pathological region so nlminb can recover.
      log_mean_matrix <- log((incidence + abs(incidence)) * 0.5 + 1e-8)
      log_jacobian <- log_jacobian +
        sum(log_R0 + log(recovery_rate) + log(1 - recovery_rate) + log(susceptible_frac) + log(1 - susceptible_frac) +
            log(1.998) + log(plogis(ar_phi_unc)) + log(1 - plogis(ar_phi_unc)) +
            log(ar_sigma_max) + log(plogis(log_ar_sigma_unc)) + log(1 - plogis(log_ar_sigma_unc)))
    } else if (epidemic_model == 1L) {
      gp_alpha <- exp(log_gp_alpha); gp_ell <- exp(log_gp_ell)
      spectral_weights <- hsgp_spectral_weights(hsgp_frequencies, gp_alpha, gp_ell, gp_kernel)
      log_jacobian <- log_jacobian + log_gp_alpha + log_gp_ell
    } else {
      ar_phi   <- -0.999 + 1.998 * plogis(ar_phi_unc)
      ar_sigma <- ar_sigma_max * plogis(log_ar_sigma_unc)
      log_jacobian <- log_jacobian +
        sum(log(1.998) + log(plogis(ar_phi_unc)) + log(1 - plogis(ar_phi_unc)) +
            log(ar_sigma_max) + log(plogis(log_ar_sigma_unc)) + log(1 - plogis(log_ar_sigma_unc)))
    }

    # -- hierarchical intercept reconstruction ---------------------------------
    if (is_hierarchical == 1L && is_sir == 0L) {
      tau_int <- exp(log_tau_intercept)
      mu_intercept_hier <- mu_global + tau_int * delta_intercept
      log_jacobian <- log_jacobian + log_tau_intercept   # Jacobian for tau = exp(log_tau)
    }

    loglik_counts <- 0
    for (s in seq_len(n_strata)) {
      if (is_sir == 1L) {
        log_mean_col <- log_mean_matrix[, s]
      } else {
        intercept_s  <- if (is_hierarchical == 1L) mu_intercept_hier[s] else mu_intercept[s]
        log_mean_col <- rep(intercept_s, n_time)
        if (n_covariates > 0) log_mean_col <- log_mean_col + as.vector(X %*% gamma[, s])
        if (epidemic_model == 1L)
          log_mean_col <- log_mean_col + as.vector(hsgp_basis_matrix %*% (basis_coefs[, s] * spectral_weights))
        else
          log_mean_col <- log_mean_col + ar1_trend(ar_innov[, s], ar_phi[s], ar_sigma[s])
      }
      log_mean_capped <- upper_bound - log1p(exp(upper_bound - log_mean_col))
      lambda <- exp(log_mean_capped)
      gstar  <- if (delay_fully_fixed == 1L) gstar_precomputed[, s] else cdf_fn(d_star[, s] + 1)
      counts_col <- case_counts[, s]
      if (is_negbin == 1L) {
        success_prob <- nb_size / (nb_size + lambda)
        loglik_counts <- loglik_counts +
          sum(counts_col * log1p(-success_prob)) + sum(nb_size * log(success_prob)) +
          sum(lgamma(counts_col + nb_size) - lgamma(nb_size) - lgamma(counts_col + 1)) -
          sum((counts_col + nb_size) * log(success_prob + gstar * (1 - success_prob)))
      } else {
        loglik_counts <- loglik_counts + sum(counts_col * log_mean_capped) - sum(gstar * lambda)
      }
    }
    if (is_negbin == 1L) log_jacobian <- log_jacobian + log_phi_nb

    # -- shared delay PMF likelihood (pooled over strata) --------------------
    loglik_delay <- 0
    if (delay_fully_fixed == 0L && length(obs_delays) > 0) {
      loglik_delay <- if (is_nonparametric == 1L)
        sum(row_sums * np_fns$log_pmf_raw(obs_delays))
      else
        .discretised_delay_loglik(obs_delays, row_sums, split_delay,
                                  delay_fns$log_cdf, delay_fns$log_survival)
    }
    # Right-censored delays: we only know the delay is <= j, contributing
    # log G_D(j) (the article's m_j^* term). G_D = CDF of the delay process.
    if (delay_fully_fixed == 0L && length(obs_delays_cens) > 0) {
      log_cdf_cens <- if (is_nonparametric == 1L) np_fns$log_cdf(obs_delays_cens)
                      else delay_fns$log_cdf(obs_delays_cens)
      loglik_delay <- loglik_delay + sum(row_sums_cens * log_cdf_cens)
    }

    # -- priors --------------------------------------------------------------
    log_prior <- 0
    if (delay_fully_fixed == 0L) {
      if (is_nonparametric == 1L) {
        log_prior <- log_prior + dirichlet_lpdf(simplex_probs, dirichlet_alpha) + sum(log(simplex_probs))
      } else if (is_custom_delay == 1L) {
        for (i in seq_len(n_params_custom)) {
          if (custom_is_free[i] == 1L)
            log_prior <- log_prior +
              prior_lpdf(custom_delay_params[i], custom_prior_dists[i], custom_prior_params[i, ])
        }
      } else {
        if (delay_mu_is_fixed == 0L)    log_prior <- log_prior + prior_lpdf(delay_log_mean, prior_mu_dist, prior_mu_params)
        if (delay_sigma_is_fixed == 0L) log_prior <- log_prior + prior_lpdf(delay_sd, prior_sigma_dist, prior_sigma_params)
        if (is_gengamma == 1L && shape_Q_is_fixed == 0L)
          log_prior <- log_prior + prior_lpdf(shape_Q, prior_shape_dist, prior_shape_params)
      }
    }
    if (is_sir == 0L) {
      if (is_hierarchical == 1L) {
        # Hierarchical intercept prior: mu_global ~ intercept_prior; delta ~ N(0,1); tau ~ HalfNormal(0,1)
        log_prior <- log_prior + prior_lpdf(mu_global, prior_intercept_dist, prior_intercept_params)
        log_prior <- log_prior + sum(dnorm(delta_intercept, 0, 1, log = TRUE))
        log_prior <- log_prior + dnorm(tau_int, 0, 1, log = TRUE)   # HalfNormal: tau > 0 always here
      } else {
        log_prior <- log_prior + prior_lpdf(mu_intercept, prior_intercept_dist, prior_intercept_params)
      }
    }
    if (is_sir == 0L && n_covariates > 0) log_prior <- log_prior + prior_lpdf(as.vector(gamma), prior_gamma_dist, prior_gamma_params)
    if (is_negbin == 1L) log_prior <- log_prior + prior_lpdf(1.0 / nb_size, prior_phi_dist, prior_phi_params)
    if (epidemic_model == 1L) {
      log_prior <- log_prior + prior_lpdf(gp_alpha, prior_gp_alpha_dist, prior_gp_alpha_params)
      log_prior <- log_prior + prior_lpdf(gp_ell,   prior_gp_ell_dist,   prior_gp_ell_params)
      log_prior <- log_prior + sum(dnorm(basis_coefs, 0, 1, log = TRUE))
    } else if (epidemic_model == 2L) {
      log_prior <- log_prior + prior_lpdf(ar_phi,   prior_ar_phi_dist,   prior_ar_phi_params)
      log_prior <- log_prior + prior_lpdf(ar_sigma, prior_ar_sigma_dist, prior_ar_sigma_params)
      log_prior <- log_prior + sum(dnorm(ar_innov, 0, 1, log = TRUE))
    } else {  # SIR
      log_prior <- log_prior + prior_lpdf(R0, prior_R0_dist, prior_R0_params)
      log_prior <- log_prior + prior_lpdf(recovery_rate, prior_gamma_sir_dist, prior_gamma_sir_params)
      log_prior <- log_prior + prior_lpdf(susceptible_frac, prior_n_eff_dist, prior_n_eff_params)
      log_prior <- log_prior + prior_lpdf(ar_phi, prior_ar_phi_sir_dist, prior_ar_phi_sir_params)
      log_prior <- log_prior + prior_lpdf(ar_sigma, prior_ar_sigma_dist, prior_ar_sigma_params)
      log_prior <- log_prior + sum(dnorm(ar_innov, 0, 1, log = TRUE))
    }

    -(loglik_delay + loglik_counts + log_prior + log_jacobian)
  }

  random_arg <- if (use_random) random else NULL
  obj <- RTMB::MakeADFun(negative_log_posterior, parameters, map = map, random = random_arg, silent = TRUE)
  list(obj = obj, random = random, epi_model = epidemic_model, is_nb = is_negbin,
       Bmat = hsgp_basis_matrix, freq = hsgp_frequencies, n_strata = n_strata)
}

#' Reconstruct per-(time, stratum) lambda / Gstar (plain numeric) from a fit
#'
#' Mirrors the objective's per-stratum / coupled-SIR mean construction in base
#' R.  Latent matrices are reshaped defensively (parList gives matrices;
#' .split_named_vector gives column-major flat vectors).  Returns `[n_time x
#' n_strata]` matrices.
#' @keywords internal
#' @noRd
.joint_reconstruct <- function(data, priors, parlist, hsgp_basis_matrix, hsgp_frequencies) {
  n_time <- data$max_time; n_strata <- as.integer(data$num_strata)
  family <- data$delay_family; is_gengamma <- family == 3L; is_nonparametric <- family == 4L
  is_custom_delay_r <- family == 5L
  reshape <- function(x, nr, nc) matrix(as.numeric(x), nr, nc)

  # -- shared delay --------------------------------------------------------
  if (is_nonparametric) {
    n_bins <- as.integer(data$np_model_length)
    simplex_probs <- if (isTRUE(priors$delay_probs$is_constant == 1L)) priors$delay_probs$fixed
      else { el <- exp(parlist$delay_logits); c(el, 1) / (sum(el) + 1) }
    delay_fns <- .nonparametric_delay_functions(simplex_probs, n_bins)
    delay_log_mean <- delay_sd <- NA_real_
  } else if (is_custom_delay_r) {
    theta_custom <- as.numeric(parlist$custom_delay_params)
    delay_fns    <- priors$cdf_factory(theta_custom)
    delay_log_mean <- delay_sd <- NA_real_
  } else {
    fix_mu <- isTRUE(priors$delay_mu$is_constant == 1L); fix_sig <- isTRUE(priors$delay_sigma$is_constant == 1L)
    fix_Q  <- is_gengamma && isTRUE(priors$delay_Q$is_constant == 1L)
    delay_log_mean <- if (fix_mu) priors$delay_mu$fixed else parlist$delay_mu
    delay_sd       <- if (fix_sig) priors$delay_sigma$fixed else 0.01 + exp(parlist$log_delay_sigma_excess)
    shape_Q        <- if (is_gengamma) (if (fix_Q) priors$delay_Q$fixed else .gengamma_shape_transform(parlist$delay_Q)$shape_Q) else 0
    delay_fns <- if (is_gengamma) .delay_distribution_functions(3L, delay_log_mean, shape_Q, delay_sd)
                 else             .delay_distribution_functions(family, delay_log_mean, delay_sd)
  }
  d_star <- if (is.matrix(data$d_star)) data$d_star else matrix(data$d_star, n_time, n_strata)

  # -- per-(time, stratum) log-mean -------------------------------------------
  log_mean <- matrix(0.0, n_time, n_strata)
  if (data$epidemic_model == 3L) {                              # coupled SIR
    R0 <- exp(parlist$log_R0); recovery_rate <- stats::plogis(parlist$u_gamma)
    susceptible_frac <- stats::plogis(parlist$u_neff); effective_pop <- susceptible_frac * data$N_pop
    initial_infected <- data$case_counts[1, ]
    ar_phi   <- -0.999 + 1.998 * stats::plogis(parlist$ar_phi_unc)
    ar_sigma <- data$ar_sigma_max * stats::plogis(parlist$log_ar_sigma_unc)
    ar_innov <- reshape(parlist$ar_innov, n_time, n_strata)
    trend <- matrix(0.0, n_time, n_strata)
    for (s in seq_len(n_strata)) {
      trend[1, s] <- ar_innov[1, s] * ar_sigma[s] / sqrt(1 - ar_phi[s]^2)
      if (n_time >= 2) for (t in 2:n_time) trend[t, s] <- ar_phi[s] * trend[t - 1, s] + ar_innov[t, s] * ar_sigma[s]
    }
    beta0 <- log(R0 * recovery_rate); incidence <- matrix(0.0, n_time, n_strata)
    susceptible <- 1 - initial_infected / effective_pop; infected <- initial_infected / effective_pop
    for (t in seq_len(n_time)) {
      total_infectious <- sum(infected)
      for (s in seq_len(n_strata)) {
        beta_ts <- exp(beta0[s] + trend[t, s])
        new_inf <- susceptible[s] * (1 - exp(-beta_ts * total_infectious))
        incidence[t, s] <- new_inf * effective_pop[s]
        susceptible[s]  <- susceptible[s] * exp(-beta_ts * total_infectious)
        infected[s]     <- new_inf + (1 - recovery_rate[s]) * infected[s]
      }
    }
    # See the note above: (incidence+|incidence|)/2 = pmax(incidence, 0) guards the
    # SIR log-mean against NaN without changing any fit where incidence >= 0.
    log_mean <- log((incidence + abs(incidence)) * 0.5 + 1e-8)
  } else {
    # Resolve intercept: hierarchical or independent
    is_hierarchical_r <- !is.null(parlist$mu_global)
    mu_intercept <- if (is_hierarchical_r)
      as.numeric(parlist$mu_global) + exp(as.numeric(parlist$log_tau_intercept)) * as.numeric(parlist$delta_intercept)
    else parlist$mu_intercept
    gamma <- if (data$P > 0) reshape(parlist$gamma, data$P, n_strata) else NULL
    if (data$epidemic_model == 1L) {
      gp_alpha <- exp(parlist$log_gp_alpha); gp_ell <- exp(parlist$log_gp_ell)
      spectral_weights <- hsgp_spectral_weights(hsgp_frequencies, gp_alpha, gp_ell, data$gp_kernel)
      basis_coefs <- reshape(parlist$basis_coefs, ncol(hsgp_basis_matrix), n_strata)
    } else {
      ar_phi   <- -0.999 + 1.998 * stats::plogis(parlist$ar_phi_unc)
      ar_sigma <- data$ar_sigma_max * stats::plogis(parlist$log_ar_sigma_unc)
      ar_innov <- reshape(parlist$ar_innov, n_time, n_strata)
    }
    for (s in seq_len(n_strata)) {
      col <- rep(mu_intercept[s], n_time)
      if (!is.null(gamma)) col <- col + as.vector(data$X %*% gamma[, s])
      if (data$epidemic_model == 1L) {
        col <- col + as.vector(hsgp_basis_matrix %*% (basis_coefs[, s] * spectral_weights))
      } else {
        tr <- numeric(n_time); tr[1] <- ar_innov[1, s] * ar_sigma[s] / sqrt(1 - ar_phi[s]^2)
        if (n_time >= 2) for (t in 2:n_time) tr[t] <- ar_phi[s] * tr[t - 1] + ar_innov[t, s] * ar_sigma[s]
        col <- col + tr
      }
      log_mean[, s] <- col
    }
  }

  ub <- data$mu_log_upper_bound
  mu_safe <- ub - log1p(exp(ub - log_mean))
  lambda  <- exp(mu_safe)
  Gstar   <- matrix(0.0, n_time, n_strata)
  for (s in seq_len(n_strata)) Gstar[, s] <- as.numeric(delay_fns$cdf(d_star[, s] + 1))
  phi_nb  <- if (data$is_negative_binomial == 1L) exp(parlist$log_phi_nb) else NA_real_

  list(mu = log_mean, mu_safe = mu_safe, lambda = lambda, Gstar = Gstar,
       log_loc = if (!is.null(delay_fns$log_location)) delay_fns$log_location else NA_real_,
       log_scale = if (!is.null(delay_fns$log_scale)) delay_fns$log_scale else NA_real_,
       delay_mu = delay_log_mean, delay_sigma = delay_sd, phi_nb = phi_nb)
}
