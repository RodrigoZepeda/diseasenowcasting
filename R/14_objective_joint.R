# =============================================================================
# Joint RTMB objective: latent epidemic (Laplace) + delay + S_k likelihood
# =============================================================================
# Reproduces the Stan joint log-posterior under delay_is_censored = 0:
#   - delay PMF likelihood uses the PLAIN discretised PMF (survival-tail robust);
#     right-censoring of recent counts enters only through Gstar.
#   - log_mean[t] = mu_intercept + (X gamma)[t] + epidemic_trend[t]
#   - Gstar[t] = F_delay(d_star[t] + 1)
#   - S_k: Poisson or NB-2 (r = 1 / phi_nb), summed over t.
#     (the dropped Poisson -log(k!) is data-constant: irrelevant for the mode,
#      so absolute log-posterior values won't equal Stan's — only modes/diffs do.)
# Latent epidemic coefficients (AR1 -> ar_innov ; HSGP -> basis_coefs ; SIR ->
# the beta random-walk ar_innov) are declared `random` only when use_random=TRUE.
# Parametric delay families 1 (LogNormal), 2 (Gamma), 3 (Generalized Gamma),
# 4 (Dirichlet / non-parametric).
#
# NAMING: verbose names for generic quantities; standard domain symbols kept
# (Gstar, lambda, R0, beta, ar_phi, ar_sigma, phi_nb) and the model PARAMETER
# names are unchanged (they are referenced by parList in the reconstruct/nowcast).
# =============================================================================

#' Build the joint RTMB objective
#'
#' @param use_random If TRUE the latent epidemic coefficients (`basis_coefs` /
#'   `ar_innov`) are declared `random` so RTMB does the marginal (nested)
#'   Laplace.  If FALSE (the default via [fit()]) they stay fixed effects and the
#'   whole parameter vector is optimised jointly — the single-optimisation,
#'   joint-mode Laplace regime cmdstanr `$laplace()` uses (much faster: no nested
#'   inner Newton solve at every outer step).
#' @keywords internal
#' @noRd
build_joint_obj <- function(data, priors, init = NULL, use_random = TRUE) {
  family <- data$delay_family
  if (!family %in% c(1L, 2L, 3L, 4L))
    cli::cli_abort("build_joint_obj supports delay families 1/2/3/4; family {family} given.")
  is_gengamma      <- family == 3L
  is_nonparametric <- family == 4L
  n_bins           <- if (is_nonparametric) as.integer(data$np_model_length) else 0L
  epidemic_model   <- data$epidemic_model
  if (!epidemic_model %in% c(1L, 2L, 3L))
    cli::cli_abort("build_joint_obj supports HSGP (1), AR1 (2), SIR (3) epidemic.")
  is_sir    <- epidemic_model == 3L
  is_negbin <- data$is_negative_binomial == 1L
  n_covariates <- data$P
  n_time       <- data$max_time

  if (epidemic_model == 1L) {
    time_scaled       <- hsgp_time_scaled(n_time, data$tmax_model)
    hsgp_basis_matrix <- hsgp_basis(time_scaled, data$gp_L_left, data$gp_L_right,
                                    data$num_basis, data$gp_basis)
    hsgp_frequencies  <- seq_len(data$num_basis) * pi / (data$gp_L_left + data$gp_L_right)
  } else { hsgp_basis_matrix <- matrix(0.0, n_time, 0L); hsgp_frequencies <- numeric(0) }

  delay_mu_is_fixed    <- !is_nonparametric && isTRUE(priors$delay_mu$is_constant == 1L)
  delay_sigma_is_fixed <- !is_nonparametric && isTRUE(priors$delay_sigma$is_constant == 1L)
  shape_Q_is_fixed     <- is_gengamma && isTRUE(priors$delay_Q$is_constant == 1L)
  dirichlet_alpha      <- if (is_nonparametric) priors$delay_probs$params else numeric(0)

  # When every active delay parameter is fixed (the multisample Stage-2 regime),
  # the delay distribution — and hence Gstar = F_delay(d_star+1) and the delay
  # PMF term — are CONSTANT.  Precompute Gstar once in plain R and pass it as
  # data so the (expensive, esp. Gamma/GenGamma) CDF is NOT re-taped/re-evaluated
  # at every optimiser iteration over all n_time points.  Biggest single speedup
  # for the delay-fixed fits that dominate the multisample.
  delay_probs_fixed <- is_nonparametric && isTRUE(priors$delay_probs$is_constant == 1L)
  delay_fully_fixed <- (!is_nonparametric && delay_mu_is_fixed && delay_sigma_is_fixed &&
                        (!is_gengamma || shape_Q_is_fixed)) || delay_probs_fixed
  gstar_precomputed <- numeric(0)
  if (delay_fully_fixed) {
    fixed_delay_fns <- if (is_nonparametric)
        .nonparametric_delay_functions(priors$delay_probs$fixed, n_bins)
      else if (is_gengamma)
        .delay_distribution_functions(3L, priors$delay_mu$fixed, priors$delay_Q$fixed, priors$delay_sigma$fixed)
      else
        .delay_distribution_functions(family, priors$delay_mu$fixed, priors$delay_sigma$fixed)
    gstar_precomputed <- as.numeric(fixed_delay_fns$cdf(data$d_star + 1))
  }

  objective_data <- list(
    family = family, is_gengamma = as.integer(is_gengamma),
    is_nonparametric = as.integer(is_nonparametric), n_bins = n_bins,
    dirichlet_alpha = dirichlet_alpha,
    delay_fully_fixed = as.integer(delay_fully_fixed), gstar_precomputed = gstar_precomputed,
    case_counts = data$case_counts, d_star = data$d_star,
    obs_delays = data$obs_delays, row_sums = data$row_sums_exact,
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
    # SIR
    is_sir = as.integer(is_sir), N_pop = data$N_pop, initial_infected = data$case_counts[1],
    prior_R0_dist = if (is_sir) priors$R0$dist else 0L, prior_R0_params = if (is_sir) .pad3(priors$R0$params) else c(0, 0, 0),
    prior_gamma_sir_dist = if (is_sir) priors$gamma_sir$dist else 0L,
    prior_gamma_sir_params = if (is_sir) .pad3(priors$gamma_sir$params) else c(0, 0, 0),
    prior_n_eff_dist = if (is_sir) priors$N_eff$dist else 0L, prior_n_eff_params = if (is_sir) .pad3(priors$N_eff$params) else c(0, 0, 0),
    delay_mu_is_fixed = as.integer(delay_mu_is_fixed), delay_mu_fixed = if (delay_mu_is_fixed) priors$delay_mu$fixed else 0,
    delay_sigma_is_fixed = as.integer(delay_sigma_is_fixed), delay_sigma_fixed = if (delay_sigma_is_fixed) priors$delay_sigma$fixed else 0,
    shape_Q_is_fixed = as.integer(shape_Q_is_fixed), shape_Q_fixed = if (shape_Q_is_fixed) priors$delay_Q$fixed else 0
  )

  init <- init %||% list()
  intercept_init <- init$mu_intercept %||% { positive_counts <- data$case_counts[data$case_counts > 0]
    if (length(positive_counts)) log(stats::median(positive_counts)) else 0 }
  delay_mu_init <- init$delay_mu %||% log(max(.wtd_median(data$m[, 3], data$m[, 2]), 1.5))
  delay_sigma_init <- init$delay_sigma %||% {
    if (is_gengamma) 0.6 else { empirical_sd <- sqrt(.wtd_var(data$m[, 3], data$m[, 2]))
      if (is.finite(empirical_sd) && empirical_sd > 0) max(2, min(empirical_sd, 60)) else 5 } }

  # SIR has its own absolute scale (no level intercept, no covariates).
  parameters <- if (is_sir) list()
                else list(mu_intercept = intercept_init,
                          gamma = if (n_covariates > 0) (init$gamma %||% rep(0, n_covariates)) else numeric(0))
  if (is_nonparametric) {
    # NP simplex via softmax with reference category: p = c(exp(z), 1)/(sum(exp(z)) + 1).
    # When the simplex is hard-fixed (Stage-2 of the two-stage Dirichlet) there
    # is no free delay parameter — Gstar is precomputed and the PMF dropped.
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
  } else {
    parameters$delay_mu               <- if (delay_mu_is_fixed) 0 else delay_mu_init
    parameters$log_delay_sigma_excess <- if (delay_sigma_is_fixed) 0 else log(max(delay_sigma_init - 0.01, 1e-6))
    if (is_gengamma) parameters$delay_Q <- if (shape_Q_is_fixed) 0 else (init$delay_Q %||% -2)  # raw
  }
  if (is_negbin) parameters$log_phi_nb <- init$log_phi_nb %||% log(20)
  if (epidemic_model == 1L) {
    parameters$log_gp_alpha <- init$log_gp_alpha %||% log(1)
    parameters$log_gp_ell   <- init$log_gp_ell   %||% log(1)
    parameters$basis_coefs  <- init$basis_coefs  %||% rep(0, data$num_basis)
    random <- "basis_coefs"
  } else if (epidemic_model == 2L) {
    parameters$ar_phi_unc <- init$ar_phi_unc %||% 0
    parameters$log_ar_sigma_unc <- init$log_ar_sigma_unc %||% (-2)
    parameters$ar_innov <- init$ar_innov %||% rep(0, n_time)
    random <- "ar_innov"
  } else {  # SIR: log beta follows an AR(1) random walk (ar_innov = latent)
    parameters$log_R0 <- init$log_R0 %||% log(2)
    parameters$u_gamma <- init$u_gamma %||% stats::qlogis(1/5)
    parameters$u_neff  <- init$u_neff  %||% stats::qlogis(0.5)
    parameters$ar_phi_unc <- init$ar_phi_unc %||% 0
    parameters$log_ar_sigma_unc <- init$log_ar_sigma_unc %||% (-2)
    parameters$ar_innov <- init$ar_innov %||% rep(0, n_time)
    random <- "ar_innov"
  }

  map <- list()
  if (!is_nonparametric) {
    if (delay_mu_is_fixed)    map$delay_mu <- factor(NA)
    if (delay_sigma_is_fixed) map$log_delay_sigma_excess <- factor(NA)
    if (is_gengamma && shape_Q_is_fixed) map$delay_Q <- factor(NA)
  }

  negative_log_posterior <- function(params) {
    RTMB::getAll(params, objective_data)
    # The SIR incidence recursion below subassigns advectors into a numeric
    # vector (incidence[t] <- ...); enable RTMB's in-place [<- for THIS scope.
    "[<-" <- RTMB::ADoverload("[<-")

    log_jacobian <- 0
    if (delay_fully_fixed == 1L) {
      delay_fns <- NULL                 # delay constant: Gstar precomputed, PMF dropped
    } else if (is_nonparametric == 1L) {
      exp_logits   <- exp(delay_logits)
      simplex_probs <- c(exp_logits, exp(0 * delay_logits[1])) / (sum(exp_logits) + 1)
      np_fns       <- .nonparametric_delay_functions(simplex_probs, n_bins)
      delay_fns    <- list(log_cdf = np_fns$log_cdf, cdf = np_fns$cdf)
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
      delay_fns <- if (is_gengamma == 1L)
          .delay_distribution_functions(3L, delay_log_mean, shape_Q, delay_sd)
        else
          .delay_distribution_functions(family, delay_log_mean, delay_sd)
    }

    # ── epidemic mean (log scale) over time ────────────────────────────────
    if (is_sir == 1L) {
      R0 <- exp(log_R0); recovery_rate <- plogis(u_gamma); susceptible_frac <- plogis(u_neff)
      effective_pop <- susceptible_frac * N_pop
      ar_phi   <- -0.999 + 1.998 * plogis(ar_phi_unc)
      ar_sigma <- ar_sigma_max * plogis(log_ar_sigma_unc)
      log_beta_trend <- ar1_trend(ar_innov, ar_phi, ar_sigma)
      log_beta_baseline <- log(R0 * recovery_rate)
      incidence  <- numeric(length(case_counts))
      susceptible <- 1 - initial_infected / effective_pop
      infected    <- initial_infected / effective_pop
      for (t in seq_along(case_counts)) {
        beta_t <- exp(log_beta_baseline + log_beta_trend[t])
        new_infections <- susceptible * (1 - exp(-beta_t * infected))
        incidence[t]   <- new_infections * effective_pop
        susceptible    <- susceptible * exp(-beta_t * infected)
        infected       <- new_infections + (1 - recovery_rate) * infected
      }
      log_mean_t <- log(incidence + 1e-8)
      log_jacobian <- log_jacobian + log_R0 +
        log(recovery_rate) + log(1 - recovery_rate) + log(susceptible_frac) + log(1 - susceptible_frac) +
        log(1.998) + log(plogis(ar_phi_unc)) + log(1 - plogis(ar_phi_unc)) +
        log(ar_sigma_max) + log(plogis(log_ar_sigma_unc)) + log(1 - plogis(log_ar_sigma_unc))
    } else {
      log_mean_t <- rep(mu_intercept, length(case_counts))
      if (n_covariates > 0) log_mean_t <- log_mean_t + as.vector(X %*% gamma)
      if (epidemic_model == 1L) {
        gp_alpha <- exp(log_gp_alpha); gp_ell <- exp(log_gp_ell)
        spectral_weights <- hsgp_spectral_weights(hsgp_frequencies, gp_alpha, gp_ell, gp_kernel)
        log_mean_t <- log_mean_t + as.vector(hsgp_basis_matrix %*% (basis_coefs * spectral_weights))
        log_jacobian <- log_jacobian + log_gp_alpha + log_gp_ell
      } else {
        ar_phi   <- -0.999 + 1.998 * plogis(ar_phi_unc)
        ar_sigma <- ar_sigma_max * plogis(log_ar_sigma_unc)
        log_mean_t <- log_mean_t + ar1_trend(ar_innov, ar_phi, ar_sigma)
        log_jacobian <- log_jacobian +
          log(1.998) + log(plogis(ar_phi_unc)) + log(1 - plogis(ar_phi_unc)) +
          log(ar_sigma_max) + log(plogis(log_ar_sigma_unc)) + log(1 - plogis(log_ar_sigma_unc))
      }
    }
    upper_bound  <- mu_log_upper_bound
    log_mean_capped <- upper_bound - log1p(exp(upper_bound - log_mean_t))   # smooth anti-explosion cap
    lambda <- exp(log_mean_capped)

    Gstar <- if (delay_fully_fixed == 1L) gstar_precomputed else delay_fns$cdf(d_star + 1)

    loglik_delay <- 0
    if (delay_fully_fixed == 0L && length(obs_delays) > 0) {
      loglik_delay <- if (is_nonparametric == 1L) sum(row_sums * np_fns$log_pmf_raw(obs_delays))
                      else .discretised_delay_loglik(obs_delays, row_sums, split_delay,
                                                     delay_fns$log_cdf, delay_fns$log_survival)
    }

    if (is_negbin == 1L) {
      nb_size <- 1.0 / exp(log_phi_nb)
      success_prob <- nb_size / (nb_size + lambda)
      loglik_counts <- sum(case_counts * log1p(-success_prob)) + sum(nb_size * log(success_prob)) +
        sum(lgamma(case_counts + nb_size) - lgamma(nb_size) - lgamma(case_counts + 1)) -
        sum((case_counts + nb_size) * log(success_prob + Gstar * (1 - success_prob)))
      log_jacobian <- log_jacobian + log_phi_nb
    } else {
      loglik_counts <- sum(case_counts * log_mean_capped) - sum(Gstar * lambda)
    }

    log_prior <- 0
    if (delay_fully_fixed == 0L) {                 # fixed delay -> constant prior, dropped
      if (is_nonparametric == 1L) {
        log_prior <- log_prior + dirichlet_lpdf(simplex_probs, dirichlet_alpha) + sum(log(simplex_probs))  # + softmax-ref Jacobian
      } else {
        if (delay_mu_is_fixed == 0L)    log_prior <- log_prior + prior_lpdf(delay_log_mean, prior_mu_dist, prior_mu_params)
        if (delay_sigma_is_fixed == 0L) log_prior <- log_prior + prior_lpdf(delay_sd, prior_sigma_dist, prior_sigma_params)
        if (is_gengamma == 1L && shape_Q_is_fixed == 0L)
          log_prior <- log_prior + prior_lpdf(shape_Q, prior_shape_dist, prior_shape_params)
      }
    }
    if (is_sir == 0L) log_prior <- log_prior + prior_lpdf(mu_intercept, prior_intercept_dist, prior_intercept_params)
    if (is_sir == 0L && n_covariates > 0) log_prior <- log_prior + prior_lpdf(gamma, prior_gamma_dist, prior_gamma_params)
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

    RTMB::REPORT(log_mean_capped); RTMB::REPORT(lambda); RTMB::REPORT(Gstar)
    -(loglik_delay + loglik_counts + log_prior + log_jacobian)
  }

  random_arg <- if (use_random) random else NULL
  obj <- RTMB::MakeADFun(negative_log_posterior, parameters, map = map, random = random_arg, silent = TRUE)
  list(obj = obj, random = random, epi_model = epidemic_model, is_nb = is_negbin,
       Bmat = hsgp_basis_matrix, freq = hsgp_frequencies)
}

#' Reconstruct log-mean / lambda / Gstar (plain numeric) from a fitted parameter list
#' @keywords internal
#' @noRd
.joint_reconstruct <- function(data, priors, parlist, hsgp_basis_matrix, hsgp_frequencies) {
  n_time <- data$max_time; family <- data$delay_family
  is_gengamma <- family == 3L; is_nonparametric <- family == 4L
  if (is_nonparametric) {
    n_bins <- as.integer(data$np_model_length)
    simplex_probs <- if (isTRUE(priors$delay_probs$is_constant == 1L)) {
      priors$delay_probs$fixed                       # Stage-2: hard-fixed simplex
    } else {
      exp_logits <- exp(parlist$delay_logits); c(exp_logits, 1) / (sum(exp_logits) + 1)
    }
    delay_fns <- .nonparametric_delay_functions(simplex_probs, n_bins)
    delay_log_mean <- delay_sd <- NA_real_
  } else {
    delay_mu_is_fixed    <- isTRUE(priors$delay_mu$is_constant == 1L)
    delay_sigma_is_fixed <- isTRUE(priors$delay_sigma$is_constant == 1L)
    shape_Q_is_fixed     <- is_gengamma && isTRUE(priors$delay_Q$is_constant == 1L)
    delay_log_mean <- if (delay_mu_is_fixed) priors$delay_mu$fixed else parlist$delay_mu
    delay_sd       <- if (delay_sigma_is_fixed) priors$delay_sigma$fixed else 0.01 + exp(parlist$log_delay_sigma_excess)
    shape_Q        <- if (is_gengamma) (if (shape_Q_is_fixed) priors$delay_Q$fixed
                                        else .gengamma_shape_transform(parlist$delay_Q)$shape_Q) else 0
    delay_fns <- if (is_gengamma) .delay_distribution_functions(3L, delay_log_mean, shape_Q, delay_sd)
                 else             .delay_distribution_functions(family, delay_log_mean, delay_sd)
  }

  if (data$epidemic_model == 3L) {
    R0 <- exp(parlist$log_R0); recovery_rate <- stats::plogis(parlist$u_gamma)
    susceptible_frac <- stats::plogis(parlist$u_neff)
    effective_pop <- susceptible_frac * data$N_pop; initial_infected <- data$case_counts[1]
    ar_phi   <- -0.999 + 1.998 * stats::plogis(parlist$ar_phi_unc)
    ar_sigma <- data$ar_sigma_max * stats::plogis(parlist$log_ar_sigma_unc)
    log_beta_trend <- numeric(n_time)
    log_beta_trend[1] <- parlist$ar_innov[1] * ar_sigma / sqrt(1 - ar_phi^2)
    if (n_time >= 2) for (t in 2:n_time)
      log_beta_trend[t] <- ar_phi * log_beta_trend[t - 1] + parlist$ar_innov[t] * ar_sigma
    log_beta_baseline <- log(R0 * recovery_rate)
    incidence <- numeric(n_time); susceptible <- 1 - initial_infected / effective_pop
    infected  <- initial_infected / effective_pop
    for (t in seq_len(n_time)) {
      beta_t <- exp(log_beta_baseline + log_beta_trend[t])
      new_infections <- susceptible * (1 - exp(-beta_t * infected))
      incidence[t] <- new_infections * effective_pop
      susceptible  <- susceptible * exp(-beta_t * infected)
      infected     <- new_infections + (1 - recovery_rate) * infected
    }
    log_mean_t <- log(incidence + 1e-8)
  } else {
    log_mean_t <- rep(parlist$mu_intercept, n_time)
    if (data$P > 0) log_mean_t <- log_mean_t + as.vector(data$X %*% parlist$gamma)
    if (data$epidemic_model == 1L) {
      gp_alpha <- exp(parlist$log_gp_alpha); gp_ell <- exp(parlist$log_gp_ell)
      spectral_weights <- hsgp_spectral_weights(hsgp_frequencies, gp_alpha, gp_ell, data$gp_kernel)
      log_mean_t <- log_mean_t + as.vector(hsgp_basis_matrix %*% (parlist$basis_coefs * spectral_weights))
    } else {
      ar_phi   <- -0.999 + 1.998 * stats::plogis(parlist$ar_phi_unc)
      ar_sigma <- data$ar_sigma_max * stats::plogis(parlist$log_ar_sigma_unc)
      ar_trend <- numeric(n_time); ar_trend[1] <- parlist$ar_innov[1] * ar_sigma / sqrt(1 - ar_phi^2)
      if (n_time >= 2) for (t in 2:n_time)
        ar_trend[t] <- ar_phi * ar_trend[t - 1] + parlist$ar_innov[t] * ar_sigma
      log_mean_t <- log_mean_t + ar_trend
    }
  }
  upper_bound <- data$mu_log_upper_bound
  log_mean_capped <- upper_bound - log1p(exp(upper_bound - log_mean_t))
  lambda <- exp(log_mean_capped)
  Gstar  <- as.numeric(delay_fns$cdf(data$d_star + 1))
  phi_nb <- if (data$is_negative_binomial == 1L) exp(parlist$log_phi_nb) else NA_real_

  list(mu = log_mean_t, mu_safe = log_mean_capped, lambda = lambda, Gstar = Gstar,
       log_loc = if (!is.null(delay_fns$log_location)) delay_fns$log_location else NA_real_,
       log_scale = if (!is.null(delay_fns$log_scale)) delay_fns$log_scale else NA_real_,
       delay_mu = delay_log_mean, delay_sigma = delay_sd, phi_nb = phi_nb)
}
