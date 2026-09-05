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
  if (!epidemic_model %in% c(1L, 2L, 3L, 4L))
    cli::cli_abort("build_joint_obj supports HSGP (1), AR1 (2), SIR (3), Custom (4) epidemic.")
  is_sir            <- epidemic_model == 3L
  is_custom_epidemic <- epidemic_model == 4L
  # User-supplied functions need RTMB's AD methods on the search path (see helper).
  if (is_custom_delay || is_custom_epidemic) .assert_rtmb_attached()
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

  # Custom epidemic (epidemic_model 4) data extracted from priors
  intensity_fn              <- if (is_custom_epidemic) priors$intensity_fn else NULL
  n_params_custom_epi      <- if (is_custom_epidemic) as.integer(priors$custom_epidemic_n_params) else 0L
  epi_prior_dists          <- if (is_custom_epidemic) priors$custom_epidemic_prior_dists else integer(0)
  epi_prior_params         <- if (is_custom_epidemic) priors$custom_epidemic_prior_params_mat else matrix(0.0, 0L, 3L)
  epi_is_free              <- if (is_custom_epidemic) priors$custom_epidemic_is_free else integer(0)
  epi_fixed_vals           <- if (is_custom_epidemic) priors$custom_epidemic_fixed_vals else numeric(0)

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
  # The censored-row kernels need the appearance pmf on a delay grid; when the
  # delay is fixed (two-stage Stage 2) there is no `cdf_fn` on the tape, so build
  # the grid here from the same fixed functions.
  appearance_grid_fixed <- NULL

  is_hierarchical <- isTRUE(hierarchical_strata) && n_strata > 1L

  # -- revised count-cumulative configuration ---------------------------------
  is_count_cumulative <- isTRUE(data$is_count_cumulative == 1L)
  cumulative_observation <- if (is_count_cumulative)
    as.integer(data$count_cumulative_observation) else 0L
  settlement_horizon <- if (is_count_cumulative)
    as.integer(data$settlement_horizon) else 0L
  if (is_count_cumulative && !cumulative_observation %in% 1:3) {
    cli::cli_abort("The count-cumulative engine has no supported observation-model selector.")
  }
  if (is_count_cumulative && !family %in% 1:3) {
    cli::cli_abort(c(
      "Unsupported count-cumulative report-delay family.",
      "i" = "Use a lognormal, gamma, or generalized-gamma report delay."
    ))
  }
  cumulative_retraction_family <- if (is_count_cumulative)
    as.integer(priors$count_cumulative_retraction_family) else 0L
  cumulative_retraction_is_gengamma <-
    cumulative_retraction_family == 3L
  cumulative_retraction_mass_fixed <- is_count_cumulative &&
    isTRUE(priors$retraction_mass$is_constant == 1L)
  cumulative_retraction_mu_fixed <- is_count_cumulative &&
    isTRUE(priors$count_cumulative_retraction_mu$is_constant == 1L)
  cumulative_retraction_sigma_fixed <- is_count_cumulative &&
    isTRUE(priors$count_cumulative_retraction_sigma$is_constant == 1L)
  cumulative_retraction_Q_fixed <- is_count_cumulative &&
    cumulative_retraction_is_gengamma &&
    isTRUE(priors$count_cumulative_retraction_Q$is_constant == 1L)
  movement_intercept_fixed <- is_count_cumulative &&
    isTRUE(priors$movement_intercept$is_constant == 1L)
  movement_age_fixed <- is_count_cumulative &&
    isTRUE(priors$movement_age$is_constant == 1L)
  movement_previous_fixed <- is_count_cumulative &&
    isTRUE(priors$movement_previous$is_constant == 1L)
  magnitude_size_fixed <- is_count_cumulative &&
    cumulative_observation == 2L &&
    isTRUE(priors$magnitude_size$is_constant == 1L)

  # The ordinary NB dispersion is part of the cumulative-level marginal only.
  # Both hurdle models have their own magnitude law; `nb_likelihood()@phi` is not
  # silently reused as hurdle dispersion.
  is_negbin <- is_negbin &&
    (!is_count_cumulative || cumulative_observation == 1L)

  # -- confirmation (count-cumulative) configuration ---------------------------
  # When the data are count-cumulative, the observation model is the signed-
  # increment Skellam / SkNB likelihood: the epidemic mean is log lambda_t (final
  # genuine count), a fraction (1 - p) of reports are retracted after a retraction
  # delay g_C, and each weekly increment m_t^d is Skellam/SkNB(alpha_d, beta_d).
  is_confirmation <- isTRUE(data$is_confirmation == 1L)
  if (is_confirmation) {
    cli::cli_abort(c(
      "The legacy fixed-`p` count-cumulative objective is no longer available.",
      "i" = "Use the dedicated `count_cumulative_process()` configuration."
    ))
  }
  conf_D <- if (is_confirmation) as.integer(min(data$max_conf_delay - 1L, 15L)) else 0L  # modelled max delay (0-indexed)

  # -- linelist retraction (cure-model) configuration ---------------------------
  # A linelist that records WHEN each case was retracted identifies the retraction
  # structure directly: the retracted rows give the lag pmf g_C and the standing
  # rows give right-censored lags, so the block is a Berkson-Gage mixture-cure
  # likelihood (31_retraction_likelihood.R).  Only the count block changes --
  # lambda_t becomes the gross report rate mu_t = lambda_t / p -- and the
  # appearance-delay block is untouched.
  is_retraction  <- isTRUE(data$is_linelist_retraction == 1L)
  # 0 = retraction (the resolution observed is negative, lag on {1, 2, ...});
  # 1 = confirmation (the resolution observed is positive, lag on {0, 1, ...}).
  resolution_mode <- if (is_retraction) as.integer(data$resolution_mode %||% 0L) else 0L
  lag_offset      <- if (resolution_mode == 0L) 0L else 1L
  has_confirm    <- is_confirmation || is_retraction
  # Escape hatch reproducing the pre-fix count-cumulative retraction intensity
  # (`beta_d = (1 - p) lambda_t g_W(d)`, missing the `/ p` that makes it a rate on
  # the GROSS reports).  Kept only so `devel/benchmark_validation_flusight.R` can
  # score the two side by side; the default is the article's formula.  Read here,
  # at tape-build time, so the AD tape sees a constant.
  legacy_eta <- as.integer(isTRUE(getOption("diseasenowcasting.legacy_retraction_rate", FALSE)))
  retract_family <- if (has_confirm) as.integer(priors$retract_family %||% 1L) else 0L
  retract_is_np       <- retract_family == 4L
  retract_is_gengamma <- retract_family == 3L
  if (is_confirmation && retract_family %in% c(4L, 5L))
    cli::cli_abort("The count-cumulative (Skellam) model needs a parametric retraction delay: lognormal, gamma or generalized gamma.")
  if (is_retraction && retract_family == 5L)
    cli::cli_abort("Custom retraction delays are not supported; use lognormal, gamma, generalized gamma or Dirichlet.")

  # Competing risks: a second lag law for the negative resolutions.  Only usable
  # when both signs are recorded -- one sign cannot identify two laws.
  is_competing <- has_confirm && !is.null(priors$negative_family)
  negative_family      <- if (is_competing) as.integer(priors$negative_family) else 0L
  negative_is_gengamma <- negative_family == 3L
  negative_mu_fixed    <- is_competing && isTRUE(priors$negative_mu$is_constant == 1L)
  negative_sd_fixed    <- is_competing && isTRUE(priors$negative_sigma$is_constant == 1L)
  negative_Q_fixed     <- negative_is_gengamma && isTRUE(priors$negative_Q$is_constant == 1L)

  confirm_p_fixed     <- has_confirm && isTRUE(priors$confirm_p$is_constant == 1L)
  retract_mu_fixed    <- has_confirm && !retract_is_np && isTRUE(priors$retract_mu$is_constant == 1L)
  retract_sd_fixed    <- has_confirm && !retract_is_np && isTRUE(priors$retract_sigma$is_constant == 1L)
  retract_Q_fixed     <- retract_is_gengamma && isTRUE(priors$retract_Q$is_constant == 1L)
  retract_probs_fixed <- retract_is_np && isTRUE(priors$retract_probs$is_constant == 1L)
  retract_bins        <- if (retract_is_np) as.integer(priors$retract_probs$bins) else 0L
  retract_alpha       <- if (retract_is_np) priors$retract_probs$params else numeric(0)

  # `p` may be shared or estimated per stratum; `g_C` is always shared.
  stratified_p <- is_retraction && isTRUE(priors$confirm_p_stratified == 1L) && n_strata > 1L
  n_confirm_p  <- if (stratified_p) n_strata else 1L
  # Which `p` each stratum uses -- the identity map when stratified, all-ones when
  # shared, so the objective has a single code path.
  confirm_p_of_stratum <- if (stratified_p) seq_len(n_strata) else rep(1L, n_strata)

  # -- per-stratum slices of the pooled cure tables (data-side) ------------------
  # The tables carry a `stratum` column; splitting them here keeps the tape free of
  # any stratum bookkeeping and its length fixed.  Report age 0 means "reported
  # today": h(0) = 1 contributes exactly zero, and dropping those rows also keeps a
  # delay CDF from being evaluated at 0, which would put a log(0) on the tape.
  retract_table  <- data$retract_table
  retract_table_positive <- data$retract_table_positive
  retract_table_negative <- data$retract_table_negative
  standing_table <- data$standing_table
  # An unresolved row of age 0 is uninformative ONLY under retraction, where
  # h(0) = 1: nothing could have been retracted yet.  Under confirmation, a case
  # reported today and not yet confirmed IS evidence -- it could have been
  # confirmed the same period and was not -- so those rows stay.
  if (is_retraction && resolution_mode == 0L && !is.null(standing_table))
    standing_table <- standing_table[standing_table[, "age"] > 0, , drop = FALSE]
  censored_patterns <- data$censored_patterns

  retract_rows_of_stratum  <- .split_rows_by_stratum(retract_table, n_strata)
  positive_rows_of_stratum <- .split_rows_by_stratum(retract_table_positive, n_strata)
  negative_rows_of_stratum <- .split_rows_by_stratum(retract_table_negative, n_strata)
  standing_rows_of_stratum <- .split_rows_by_stratum(standing_table, n_strata)
  censored_rows_of_stratum <- .split_rows_by_stratum(censored_patterns, n_strata)
  n_retracted_by_stratum   <- if (is_retraction) as.numeric(data$n_retracted_by_stratum)
                              else rep(0, n_strata)
  retract_split <- if (!is.null(retract_table) && nrow(retract_table) > 0)
    max(2, .wtd_median(retract_table[, "lag"], retract_table[, "count"])) else 2
  # Largest delay any censored kernel indexes; 0 disables the grid construction.
  retract_grid_max <- if (is_retraction && !is.null(censored_patterns) &&
                          nrow(censored_patterns) > 0)
    as.integer(data$retract_grid_max) else 0L
  if (retract_grid_max > 0L && delay_fully_fixed)
    appearance_grid_fixed <- .delay_grid(fixed_delay_fns$cdf, retract_grid_max)

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
    is_custom_epidemic = as.integer(is_custom_epidemic),
    n_params_custom_epi = n_params_custom_epi,
    epi_prior_dists = epi_prior_dists, epi_prior_params = epi_prior_params,
    epi_is_free = epi_is_free,
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
    shape_Q_is_fixed = as.integer(shape_Q_is_fixed), shape_Q_fixed = if (shape_Q_is_fixed) priors$delay_Q$fixed else 0,
    # revised count-cumulative observation model
    is_count_cumulative = as.integer(is_count_cumulative),
    cumulative_observation = cumulative_observation,
    settlement_horizon = settlement_horizon,
    cumulative_level_array = if (is_count_cumulative)
      data$cumulative_level_array else array(0.0, c(0L, 0L, 0L)),
    signed_update_array = if (is_count_cumulative)
      data$signed_update_array else array(0.0, c(0L, 0L, 0L)),
    observation_mask = if (is_count_cumulative)
      data$observation_mask else array(FALSE, c(0L, 0L, 0L)),
    previous_nonzero_array = if (is_count_cumulative)
      data$previous_nonzero_array else array(0.0, c(0L, 0L, 0L)),
    cumulative_retraction_family = cumulative_retraction_family,
    cumulative_retraction_mass_fixed = as.integer(cumulative_retraction_mass_fixed),
    cumulative_retraction_mass_value = if (cumulative_retraction_mass_fixed)
      priors$retraction_mass$fixed else 0,
    prior_cumulative_retraction_mass_dist = if (is_count_cumulative &&
      !cumulative_retraction_mass_fixed) priors$retraction_mass$dist else 0L,
    prior_cumulative_retraction_mass_params = if (is_count_cumulative &&
      !cumulative_retraction_mass_fixed) .pad3(priors$retraction_mass$params) else c(0, 0, 0),
    cumulative_retraction_mu_fixed = as.integer(cumulative_retraction_mu_fixed),
    cumulative_retraction_mu_value = if (cumulative_retraction_mu_fixed)
      priors$count_cumulative_retraction_mu$fixed else 0,
    cumulative_retraction_sigma_fixed = as.integer(cumulative_retraction_sigma_fixed),
    cumulative_retraction_sigma_value = if (cumulative_retraction_sigma_fixed)
      priors$count_cumulative_retraction_sigma$fixed else 0,
    cumulative_retraction_Q_fixed = as.integer(cumulative_retraction_Q_fixed),
    cumulative_retraction_Q_value = if (cumulative_retraction_Q_fixed)
      priors$count_cumulative_retraction_Q$fixed else 0,
    prior_cumulative_retraction_mu_dist = if (is_count_cumulative &&
      !cumulative_retraction_mu_fixed) priors$count_cumulative_retraction_mu$dist else 0L,
    prior_cumulative_retraction_mu_params = if (is_count_cumulative &&
      !cumulative_retraction_mu_fixed) .pad3(priors$count_cumulative_retraction_mu$params) else c(0, 0, 0),
    prior_cumulative_retraction_sigma_dist = if (is_count_cumulative &&
      !cumulative_retraction_sigma_fixed) priors$count_cumulative_retraction_sigma$dist else 0L,
    prior_cumulative_retraction_sigma_params = if (is_count_cumulative &&
      !cumulative_retraction_sigma_fixed) .pad3(priors$count_cumulative_retraction_sigma$params) else c(0, 0, 0),
    prior_cumulative_retraction_Q_dist = if (is_count_cumulative &&
      cumulative_retraction_is_gengamma && !cumulative_retraction_Q_fixed)
      priors$count_cumulative_retraction_Q$dist else 0L,
    prior_cumulative_retraction_Q_params = if (is_count_cumulative &&
      cumulative_retraction_is_gengamma && !cumulative_retraction_Q_fixed)
      .pad3(priors$count_cumulative_retraction_Q$params) else c(0, 0, 0),
    movement_intercept_fixed = as.integer(movement_intercept_fixed),
    movement_intercept_value = if (movement_intercept_fixed)
      priors$movement_intercept$fixed else 0,
    movement_age_fixed = as.integer(movement_age_fixed),
    movement_age_value = if (movement_age_fixed) priors$movement_age$fixed else 0,
    movement_previous_fixed = as.integer(movement_previous_fixed),
    movement_previous_value = if (movement_previous_fixed)
      priors$movement_previous$fixed else 0,
    prior_movement_intercept_dist = if (is_count_cumulative &&
      !movement_intercept_fixed) priors$movement_intercept$dist else 0L,
    prior_movement_intercept_params = if (is_count_cumulative &&
      !movement_intercept_fixed) .pad3(priors$movement_intercept$params) else c(0, 0, 0),
    prior_movement_age_dist = if (is_count_cumulative && !movement_age_fixed)
      priors$movement_age$dist else 0L,
    prior_movement_age_params = if (is_count_cumulative && !movement_age_fixed)
      .pad3(priors$movement_age$params) else c(0, 0, 0),
    prior_movement_previous_dist = if (is_count_cumulative &&
      !movement_previous_fixed) priors$movement_previous$dist else 0L,
    prior_movement_previous_params = if (is_count_cumulative &&
      !movement_previous_fixed) .pad3(priors$movement_previous$params) else c(0, 0, 0),
    magnitude_size_fixed = as.integer(magnitude_size_fixed),
    magnitude_size_value = if (magnitude_size_fixed)
      priors$magnitude_size$fixed else 0,
    prior_magnitude_size_dist = if (is_count_cumulative &&
      cumulative_observation == 2L && !magnitude_size_fixed)
      priors$magnitude_size$dist else 0L,
    prior_magnitude_size_params = if (is_count_cumulative &&
      cumulative_observation == 2L && !magnitude_size_fixed)
      .pad3(priors$magnitude_size$params) else c(0, 0, 0),
    # confirmation / retraction
    is_confirmation = as.integer(is_confirmation), conf_D = conf_D, retract_family = retract_family,
    increment_array = if (is_confirmation) data$increment_array else array(0.0, c(0L, 0L, 0L)),
    confirm_p_fixed = as.integer(confirm_p_fixed), confirm_p_val = if (confirm_p_fixed) priors$confirm_p$fixed else 0,
    prior_confirm_p_dist = if (has_confirm && !confirm_p_fixed) priors$confirm_p$dist else 0L,
    prior_confirm_p_params = if (has_confirm && !confirm_p_fixed) .pad3(priors$confirm_p$params) else c(0, 0, 0),
    retract_mu_fixed = as.integer(retract_mu_fixed), retract_mu_val = if (retract_mu_fixed) priors$retract_mu$fixed else 0,
    retract_sd_fixed = as.integer(retract_sd_fixed), retract_sd_val = if (retract_sd_fixed) priors$retract_sigma$fixed else 0,
    prior_retract_mu_dist = if (has_confirm && !retract_is_np && !retract_mu_fixed) priors$retract_mu$dist else 0L,
    prior_retract_mu_params = if (has_confirm && !retract_is_np && !retract_mu_fixed) .pad3(priors$retract_mu$params) else c(0, 0, 0),
    prior_retract_sd_dist = if (has_confirm && !retract_sd_fixed) priors$retract_sigma$dist else 0L,
    prior_retract_sd_params = if (has_confirm && !retract_sd_fixed) .pad3(priors$retract_sigma$params) else c(0, 0, 0),
    # linelist retraction (cure block)
    is_retraction = as.integer(is_retraction),
    retract_is_np = as.integer(retract_is_np), retract_is_gengamma = as.integer(retract_is_gengamma),
    retract_bins = retract_bins, retract_alpha = retract_alpha,
    retract_probs_fixed = as.integer(retract_probs_fixed),
    retract_probs_val = if (retract_probs_fixed) priors$retract_probs$fixed else numeric(0),
    lag_offset = lag_offset, resolution_mode = resolution_mode, legacy_eta = legacy_eta,
    is_competing = as.integer(is_competing), negative_family = negative_family,
    negative_is_gengamma = as.integer(negative_is_gengamma),
    negative_mu_fixed = as.integer(negative_mu_fixed),
    negative_mu_val = if (negative_mu_fixed) priors$negative_mu$fixed else 0,
    negative_sd_fixed = as.integer(negative_sd_fixed),
    negative_sd_val = if (negative_sd_fixed) priors$negative_sigma$fixed else 0,
    negative_Q_fixed = as.integer(negative_Q_fixed),
    negative_Q_val = if (negative_Q_fixed) priors$negative_Q$fixed else 0,
    prior_negative_mu_dist = if (is_competing && !negative_mu_fixed) priors$negative_mu$dist else 0L,
    prior_negative_mu_params = if (is_competing && !negative_mu_fixed) .pad3(priors$negative_mu$params) else c(0, 0, 0),
    prior_negative_sd_dist = if (is_competing && !negative_sd_fixed) priors$negative_sigma$dist else 0L,
    prior_negative_sd_params = if (is_competing && !negative_sd_fixed) .pad3(priors$negative_sigma$params) else c(0, 0, 0),
    prior_negative_Q_dist = if (negative_is_gengamma && !negative_Q_fixed) priors$negative_Q$dist else 0L,
    prior_negative_Q_params = if (negative_is_gengamma && !negative_Q_fixed) .pad3(priors$negative_Q$params) else c(0, 0, 0),
    n_positive_by_stratum = if (is_retraction) as.numeric(data$n_positive_by_stratum) else numeric(0),
    n_negative_by_stratum = if (is_retraction) as.numeric(data$n_negative_by_stratum) else numeric(0),
    # NOTE: the cure-block tables, their per-stratum row indices, `n_confirm_p`,
    # `retract_split` and `retract_grid_max` are captured from this function's
    # frame rather than passed through `getAll()`.  They are pure data (matrices
    # and integer index lists), and keeping them out of the AD data list avoids
    # having to flatten a ragged structure.
    retract_Q_fixed = as.integer(retract_Q_fixed),
    retract_Q_val = if (retract_Q_fixed) priors$retract_Q$fixed else 0,
    prior_retract_Q_dist = if (retract_is_gengamma && !retract_Q_fixed) priors$retract_Q$dist else 0L,
    prior_retract_Q_params = if (retract_is_gengamma && !retract_Q_fixed) .pad3(priors$retract_Q$params) else c(0, 0, 0)
  )

  # -- parameter initial values (per-stratum where applicable) ------------------
  init <- init %||% list()
  positive_col_log_median <- function(col) { positive <- col[col > 0]
    if (length(positive)) log(stats::median(positive)) else 0 }
  intercept_init <- init$mu_intercept %||% apply(data$case_counts, 2, positive_col_log_median)
  if (length(intercept_init) != n_strata) intercept_init <- rep_len(intercept_init, n_strata)
  # For confirmation the delay is the APPEARANCE delay, NOT the revision-delay
  # aggregate of `m` (which is polluted by old weeks first seen at large delays).
  # Seed it from the empirical appearance profile of the positive increments: the
  # mean and spread of the delay at which cumulative mass is added.  A too-short
  # init (e.g. the old fixed log(1)) makes g_D(0) ~ 1 and the later-delay
  # appearances impossible (-Inf) whenever the stream builds up slowly (e.g. daily
  # covid, mean appearance delay ~7 days), so estimate the delay scale from data.
  cumulative_appearance_moments <- if (is_confirmation || is_count_cumulative) {
    appearance_updates <- if (is_count_cumulative)
      data$signed_update_array else data$increment_array
    positive_by_delay <- apply(pmax(appearance_updates, 0), 2, sum)
    delays_grid <- seq_along(positive_by_delay) - 1
    total_mass  <- sum(positive_by_delay)
    if (total_mass > 0) {
      mean_delay <- sum(delays_grid * positive_by_delay) / total_mass
      var_delay  <- sum((delays_grid - mean_delay)^2 * positive_by_delay) / total_mass
      list(mean = mean_delay, var = var_delay)
    } else {
      list(mean = 1, var = 1)
    }
  } else NULL

  delay_mu_init  <- init$delay_mu %||%
    (if (is_confirmation || is_count_cumulative)
       log(max(cumulative_appearance_moments$mean, 1))
     else log(max(.wtd_median(data$m[, 3], data$m[, 2]), 1.5)))
  delay_sigma_init <- init$delay_sigma %||% {
    if (is_confirmation || is_count_cumulative) {
      # Lognormal shape implied by the empirical delay mean/variance, clamped to a
      # numerically safe band.
      mu_hat <- max(cumulative_appearance_moments$mean, 0.5)
      max(0.5, min(sqrt(log(1 + cumulative_appearance_moments$var / mu_hat^2)), 2))
    }
    else if (is_gengamma) 0.6 else { empirical_sd <- sqrt(.wtd_var(data$m[, 3], data$m[, 2]))
      if (is.finite(empirical_sd) && empirical_sd > 0) max(2, min(empirical_sd, 60)) else 5 } }

  parameters <- if (is_sir || is_custom_epidemic) list()
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
  # Dedicated count-cumulative parameters.  These are deliberately separate
  # from the linelist validation parameters below: the cumulative stream
  # identifies the defective kernel h_R, not a biological confirmation
  # probability and conditional validation-delay law.
  if (is_count_cumulative) {
    parameters$cumulative_retraction_mass_raw <-
      if (cumulative_retraction_mass_fixed) 0 else
        (init$cumulative_retraction_mass_raw %||% stats::qlogis(0.05))
    parameters$cumulative_retraction_mu <-
      if (cumulative_retraction_mu_fixed) 0 else
        (init$cumulative_retraction_mu %||% log(2))
    parameters$log_cumulative_retraction_sigma_excess <-
      if (cumulative_retraction_sigma_fixed) 0 else
        (init$log_cumulative_retraction_sigma_excess %||% log(0.99))
    if (cumulative_retraction_is_gengamma) {
      parameters$cumulative_retraction_Q <-
        if (cumulative_retraction_Q_fixed) 0 else
          (init$cumulative_retraction_Q %||% -2)
    }
    if (cumulative_observation %in% c(2L, 3L)) {
      parameters$movement_intercept <-
        if (movement_intercept_fixed) 0 else
          (init$movement_intercept %||% -1)
      parameters$movement_age <-
        if (movement_age_fixed) 0 else (init$movement_age %||% 0)
      parameters$movement_previous <-
        if (movement_previous_fixed) 0 else
          (init$movement_previous %||% 0)
    }
    if (cumulative_observation == 2L) {
      parameters$log_magnitude_size <-
        if (magnitude_size_fixed) 0 else
          (init$log_magnitude_size %||% log(10))
    }
  }
  # confirmation / retraction parameters (only estimated when free).  A linelist
  # observes the retraction lags outright, so seed `p` and the lag location from
  # the data rather than from the count-cumulative fallbacks.
  if (has_confirm) {
    retraction_lags   <- if (!is.null(retract_table)) retract_table[, "lag"] else numeric(0)
    retraction_counts <- if (!is.null(retract_table)) retract_table[, "count"] else numeric(0)
    confirm_p_init <- if (is_retraction)
      .empirical_confirmation_probability(retraction_lags, retraction_counts,
                                          if (!is.null(data$standing_table)) data$standing_table[, "age"] else numeric(0),
                                          if (!is.null(data$standing_table)) data$standing_table[, "count"] else numeric(0))
      else 0.95
    mean_lag_init <- if (sum(retraction_counts) > 0)
      sum(retraction_lags * retraction_counts) / sum(retraction_counts) else 1.5
    if (!confirm_p_fixed) parameters$logit_confirm_p <- init$logit_confirm_p %||%
      rep(stats::qlogis(min(max(confirm_p_init, 0.5), 0.999)), n_confirm_p)
    if (retract_is_np) {
      if (!retract_probs_fixed) parameters$retract_logits <- init$retract_logits %||% {
        binned    <- pmin(as.integer(retraction_lags), retract_bins + 1L)
        bin_total <- tapply(retraction_counts, factor(binned, levels = 1:(retract_bins + 1L)), sum)
        bin_total[is.na(bin_total)] <- 0
        empirical_pmf <- (as.numeric(bin_total) + 0.5) / sum(as.numeric(bin_total) + 0.5)
        log(empirical_pmf[1:retract_bins]) - log(empirical_pmf[retract_bins + 1])
      }
    } else {
      if (!retract_mu_fixed) parameters$retract_mu         <- init$retract_mu         %||% log(max(mean_lag_init, 1))
      if (!retract_sd_fixed) parameters$log_retract_sd_exc <- init$log_retract_sd_exc %||% log(1.0)
      if (retract_is_gengamma && !retract_Q_fixed)
        parameters$retract_Q <- init$retract_Q %||% -2
    }
    if (is_competing) {
      negative_lag_init <- if (!is.null(retract_table_negative) &&
                               sum(retract_table_negative[, "count"]) > 0)
        sum(retract_table_negative[, "lag"] * retract_table_negative[, "count"]) /
          sum(retract_table_negative[, "count"]) else mean_lag_init
      if (!negative_mu_fixed) parameters$negative_mu <- init$negative_mu %||% log(max(negative_lag_init, 1))
      if (!negative_sd_fixed) parameters$log_negative_sd_exc <- init$log_negative_sd_exc %||% log(1.0)
      if (negative_is_gengamma && !negative_Q_fixed)
        parameters$negative_Q <- init$negative_Q %||% -2
    }
  }
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
  } else if (is_custom_epidemic) {
    custom_epi_inits <- priors$custom_epidemic_inits %||% rep(0.0, n_params_custom_epi)
    init_vals <- numeric(n_params_custom_epi)
    for (i in seq_len(n_params_custom_epi)) {
      init_vals[i] <- if (epi_is_free[i] == 0L) epi_fixed_vals[i]
                      else (init$custom_epidemic_params[[i]] %||% custom_epi_inits[i])
    }
    parameters$custom_epidemic_params <- init_vals
    random <- character(0)
  } else {  # epidemic_model == 3L: SIR (coupled) — per-stratum R0/gamma/N_eff + AR(1) beta walk
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
  if (is_count_cumulative) {
    if (cumulative_retraction_mass_fixed)
      map$cumulative_retraction_mass_raw <- factor(NA)
    if (cumulative_retraction_mu_fixed)
      map$cumulative_retraction_mu <- factor(NA)
    if (cumulative_retraction_sigma_fixed)
      map$log_cumulative_retraction_sigma_excess <- factor(NA)
    if (cumulative_retraction_is_gengamma && cumulative_retraction_Q_fixed)
      map$cumulative_retraction_Q <- factor(NA)
    if (cumulative_observation %in% c(2L, 3L)) {
      if (movement_intercept_fixed) map$movement_intercept <- factor(NA)
      if (movement_age_fixed) map$movement_age <- factor(NA)
      if (movement_previous_fixed) map$movement_previous <- factor(NA)
    }
    if (cumulative_observation == 2L && magnitude_size_fixed)
      map$log_magnitude_size <- factor(NA)
  }
  if (is_custom_epidemic && n_params_custom_epi > 0L && any(epi_is_free == 0L)) {
    epi_map_vals <- rep(NA_integer_, n_params_custom_epi)
    free_epi_idx <- 0L
    for (i in seq_len(n_params_custom_epi)) {
      if (epi_is_free[i] == 1L) {
        free_epi_idx <- free_epi_idx + 1L
        epi_map_vals[i] <- free_epi_idx
      }
    }
    map$custom_epidemic_params <- factor(epi_map_vals)
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

    # -- finite-horizon count-cumulative kernel ------------------------------
    # Both delay PMFs are conditional on falling inside their configured finite
    # support.  The primitive object used below is h_R = mass * g_R; no `p` or
    # biological validation probability enters this observation model.
    if (is_count_cumulative == 1L) {
      cumulative_report_cdf <- if (delay_fully_fixed == 1L) {
        fixed_delay_fns$cdf(seq_len(settlement_horizon + 1L))
      } else {
        cdf_fn(seq_len(settlement_horizon + 1L))
      }
      cumulative_report_pmf <- c(
        cumulative_report_cdf[1L],
        cumulative_report_cdf[-1L] -
          cumulative_report_cdf[-length(cumulative_report_cdf)]
      )
      cumulative_report_pmf <- cumulative_report_pmf /
        sum(cumulative_report_pmf)

      cumulative_retraction_mu_v <-
        if (cumulative_retraction_mu_fixed == 1L)
          cumulative_retraction_mu_value else cumulative_retraction_mu
      cumulative_retraction_sigma_v <-
        if (cumulative_retraction_sigma_fixed == 1L)
          cumulative_retraction_sigma_value else
            0.01 + exp(log_cumulative_retraction_sigma_excess)
      if (cumulative_retraction_sigma_fixed == 0L) {
        log_jacobian <- log_jacobian +
          log_cumulative_retraction_sigma_excess
      }
      cumulative_retraction_shape_Q <- 0
      if (cumulative_retraction_family == 3L) {
        if (cumulative_retraction_Q_fixed == 1L) {
          cumulative_retraction_shape_Q <- cumulative_retraction_Q_value
        } else {
          cumulative_retraction_shape_transform <-
            .gengamma_shape_transform(cumulative_retraction_Q)
          cumulative_retraction_shape_Q <-
            cumulative_retraction_shape_transform$shape_Q
          log_jacobian <- log_jacobian +
            cumulative_retraction_shape_transform$log_jacobian
        }
        cumulative_retraction_fns <- .delay_distribution_functions(
          3L, cumulative_retraction_mu_v,
          cumulative_retraction_shape_Q, cumulative_retraction_sigma_v
        )
      } else {
        cumulative_retraction_fns <- .delay_distribution_functions(
          cumulative_retraction_family, cumulative_retraction_mu_v,
          cumulative_retraction_sigma_v
        )
      }
      cumulative_retraction_cdf <- cumulative_retraction_fns$cdf(
        seq_len(settlement_horizon)
      )
      cumulative_retraction_pmf <- c(
        cumulative_retraction_cdf[1L],
        cumulative_retraction_cdf[-1L] -
          cumulative_retraction_cdf[-length(cumulative_retraction_cdf)]
      )
      cumulative_retraction_pmf <- cumulative_retraction_pmf /
        sum(cumulative_retraction_pmf)

      cumulative_retraction_mass <-
        if (cumulative_retraction_mass_fixed == 1L)
          cumulative_retraction_mass_value else
            plogis(cumulative_retraction_mass_raw)
      if (cumulative_retraction_mass_fixed == 0L) {
        log_jacobian <- log_jacobian +
          log(cumulative_retraction_mass) +
          log1p(-cumulative_retraction_mass)
      }
      cumulative_components <- .count_cumulative_components(
        cumulative_report_pmf, cumulative_retraction_pmf,
        cumulative_retraction_mass, settlement_horizon
      )

      if (cumulative_observation %in% c(2L, 3L)) {
        movement_intercept_v <- if (movement_intercept_fixed == 1L)
          movement_intercept_value else movement_intercept
        movement_age_v <- if (movement_age_fixed == 1L)
          movement_age_value else movement_age
        movement_previous_v <- if (movement_previous_fixed == 1L)
          movement_previous_value else movement_previous
      }
      if (cumulative_observation == 2L) {
        magnitude_size <- if (magnitude_size_fixed == 1L)
          magnitude_size_value else exp(log_magnitude_size)
        if (magnitude_size_fixed == 0L)
          log_jacobian <- log_jacobian + log_magnitude_size
      }
    }

    # -- confirmation delay algebra (shared across strata) -------------------
    # g_D = appearance-delay pmf on 0..conf_D (from the main delay cdf); g_C =
    # retraction-delay pmf (g_C(0) = 0); g_W = g_D * g_C; p = confirmation prob.
    if (is_confirmation == 1L) {
      confirm_p <- if (confirm_p_fixed == 1L) confirm_p_val else plogis(logit_confirm_p)
      appearance_cdf <- cdf_fn(seq_len(conf_D + 1L))                 # cdf at 1..conf_D+1
      g_D_conf <- c(appearance_cdf[1], appearance_cdf[-1] - appearance_cdf[-length(appearance_cdf)])
      retract_mu_v <- if (retract_mu_fixed == 1L) retract_mu_val else retract_mu
      retract_sd_v <- if (retract_sd_fixed == 1L) retract_sd_val else 0.01 + exp(log_retract_sd_exc)
      retract_fns  <- .delay_distribution_functions(retract_family, retract_mu_v, retract_sd_v)
      # cdf at delays 1..conf_D (cdf(0) = 0 by definition, set explicitly to avoid
      # log(0) = -Inf corrupting the AD tape); g_C(0) = 0 (retraction after report).
      # NB: lead every c() with an ADVECTOR zero (`x[1] * 0`) -- a plain-numeric
      # first argument makes c() dispatch to base and strips the advector class.
      retract_cdf_pos <- retract_fns$cdf(seq_len(conf_D))
      retract_cdf     <- c(retract_cdf_pos[1] * 0, retract_cdf_pos)
      g_C_body <- retract_cdf[-1] - retract_cdf[-length(retract_cdf)]
      g_C_conf <- c(g_C_body[1] * 0, g_C_body)
      g_W_conf <- .convolve_delays(g_D_conf, g_C_conf)
      if (retract_sd_fixed == 0L) log_jacobian <- log_jacobian + log_retract_sd_exc
    }

    # -- linelist retraction: the cure block ---------------------------------
    # R log(1-p) + sum_c r_c log g_C(c) + sum_j u_j log[p + (1-p) Sbar_C(j)] over
    # the exactly observed rows, plus one log-of-a-sum per censoring pattern.
    # Free of the epidemic process, so it is evaluated here rather than inside the
    # per-stratum epidemic loop; `g_C` is shared across strata, `p` need not be.
    loglik_retraction <- 0
    if (is_retraction == 1L) {
      confirm_p_vec <- if (confirm_p_fixed == 1L) rep(confirm_p_val, n_confirm_p)
                       else plogis(logit_confirm_p)
      confirm_p <- confirm_p_vec[1]                      # the shared-p case
      if (retract_is_np == 1L) {
        retract_exp_logits <- exp(retract_logits)
        retract_simplex    <- c(retract_exp_logits, exp(0 * retract_logits[1])) /
                              (sum(retract_exp_logits) + 1)
        retract_fns <- .nonparametric_delay_functions(retract_simplex, retract_bins)
      } else {
        retract_mu_v <- if (retract_mu_fixed == 1L) retract_mu_val else retract_mu
        retract_sd_v <- if (retract_sd_fixed == 1L) retract_sd_val else 0.01 + exp(log_retract_sd_exc)
        if (retract_sd_fixed == 0L) log_jacobian <- log_jacobian + log_retract_sd_exc
        if (retract_is_gengamma == 1L) {
          if (retract_Q_fixed == 1L) {
            retract_shape_Q <- retract_Q_val
          } else {
            retract_shape_transform <- .gengamma_shape_transform(retract_Q)
            retract_shape_Q <- retract_shape_transform$shape_Q
            log_jacobian    <- log_jacobian + retract_shape_transform$log_jacobian
          }
          retract_fns <- .delay_distribution_functions(3L, retract_mu_v, retract_shape_Q, retract_sd_v)
        } else {
          retract_fns <- .delay_distribution_functions(retract_family, retract_mu_v, retract_sd_v)
        }
      }
      # Competing risks: the NEGATIVE resolutions get their own lag law, so an
      # unresolved row's age becomes informative about which way it will go.
      if (is_competing == 1L) {
        negative_mu_v <- if (negative_mu_fixed == 1L) negative_mu_val else negative_mu
        negative_sd_v <- if (negative_sd_fixed == 1L) negative_sd_val else 0.01 + exp(log_negative_sd_exc)
        if (negative_sd_fixed == 0L) log_jacobian <- log_jacobian + log_negative_sd_exc
        if (negative_is_gengamma == 1L) {
          if (negative_Q_fixed == 1L) {
            negative_shape_Q <- negative_Q_val
          } else {
            negative_shape_transform <- .gengamma_shape_transform(negative_Q)
            negative_shape_Q <- negative_shape_transform$shape_Q
            log_jacobian     <- log_jacobian + negative_shape_transform$log_jacobian
          }
          negative_fns <- .delay_distribution_functions(3L, negative_mu_v, negative_shape_Q, negative_sd_v)
        } else {
          negative_fns <- .delay_distribution_functions(negative_family, negative_mu_v, negative_sd_v)
        }
      }

      # Grids g_D(a), g_C(c), G_C(c) indexed from delay 0 -- built once and shared
      # by every censoring pattern, so the kernels are plain vector lookups.  Only
      # needed when some row is partially observed.
      if (retract_grid_max > 0L) {
        appearance_grid <- if (delay_fully_fixed == 1L) appearance_grid_fixed
                           else .delay_grid(cdf_fn, retract_grid_max)
        retraction_grid <- .resolution_lag_grid(retract_fns$cdf, retract_grid_max, lag_offset)
      }

      for (stratum in seq_len(n_strata)) {
        confirm_p_s   <- confirm_p_vec[confirm_p_of_stratum[stratum]]
        retract_slice <- retract_rows_of_stratum[[stratum]]
        standing_slice <- standing_rows_of_stratum[[stratum]]
        if (is_competing == 1L) {
          positive_slice <- positive_rows_of_stratum[[stratum]]
          negative_slice <- negative_rows_of_stratum[[stratum]]
          loglik_retraction <- loglik_retraction +
            .loglik_competing_risks(retract_fns, negative_fns, retract_is_np,
              if (length(positive_slice)) retract_table_positive[positive_slice, "lag"] else numeric(0),
              if (length(positive_slice)) retract_table_positive[positive_slice, "count"] else numeric(0),
              if (length(negative_slice)) retract_table_negative[negative_slice, "lag"] else numeric(0),
              if (length(negative_slice)) retract_table_negative[negative_slice, "count"] else numeric(0),
              if (length(standing_slice)) standing_table[standing_slice, "age"] else numeric(0),
              if (length(standing_slice)) standing_table[standing_slice, "count"] else numeric(0),
              confirm_p_s, retract_split, lag_offset)
          next
        }
        loglik_retraction <- loglik_retraction +
          .loglik_retraction(retract_fns, retract_is_np,
                             if (length(retract_slice)) retract_table[retract_slice, "lag"] else numeric(0),
                             if (length(retract_slice)) retract_table[retract_slice, "count"] else numeric(0),
                             if (length(standing_slice)) standing_table[standing_slice, "age"] else numeric(0),
                             if (length(standing_slice)) standing_table[standing_slice, "count"] else numeric(0),
                             n_retracted_by_stratum[stratum], confirm_p_s, retract_split,
                             lag_offset, resolution_mode,
                             n_positive_by_stratum[stratum], n_negative_by_stratum[stratum])
        censored_slice <- censored_rows_of_stratum[[stratum]]
        if (retract_grid_max > 0L && length(censored_slice) > 0)
          loglik_retraction <- loglik_retraction +
            .loglik_retraction_censored(censored_patterns[censored_slice, , drop = FALSE],
                                        appearance_grid$pmf, retraction_grid$pmf,
                                        retraction_grid$cdf, confirm_p_s, lag_offset,
                                        resolution_mode)
      }
    }

    # -- per-stratum epidemic mean + S_k accumulation -----------------------
    # Accumulate the count log-likelihood cell-by-cell.  For HSGP/AR1 each
    # stratum is independent (column loop); SIR and custom epidemic produce the
    # full log_mean[T×S] matrix directly.
    log_mean_matrix <- NULL
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
    } else if (is_custom_epidemic == 1L) {
      # The user's intensity_fn returns the full log_mean[T x S] directly; no
      # intercept or trend is added on top (it owns the whole trajectory).
      log_mean_matrix <- intensity_fn(custom_epidemic_params)
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
    if (is_hierarchical == 1L && is_sir == 0L && is_custom_epidemic == 0L) {
      tau_int <- exp(log_tau_intercept)
      mu_intercept_hier <- mu_global + tau_int * delta_intercept
      log_jacobian <- log_jacobian + log_tau_intercept   # Jacobian for tau = exp(log_tau)
    }

    loglik_counts <- 0
    for (s in seq_len(n_strata)) {
      if (is_sir == 1L || is_custom_epidemic == 1L) {
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
      if (is_count_cumulative == 1L) {
        # mu_t is the latent settled-process intensity.  The level likelihood
        # uses E[C_t(d)] = mu_t q_C(d); both hurdle variants instead use the
        # signed delay update and preserve E[Delta] = alpha - omega.
        for (t in seq_len(n_time)) {
          for (delay in 0:settlement_horizon) {
            delay_index <- delay + 1L
            if (!observation_mask[t, delay_index, s]) next
            if (cumulative_observation == 1L) {
              cumulative_mean <- lambda[t] *
                cumulative_components$q_C[delay_index] + 1e-10
              loglik_counts <- loglik_counts +
                .count_cumulative_level_logpmf(
                  cumulative_level_array[t, delay_index, s],
                  cumulative_mean, is_negbin, nb_size
                )
            } else {
              alpha <- lambda[t] *
                cumulative_components$alpha_unit[delay_index] + 1e-12
              omega <- lambda[t] *
                cumulative_components$omega_unit[delay_index] + 1e-12
              total <- alpha + omega
              movement_eta <- movement_intercept_v +
                movement_age_v * log1p(delay) +
                movement_previous_v *
                  previous_nonzero_array[t, delay_index, s]
              movement_probability <-
                .count_cumulative_movement_probability(total, movement_eta)
              update <- signed_update_array[t, delay_index, s]
              if (cumulative_observation == 2L) {
                loglik_counts <- loglik_counts +
                  .hurdle_ztnb_update_logpmf(
                    update, alpha, omega, movement_probability,
                    magnitude_size
                  )
              } else {
                loglik_counts <- loglik_counts +
                  .hurdle_ztpoisson_update_logpmf(
                    update, alpha, omega, movement_probability
                  )
              }
            }
          }
        }
      } else if (is_confirmation == 1L) {
        # -- signed-increment Skellam / SkNB likelihood ---------------------
        # lambda_t is the confirmed (final) mean; mu_t = lambda_t/p is the GROSS
        # report rate (the epidemic process mean of the article), and the retracted
        # rate is eta_t = (1 - p) mu_t -- a fraction (1 - p) of the GROSS reports is
        # erroneous, not a fraction of the genuine ones.  Each week's observed
        # increment path m_t^0..m_t^{d*} is Skellam (Poisson) or SkNB (NB, shared
        # gamma frailty) with
        #
        #   alpha_d = mu_t g_D(d)                       [article eq. alphasimplified]
        #   beta_d  = mu_t (1 - p) (g_D * g_C)(d)       [article eq. omegadef]
        #
        # `eta_stream` therefore divides by `p` exactly as `mu_stream` does.  It used
        # not to, which made beta_d a factor of `p` too small.  That is NOT a
        # reparametrisation: matching both moments needs p' solving
        # p'^2 - p' + (1 - p) = 0 AND lambda' = lambda p'/p, so the reported epidemic
        # mean -- the nowcast target -- came out biased low (-0.04% at p = 0.98,
        # -1.4% at p = 0.9, -9.6% at p = 0.8), and for p < 0.75 the discriminant is
        # negative and the old form could not represent the model at all.
        mu_stream  <- lambda / confirm_p
        eta_stream <- (1 - confirm_p) * (if (legacy_eta == 1L) lambda else mu_stream)
        stratum_increments <- increment_array[, , s]
        for (t in seq_len(n_time)) {
          horizon_t <- min(as.integer(d_star[t, s]), conf_D)   # 0-indexed observed delays
          if (horizon_t < 0L) next
          delay_seq  <- 0:horizon_t
          increments <- stratum_increments[t, delay_seq + 1L]
          alpha_path <- mu_stream[t]  * g_D_conf[delay_seq + 1L]
          beta_path  <- eta_stream[t] * g_W_conf[delay_seq + 1L]
          bin_type   <- ifelse(delay_seq == 0L, 0L, 1L)        # d=0 pure addition, d>=1 mixed
          loglik_counts <- loglik_counts +
            if (is_negbin == 1L) .loglik_sknb_path(increments, alpha_path, beta_path, bin_type, nb_size)
            else                 .loglik_skellam_path(increments, alpha_path, beta_path, bin_type)
        }
      } else {
        # Under retractions the rows we count are GROSS reports -- genuine plus
        # not-yet-caught erroneous ones -- so the count block models the gross rate
        # mu_t = lambda_t / p while the epidemic process keeps targeting the settled
        # genuine mean lambda_t.  (`p` is exactly aliased with the intercept here,
        # which is why the count block carries no information about it: all of that
        # sits in the cure block above.)
        confirm_p_s    <- if (is_retraction == 1L) confirm_p_vec[confirm_p_of_stratum[s]] else 1
        log_mean_gross <- if (is_retraction == 1L) log_mean_capped - log(confirm_p_s) else log_mean_capped
        lambda_gross   <- if (is_retraction == 1L) lambda / confirm_p_s else lambda
        gstar  <- if (delay_fully_fixed == 1L) gstar_precomputed[, s] else cdf_fn(d_star[, s] + 1)
        counts_col <- case_counts[, s]
        if (is_negbin == 1L) {
          success_prob <- nb_size / (nb_size + lambda_gross)
          loglik_counts <- loglik_counts +
            sum(counts_col * log1p(-success_prob)) + sum(nb_size * log(success_prob)) +
            sum(lgamma(counts_col + nb_size) - lgamma(nb_size) - lgamma(counts_col + 1)) -
            sum((counts_col + nb_size) * log(success_prob + gstar * (1 - success_prob)))
        } else {
          loglik_counts <- loglik_counts + sum(counts_col * log_mean_gross) - sum(gstar * lambda_gross)
        }
      }
    }
    if (is_negbin == 1L) log_jacobian <- log_jacobian + log_phi_nb

    # -- shared delay PMF likelihood (pooled over strata) --------------------
    # Skipped for confirmation: there are no individual delay observations -- the
    # appearance delay is informed by the signed-increment likelihood directly.
    loglik_delay <- 0
    if (is_count_cumulative == 0L && is_confirmation == 0L &&
        delay_fully_fixed == 0L && length(obs_delays) > 0) {
      loglik_delay <- if (is_nonparametric == 1L)
        sum(row_sums * np_fns$log_pmf_raw(obs_delays))
      else
        .discretised_delay_loglik(obs_delays, row_sums, split_delay,
                                  delay_fns$log_cdf, delay_fns$log_survival)
    }
    # Right-censored delays: we only know the delay is <= j, contributing
    # log G_D(j) (the article's m_j^* term). G_D = CDF of the delay process.
    # Skipped under the retraction model: there the censored rows already carry
    # their FULL contribution (appearance mass times the standing / retraction
    # factor) through the censoring patterns, and adding log G_D(j) on top would
    # count their appearance twice.
    if (is_count_cumulative == 0L && is_confirmation == 0L &&
        is_retraction == 0L && delay_fully_fixed == 0L &&
        length(obs_delays_cens) > 0) {
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
    if (is_sir == 0L && is_custom_epidemic == 0L) {
      if (is_hierarchical == 1L) {
        # Hierarchical intercept prior: mu_global ~ intercept_prior; delta ~ N(0,1); tau ~ HalfNormal(0,1)
        log_prior <- log_prior + prior_lpdf(mu_global, prior_intercept_dist, prior_intercept_params)
        log_prior <- log_prior + sum(dnorm(delta_intercept, 0, 1, log = TRUE))
        log_prior <- log_prior + dnorm(tau_int, 0, 1, log = TRUE)   # HalfNormal: tau > 0 always here
      } else {
        log_prior <- log_prior + prior_lpdf(mu_intercept, prior_intercept_dist, prior_intercept_params)
      }
    }
    if (is_sir == 0L && is_custom_epidemic == 0L && n_covariates > 0)
      log_prior <- log_prior + prior_lpdf(as.vector(gamma), prior_gamma_dist, prior_gamma_params)
    if (is_negbin == 1L) log_prior <- log_prior + prior_lpdf(1.0 / nb_size, prior_phi_dist, prior_phi_params)
    # Count-cumulative priors are intentionally disjoint from confirm_p and the
    # linelist validation-delay priors.  The mass prior is evaluated on h_R's
    # finite-horizon mass; hurdle ZTP has no magnitude-dispersion parameter.
    if (is_count_cumulative == 1L) {
      if (cumulative_retraction_mass_fixed == 0L) {
        log_prior <- log_prior + prior_lpdf(
          cumulative_retraction_mass,
          prior_cumulative_retraction_mass_dist,
          prior_cumulative_retraction_mass_params
        )
      }
      if (cumulative_retraction_mu_fixed == 0L) {
        log_prior <- log_prior + prior_lpdf(
          cumulative_retraction_mu_v,
          prior_cumulative_retraction_mu_dist,
          prior_cumulative_retraction_mu_params
        )
      }
      if (cumulative_retraction_sigma_fixed == 0L) {
        log_prior <- log_prior + prior_lpdf(
          cumulative_retraction_sigma_v,
          prior_cumulative_retraction_sigma_dist,
          prior_cumulative_retraction_sigma_params
        )
      }
      if (cumulative_retraction_family == 3L &&
          cumulative_retraction_Q_fixed == 0L) {
        log_prior <- log_prior + prior_lpdf(
          cumulative_retraction_shape_Q,
          prior_cumulative_retraction_Q_dist,
          prior_cumulative_retraction_Q_params
        )
      }
      if (cumulative_observation %in% c(2L, 3L)) {
        if (movement_intercept_fixed == 0L)
          log_prior <- log_prior + prior_lpdf(
            movement_intercept_v, prior_movement_intercept_dist,
            prior_movement_intercept_params
          )
        if (movement_age_fixed == 0L)
          log_prior <- log_prior + prior_lpdf(
            movement_age_v, prior_movement_age_dist,
            prior_movement_age_params
          )
        if (movement_previous_fixed == 0L)
          log_prior <- log_prior + prior_lpdf(
            movement_previous_v, prior_movement_previous_dist,
            prior_movement_previous_params
          )
      }
      if (cumulative_observation == 2L && magnitude_size_fixed == 0L) {
        log_prior <- log_prior + prior_lpdf(
          magnitude_size, prior_magnitude_size_dist,
          prior_magnitude_size_params
        )
      }
    }
    # confirmation / retraction priors (p on the natural scale, retraction delay)
    if (is_confirmation == 1L || is_retraction == 1L) {
      if (confirm_p_fixed == 0L) {
        # The same prior is applied to each stratum's p when `stratified_p`.
        confirm_p_all <- if (is_retraction == 1L) confirm_p_vec else confirm_p
        log_prior <- log_prior + prior_lpdf(confirm_p_all, prior_confirm_p_dist, prior_confirm_p_params) +
          sum(log(confirm_p_all) + log(1 - confirm_p_all))    # logit Jacobian
      }
      if (retract_is_np == 1L) {
        if (retract_probs_fixed == 0L)
          log_prior <- log_prior + dirichlet_lpdf(retract_simplex, retract_alpha) + sum(log(retract_simplex))
      } else {
        if (retract_mu_fixed == 0L) log_prior <- log_prior + prior_lpdf(retract_mu, prior_retract_mu_dist, prior_retract_mu_params)
        if (retract_sd_fixed == 0L) log_prior <- log_prior + prior_lpdf(retract_sd_v, prior_retract_sd_dist, prior_retract_sd_params)
        if (retract_is_gengamma == 1L && retract_Q_fixed == 0L)
          log_prior <- log_prior + prior_lpdf(retract_shape_Q, prior_retract_Q_dist, prior_retract_Q_params)
      }
      if (is_competing == 1L) {
        if (negative_mu_fixed == 0L) log_prior <- log_prior + prior_lpdf(negative_mu, prior_negative_mu_dist, prior_negative_mu_params)
        if (negative_sd_fixed == 0L) log_prior <- log_prior + prior_lpdf(negative_sd_v, prior_negative_sd_dist, prior_negative_sd_params)
        if (negative_is_gengamma == 1L && negative_Q_fixed == 0L)
          log_prior <- log_prior + prior_lpdf(negative_shape_Q, prior_negative_Q_dist, prior_negative_Q_params)
      }
    }
    if (epidemic_model == 1L) {
      log_prior <- log_prior + prior_lpdf(gp_alpha, prior_gp_alpha_dist, prior_gp_alpha_params)
      log_prior <- log_prior + prior_lpdf(gp_ell,   prior_gp_ell_dist,   prior_gp_ell_params)
      log_prior <- log_prior + sum(dnorm(basis_coefs, 0, 1, log = TRUE))
    } else if (epidemic_model == 2L) {
      log_prior <- log_prior + prior_lpdf(ar_phi,   prior_ar_phi_dist,   prior_ar_phi_params)
      log_prior <- log_prior + prior_lpdf(ar_sigma, prior_ar_sigma_dist, prior_ar_sigma_params)
      log_prior <- log_prior + sum(dnorm(ar_innov, 0, 1, log = TRUE))
    } else if (is_custom_epidemic == 1L) {
      for (i in seq_len(n_params_custom_epi)) {
        if (epi_is_free[i] == 1L)
          log_prior <- log_prior +
            prior_lpdf(custom_epidemic_params[i], epi_prior_dists[i], epi_prior_params[i, ])
      }
    } else {  # epidemic_model == 3L: SIR
      log_prior <- log_prior + prior_lpdf(R0, prior_R0_dist, prior_R0_params)
      log_prior <- log_prior + prior_lpdf(recovery_rate, prior_gamma_sir_dist, prior_gamma_sir_params)
      log_prior <- log_prior + prior_lpdf(susceptible_frac, prior_n_eff_dist, prior_n_eff_params)
      log_prior <- log_prior + prior_lpdf(ar_phi, prior_ar_phi_sir_dist, prior_ar_phi_sir_params)
      log_prior <- log_prior + prior_lpdf(ar_sigma, prior_ar_sigma_dist, prior_ar_sigma_params)
      log_prior <- log_prior + sum(dnorm(ar_innov, 0, 1, log = TRUE))
    }

    -(loglik_delay + loglik_counts + loglik_retraction + log_prior + log_jacobian)
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
  if (data$epidemic_model == 4L) {                              # custom epidemic
    theta_epi <- as.numeric(parlist$custom_epidemic_params)
    log_mean   <- matrix(as.numeric(priors$intensity_fn(theta_epi)), n_time, n_strata)
  } else if (data$epidemic_model == 3L) {                       # coupled SIR
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
  uses_nb_dispersion <- data$is_negative_binomial == 1L &&
    (!isTRUE(data$is_count_cumulative == 1L) ||
       identical(as.integer(data$count_cumulative_observation), 1L))
  phi_nb <- if (uses_nb_dispersion)
    exp(as.numeric(parlist$log_phi_nb)) else NA_real_

  # -- count-cumulative finite-horizon reconstruction ------------------------
  count_cumulative <- NULL
  if (isTRUE(data$is_count_cumulative == 1L)) {
    H <- as.integer(data$settlement_horizon)
    report_cdf <- as.numeric(delay_fns$cdf(seq_len(H + 1L)))
    report_pmf <- c(report_cdf[1L], diff(report_cdf))
    report_pmf <- report_pmf / sum(report_pmf)

    retract_family <- as.integer(priors$count_cumulative_retraction_family)
    retract_mu <- if (isTRUE(priors$count_cumulative_retraction_mu$is_constant == 1L))
      priors$count_cumulative_retraction_mu$fixed else
        as.numeric(parlist$cumulative_retraction_mu)
    retract_sigma <- if (isTRUE(priors$count_cumulative_retraction_sigma$is_constant == 1L))
      priors$count_cumulative_retraction_sigma$fixed else
        0.01 + exp(as.numeric(parlist$log_cumulative_retraction_sigma_excess))
    retract_fns <- if (retract_family == 3L) {
      retract_Q <- if (isTRUE(priors$count_cumulative_retraction_Q$is_constant == 1L))
        priors$count_cumulative_retraction_Q$fixed else
          .gengamma_shape_transform(
            as.numeric(parlist$cumulative_retraction_Q)
          )$shape_Q
      .delay_distribution_functions(3L, retract_mu, retract_Q, retract_sigma)
    } else {
      .delay_distribution_functions(retract_family, retract_mu, retract_sigma)
    }
    retract_cdf <- as.numeric(retract_fns$cdf(seq_len(H)))
    retract_pmf <- c(retract_cdf[1L], diff(retract_cdf))
    retract_pmf <- retract_pmf / sum(retract_pmf)
    retract_mass <- if (isTRUE(priors$retraction_mass$is_constant == 1L))
      priors$retraction_mass$fixed else
        stats::plogis(as.numeric(parlist$cumulative_retraction_mass_raw))
    components <- .count_cumulative_components(
      report_pmf, retract_pmf, retract_mass, H
    )

    observation <- as.integer(data$count_cumulative_observation)
    movement <- magnitude_size <- NULL
    if (observation %in% c(2L, 3L)) {
      movement <- c(
        intercept = if (isTRUE(priors$movement_intercept$is_constant == 1L))
          priors$movement_intercept$fixed else as.numeric(parlist$movement_intercept),
        age = if (isTRUE(priors$movement_age$is_constant == 1L))
          priors$movement_age$fixed else as.numeric(parlist$movement_age),
        previous = if (isTRUE(priors$movement_previous$is_constant == 1L))
          priors$movement_previous$fixed else as.numeric(parlist$movement_previous)
      )
    }
    if (observation == 2L) {
      magnitude_size <- if (isTRUE(priors$magnitude_size$is_constant == 1L))
        priors$magnitude_size$fixed else
          exp(as.numeric(parlist$log_magnitude_size))
    }
    count_cumulative <- c(
      list(
        observation = observation,
        settlement_horizon = H,
        report_pmf = report_pmf,
        retraction_pmf = retract_pmf,
        retraction_mass = retract_mass,
        movement = movement,
        magnitude_size = magnitude_size
      ),
      components
    )
  }

  # -- confirmation completion (count-cumulative) -----------------------------
  # The final settled count is C_obs(d*) + future genuine additions - still-
  # standing erroneous mass.  Reconstruct the per-(time, stratum) means of those
  # two completion terms from lambda, p, the appearance delay g_D and the
  # retraction delay g_C; predict() draws them and adds to the observed cumulative.
  confirmation <- NULL
  if (isTRUE(data$is_confirmation == 1L)) {
    conf_D <- min(as.integer(data$max_conf_delay) - 1L, 15L)
    # Mirrors the tape-side gate in build_joint_obj(): the reconstruction has to use
    # the same retraction rate the objective was fitted under, or predict() and the
    # fit disagree.
    legacy_eta <- isTRUE(getOption("diseasenowcasting.legacy_retraction_rate", FALSE))
    p <- if (isTRUE(priors$confirm_p$is_constant == 1L)) priors$confirm_p$fixed
         else stats::plogis(as.numeric(parlist$logit_confirm_p))
    appearance_cdf <- as.numeric(delay_fns$cdf(seq_len(conf_D + 1L)))
    g_D_conf <- c(appearance_cdf[1], diff(appearance_cdf)); G_D_conf <- cumsum(g_D_conf)
    retract_mu_v <- if (isTRUE(priors$retract_mu$is_constant == 1L)) priors$retract_mu$fixed else as.numeric(parlist$retract_mu)
    retract_sd_v <- if (isTRUE(priors$retract_sigma$is_constant == 1L)) priors$retract_sigma$fixed else 0.01 + exp(as.numeric(parlist$log_retract_sd_exc))
    retract_fns  <- .delay_distribution_functions(as.integer(priors$retract_family), retract_mu_v, retract_sd_v)
    retract_cdf  <- c(0, as.numeric(retract_fns$cdf(seq_len(conf_D))))
    g_C_conf <- c(0, diff(retract_cdf)); g_W_conf <- .convolve_delays(g_D_conf, g_C_conf); G_W_conf <- cumsum(g_W_conf)
    addition_mean <- retraction_mean <- matrix(0.0, n_time, n_strata)
    for (s in seq_len(n_strata)) for (t in seq_len(n_time)) {
      horizon <- min(as.integer(d_star[t, s]), conf_D)
      appeared    <- G_D_conf[horizon + 1L]
      not_retract <- G_D_conf[horizon + 1L] - G_W_conf[horizon + 1L]
      # Future genuine additions: lambda_t (1 - G_D(d*)) -- genuine cases not yet
      # reported.  Still-standing erroneous mass: mu_t (1 - p) sum_d g_D(d)
      # Sbar_C(d* - d) = mu_t (1 - p) (G_D(d*) - G_W(d*)), where mu_t = lambda_t / p
      # is the GROSS rate.  The `/ p` matches `observed_mean` below (which always had
      # it) and `eta_stream` in the objective; it used to be missing here, so the
      # erroneous mass subtracted from the settled count was a factor of `p` too small.
      addition_mean[t, s]   <- lambda[t, s] * (1 - appeared)
      retraction_mean[t, s] <- (1 - p) * (if (legacy_eta) lambda[t, s] else lambda[t, s] / p) *
        not_retract
    }
    # E[C_t(d*)] = mu_t [G_D(d*) - (1 - p) G_W(d*)]: the mean of the cumulative
    # actually on the books, which conditions the frailty in predict().
    observed_mean <- matrix(0.0, n_time, n_strata)
    for (s in seq_len(n_strata)) for (t in seq_len(n_time)) {
      horizon <- min(as.integer(d_star[t, s]), conf_D)
      observed_mean[t, s] <- (lambda[t, s] / p) *
        (G_D_conf[horizon + 1L] - (1 - p) * G_W_conf[horizon + 1L])
    }
    confirmation <- list(p = p, addition_mean = addition_mean,
                         retraction_mean = retraction_mean, observed_mean = observed_mean)
  }

  # -- linelist retraction completion -----------------------------------------
  # Each STANDING row of age j survives into the settled count independently with
  # probability rho(j) = p / (p + (1 - p) Sbar_C(j)); the rest of the settled count
  # is the genuine cases not yet reported, Poisson(lambda_t (1 - G_D(d*))).  predict()
  # draws the binomial thinning per (event-time, age, stratum) and adds the future
  # term, so all that is needed here is `p` and the rho curve over report ages.
  retraction <- NULL
  if (isTRUE(data$is_linelist_retraction == 1L)) {
    confirm_p_vec <- if (isTRUE(priors$confirm_p$is_constant == 1L)) priors$confirm_p$fixed
                     else stats::plogis(as.numeric(parlist$logit_confirm_p))
    # One p per stratum when `stratified_p`, otherwise the shared one recycled.
    confirm_p_by_stratum <- if (length(confirm_p_vec) == n_strata) confirm_p_vec
                            else rep(confirm_p_vec[1], n_strata)
    confirm_p <- confirm_p_vec[1]
    retract_family <- as.integer(priors$retract_family)
    survival_fn <- if (retract_family == 4L) {
      retract_simplex <- if (isTRUE(priors$retract_probs$is_constant == 1L)) priors$retract_probs$fixed
        else { exp_logits <- exp(as.numeric(parlist$retract_logits))
               c(exp_logits, 1) / (sum(exp_logits) + 1) }
      .nonparametric_delay_functions(retract_simplex, as.integer(priors$retract_probs$bins))$survival
    } else {
      retract_mu_v <- if (isTRUE(priors$retract_mu$is_constant == 1L)) priors$retract_mu$fixed
                      else as.numeric(parlist$retract_mu)
      retract_sd_v <- if (isTRUE(priors$retract_sigma$is_constant == 1L)) priors$retract_sigma$fixed
                      else 0.01 + exp(as.numeric(parlist$log_retract_sd_exc))
      retract_fns <- if (retract_family == 3L) {
        retract_shape_Q <- if (isTRUE(priors$retract_Q$is_constant == 1L)) priors$retract_Q$fixed
                           else .gengamma_shape_transform(as.numeric(parlist$retract_Q))$shape_Q
        .delay_distribution_functions(3L, retract_mu_v, retract_shape_Q, retract_sd_v)
      } else {
        .delay_distribution_functions(retract_family, retract_mu_v, retract_sd_v)
      }
      function(age) exp(as.numeric(retract_fns$log_survival(age)))
    }
    resolution_mode <- as.integer(data$resolution_mode %||% 0L)
    lag_offset <- if (resolution_mode == 0L) 0L else 1L
    # rho[age + 1, stratum]: the exact-row lookup used by the predictive thinning.
    report_ages <- 0:max(as.integer(data$max_report_age %||% 0L), 0L)
    negative_survival_fn <- NULL
    if (!is.null(priors$negative_family)) {
      negative_family <- as.integer(priors$negative_family)
      negative_mu_v <- if (isTRUE(priors$negative_mu$is_constant == 1L)) priors$negative_mu$fixed
                       else as.numeric(parlist$negative_mu)
      negative_sd_v <- if (isTRUE(priors$negative_sigma$is_constant == 1L)) priors$negative_sigma$fixed
                       else 0.01 + exp(as.numeric(parlist$log_negative_sd_exc))
      negative_fns <- if (negative_family == 3L) {
        negative_shape_Q <- if (isTRUE(priors$negative_Q$is_constant == 1L)) priors$negative_Q$fixed
                            else .gengamma_shape_transform(as.numeric(parlist$negative_Q))$shape_Q
        .delay_distribution_functions(3L, negative_mu_v, negative_shape_Q, negative_sd_v)
      } else .delay_distribution_functions(negative_family, negative_mu_v, negative_sd_v)
      negative_survival_fn <- function(age) exp(as.numeric(negative_fns$log_survival(age)))
    }
    rho <- vapply(confirm_p_by_stratum,
                  function(p_s) if (is.null(negative_survival_fn))
                    .retraction_genuine_probability(report_ages, p_s, survival_fn,
                                                    lag_offset, resolution_mode)
                  else
                    .competing_risks_genuine_probability(report_ages, p_s, survival_fn,
                                                         negative_survival_fn, lag_offset),
                  numeric(length(report_ages)))
    rho <- matrix(rho, length(report_ages), n_strata)

    # Censored standing rows: rho has to be averaged over the appearance delays the
    # row is compatible with, so it is computed per pattern rather than looked up.
    censored_standing <- data$standing_censored_rows
    rho_censored <- NULL
    if (!is.null(censored_standing) && nrow(censored_standing) > 0) {
      grid_max        <- as.integer(data$retract_grid_max %||% 0L)
      appearance_pmf  <- as.numeric(.delay_grid(delay_fns$cdf, grid_max)$pmf)
      retract_cdf_grid <- as.numeric(.resolution_lag_grid(
        if (retract_family == 4L) function(lag) 1 - survival_fn(lag) else retract_fns$cdf,
        grid_max, lag_offset)$cdf)
      rho_censored <- numeric(nrow(censored_standing))
      for (stratum in seq_len(n_strata)) {
        in_stratum <- as.integer(censored_standing[, "stratum"]) == stratum
        if (!any(in_stratum)) next
        rho_censored[in_stratum] <- .retraction_genuine_probability_censored(
          censored_standing[in_stratum, , drop = FALSE],
          confirm_p_by_stratum[stratum], appearance_pmf, retract_cdf_grid, lag_offset,
          resolution_mode)
      }
    }

    retraction <- list(p = confirm_p, p_by_stratum = confirm_p_by_stratum,
                       ages = report_ages, rho = rho, rho_censored = rho_censored,
                       # A CONFIRMED row is already in the target and enters the
                       # nowcast with weight 1; a RETRACTED one is gone (weight 0).
                       # A CONFIRMED row is already in the target (weight 1); a
                       # RETRACTED one is out (weight 0).  `resolved_counts` already
                       # holds only the positives, so the weight is 1 whenever any
                       # positive sign is recorded at all.
                       resolved_weight = if (resolution_mode >= 1L) 1 else 0,
                       lag_offset = lag_offset, resolution_mode = resolution_mode)
  }

  list(mu = log_mean, mu_safe = mu_safe, lambda = lambda, Gstar = Gstar,
       log_loc = if (!is.null(delay_fns$log_location)) delay_fns$log_location else NA_real_,
       log_scale = if (!is.null(delay_fns$log_scale)) delay_fns$log_scale else NA_real_,
       delay_mu = delay_log_mean, delay_sigma = delay_sd, phi_nb = phi_nb,
       count_cumulative = count_cumulative,
       confirmation = confirmation, retraction = retraction)
}
