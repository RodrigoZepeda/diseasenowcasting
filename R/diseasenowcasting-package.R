#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import tbl.now
#' @importFrom ggplot2 autoplot
#' @importFrom lifecycle deprecated
#' @importFrom methods as
#' @importFrom RTMB pnorm dnorm dgamma pgamma dbeta dexp dweibull dchisq dnbinom plogis dpois besselI
#' @importFrom rlang .data
#' @importFrom stats nlminb optim median sd quantile approx setNames coef predict
#' @importFrom stats rnbinom rpois rnorm rcauchy rt runif rgamma rweibull rlnorm rchisq rexp rlogis rbeta
#' @importFrom utils head tail
## usethis namespace: end
NULL

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

# Null-coalescing helper available on all supported R versions.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# R CMD check NOTE suppression.
#
# Non-standard evaluation elsewhere is handled in the code itself, so it needs
# no declarations here:
#   * dplyr/ggplot2 data-masking uses the rlang `.data` pronoun (`aes(x = .data$delay)`);
#   * tbl.now tidy-select uses injection (`event_date = !!as.symbol("onset")`);
#   * the backtest worker takes its row index as a real function argument
#     (`future.apply::future_lapply(..., function(cell_row) ...)`).
#
# What remains below are the only variables with no lexical binding the static
# checker can see: RTMB `getAll(params, objective_data)` drops every parameter /
# data name into the objective closure's environment (the AD programming model).
# This cannot be expressed with `.data`, so it must be declared for `R CMD check`.
utils::globalVariables(c(
  # RTMB getAll() closure variables (delay-only objective)
  "delay_logits", "censoring_col", "obs_delays", "row_sums_exact",
  "col_sums_exact", "col_sums_cens",
  "dirichlet_alpha", "n_bins", "obs_delays_cens", "row_sums_cens",
  # RTMB getAll() closure variables (joint objective)
  "mu_intercept", "gamma", "basis_coefs", "ar_innov",
  "log_gp_alpha", "log_gp_ell", "ar_phi_unc", "log_ar_sigma_unc",
  "log_R0", "u_gamma", "u_neff", "log_phi_nb",
  "delay_mu", "log_delay_sigma_excess", "delay_Q",
  "mu_global", "log_tau_intercept", "delta_intercept",
  "delay_mu_fixed", "delay_sigma_fixed", "shape_Q_fixed",
  "case_counts", "d_star", "X", "hsgp_basis_matrix", "hsgp_frequencies",
  "row_sums", "split_delay", "gp_kernel",
  "n_time", "n_strata", "is_hierarchical", "n_covariates",
  "mu_log_upper_bound", "ar_sigma_max", "N_pop", "initial_infected",
  "is_negbin", "is_sir", "is_gengamma", "is_nonparametric",
  "epidemic_model", "delay_fully_fixed", "gstar_precomputed",
  "prior_mu_dist", "prior_mu_params", "prior_sigma_dist", "prior_sigma_params",
  "prior_shape_dist", "prior_shape_params", "prior_intercept_dist", "prior_intercept_params",
  "prior_gamma_dist", "prior_gamma_params", "prior_phi_dist", "prior_phi_params",
  "prior_gp_alpha_dist", "prior_gp_alpha_params", "prior_gp_ell_dist", "prior_gp_ell_params",
  "prior_ar_phi_dist", "prior_ar_phi_params", "prior_ar_sigma_dist", "prior_ar_sigma_params",
  "prior_ar_phi_sir_dist", "prior_ar_phi_sir_params",
  "prior_R0_dist", "prior_R0_params", "prior_gamma_sir_dist", "prior_gamma_sir_params",
  "prior_n_eff_dist", "prior_n_eff_params",
  "delay_mu_is_fixed", "delay_sigma_is_fixed", "shape_Q_is_fixed",
  # RTMB getAll() closure variables (custom delay, family 5)
  "custom_delay_params", "is_custom_delay", "n_params_custom",
  "custom_prior_dists", "custom_prior_params", "custom_is_free",
  # validate_custom_delay() RTMB tape parameter
  "cdf_validate_theta",
  # RTMB getAll() closure variables (custom epidemic, epidemic_model 4)
  "custom_epidemic_params", "is_custom_epidemic", "n_params_custom_epi",
  "epi_prior_dists", "epi_prior_params", "epi_is_free",
  # validate_custom_epidemic() RTMB tape parameter
  "custom_validate_theta_epi",
  # RTMB getAll() closure variables (confirmation / count-cumulative objective)
  "logit_confirm_p", "retract_mu", "log_retract_sd_exc",
  "increment_array", "conf_D", "retract_family", "is_confirmation",
  "confirm_p_fixed", "confirm_p_val", "retract_mu_fixed", "retract_mu_val",
  "retract_sd_fixed", "retract_sd_val",
  "prior_confirm_p_dist", "prior_confirm_p_params",
  "prior_retract_mu_dist", "prior_retract_mu_params",
  "prior_retract_sd_dist", "prior_retract_sd_params",
  "retract_logits", "retract_Q", "retract_Q_val",
  "prior_retract_Q_dist", "prior_retract_Q_params",
  "negative_mu", "negative_mu_val", "log_negative_sd_exc",
  "negative_sd_val", "negative_Q", "negative_Q_val",
  "n_positive_by_stratum", "n_negative_by_stratum",
  "prior_negative_mu_dist", "prior_negative_mu_params",
  "prior_negative_sd_dist", "prior_negative_sd_params",
  "prior_negative_Q_dist", "prior_negative_Q_params",
  # RTMB getAll() closure variables (revised count-cumulative objective)
  "cumulative_retraction_mass_raw", "cumulative_retraction_mass_value",
  "cumulative_retraction_mu", "cumulative_retraction_mu_value",
  "log_cumulative_retraction_sigma_excess",
  "cumulative_retraction_sigma_value",
  "cumulative_retraction_Q", "cumulative_retraction_Q_value",
  "movement_intercept", "movement_intercept_value",
  "movement_age", "movement_age_value",
  "movement_previous", "movement_previous_value",
  "log_magnitude_size", "magnitude_size_value",
  "observation_mask", "cumulative_level_array",
  "previous_nonzero_array", "signed_update_array",
  "prior_cumulative_retraction_mass_dist",
  "prior_cumulative_retraction_mass_params",
  "prior_cumulative_retraction_mu_dist",
  "prior_cumulative_retraction_mu_params",
  "prior_cumulative_retraction_sigma_dist",
  "prior_cumulative_retraction_sigma_params",
  "prior_cumulative_retraction_Q_dist",
  "prior_cumulative_retraction_Q_params",
  "prior_movement_intercept_dist", "prior_movement_intercept_params",
  "prior_movement_age_dist", "prior_movement_age_params",
  "prior_movement_previous_dist", "prior_movement_previous_params",
  "prior_magnitude_size_dist", "prior_magnitude_size_params"
))
