#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import tbl.now
#' @importFrom doFuture %dofuture%
#' @importFrom foreach foreach %dopar%
#' @importFrom ggplot2 autoplot
#' @importFrom lifecycle deprecated
#' @importFrom methods as
#' @importFrom RTMB pnorm dnorm dgamma pgamma dbeta dexp dweibull dchisq dnbinom plogis
#' @importFrom stats nlminb optim median sd quantile approx setNames coef predict
#' @importFrom stats rnbinom rpois rnorm rcauchy rt runif rgamma rweibull rlnorm rchisq rexp rlogis rbeta
#' @importFrom utils head tail
## usethis namespace: end
NULL

# Null-coalescing helper available on all supported R versions.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Suppress R CMD check NOTEs for column names used in aes() / dplyr NSE
utils::globalVariables(c(
  # ggplot2 column aesthetics (autoplot methods)
  "t", "median", "q5", "q25", "q75", "q95", "q2.5", "q97.5", ".event_num",
  "delay", "weight", "cdf", "density", "lo", "hi", "med", "pmf",
  "t_idx", "observed", "pred",
  "wis", "disease", "group",
  "x_val", "reported", "predicted_total", "q_lo", "q_hi", "stratum", "event_date",
  # dplyr column names (score/backtest)
  "model", "date_run", "final", "quantile_level", "predicted",
  "epidemic_label", "nb_label", "delay_label", "n",
  "overprediction", "underprediction", "dispersion",
  "interval_coverage_50", "interval_coverage_90",
  # dplyr NSE column names used in prepare_data helpers (09_prepare_data.R)
  "time", "strata", "count",
  # dplyr NSE column names used in update helpers (22_update.R)
  "is_surprising", "direction",
  # foreach %dofuture% iterator variable (23_backtest.R)
  "cell_row",
  # tbl.now / nowcast internals
  ".data", "window", "epidemic", "n_events", "now",
  "onset", "reported",   # NSE column names in .temporal_effect_matrix grid
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
  # RTMB getAll() closure variables (custom process, epidemic_model 4)
  "custom_process_params", "is_custom_process", "n_params_custom_proc",
  "proc_prior_dists", "proc_prior_params", "proc_is_free",
  # validate_custom_process() RTMB tape parameter
  "custom_validate_theta_proc",
  # surprise() internal variable
  "parlist"
))
