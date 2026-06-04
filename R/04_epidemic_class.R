# =============================================================================
# Epidemic process classes
# =============================================================================

#' Epidemic process base class
#' @keywords internal
#' @noRd
epidemic_process_class <- S7::new_class(
  "epidemic_process_class",
  properties = list(
    name   = S7::class_character,
    num_id = S7::class_numeric     # 1=HSGP 2=AR1 3=SIR
  ),
  validator = function(self) {
    if (length(self@num_id) != 1)
      cli::cli_abort("Numeric id `num_id` must be of length 1.")
    if (!(self@name %in% .valid_epidemic_processes))
      cli::cli_abort("Invalid epidemic process `name`: {self@name}. Supported: {.val {(.valid_epidemic_processes)}}")
  }
)

#' @keywords internal
#' @noRd
hsgp_epidemic_class <- S7::new_class(
  "hsgp_epidemic_class",
  parent = epidemic_process_class,
  properties = list(
    alpha        = .valid_param_slot,
    ell          = .valid_param_slot,
    gp_kernel       = S7::class_numeric,
    gp_basis        = S7::class_numeric,
    num_basis       = S7::class_numeric,
    tmax_model      = S7::class_numeric
  ),
  constructor = function(alpha = numeric(0), ell = numeric(0),
                         gp_kernel = "matern32", gp_basis = "dirichlet",
                         num_basis = 20, tmax_model = 1000L) {
    S7::new_object(S7::S7_object(),
                   name = "HSGP", num_id = 1L,
                   alpha = alpha, ell = ell,
                   gp_kernel = .parse_gp_kernel(gp_kernel),
                   gp_basis  = .parse_gp_basis(gp_basis),
                   num_basis = if (length(num_basis) == 0) 0L else as.integer(num_basis),
                   tmax_model = as.integer(tmax_model))
  },
  validator = function(self) {
    if (!valid_positive_prior(self@alpha)) cli::cli_abort("Invalid `alpha`.")
    if (!valid_positive_prior(self@ell))   cli::cli_abort("Invalid `ell`.")
    if (self@num_basis < 0)  cli::cli_abort("`num_basis` should be >= 0.")
    if (self@tmax_model < 0) cli::cli_abort("`tmax_model` should be >= 0.")
  }
)

#' @keywords internal
#' @noRd
ar1_epidemic_class <- S7::new_class(
  "ar1_epidemic_class",
  parent = epidemic_process_class,
  properties = list(phi = .valid_param_slot, sigma = .valid_param_slot, error = .valid_param_slot),
  constructor = function(phi = numeric(0), sigma = numeric(0), error = numeric(0)) {
    S7::new_object(S7::S7_object(),
                   name = "AR1", num_id = 2L, phi = phi, sigma = sigma, error = error)
  },
  validator = function(self) {
    if (!valid_positive_prior(self@sigma)) cli::cli_abort("Invalid `sigma`.")
  }
)

#' @keywords internal
#' @noRd
sir_epidemic_class <- S7::new_class(
  "sir_epidemic_class",
  parent = epidemic_process_class,
  properties = list(
    R0 = .valid_param_slot, gamma = .valid_param_slot, N_eff = .valid_param_slot,
    N_pop = S7::class_numeric, use_beta_rw_trend = S7::class_logical
  ),
  constructor = function(R0 = numeric(0), gamma = numeric(0), N_eff = numeric(0),
                         N_pop = 10000, use_beta_rw_trend = TRUE) {
    S7::new_object(S7::S7_object(),
                   name = "SIR", num_id = 3L, R0 = R0, gamma = gamma, N_eff = N_eff,
                   N_pop = as.numeric(N_pop), use_beta_rw_trend = as.logical(use_beta_rw_trend))
  },
  validator = function(self) {
    if (!valid_positive_prior(self@R0))    cli::cli_abort("Invalid `R0`.")
    if (!valid_positive_prior(self@gamma)) cli::cli_abort("Invalid `gamma`.")
    if (!valid_positive_prior(self@N_eff)) cli::cli_abort("Invalid `N_eff`.")
    if (self@N_pop < 0) cli::cli_abort("`N_pop` should be >= 0.")
    if (length(self@use_beta_rw_trend) > 1) cli::cli_abort("`use_beta_rw_trend` should be a single TRUE/FALSE.")
  }
)

#' Epidemic process for the Bayesian Nowcast
#'
#' Specify the latent epidemic process.  Parameter slots accept a fixed
#' numeric, a `prior_class`, or `numeric(0)` for the default prior.  The
#' log-incidence mean intercept is inherited from the likelihood (`mu`).
#'
#' @param alpha HSGP GP amplitude prior (> 0).
#' @param ell   HSGP GP length-scale prior (> 0).
#' @param gp_kernel HSGP kernel: `"sq_exp"`, `"matern32"` (default), `"matern52"`.
#' @param gp_basis HSGP eigenbasis: `"dirichlet"`/`"sine"` (default) or `"neumann"`/`"cosine"`.
#' @param num_basis HSGP basis count; `numeric(0)`/`0` = auto from series length.
#' @param tmax_model HSGP time normalisation; `0` = auto (newest point at the right boundary).
#' @param phi   AR(1) autocorrelation prior in (-1, 1).
#' @param sigma AR(1) innovation SD prior (> 0).
#' @param error AR(1) standardised innovation prior.
#' @param R0    SIR basic reproduction number prior (> 0).
#' @param gamma SIR recovery rate prior in (0, 1).
#' @param N_eff SIR effective susceptible fraction prior in (0, 1).
#' @param N_pop SIR total population (default 10000).
#' @param use_beta_rw_trend SIR: beta follows an AR(1) walk if TRUE (default).
#'
#' @returns An `epidemic_process_class` object.
#'
#' @section Default priors:
#' When a prior argument is left empty, [default_priors()] supplies these
#' defaults (see also [nowcast(prior_only = TRUE)][nowcast] to visualise them):
#'
#' **HSGP** (`hsgp_epidemic`):
#' \itemize{
#'   \item `alpha` (GP amplitude): `half_normal_prior(0, 1)`
#'   \item `ell` (GP length-scale): `inv_gamma_prior(3, 1)`
#' }
#'
#' **AR(1)** (`ar1_epidemic`):
#' \itemize{
#'   \item `phi` (autocorrelation): `std_normal_prior()`
#'   \item `sigma` (innovation SD): `exponential_prior(100)`
#'   \item innovations: `std_normal_prior()`
#' }
#'
#' **SIR** (`sir_epidemic`):
#' \itemize{
#'   \item `R0`: `lognormal_prior(log(2), 0.5)`
#'   \item `gamma` (recovery rate): `lognormal_prior(log(1/5), 0.5)`
#'   \item `N_eff` (susceptible fraction): `beta_prior(2, 5)`
#' }
#'
#' The log-incidence intercept comes from the likelihood (`mu`), defaulting to a
#' data-informed `normal_prior()` centred at the log median daily count.
#'
#' @examples
#' hsgp_epidemic()
#' hsgp_epidemic(gp_kernel = "sq_exp", num_basis = 20)
#' ar1_epidemic(phi = 0.9)
#' sir_epidemic(R0 = 2.5, use_beta_rw_trend = FALSE)
#'
#' @name epidemic_process
NULL

#' @rdname epidemic_process
#' @export
hsgp_epidemic <- function(alpha = numeric(0), ell = numeric(0),
                          gp_kernel = "matern32", gp_basis = "dirichlet",
                          num_basis = 0, tmax_model = 0) {
  hsgp_epidemic_class(alpha = alpha, ell = ell,
                      gp_kernel = gp_kernel, gp_basis = gp_basis, num_basis = num_basis,
                      tmax_model = tmax_model)
}

#' @rdname epidemic_process
#' @export
ar1_epidemic <- function(phi = numeric(0), sigma = numeric(0), error = numeric(0)) {
  ar1_epidemic_class(phi = phi, sigma = sigma, error = error)
}

#' @rdname epidemic_process
#' @export
sir_epidemic <- function(R0 = numeric(0), gamma = numeric(0), N_eff = numeric(0),
                         N_pop = 10000, use_beta_rw_trend = TRUE) {
  sir_epidemic_class(R0 = R0, gamma = gamma, N_eff = N_eff,
                     N_pop = N_pop, use_beta_rw_trend = use_beta_rw_trend)
}
