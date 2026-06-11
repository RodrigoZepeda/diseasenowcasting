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
    num_id = S7::class_numeric     # 1=HSGP 2=AR1 3=SIR 4=Custom
  ),
  validator = function(self) {
    if (length(self@num_id) != 1)
      cli::cli_abort("Numeric id `num_id` must be of length 1.")
    if (self@num_id != 4L && !(self@name %in% .valid_epidemic_processes))
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

# =============================================================================
# Custom epidemic process (num_id = 4)
# =============================================================================

#' @keywords internal
#' @noRd
custom_process_class <- S7::new_class(
  "custom_process_class",
  parent = epidemic_process_class,
  properties = list(
    intensity_fn  = S7::class_function,
    n_params      = S7::class_numeric,
    priors        = S7::class_list,
    param_names   = S7::class_character,
    inits         = S7::class_numeric
  ),
  constructor = function(intensity_fn, n_params, priors = list(), name = "Custom",
                         param_names = character(0), inits = numeric(0)) {
    n_p     <- as.integer(n_params)
    pnames  <- if (length(param_names) == 0) paste0("theta", seq_len(n_p)) else as.character(param_names)
    pinits  <- if (length(inits) == 0) rep(0.0, n_p) else as.numeric(inits)
    ppriors <- if (length(priors) == 0) vector("list", n_p) else priors
    S7::new_object(S7::S7_object(),
                   name = as.character(name), num_id = 4L,
                   intensity_fn = intensity_fn,
                   n_params = n_p, priors = ppriors,
                   param_names = pnames, inits = pinits)
  },
  validator = function(self) {
    if (self@n_params < 1L) cli::cli_abort("`n_params` must be >= 1.")
    if (length(self@inits) != self@n_params)
      cli::cli_abort("`inits` length ({length(self@inits)}) must equal `n_params` ({self@n_params}).")
    if (length(self@param_names) != self@n_params)
      cli::cli_abort("`param_names` length ({length(self@param_names)}) must equal `n_params`.")
    if (length(self@priors) != self@n_params)
      cli::cli_abort("`priors` list length ({length(self@priors)}) must equal `n_params`.")
  }
)

#' User-defined epidemic process
#'
#' Lets you supply any RTMB-traceable function `intensity_fn(theta)` that
#' returns the log expected-incidence trajectory `log_mean[n_time x n_strata]`
#' as the latent epidemic process for the nowcast.  This makes the framework
#' epidemic-process agnostic: random walks, ODE models, regression surfaces,
#' and anything else that can be written in AD-safe arithmetic are all valid.
#'
#' @param intensity_fn A function `function(theta)` that takes a numeric
#'   parameter vector and returns a numeric matrix of dimensions
#'   `[n_time x n_strata]` containing log expected incidence (the full
#'   `log_mean`, not just a trend — include any intercept inside the function).
#'   Must use only RTMB-traceable operations: `+`, `-`, `*`, `/`, `exp`, `log`,
#'   `sqrt`, `abs`, `sum`, `for` loops of *fixed* length (not data-dependent).
#'   Never use `if`/`ifelse` on parameter values, `pmax`/`pmin` on AD types, or
#'   external solvers.  Call [validate_custom_process()] to check traceability
#'   before fitting.
#' @param n_params Integer.  Total number of parameters in `theta`.  Must be
#'   known at construction time; for time-varying processes (e.g. a random walk
#'   with `n_time` innovations), inspect `prepare_data()$max_time` first to get
#'   the correct count.
#' @param priors A list of length `n_params`.  Each element is either a
#'   `prior_class` object such as [normal_prior()] (free parameter, estimated)
#'   or a single numeric
#'   scalar (fixed parameter, held constant during optimisation).
#'   An empty list defaults all parameters to `std_normal_prior()`.
#' @param name Character label shown in print and diagnostic output.
#' @param param_names Character vector of length `n_params` with human-readable
#'   names for the parameters.  Defaults to `"theta1"`, `"theta2"`, …
#' @param inits Numeric vector of length `n_params` with starting values.
#'   Defaults to zero for every parameter.
#'
#' @returns A `custom_process_class` object (subclass of
#'   `epidemic_process_class`).
#'
#' @seealso [validate_custom_process()], [epidemic_process]
#'
#' @examples
#' # Pure random walk on log-incidence (n_time = 20)
#' # Uses cumsum() — no [<- assignment needed, so fully AD-safe.
#' n_times <- 20L
#' rw_fn <- function(theta) {
#'   log_mu0  <- theta[1]
#'   sigma_rw <- exp(theta[2])
#'   eps      <- theta[3:(2L + n_times)]
#'   lm       <- log_mu0 + cumsum(sigma_rw * eps)
#'   matrix(lm, n_times, 1L)
#' }
#' proc <- custom_process(
#'   rw_fn,
#'   n_params = 2L + n_times,
#'   priors   = c(list(normal_prior(2, 1), normal_prior(-2, 0.5)),
#'                rep(list(std_normal_prior()), n_times)),
#'   name     = "RandomWalk",
#'   param_names = c("log_mu0", "log_sigma", paste0("eps_", seq_len(n_times))),
#'   inits    = c(2, -2, rep(0, n_times))
#' )
#' @name custom_process
#' @export
custom_process <- function(intensity_fn, n_params, priors = list(), name = "Custom",
                           param_names = character(0), inits = numeric(0)) {
  custom_process_class(intensity_fn = intensity_fn, n_params = as.integer(n_params),
                       priors = priors, name = name,
                       param_names = param_names, inits = inits)
}

#' Validate a user-defined epidemic process for RTMB traceability
#'
#' Tapes `intensity_fn` through `RTMB::MakeADFun` at the supplied (or default)
#' initial values and confirms that the objective value and gradient are both
#' finite.  Emits an informative error if the function is not AD-safe.
#'
#' @param process A `custom_process_class` object from [custom_process()].
#' @param test_theta Optional numeric vector of length `n_params` to use as the
#'   test point.  Defaults to `process@inits`.
#' @returns `process`, invisibly.  Emits a success message if the check passes.
#' @export
validate_custom_process <- function(process, test_theta = NULL) {
  if (!S7::S7_inherits(process, custom_process_class))
    cli::cli_abort("`process` must be a {.cls custom_process_class} object.")
  n_p    <- as.integer(process@n_params)
  theta0 <- if (!is.null(test_theta)) as.numeric(test_theta) else process@inits
  if (length(theta0) != n_p) theta0 <- rep(0.0, n_p)

  fn_to_tape <- process@intensity_fn

  obj <- tryCatch(
    RTMB::MakeADFun(
      function(params) {
        RTMB::getAll(params)
        log_mean_mat <- fn_to_tape(custom_validate_theta_proc)
        -sum(log_mean_mat)
      },
      list(custom_validate_theta_proc = theta0),
      silent = TRUE
    ),
    error = function(e)
      cli::cli_abort(c("RTMB tape construction failed for custom process `{process@name}`.",
                       "i" = "Error: {e$message}",
                       "i" = "Use vector ops ({.code cumsum}, {.code +}, {.code *}, {.code exp}) instead of index assignment.",
                       "i" = "For loops with index assignment: add \"'[<-' <- RTMB::ADoverload('[<-')\" at the top of {.fn intensity_fn}."))
  )

  fn_val <- obj$fn()
  gr_val <- obj$gr()
  if (!is.finite(fn_val))
    cli::cli_abort(c("Custom process `{process@name}` objective is not finite at test_theta.",
                     "i" = "Check for log(0), division by zero, or NaN in {.fn intensity_fn}."))
  if (!all(is.finite(gr_val)))
    cli::cli_abort(c("Custom process `{process@name}` gradient contains non-finite values.",
                     "i" = "Avoid {.code if}/branching on parameter values and {.code pmax}/{.code pmin} on AD types."))

  cli::cli_alert_success("Custom process `{process@name}` passes RTMB traceability check.")
  invisible(process)
}
