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
                                   # 5=ARIMA 6=ETS family 7=STS
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
    .check_fixed_domain(self@alpha, "alpha", "hsgp_epidemic", "the positive line",
                        function(v) v > 0)
    .check_fixed_domain(self@ell, "ell", "hsgp_epidemic", "the positive line",
                        function(v) v > 0)
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
    .check_fixed_domain(self@phi, "phi", "ar1_epidemic", "the open interval (-1, 1)",
                        function(v) v > -0.999 & v < 0.999)
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
    .check_fixed_domain(self@gamma, "gamma", "sir_epidemic", "the open interval (0, 1)",
                        function(v) v > 0 & v < 1)
    .check_fixed_domain(self@N_eff, "N_eff", "sir_epidemic", "the open interval (0, 1)",
                        function(v) v > 0 & v < 1)
    .check_fixed_domain(self@R0, "R0", "sir_epidemic", "the positive line",
                        function(v) v > 0)
    if (self@N_pop < 0) cli::cli_abort("`N_pop` should be >= 0.")
    if (length(self@use_beta_rw_trend) > 1) cli::cli_abort("`use_beta_rw_trend` should be a single TRUE/FALSE.")
  }
)

#' Epidemic process for the Bayesian Nowcast
#'
#' Specify the latent epidemic process.  Parameter slots accept a `prior_class`
#' (estimate it), a fixed numeric (hold it at that value), or `numeric(0)` for
#' the default prior.  The log-incidence mean intercept is inherited from the
#' likelihood (`mu`).
#'
#' A fixed value is held exactly: the parameter is seeded there, dropped from the
#' optimisation, and its prior and Jacobian terms drop with it.  For a stratified
#' fit, supply one value to share across strata or one per stratum.  `coef()`
#' reports a held parameter; `parameters()` does not, because it reports
#' estimates with credible intervals and a held parameter has none.
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
custom_epidemic_class <- S7::new_class(
  "custom_epidemic_class",
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
#' `r lifecycle::badge('experimental')`
#' 
#' @param intensity_fn A function `function(theta)` that takes a numeric
#'   parameter vector and returns a numeric matrix of dimensions
#'   `[n_time x n_strata]` containing log expected incidence (the full
#'   `log_mean`, not just a trend — include any intercept inside the function).
#'   Must use only RTMB-traceable operations: `+`, `-`, `*`, `/`, `exp`, `log`,
#'   `sqrt`, `abs`, `sum`, `for` loops of *fixed* length (not data-dependent).
#'   Never use `if`/`ifelse` on parameter values, `pmax`/`pmin` on AD types, or
#'   external solvers.  Call [validate_custom_epidemic()] to check traceability
#'   before fitting.
#' @param priors A list with one element per parameter in `theta`.  Each element
#'   is either a `prior_class` object such as [normal_prior()] (free parameter,
#'   estimated) or a single numeric scalar (fixed parameter, held constant
#'   during optimisation).  The number of parameters is inferred from the length
#'   of this list (or from `param_names` / `inits` if `priors` is omitted).  For
#'   time-varying processes (e.g. a random walk with one innovation per
#'   event-time), get the number of event-times with [infer_max_time()] first.
#' @param name Character label shown in print and diagnostic output.
#' @param param_names Character vector naming the parameters; its length sets the
#'   number of parameters if `priors` is empty.  Defaults to `"theta1"`, …
#' @param inits Numeric vector of starting values; its length sets the number of
#'   parameters if `priors` and `param_names` are empty.  Defaults to zeros.
#'
#' @returns A `custom_epidemic_class` object (subclass of
#'   `epidemic_process_class`).
#'
#' @seealso [validate_custom_epidemic()], [epidemic_process]
#'
#' @examples
#' # Pure random walk on log-incidence (max_time = 20)
#' # Uses cumsum() — no [<- assignment needed, so fully AD-safe.
#' max_time <- 20L
#' rw_fn <- function(theta) {
#'   log_mu0  <- theta[1]
#'   sigma_rw <- exp(theta[2])
#'   eps      <- theta[3:(2L + max_time)]
#'   lm       <- log_mu0 + cumsum(sigma_rw * eps)
#'   matrix(lm, max_time, 1L)
#' }
#' # n_params is inferred (here 2 + max_time) from the priors list:
#' custom_epi <- custom_epidemic(
#'   rw_fn,
#'   priors   = c(list(normal_prior(2, 1), normal_prior(-2, 0.5)),
#'                rep(list(std_normal_prior()), max_time)),
#'   name     = "RandomWalk",
#'   param_names = c("log_mu0", "log_sigma", paste0("eps_", seq_len(max_time))),
#'   inits    = c(2, -2, rep(0, max_time))
#' )
#' @name custom_epidemic
#' @export
custom_epidemic <- function(intensity_fn, priors = list(), name = "Custom",
                           param_names = character(0), inits = numeric(0)) {
  n_params <- .infer_n_params(priors, param_names, inits)
  custom_epidemic_class(intensity_fn = intensity_fn, n_params = n_params,
                       priors = priors, name = name,
                       param_names = param_names, inits = inits)
}

#' Validate a user-defined epidemic process for RTMB traceability
#'
#' `r lifecycle::badge('experimental')`
#' 
#' Passes `intensity_fn` through `RTMB::MakeADFun` at the supplied (or default)
#' initial values and confirms that the objective value and gradient are both
#' finite.  Emits an informative error if the function is not AD-safe.
#'
#' @param epidemic A `custom_epidemic_class` object from [custom_epidemic()].
#' @param test_theta Optional numeric vector of length `n_params` to use as the
#'   test point.  Defaults to `epidemic@inits`.
#' @returns `epidemic`, invisibly.  Emits a success message if the check passes.
#' @examples
#' # Custom components tape USER functions, so RTMB must be attached
#' # (it is kept in Imports, not Depends, so attach it yourself):
#' library(RTMB)
#' max_time <- 15L
#' rw_fn <- function(theta)
#'   matrix(theta[1] + cumsum(exp(theta[2]) * theta[3:(2L + max_time)]), max_time, 1L)
#' custom_epi <- custom_epidemic(
#'   rw_fn,
#'   priors = c(list(normal_prior(2, 1), normal_prior(-2, 0.5)),
#'              rep(list(std_normal_prior()), max_time)),
#'   inits  = c(2, -2, rep(0, max_time))
#' )
#' validate_custom_epidemic(custom_epi)
#' @export
validate_custom_epidemic <- function(epidemic, test_theta = NULL) {
  if (!S7::S7_inherits(epidemic, custom_epidemic_class))
    cli::cli_abort("`epidemic` must be a {.cls custom_epidemic_class} object.")
  .assert_rtmb_attached("custom epidemic processes")
  n_p    <- as.integer(epidemic@n_params)
  theta0 <- if (!is.null(test_theta)) as.numeric(test_theta) else epidemic@inits
  if (length(theta0) != n_p) theta0 <- rep(0.0, n_p)

  fn_to_tape <- epidemic@intensity_fn

  obj <- tryCatch(
    RTMB::MakeADFun(
      function(params) {
        RTMB::getAll(params)
        log_mean_mat <- fn_to_tape(custom_validate_theta_epi)
        -sum(log_mean_mat)
      },
      list(custom_validate_theta_epi = theta0),
      silent = TRUE
    ),
    error = function(e)
      cli::cli_abort(c("RTMB tape construction failed for custom epidemic `{epidemic@name}`.",
                       "i" = "Error: {e$message}",
                       "i" = "Use vector ops ({.code cumsum}, {.code +}, {.code *}, {.code exp}) instead of index assignment.",
                       "i" = "For loops with index assignment: add \"'[<-' <- RTMB::ADoverload('[<-')\" at the top of {.fn intensity_fn}."))
  )

  fn_val <- obj$fn()
  gr_val <- obj$gr()
  if (!is.finite(fn_val))
    cli::cli_abort(c("Custom epidemic `{epidemic@name}` objective is not finite at test_theta.",
                     "i" = "Check for log(0), division by zero, or NaN in {.fn intensity_fn}."))
  if (!all(is.finite(gr_val)))
    cli::cli_abort(c("Custom epidemic `{epidemic@name}` gradient contains non-finite values.",
                     "i" = "Avoid {.code if}/branching on parameter values and {.code pmax}/{.code pmin} on AD types."))

  cli::cli_alert_success("Custom epidemic `{epidemic@name}` passes RTMB traceability check.")
  invisible(epidemic)
}

# =============================================================================
# Classical time-series epidemic processes (num_id 5 = ARIMA, 6 = ETS family,
# 7 = structural time series)
# =============================================================================
# These three engines cover ARIMA / random walk / naive / Theta / exponential
# smoothing / structural trends.  They are deliberately grouped by ENGINE rather
# than by name: a random walk is an ETS with the slope switched off, and Theta is
# that same process with a drift, so giving each its own objective branch would
# be three copies of one recursion that could drift apart.  The names still
# differ, because a scoreboard that says "RW" is more useful than one that says
# "ETS(none, drift = FALSE)".
#
# All three add their trend to `mu_intercept + X %*% gamma` exactly as HSGP and
# AR(1) do, so `tbl_now` covariates and temporal effects (day-of-week dummies,
# Fourier seasonal terms) apply to them unchanged.
# =============================================================================

#' @keywords internal
#' @noRd
arima_epidemic_class <- S7::new_class(
  "arima_epidemic_class",
  parent = epidemic_process_class,
  properties = list(
    p = S7::class_numeric, d = S7::class_numeric, q = S7::class_numeric,
    include_drift = S7::class_logical,
    ar = .valid_param_slot, ma = .valid_param_slot,
    sigma = .valid_param_slot, drift = .valid_param_slot
  ),
  constructor = function(p = 2, d = 1, q = 0, include_drift = NA,
                         ar = numeric(0), ma = numeric(0),
                         sigma = numeric(0), drift = numeric(0)) {
    order_d <- as.integer(d)
    drift_on <- if (length(include_drift) == 1L && !is.na(include_drift))
      as.logical(include_drift) else order_d >= 1L
    S7::new_object(S7::S7_object(),
                   name = "ARIMA", num_id = 5L,
                   p = as.integer(p), d = order_d, q = as.integer(q),
                   include_drift = drift_on,
                   ar = ar, ma = ma, sigma = sigma, drift = drift)
  },
  validator = function(self) {
    if (length(self@p) != 1 || self@p < 0) cli::cli_abort("`p` must be a single integer >= 0.")
    if (length(self@d) != 1 || self@d < 0) cli::cli_abort("`d` must be a single integer >= 0.")
    if (length(self@q) != 1 || self@q < 0) cli::cli_abort("`q` must be a single integer >= 0.")
    if (self@d > 2) cli::cli_abort("`d` above 2 is not supported; log incidence differenced twice is already a very flexible trend.")
    if (self@p > 5 || self@q > 5) cli::cli_abort("`p` and `q` are capped at 5; higher orders are not identified from an epidemic series this short.")
    if (isTRUE(self@include_drift) && self@d == 0)
      cli::cli_abort(c(
        "An ARIMA drift is not identified at {.code d = 0}.",
        "i" = "With no differencing the ARMA mean and the epidemic intercept {.code mu_intercept} are the same quantity, so the two would trade off invisibly.",
        "*" = "Use {.code d >= 1}, or leave {.code include_drift = FALSE}."
      ))
    .check_fixed_domain(self@ar, "ar", "arima_epidemic", "the open interval (-1, 1)",
                        function(v) v > -0.999 & v < 0.999)
    .check_fixed_domain(self@ma, "ma", "arima_epidemic", "the open interval (-1, 1)",
                        function(v) v > -0.999 & v < 0.999)
    .check_fixed_domain(self@sigma, "sigma", "arima_epidemic", "the positive line",
                        function(v) v > 0)
    if (!valid_positive_prior(self@sigma)) cli::cli_abort("Invalid `sigma`: it must be a prior on the positive line.")
  }
)

#' @keywords internal
#' @noRd
ets_epidemic_class <- S7::new_class(
  "ets_epidemic_class",
  parent = epidemic_process_class,
  properties = list(
    trend = S7::class_character,          # "none" | "additive"
    damped = S7::class_logical,
    include_drift = S7::class_logical,
    sigma = .valid_param_slot, beta = .valid_param_slot,
    damping = .valid_param_slot, drift = .valid_param_slot,
    slope_init = .valid_param_slot
  ),
  constructor = function(trend = "additive", damped = TRUE, include_drift = FALSE,
                         sigma = numeric(0), beta = numeric(0),
                         damping = numeric(0), drift = numeric(0),
                         slope_init = numeric(0),
                         name = "ETS", num_id = 6L) {
    S7::new_object(S7::S7_object(),
                   name = as.character(name), num_id = as.integer(num_id),
                   trend = .parse_ets_trend(trend),
                   damped = as.logical(damped),
                   include_drift = as.logical(include_drift),
                   sigma = sigma, beta = beta, damping = damping,
                   drift = drift, slope_init = slope_init)
  },
  validator = function(self) {
    if (!self@trend %in% c("none", "additive"))
      cli::cli_abort("`trend` must be \"none\" or \"additive\".")
    if (length(self@damped) != 1) cli::cli_abort("`damped` must be a single TRUE/FALSE.")
    if (length(self@include_drift) != 1) cli::cli_abort("`include_drift` must be a single TRUE/FALSE.")
    .check_fixed_domain(self@sigma, "sigma", "ets_epidemic", "the positive line",
                        function(v) v > 0)
    .check_fixed_domain(self@beta, "beta", "ets_epidemic", "the open interval (0, 1)",
                        function(v) v > 0 & v < 1)
    .check_fixed_domain(self@damping, "damping", "ets_epidemic", "the open interval (0, 1)",
                        function(v) v > 0 & v < 1)
    if (!valid_positive_prior(self@sigma)) cli::cli_abort("Invalid `sigma`: it must be a prior on the positive line.")
  }
)

#' @keywords internal
#' @noRd
random_walk_epidemic_class <- S7::new_class(
  "random_walk_epidemic_class",
  parent = ets_epidemic_class,
  constructor = function(sigma = numeric(0), drift = numeric(0),
                         include_drift = FALSE, name = "RW") {
    S7::new_object(S7::S7_object(),
                   name = as.character(name), num_id = 6L,
                   trend = "none", damped = FALSE,
                   include_drift = as.logical(include_drift),
                   sigma = sigma, beta = numeric(0), damping = numeric(0),
                   drift = drift, slope_init = numeric(0))
  }
)

#' @keywords internal
#' @noRd
theta_epidemic_class <- S7::new_class(
  "theta_epidemic_class",
  parent = ets_epidemic_class,
  constructor = function(sigma = numeric(0), drift = numeric(0)) {
    S7::new_object(S7::S7_object(),
                   name = "Theta", num_id = 6L,
                   trend = "none", damped = FALSE, include_drift = TRUE,
                   sigma = sigma, beta = numeric(0), damping = numeric(0),
                   drift = drift, slope_init = numeric(0))
  }
)

#' @keywords internal
#' @noRd
sts_epidemic_class <- S7::new_class(
  "sts_epidemic_class",
  parent = epidemic_process_class,
  properties = list(
    trend = S7::class_character,   # "semilocal" | "local_linear" | "local_level"
    level_sigma = .valid_param_slot, slope_sigma = .valid_param_slot,
    slope_phi = .valid_param_slot, slope_mean = .valid_param_slot,
    slope_init = .valid_param_slot
  ),
  constructor = function(trend = "semilocal",
                         level_sigma = numeric(0), slope_sigma = numeric(0),
                         slope_phi = numeric(0), slope_mean = numeric(0),
                         slope_init = numeric(0)) {
    S7::new_object(S7::S7_object(),
                   name = "STS", num_id = 7L,
                   trend = .parse_sts_trend(trend),
                   level_sigma = level_sigma, slope_sigma = slope_sigma,
                   slope_phi = slope_phi, slope_mean = slope_mean,
                   slope_init = slope_init)
  },
  validator = function(self) {
    if (!self@trend %in% c("semilocal", "local_linear", "local_level"))
      cli::cli_abort("`trend` must be \"semilocal\", \"local_linear\" or \"local_level\".")
    .check_fixed_domain(self@level_sigma, "level_sigma", "sts_epidemic", "the positive line",
                        function(v) v > 0)
    .check_fixed_domain(self@slope_sigma, "slope_sigma", "sts_epidemic", "the positive line",
                        function(v) v > 0)
    .check_fixed_domain(self@slope_phi, "slope_phi", "sts_epidemic", "the open interval (-1, 1)",
                        function(v) v > -0.999 & v < 0.999)
    if (!valid_positive_prior(self@level_sigma)) cli::cli_abort("Invalid `level_sigma`.")
    if (!valid_positive_prior(self@slope_sigma)) cli::cli_abort("Invalid `slope_sigma`.")
  }
)

#' @keywords internal
#' @noRd
.parse_ets_trend <- function(trend) {
  key <- tolower(trimws(as.character(trend)))
  key <- switch(key, "n" = "none", "a" = "additive", "additive" = "additive",
                "none" = "none", "level" = "none", "linear" = "additive", key)
  if (!key %in% c("none", "additive"))
    cli::cli_abort("`trend` must be one of \"none\" or \"additive\". Got: {.val {trend}}")
  key
}

#' @keywords internal
#' @noRd
.parse_sts_trend <- function(trend) {
  key <- tolower(trimws(as.character(trend)))
  key <- gsub("[ -]", "_", key)
  key <- switch(key, "semi_local" = "semilocal", "semilocal" = "semilocal",
                "semi_local_linear" = "semilocal", "semilocal_linear" = "semilocal",
                "local_linear" = "local_linear", "locallinear" = "local_linear",
                "local_level" = "local_level", "locallevel" = "local_level",
                "level" = "local_level", key)
  if (!key %in% c("semilocal", "local_linear", "local_level"))
    cli::cli_abort("`trend` must be one of \"semilocal\", \"local_linear\" or \"local_level\". Got: {.val {trend}}")
  key
}

#' Classical time-series epidemic processes
#'
#' Latent trends for log incidence taken from the forecasting literature: ARIMA,
#' a structural time series, single-source exponential smoothing, and the random
#' walk / naive and Theta baselines.  Each one plays the same role as
#' [hsgp_epidemic()] or [ar1_epidemic()] -- it supplies the trend in
#'
#' \deqn{\log \mu_{t,s} = \texttt{mu\_intercept}_s + (X\gamma_{\cdot,s})_t + \mathrm{trend}_s(t)}
#'
#' so `tbl_now` covariates and temporal effects (day-of-week dummies, Fourier
#' seasonal terms from `temporal_effects(seasons = )`) apply to them unchanged,
#' and every trend coefficient is estimated per stratum.
#'
#' @section Seasonality:
#' None of these carry seasonal *states* (no SARIMA `(P, D, Q)_s`, no
#' Holt-Winters seasonal vector).  Seasonality is handled by the covariate path
#' instead: `temporal_effects(day_of_week = TRUE)` adds reference-coded weekday
#' dummies and `temporal_effects(seasons = c(7, 52))` adds Fourier pairs, both of
#' which cost a handful of coefficients rather than `s` latent states per
#' stratum.  On the series lengths this package sees, an `s = 52` seasonal state
#' is the difference between a fit that converges and one that does not.
#'
#' @section Which is which:
#' \describe{
#'   \item{`arima_epidemic(p, d, q)`}{The `d`-th difference of the trend is a
#'     conditional ARMA(p, q).  AR and MA coefficients are parameterised through
#'     their partial autocorrelations, so stationarity and invertibility hold by
#'     construction and the optimiser cannot wander into an explosive region.}
#'   \item{`sts_epidemic(trend = "semilocal")`}{A local level plus a slope that
#'     reverts to a long-run value: `delta_t = D + phi (delta_{t-1} - D) + noise`.
#'     This is the trend to reach for when a local linear trend would extrapolate
#'     an epidemic's current growth rate off the top of the plot.  `"local_linear"`
#'     and `"local_level"` are the `phi = 1, D = 0` and no-slope special cases.}
#'   \item{`ets_epidemic(trend = "additive", damped = TRUE)`}{The
#'     single-source-of-error damped local trend of Hyndman et al. (2008): level
#'     and slope are driven by the *same* innovation, a rank-one restriction of
#'     the structural model above.}
#'   \item{`random_walk_epidemic()` / `naive_epidemic()`}{A random walk on log
#'     incidence -- the baseline every other process has to beat.
#'     `naive_epidemic()` is the named baseline slot; it currently returns a
#'     random walk, and is documented separately so the baseline can be changed
#'     later without breaking the meaning of `random_walk_epidemic()`.}
#'   \item{`theta_epidemic()`}{The Theta method's model form: a random walk with
#'     an estimated drift.  Hyndman and Billah (2003) show the Theta method is
#'     simple exponential smoothing with drift; under a count likelihood the
#'     exponential smoothing is what the Kalman filter for a local level model
#'     already does, with the smoothing weight set by the signal-to-noise ratio,
#'     so what is left to estimate is the drift.}
#' }
#'
#' @section Default priors:
#' \itemize{
#'   \item ARIMA: AR/MA partial autocorrelations `normal_prior(0, 0.5)` on
#'     (-1, 1) -- shrunk towards zero because a partial autocorrelation near 1 on
#'     a differenced series is an I(2) level, whose predictive variance grows
#'     like the square of the nowcast horizon;
#'     `sigma` `exponential_prior(10)` on (0, `ar_sigma_max`); `drift`
#'     `normal_prior(0, 0.1)`.
#'   \item STS: `level_sigma` `exponential_prior(10)`, `slope_sigma`
#'     `exponential_prior(100)` (slopes should move an order of magnitude more
#'     slowly than levels), `slope_phi` `std_normal_prior()` on (-1, 1),
#'     `slope_mean` and `slope_init` `normal_prior(0, 0.1)`.
#'   \item ETS family: `sigma` `exponential_prior(10)`, `beta`
#'     `beta_prior(2, 8)` on (0, 1), `damping` `beta_prior(5, 2)` mapped onto
#'     (0.8, 0.998), `drift` and `slope_init` `normal_prior(0, 0.1)`.
#' }
#' All innovations are standard normal (non-centred), as they are for AR(1).
#'
#' @param p,d,q ARIMA orders, defaulting to `(2, 1, 0)`.  `d` is capped at 2 and
#'   `p`, `q` at 5.  An MA term is available but not the default: on a latent
#'   trend an ARMA(1, 1) sits close to a common factor, where `ar` and `ma`
#'   nearly cancel and neither is well identified.  In the package's backtest
#'   `(1, 1, 1)` was the worst of the nine processes tried (relative WIS 2.06 vs
#'   1.63 for `(2, 1, 0)`) with bands 2.4x the settled count and 15 of the 21
#'   over-wide fits recorded across the whole grid.
#' @param include_drift Include a drift term.  For ARIMA it defaults to `TRUE`
#'   when `d >= 1` and is refused at `d = 0`, where the ARMA mean and
#'   `mu_intercept` are the same quantity.
#' @param ar,ma ARIMA partial-autocorrelation priors in (-1, 1).
#' @param sigma Innovation SD prior (> 0), or a number to hold it there.
#' @param drift Drift prior (unbounded), or a number to hold it there.
#' @param trend For `sts_epidemic()`: `"semilocal"` (default), `"local_linear"`
#'   or `"local_level"`.  For `ets_epidemic()`: `"additive"` (default) or
#'   `"none"`.
#' @param damped ETS: damp the slope towards zero (default `TRUE`).
#' @param beta ETS slope-to-level innovation ratio prior in (0, 1)
#'   (Hyndman's `beta*`).
#' @param damping ETS damping prior, mapped onto (0.8, 0.998).
#' @param slope_init Prior for the slope at the start of the series.
#' @param level_sigma,slope_sigma STS level and slope innovation SD priors (> 0).
#' @param slope_phi STS slope persistence prior in (-1, 1).
#' @param slope_mean STS long-run slope prior (unbounded).
#'
#' @returns An `epidemic_process_class` object, usable as the `epidemic`
#'   argument of [model()].
#'
#' @references
#' Hyndman, R.J., Koehler, A.B., Ord, J.K. and Snyder, R.D. (2008)
#' *Forecasting with Exponential Smoothing: The State Space Approach*. Springer.
#'
#' Hyndman, R.J. and Billah, B. (2003) Unmasking the Theta method.
#' *International Journal of Forecasting* 19(2), 287-290.
#'
#' Monahan, J.F. (1984) A note on enforcing stationarity in autoregressive-moving
#' average models. *Biometrika* 71(2), 403-404.
#'
#' @section Long series:
#' These trends carry one latent innovation per event-time (two for
#' `sts_epidemic()` with a slope), so the Laplace approximation's Hessian grows
#' with the series.  On the package's 1,095-week dengue series they converge far
#' less reliably than on shorter ones -- `ets_epidemic()` passed
#' [fit_check()] on 15% of those fits against 100% on every other dataset
#' tested, and `ar1_epidemic()` shows the same pattern more mildly.  Only
#' [hsgp_epidemic()], whose basis is a fixed 20-or-so coefficients rather than
#' one per time point, was unaffected.  Prefer it past roughly 500 event-times,
#' or shorten the window.
#'
#' @seealso [epidemic_process] for HSGP, AR(1) and SIR; [custom_epidemic()] for
#'   an arbitrary user-written trend.
#'
#' @examples
#' arima_epidemic()                                    # ARIMA(2, 1, 0) with drift
#' arima_epidemic(p = 1, d = 1, q = 1)
#' arima_epidemic(p = 2, d = 1, q = 0, include_drift = FALSE)
#' sts_epidemic(trend = "semilocal")
#' ets_epidemic(trend = "additive", damped = TRUE)
#' random_walk_epidemic()
#' naive_epidemic()
#' theta_epidemic()
#'
#' @name timeseries_epidemic
NULL

#' @rdname timeseries_epidemic
#' @export
arima_epidemic <- function(p = 2, d = 1, q = 0, include_drift = NA,
                           ar = numeric(0), ma = numeric(0),
                           sigma = numeric(0), drift = numeric(0)) {
  arima_epidemic_class(p = p, d = d, q = q, include_drift = include_drift,
                       ar = ar, ma = ma, sigma = sigma, drift = drift)
}

#' @rdname timeseries_epidemic
#' @export
sts_epidemic <- function(trend = "semilocal",
                         level_sigma = numeric(0), slope_sigma = numeric(0),
                         slope_phi = numeric(0), slope_mean = numeric(0),
                         slope_init = numeric(0)) {
  sts_epidemic_class(trend = trend, level_sigma = level_sigma,
                     slope_sigma = slope_sigma, slope_phi = slope_phi,
                     slope_mean = slope_mean, slope_init = slope_init)
}

#' @rdname timeseries_epidemic
#' @export
ets_epidemic <- function(trend = "additive", damped = TRUE, include_drift = FALSE,
                         sigma = numeric(0), beta = numeric(0),
                         damping = numeric(0), drift = numeric(0),
                         slope_init = numeric(0)) {
  ets_epidemic_class(trend = trend, damped = damped, include_drift = include_drift,
                     sigma = sigma, beta = beta, damping = damping,
                     drift = drift, slope_init = slope_init)
}

#' @rdname timeseries_epidemic
#' @export
random_walk_epidemic <- function(sigma = numeric(0), drift = numeric(0),
                                 include_drift = FALSE) {
  random_walk_epidemic_class(sigma = sigma, drift = drift,
                             include_drift = include_drift, name = "RW")
}

#' @rdname timeseries_epidemic
#' @export
naive_epidemic <- function(sigma = numeric(0)) {
  random_walk_epidemic_class(sigma = sigma, include_drift = FALSE, name = "Naive")
}

#' @rdname timeseries_epidemic
#' @export
theta_epidemic <- function(sigma = numeric(0), drift = numeric(0)) {
  theta_epidemic_class(sigma = sigma, drift = drift)
}
