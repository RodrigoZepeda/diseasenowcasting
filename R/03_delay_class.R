# =============================================================================
# Delay process classes
# =============================================================================

#' Delay process base class
#' @keywords internal
#' @noRd
delay_process_class <- S7::new_class(
  "delay_process_class",
  properties = list(
    name                = S7::class_character,
    num_id              = S7::class_numeric,   # delay family code
    num_delay_seasons   = S7::class_numeric,   # 1 = no seasonality
    season_distribution = prior_class
  ),
  constructor = function(name   = character(0),
                         num_id = numeric(0),
                         num_delay_seasons = 1,
                         season_distribution = std_normal_prior()) {
    S7::new_object(S7::S7_object(),
                   name = name, num_id = num_id,
                   num_delay_seasons = as.integer(num_delay_seasons),
                   season_distribution = season_distribution)
  },
  validator = function(self) {
    if (length(self@num_id) != 1) {
      cli::cli_abort("Numeric id `num_id` must be of length 1.")
    }
    if (self@num_id != 5L && !(self@name %in% .valid_delays)) {
      cli::cli_abort("Invalid delay `name`: {self@name}. Supported: {.val {(.valid_delays)}}")
    }
    if (self@num_delay_seasons < 0) {
      cli::cli_abort("`num_delay_seasons` should be a positive integer. Got: {self@num_delay_seasons}.")
    }
  }
)

#' @keywords internal
#' @noRd
lognormal_delay_class <- S7::new_class(
  "lognormal_delay_class",
  parent = delay_process_class,
  properties = list(mu = .valid_param_slot, sigma = .valid_param_slot),
  constructor = function(mu = numeric(0), sigma = numeric(0),
                         num_delay_seasons = 1L,
                         season_distribution = std_normal_prior()) {
    S7::new_object(S7::S7_object(),
                   name = "LogNormal", num_id = 1L, mu = mu, sigma = sigma,
                   num_delay_seasons = as.integer(num_delay_seasons),
                   season_distribution = season_distribution)
  },
  validator = function(self) {
    if (!valid_positive_prior(self@sigma))
      cli::cli_abort("Invalid `sigma`. Should be positive number or positive prior.")
  }
)

#' @keywords internal
#' @noRd
gamma_delay_class <- S7::new_class(
  "gamma_delay_class",
  parent = delay_process_class,
  properties = list(shape = .valid_param_slot, rate = .valid_param_slot),
  constructor = function(shape = numeric(0), rate = numeric(0),
                         num_delay_seasons = 1L,
                         season_distribution = std_normal_prior()) {
    S7::new_object(S7::S7_object(),
                   name = "Gamma", num_id = 2L, shape = shape, rate = rate,
                   num_delay_seasons = as.integer(num_delay_seasons),
                   season_distribution = season_distribution)
  },
  validator = function(self) {
    if (!valid_positive_prior(self@shape))
      cli::cli_abort("Invalid `shape`. Should be positive number or positive prior.")
    if (!valid_positive_prior(self@rate))
      cli::cli_abort("Invalid `rate`. Should be positive number or positive prior.")
  }
)

#' @keywords internal
#' @noRd
generalized_gamma_delay_class <- S7::new_class(
  "generalized_gamma_delay_class",
  parent = delay_process_class,
  properties = list(mu = .valid_param_slot, sigma = .valid_param_slot, Q = .valid_param_slot),
  constructor = function(mu = numeric(0), sigma = numeric(0), Q = numeric(0),
                         num_delay_seasons = 1L,
                         season_distribution = std_normal_prior()) {
    S7::new_object(S7::S7_object(),
                   name = "GeneralizedGamma", num_id = 3L,
                   mu = mu, sigma = sigma, Q = Q,
                   num_delay_seasons = as.integer(num_delay_seasons),
                   season_distribution = season_distribution)
  },
  validator = function(self) {
    if (!valid_positive_prior(self@sigma))
      cli::cli_abort("Invalid `sigma`. Should be positive number or positive prior.")
  }
)

#' @keywords internal
#' @noRd
dirichlet_delay_class <- S7::new_class(
  "dirichlet_delay_class",
  parent = delay_process_class,
  properties = list(alpha = .valid_param_slot, bins = S7::class_numeric),
  constructor = function(alpha = numeric(0), bins = NULL) {
    S7::new_object(S7::S7_object(),
                   name = "Dirichlet", num_id = 4L,
                   alpha = alpha, bins = as.integer(bins),
                   num_delay_seasons = 1,
                   season_distribution = std_normal_prior())
  }
)

#' Delay distribution for the Bayesian Nowcast
#'
#' Specify the reporting-delay distribution.  Parameter slots accept a fixed
#' numeric, a `prior_class` (e.g. [normal_prior()]), or `numeric(0)` for the
#' default prior.
#'
#' @param mu    Log-mean intercept (`delay_mu`).
#' @param sigma Log-scale / SD parameter > 0.
#' @param shape,rate Gamma delay parameters (the `shape` slot is the log-mean,
#'   the `rate` slot the delay SD; see the original parameterisation).
#' @param Q     GenGamma shape (`delay_Q`); `Q = 0` recovers lognormal.
#' @param alpha Dirichlet concentration (scalar broadcast to all bins).
#' @param bins  Dirichlet: number of explicit delay bins (geometric tail beyond).
#' @param num_delay_seasons Number of periodic delay seasons. Default 1.
#' @param season_distribution Prior for the delay-season effects.
#'
#' @returns A `delay_process_class` object.
#'
#' @section Default priors:
#' When a prior argument is left empty, [default_priors()] supplies these
#' defaults.  The delay *mean* prior is **data-informed**: a `normal_prior()` on
#' the log scale, centred at the log of the median observed delay.
#' \itemize{
#'   \item **LogNormal**: `mu` ~ data-informed `normal_prior(log median delay)`;
#'         `sigma` ~ `gamma_prior(2, 2)`.
#'   \item **Gamma**: `shape` ~ data-informed `normal_prior(...)`;
#'         `rate` ~ `gamma_prior(2, 2 / sd)` (data-informed SD).
#'   \item **Generalised Gamma**: `mu` ~ data-informed `normal_prior(...)`;
#'         `sigma` ~ `gamma_prior(2, 0.1)`; `Q` (shape) ~ `normal_prior(0, 0.5)`.
#'   \item **Dirichlet**: `alpha` ~ a per-bin concentration vector, data-informed
#'         from the empirical delay pmf (`0.05 + (bins+1) * pmf`), else
#'         `rep(1, bins + 1)`.
#' }
#'
#' @examples
#' lognormal_delay()
#' lognormal_delay(mu = normal_prior(log(7), 0.5))
#' gamma_delay()
#' generalized_gamma_delay(Q = 1)
#' dirichlet_delay(alpha = 1, bins = 21)
#'
#' @name delay_process
NULL

#' @rdname delay_process
#' @export
lognormal_delay <- function(mu = numeric(0), sigma = numeric(0),
                            num_delay_seasons = 1L,
                            season_distribution = std_normal_prior()) {
  lognormal_delay_class(mu = mu, sigma = sigma,
                        num_delay_seasons = num_delay_seasons,
                        season_distribution = season_distribution)
}

#' @rdname delay_process
#' @export
gamma_delay <- function(shape = numeric(0), rate = numeric(0),
                        num_delay_seasons = 1L,
                        season_distribution = std_normal_prior()) {
  gamma_delay_class(shape = shape, rate = rate,
                    num_delay_seasons = num_delay_seasons)
}

#' @rdname delay_process
#' @export
generalized_gamma_delay <- function(mu = numeric(0), sigma = numeric(0), Q = numeric(0),
                                    num_delay_seasons = 1L,
                                    season_distribution = std_normal_prior()) {
  generalized_gamma_delay_class(mu = mu, sigma = sigma, Q = Q,
                                num_delay_seasons = num_delay_seasons,
                                season_distribution = season_distribution)
}

#' @rdname delay_process
#' @export
dirichlet_delay <- function(alpha = numeric(0), bins = numeric(0)) {
  dirichlet_delay_class(alpha = alpha, bins = bins)
}

# =============================================================================
# Custom delay (family 5) — user-supplied CDF factory
# =============================================================================

#' @keywords internal
#' @noRd
custom_delay_class <- S7::new_class(
  "custom_delay_class",
  parent = delay_process_class,
  properties = list(
    cdf_factory  = S7::class_function,
    n_params     = S7::class_numeric,
    priors       = S7::class_list,
    param_names  = S7::class_character,
    inits        = S7::class_numeric
  ),
  constructor = function(cdf_factory, n_params, priors = list(),
                         name = "Custom", param_names = character(0),
                         inits = numeric(0),
                         num_delay_seasons = 1L,
                         season_distribution = std_normal_prior()) {
    n_params <- as.integer(n_params)
    if (length(priors) == 0) priors <- replicate(n_params, std_normal_prior(), simplify = FALSE)
    if (length(param_names) == 0) param_names <- paste0("param_", seq_len(n_params))
    if (length(inits) == 0) inits <- rep(0.0, n_params)
    S7::new_object(S7::S7_object(),
                   cdf_factory = cdf_factory, n_params = n_params,
                   priors = priors, param_names = param_names, inits = inits,
                   name = name, num_id = 5L,
                   num_delay_seasons = as.integer(num_delay_seasons),
                   season_distribution = season_distribution)
  },
  validator = function(self) {
    if (self@n_params < 1L)
      cli::cli_abort("`n_params` must be >= 1.")
    if (length(self@priors) != self@n_params)
      cli::cli_abort("`priors` must have exactly {self@n_params} element(s), one per parameter.")
    if (length(self@param_names) != self@n_params)
      cli::cli_abort("`param_names` must have exactly {self@n_params} element(s).")
    if (length(self@inits) != self@n_params)
      cli::cli_abort("`inits` must have exactly {self@n_params} element(s).")
    for (i in seq_len(self@n_params)) {
      p <- self@priors[[i]]
      if (!S7::S7_inherits(p, prior_class) && !(is.numeric(p) && length(p) == 1))
        cli::cli_abort("Each element of `priors` must be a `prior_class` (free parameter) or a length-1 numeric (fixed value). Element {i} is neither.")
    }
  }
)

#' User-defined delay distribution
#' 
#' `r lifecycle::badge('experimental')`
#' 
#' Allows any reporting-delay distribution that can be expressed with
#' RTMB-traceable functions.  You supply the cumulative distribution function
#' `cdf`; `log_cdf` and `log_survival` are optional and default to the obvious
#' transforms of `cdf`.  Each is given as a function of the parameter vector
#' `theta` that **returns a function of the delay** `d`:
#'
#' \preformatted{cdf(theta)          -> function(d) returning F(d)
#' log_cdf(theta)      -> function(d) returning log F(d)
#' log_survival(theta) -> function(d) returning log(1 - F(d))}
#'
#' Inside the RTMB tape `theta` is an `advector`, so every operation must be
#' AD-traceable (see the *RTMB traceability* section).
#'
#' @param cdf A function `cdf(theta)` returning a function of the delay `d` that
#'   evaluates the CDF `F(d)` (values in (0, 1)).  This is the only required piece.
#' @param log_cdf Optional function `log_cdf(theta)` returning a function of `d`
#'   for `log F(d)`.  Defaults to `function(theta) function(d) log(cdf(theta)(d))`.
#' @param log_survival Optional function `log_survival(theta)` returning a
#'   function of `d` for `log(1 - F(d))`.  Defaults to
#'   `function(theta) function(d) log(1 - cdf(theta)(d))`.  Supplying it
#'   explicitly is recommended for heavy-tailed delays, where the default
#'   `log(1 - F)` loses precision as `F -> 1` (e.g. for a Weibull,
#'   `log_survival = -(d / scale)^shape` is exact and stable).
#' @param priors A list with one element per parameter.  Each element is either a
#'   `prior_class` object (free parameter, assigned that prior) or a length-1
#'   numeric (parameter is fixed to that value and not estimated).  The number of
#'   parameters is inferred from the length of this list (or from `param_names` /
#'   `inits` if `priors` is omitted).
#' @param name A display name for the distribution (used in print output).
#' @param param_names Optional character vector naming each parameter (used in
#'   diagnostics); its length sets the number of parameters if `priors` is empty.
#' @param inits Numeric vector of initial values for the parameters (on the
#'   unconstrained scale passed to the functions); its length sets the number of
#'   parameters if `priors` and `param_names` are empty.  Defaults to zeros.
#' @param num_delay_seasons Number of periodic delay seasons.  Default 1.
#' @param season_distribution Prior for the delay-season effects.
#'
#' @returns A `custom_delay_class` object (a `delay_process_class`).
#'
#' @section RTMB traceability:
#' All operations inside `cdf` / `log_cdf` / `log_survival` must be differentiable
#' under RTMB's CppAD tape:
#' \itemize{
#'   \item Allowed: `+`, `-`, `*`, `/`, `exp`, `log`, `sqrt`, `pnorm`,
#'     `pgamma`, `lgamma`, `abs`, `sum`, scalar multiplication, fixed-length
#'     `for` loops (length must not depend on parameter values).
#'   \item Not allowed: `if`/`ifelse` on parameter values, `pmax`/`pmin` on AD
#'     types (use `(x + abs(x)) / 2` for `pmax(x, 0)`), external solvers,
#'     anything non-differentiable.
#' }
#' Use [validate_custom_delay()] to check your functions before fitting.
#'
#' @examples
#' # Weibull delay: theta = c(log_shape, log_scale).
#' # Only `cdf` is required; we also supply `log_survival` (exact in the tail).
#' weibull_cdf <- function(theta) {
#'   shape <- exp(theta[1]); scale <- exp(theta[2])
#'   function(d) 1 - exp(-(d / scale)^shape)
#' }
#' weibull_log_survival <- function(theta) {
#'   shape <- exp(theta[1]); scale <- exp(theta[2])
#'   function(d) -(d / scale)^shape
#' }
#' custom_delay(
#'   cdf          = weibull_cdf,
#'   log_survival = weibull_log_survival,
#'   priors       = list(normal_prior(0, 1), normal_prior(log(7), 1)),
#'   name         = "Weibull",
#'   param_names  = c("log_shape", "log_scale"),
#'   inits        = c(0, log(7))
#' )
#'
#' @seealso [validate_custom_delay()], [delay_process]
#' @export
custom_delay <- function(cdf,
                         log_cdf      = function(theta) function(d) log(cdf(theta)(d)),
                         log_survival = function(theta) function(d) log(1 - cdf(theta)(d)),
                         priors = list(), name = "Custom", param_names = NULL,
                         inits = NULL,
                         num_delay_seasons = 1L,
                         season_distribution = std_normal_prior()) {
  n_params <- .infer_n_params(priors, param_names, inits)
  # Assemble the internal factory the objective consumes from the three pieces:
  # each of cdf / log_cdf / log_survival is `function(theta) -> function(d)`.
  cdf_factory <- function(theta) list(
    cdf          = cdf(theta),
    log_cdf      = log_cdf(theta),
    log_survival = log_survival(theta)
  )
  # The class constructor fills any empty argument to length `n_params`.
  custom_delay_class(
    cdf_factory         = cdf_factory,
    n_params            = n_params,
    priors              = priors,
    name                = name,
    param_names         = param_names %||% character(0),
    inits               = inits %||% numeric(0),
    num_delay_seasons   = num_delay_seasons,
    season_distribution = season_distribution
  )
}

#' Validate a custom delay distribution for RTMB traceability
#' 
#' `r lifecycle::badge('experimental')`
#' 
#' Tests the user-supplied `cdf` / `log_cdf` / `log_survival` on a dummy
#' numeric vector, checking that `obj$fn()` and `obj$gr()` are finite.  Emits a
#' clear error (including common causes) on failure.
#'
#' @param delay A `custom_delay_class` object from [custom_delay()].
#' @param test_theta Optional numeric vector of length `n_params` to use as the
#'   test point.  Defaults to the `inits` slot.
#' @param test_delays Integer vector of delay values to evaluate (default 1:14).
#' @returns Invisibly returns `TRUE` on success.
#' @examples
#' # Custom components tape USER functions, so RTMB must be attached
#' # (it is kept in Imports, not Depends, so attach it yourself):
#' library(RTMB)
#' weibull_cdf <- function(theta) {
#'   shape <- exp(theta[1]); scale <- exp(theta[2])
#'   function(d) 1 - exp(-(d / scale)^shape)
#' }
#' dly <- custom_delay(weibull_cdf,
#'                     priors = list(normal_prior(0, 1), normal_prior(log(7), 1)),
#'                     name = "Weibull", inits = c(0, log(7)))
#' validate_custom_delay(dly)
#' @export
validate_custom_delay <- function(delay, test_theta = NULL, test_delays = 1:14) {
  if (!S7::S7_inherits(delay, custom_delay_class))
    cli::cli_abort("`delay` must be a `custom_delay_class` object from `custom_delay()`.")
  .assert_rtmb_attached("custom delay distributions")
  n <- delay@n_params
  theta_test <- test_theta %||% delay@inits
  if (length(theta_test) != n)
    cli::cli_abort("`test_theta` must have length {n} (= `n_params`).")

  factory <- delay@cdf_factory
  test_delays <- as.numeric(test_delays)

  result <- tryCatch({
    obj <- RTMB::MakeADFun(
      function(params) {
        RTMB::getAll(params)
        fns <- factory(cdf_validate_theta)
        sum(fns$log_cdf(test_delays)) + sum(fns$log_survival(test_delays))
      },
      list(cdf_validate_theta = theta_test),
      silent = TRUE
    )
    fn_val <- obj$fn(theta_test)
    gr_val <- obj$gr(theta_test)
    list(fn = fn_val, gr = gr_val)
  }, error = function(e) {
    cli::cli_abort(c(
      "validate_custom_delay: RTMB failed to tape `cdf_factory`.",
      "x" = conditionMessage(e),
      "i" = paste0("Common causes: `if`/`ifelse` on parameter values, `pmax`/`pmin` on AD types ",
                   "(use `(x + abs(x)) / 2`), non-differentiable operations, ",
                   "or `for` loops whose length depends on a parameter.")
    ))
  })

  if (!all(is.finite(result$fn)))
    cli::cli_abort(c("validate_custom_delay: `cdf_factory` returned non-finite log-density at the test point.",
                     "i" = "Check that `log_cdf` and `log_survival` are finite for delays {test_delays}."))
  if (!all(is.finite(result$gr)))
    cli::cli_abort(c("validate_custom_delay: `cdf_factory` returned non-finite gradient at the test point.",
                     "i" = "Check for log(0) or division by zero in your CDF expressions."))

  cli::cli_inform(c("v" = "Custom delay `{delay@name}` passes RTMB traceability check."))
  invisible(TRUE)
}
