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
    if (!(self@name %in% .valid_delays)) {
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
