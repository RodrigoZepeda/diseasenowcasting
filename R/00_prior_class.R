# =============================================================================
# Prior classes
# =============================================================================
# A `prior_class` carries a distribution code (`num_id`) and up to three
# parameters (`params`).  The codes match the table in [prior_lpdf()] so the same
# prior objects drive both prior construction and AD log-density evaluation
# inside the RTMB objective.
# =============================================================================

#' Prior class
#'
#' @description
#' Internal S7 class storing a prior as a distribution code plus parameters.
#'
#' @param name Human-readable distribution name.
#' @param num_id Integer distribution code (see `prior_lpdf()`).
#' @param params Length-3 numeric vector (trailing zeros for shorter families).
#'   Retained under the historical name `stan_params` for compatibility.
#'
#' @keywords internal
#' @noRd
prior_class <- S7::new_class(
  "prior_class",
  properties = list(
    name        = S7::class_character,
    num_id      = S7::class_numeric,
    stan_params = S7::class_numeric    # always <= length 3
  ),
  constructor = function(name        = character(0),
                         num_id      = numeric(0),
                         stan_params = numeric(0)) {
    S7::new_object(S7::S7_object(),
                   name        = name,
                   num_id      = num_id,
                   stan_params = stan_params)
  },
  validator = function(self) {
    if (length(self@num_id) != 1) {
      cli::cli_abort(
        "Numeric id `num_id` must be of length 1. Object of length {length(self@num_id)} was given."
      )
    }
    if (!(self@name %in% .valid_priors)) {
      cli::cli_abort(
        "Invalid prior `name` provided: {self@name}. Supported priors: {.val {(.valid_priors)}}"
      )
    }
    if (length(self@stan_params) > 3) {
      cli::cli_abort("Invalid parameter length. At most 3 parameters are allowed.")
    }
  }
)

#' Priors for model parameters
#'
#' Specify a prior distribution for any estimated parameter.  The result is a
#' `prior_class` object that can be passed to any parameter slot in
#' [poisson_likelihood()], [nb_likelihood()], the delay process constructors,
#' or the epidemic process constructors.
#'
#' @param mean,location  Location parameter.
#' @param sd,scale       Scale parameter (must be > 0).
#' @param df             Degrees of freedom (must be > 0).
#' @param rate           Rate parameter (must be > 0).
#' @param shape          Shape parameter (must be > 0).
#' @param alpha,beta     Beta distribution shape parameters (must be > 0).
#' @param meanlog        Mean on the log scale (lognormal).
#' @param sdlog          SD on the log scale (lognormal, must be > 0).
#'
#' @returns A `prior_class` object.
#'
#' @examples
#' normal_prior(0, 1)
#' gamma_prior(2, 0.1)
#' exponential_prior(1)
#' flat_prior()
#'
#' @name priors
NULL

#' @rdname priors
#' @export
std_normal_prior <- function() {
  prior_class(name = "StdNormal", num_id = 0L, stan_params = numeric(0))
}

#' @rdname priors
#' @export
normal_prior <- function(mean, sd) {
  prior_class(name = "Normal", num_id = 1L, stan_params = c(mean, sd))
}

#' @rdname priors
#' @export
cauchy_prior <- function(location, scale) {
  prior_class(name = "Cauchy", num_id = 2L, stan_params = c(location, scale))
}

#' @rdname priors
#' @export
student_t_prior <- function(df, location, scale) {
  prior_class(name = "StudentT", num_id = 3L, stan_params = c(df, location, scale))
}

#' @rdname priors
#' @export
double_exponential_prior <- function(mean, sd) {
  prior_class(name = "DoubleExponential", num_id = 4L, stan_params = c(mean, sd))
}

#' @rdname priors
#' @export
flat_prior <- function() {
  prior_class(name = "Flat", num_id = 5L, stan_params = numeric(0))
}

#' @rdname priors
#' @export
positive_flat_prior <- function() {
  prior_class(name = "FlatPos", num_id = 5L, stan_params = numeric(0))
}

#' @rdname priors
#' @export
half_std_normal_prior <- function() {
  prior_class(name = "HalfStdNormal", num_id = 100L, stan_params = numeric(0))
}

#' @rdname priors
#' @export
half_normal_prior <- function(location, scale) {
  prior_class(name = "HalfNormal", num_id = 101L, stan_params = c(location, scale))
}

#' @rdname priors
#' @export
half_cauchy_prior <- function(location, scale) {
  prior_class(name = "HalfCauchy", num_id = 102L, stan_params = c(location, scale))
}

#' @rdname priors
#' @export
half_student_t_prior <- function(df, scale) {
  prior_class(name = "HalfStudentT", num_id = 103L, stan_params = c(df, scale))
}

#' @rdname priors
#' @export
half_double_exponential_prior <- function(mean, sd) {
  prior_class(name = "HalfDoubleExponential", num_id = 104L, stan_params = c(mean, sd))
}

#' @rdname priors
#' @export
gamma_prior <- function(shape, rate) {
  prior_class(name = "Gamma", num_id = 105L, stan_params = c(shape, rate))
}

#' @rdname priors
#' @export
weibull_prior <- function(shape, scale) {
  prior_class(name = "Weibull", num_id = 107L, stan_params = c(shape, scale))
}

#' @rdname priors
#' @export
inv_gamma_prior <- function(shape, scale) {
  prior_class(name = "InvGamma", num_id = 108L, stan_params = c(shape, scale))
}

#' @rdname priors
#' @export
lognormal_prior <- function(meanlog, sdlog) {
  prior_class(name = "LogNormal", num_id = 109L, stan_params = c(meanlog, sdlog))
}

#' @rdname priors
#' @export
chi_square_prior <- function(df) {
  prior_class(name = "ChiSquare", num_id = 110L, stan_params = df)
}

#' @rdname priors
#' @export
exponential_prior <- function(rate) {
  prior_class(name = "Exponential", num_id = 111L, stan_params = rate)
}

#' @rdname priors
#' @export
logistic_prior <- function(location, scale) {
  prior_class(name = "Logistic", num_id = 112L, stan_params = c(location, scale))
}

#' @rdname priors
#' @export
beta_prior <- function(alpha, beta) {
  prior_class(name = "Beta", num_id = 113L, stan_params = c(alpha, beta))
}
