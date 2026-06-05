# =============================================================================
# sample() generic -- draw from a prior or likelihood (numeric, non-AD)
# =============================================================================

#' Draw random samples from a prior (or fall back to [base::sample()])
#'
#' A generic that draws from a `prior_class` with the appropriate random
#' number generator.  For **any other object** (numeric, character, factor,
#' `Date`, list, ...) it dispatches to [base::sample()], so `sample()` keeps its
#' usual base behaviour outside the package.
#'
#' @param object A `prior_class`, or any object accepted by [base::sample()].
#' @param size Number of draws.
#' @param ... Passed through to the underlying sampler.
#' @returns For a prior, a numeric vector of length `size`; otherwise whatever
#'   [base::sample()] returns.
#'
#' @examples
#' sample(normal_prior(log(7), 0.5), 10)
#' sample(gamma_prior(2, 0.1), 10)
#' sample(as.Date("2020-01-01") + 0:9, 3)   # falls back to base::sample()
#'
#' @name sample
#' @export
sample <- S7::new_generic("sample", "object", function(object, size, ...) {
  S7::S7_dispatch()
})

#' @name sample
#' @export
# Default for ANY other class (numeric, character, factor, Date, ...): the base
# sampler.  Registered on class_any so it is the lowest-priority fallback; the
# prior_class method below is more specific and still wins for priors.
S7::method(sample, S7::class_any) <-
  function(object, size, ...) base::sample(object, size = size, ...)

#' @name sample
#' @export
S7::method(sample, prior_class) <- function(object, size, ...) {
  # Distribution parameters, padded to length 3 (params[1] = location/shape,
  # params[2] = scale/rate, params[3] = extra, e.g. Student-t scale).
  params <- object@stan_params

  # Double-exponential (Laplace) draws use the inverse-CDF: with u ~ U(-1/2, 1/2),
  # the quantile is  location - scale * sign(u) * log(1 - 2|u|).
  laplace_draw <- function(location, scale) {
    u <- runif(size, -0.5, 0.5)
    location - scale * sign(u) * log(1 - 2 * abs(u))
  }

  switch(object@name,
    "StdNormal"  = rnorm(size, 0, 1),
    "Normal"     = rnorm(size, params[1], params[2]),
    "Cauchy"     = rcauchy(size, params[1], params[2]),
    "StudentT"   = rt(size, df = params[1]) * params[3] + params[2],
    "DoubleExponential" = laplace_draw(params[1], params[2]),
    "Flat"       = runif(size, -.Machine$integer.max, .Machine$integer.max),
    "HalfStdNormal" = abs(rnorm(size, 0, 1)),
    "HalfNormal"    = abs(rnorm(size, params[1], params[2])),
    "HalfCauchy"    = abs(rcauchy(size, params[1], params[2])),
    "HalfStudentT"  = abs(rt(size, df = params[1]) * params[2]),
    "HalfDoubleExponential" = abs(laplace_draw(params[1], params[2])),
    "Gamma"       = rgamma(size, shape = params[1], rate = params[2]),
    "Weibull"     = rweibull(size, shape = params[1], scale = params[2]),
    "InvGamma"    = 1 / rgamma(size, shape = params[1], rate = params[2]),
    "LogNormal"   = rlnorm(size, meanlog = params[1], sdlog = params[2]),
    "ChiSquare"   = rchisq(size, df = params[1]),
    "Exponential" = rexp(size, rate = params[1]),
    "Logistic"    = rlogis(size, location = params[1], scale = params[2]),
    "Beta"        = rbeta(size, shape1 = params[1], shape2 = params[2]),
    "FlatPos"     = runif(size, 0, .Machine$integer.max),
    cli::cli_abort("sample(): no sampler registered for prior {.val {object@name}}.")
  )
}
