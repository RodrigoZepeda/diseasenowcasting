# =============================================================================
# sample() generic — draw from a prior or likelihood (numeric, non-AD)
# =============================================================================

#' Draw random samples from a prior or likelihood
#'
#' @param object A `prior_class`, `likelihood_class`, or base vector.
#' @param size Number of draws.
#' @param ... Passed to the likelihood method (`mu`, `phi`).
#' @returns A numeric/integer vector of length `size`.
#'
#' @examples
#' sample(normal_prior(log(7), 0.5), 10)
#' sample(gamma_prior(2, 0.1), 10)
#'
#' @name sample
#' @export
sample <- S7::new_generic("sample", "object", function(object, size, ...) {
  S7::S7_dispatch()
})

#' @name sample
#' @export
S7::method(sample, S7::new_union(S7::class_character, S7::class_numeric,
                                 S7::class_vector, S7::class_factor)) <-
  function(object, size, ...) base::sample(object, size = size, ...)

#' @name sample
#' @export
S7::method(sample, prior_class) <- function(object, size, ...) {
  p <- object@stan_params
  switch(object@name,
    "StdNormal"  = rnorm(size, 0, 1),
    "Normal"     = rnorm(size, p[1], p[2]),
    "Cauchy"     = rcauchy(size, p[1], p[2]),
    "StudentT"   = rt(size, df = p[1]) * p[3] + p[2],
    "DoubleExponential" = { u <- runif(size, -0.5, 0.5); p[1] - p[2] * sign(u) * log(1 - 2 * abs(u)) },
    "Flat"       = runif(size, -.Machine$integer.max, .Machine$integer.max),
    "HalfStdNormal" = abs(rnorm(size, 0, 1)),
    "HalfNormal"    = abs(rnorm(size, p[1], p[2])),
    "HalfCauchy"    = abs(rcauchy(size, p[1], p[2])),
    "HalfStudentT"  = abs(rt(size, df = p[1]) * p[2]),
    "HalfDoubleExponential" = { u <- runif(size, -0.5, 0.5); abs(p[1] - p[2] * sign(u) * log(1 - 2 * abs(u))) },
    "Gamma"       = rgamma(size, shape = p[1], rate = p[2]),
    "Weibull"     = rweibull(size, shape = p[1], scale = p[2]),
    "InvGamma"    = 1 / rgamma(size, shape = p[1], rate = p[2]),
    "LogNormal"   = rlnorm(size, meanlog = p[1], sdlog = p[2]),
    "ChiSquare"   = rchisq(size, df = p[1]),
    "Exponential" = rexp(size, rate = p[1]),
    "Logistic"    = rlogis(size, location = p[1], scale = p[2]),
    "Beta"        = rbeta(size, shape1 = p[1], shape2 = p[2]),
    "FlatPos"     = runif(size, 0, .Machine$integer.max),
    cli::cli_abort("sample(): no sampler registered for prior {.val {object@name}}.")
  )
}
