# =============================================================================
# Prior log-density evaluator (AD-friendly)
# =============================================================================
# Direct port of src/stan/continuous_lpdf.stan's dist-code dispatcher.  All
# densities are written with elementary operations (log, exp, lgamma, pnorm,
# atan) so they differentiate correctly under RTMB / CppAD.  Normalising
# constants are kept (they are constant w.r.t. the parameter, so the MAP and
# gradient match Stan's `_lupdf` forms exactly).
#
# `value` may be an AD scalar/vector (a parameter); `params` is the data
# hyperparameter vector (length up to 3).  Returns the SUMMED log-density over
# all elements of `value` (every element shares the same prior, matching the
# Stan vectorised form).
# =============================================================================

#' Evaluate a prior log-density by distribution code
#'
#' @param value Numeric/AD value or vector (the parameter).
#' @param num_id Integer distribution code (see [priors]).
#' @param params Length-3 numeric hyperparameter vector (trailing zeros ignored).
#' @returns Summed log-density (AD scalar).
#' @keywords internal
#' @noRd
prior_lpdf <- function(value, num_id, params) {
  if (length(value) == 0) return(0)
  log_two_pi <- log(2 * pi)

  # Standard log CCDF at 0 for a truncation normaliser (AD-safe via pnorm).
  log_upper_tail_at_zero <- function(location, scale)
    pnorm(0, location, scale, lower.tail = FALSE, log.p = TRUE)

  log_density <- switch(
    as.character(num_id),

    # -- (-inf, inf) ------------------------------------------------------
    "0"  = sum(dnorm(value, 0, 1, log = TRUE)),
    "1"  = sum(dnorm(value, params[1], params[2], log = TRUE)),
    "2"  = sum(-log(pi * params[2]) - log1p(((value - params[1]) / params[2])^2)),       # Cauchy
    "3"  = {                                                                              # StudentT(df, loc, scale)
      degrees_freedom <- params[1]; location <- params[2]; scale <- params[3]
      standardised <- (value - location) / scale
      sum(lgamma((degrees_freedom + 1) / 2) - lgamma(degrees_freedom / 2) -
          0.5 * log(degrees_freedom * pi) - log(scale) -
          ((degrees_freedom + 1) / 2) * log1p(standardised^2 / degrees_freedom))
    },
    "4"  = sum(-log(2 * params[2]) - abs(value - params[1]) / params[2]),                 # DoubleExponential
    "5"  = 0,                                                                             # Flat

    # -- (0, inf): positive families (start at 100) -----------------------
    "100" = sum(dnorm(value, 0, 1, log = TRUE)) - length(value) * log(0.5),              # HalfStdNormal
    "101" = sum(dnorm(value, params[1], params[2], log = TRUE) -
                log_upper_tail_at_zero(params[1], params[2])),                            # HalfNormal
    "102" = {                                                                             # HalfCauchy
      log_density_unnorm  <- -log(pi * params[2]) - log1p(((value - params[1]) / params[2])^2)
      log_upper_tail_zero <- log(0.5 - atan(-params[1] / params[2]) / pi)
      sum(log_density_unnorm - log_upper_tail_zero)
    },
    "103" = {                                                                             # HalfStudentT(df, scale), loc 0
      degrees_freedom <- params[1]; scale <- params[2]; standardised <- value / scale
      log_density_unnorm <- lgamma((degrees_freedom + 1) / 2) - lgamma(degrees_freedom / 2) -
        0.5 * log(degrees_freedom * pi) - log(scale) -
        ((degrees_freedom + 1) / 2) * log1p(standardised^2 / degrees_freedom)
      sum(log_density_unnorm) - length(value) * log(0.5)                                  # symmetric t: CCDF(0) = 0.5
    },
    "104" = {                                                                             # HalfDoubleExponential
      log_density_unnorm  <- -log(2 * params[2]) - abs(value - params[1]) / params[2]
      log_upper_tail_zero <- if (params[1] >= 0) log(0.5) - params[1] / params[2]
                             else log1p(-0.5 * exp(params[1] / params[2]))
      sum(log_density_unnorm - log_upper_tail_zero)
    },
    "105" = sum(dgamma(value, shape = params[1], rate = params[2], log = TRUE)),          # Gamma
    "107" = sum(dweibull(value, shape = params[1], scale = params[2], log = TRUE)),       # Weibull
    "108" = {                                                                             # InvGamma(shape, scale)
      shape <- params[1]; scale <- params[2]
      sum(shape * log(scale) - lgamma(shape) - (shape + 1) * log(value) - scale / value)
    },
    "109" = sum(-log(value) - log(params[2]) - 0.5 * log_two_pi -
                (log(value) - params[1])^2 / (2 * params[2]^2)),                          # LogNormal
    "110" = sum(dchisq(value, df = params[1], log = TRUE)),                               # ChiSquare
    "111" = sum(dexp(value, rate = params[1], log = TRUE)),                               # Exponential
    "112" = sum(-(value - params[1]) / params[2] - log(params[2]) -                       # Logistic
                2 * log1p(exp(-(value - params[1]) / params[2]))),
    "113" = sum(dbeta(value, params[1], params[2], log = TRUE)),                          # Beta
    cli::cli_abort("Unknown prior distribution code num_id = {num_id}.")
  )
  log_density
}

#' Dirichlet log-density (AD-friendly), used for the non-parametric delay simplex.
#' @param probs simplex (sums to 1).
#' @param concentration concentration parameter vector (alpha).
#' @keywords internal
#' @noRd
dirichlet_lpdf <- function(probs, concentration) {
  sum((concentration - 1) * log(probs)) + lgamma(sum(concentration)) - sum(lgamma(concentration))
}
