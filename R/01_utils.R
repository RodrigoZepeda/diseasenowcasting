# =============================================================================
# Internal helpers (ported from diseasenowcast2 R/1_utils.R, Stan-free)
# =============================================================================

#' Resolve a prior-or-number argument to a concrete numeric value (or NULL)
#'
#' Several constructors accept a slot that may be a `prior_class` object, a plain
#' number, or left empty.  This collapses those three cases to a single value:
#' a prior is turned into one random draw, a number is returned as-is, and
#' anything else (e.g. `numeric(0)`) yields `NULL`.
#'
#' @param object A `prior_class` object, a length >= 1 numeric, or empty.
#' @returns A numeric value (one draw from the prior, or the number itself), or
#'   `NULL` when `object` carries no usable value.
#' @noRd
#' @keywords internal
.resolve_prior <- function(object) {
  if (S7::S7_inherits(object, prior_class)) return(sample(object, 1L))
  if (is.numeric(object) && length(object) > 0) return(object)
  NULL
}

#' Weighted median (base R; matches `Hmisc::wtd.quantile(probs = 0.5)`)
#'
#' @param values  Numeric vector of observations.
#' @param weights Numeric vector of non-negative weights, same length as `values`.
#' @returns The weighted median (a single numeric), or `NA_real_` when no
#'   observation has finite value and positive weight.
#' @noRd
#' @keywords internal
.wtd_median <- function(values, weights) {
  # Keep only observations that can contribute (finite value, finite & positive weight).
  is_usable <- is.finite(values) & is.finite(weights) & weights > 0
  values    <- values[is_usable]
  weights   <- weights[is_usable]
  if (length(values) == 0) return(NA_real_)

  # Sort by value so the cumulative weight is monotone in `values`.
  order_by_value <- order(values)
  values  <- values[order_by_value]
  weights <- weights[order_by_value]

  # Cumulative weight at the *centre* of each observation's weight mass,
  # normalised to (0, 1) -- this is the standard mid-point definition Hmisc uses.
  cumulative_weight_fraction <- (cumsum(weights) - 0.5 * weights) / sum(weights)

  # The weighted median is the value at cumulative-weight fraction 0.5
  # (linearly interpolated; `rule = 2` clamps at the ends).
  stats::approx(cumulative_weight_fraction, values,
                xout = 0.5, rule = 2, ties = "ordered")$y
}

#' Weighted variance (base R; matches `Hmisc::wtd.var(normwt = FALSE)`)
#'
#' @param values  Numeric vector of observations.
#' @param weights Numeric vector of non-negative weights, same length as `values`.
#' @returns The (Bessel-corrected) weighted variance, or `NA_real_` when there is
#'   too little information (total weight <= 1 or fewer than two observations).
#' @noRd
#' @keywords internal
.wtd_var <- function(values, weights) {
  is_usable <- is.finite(values) & is.finite(weights) & weights > 0
  values    <- values[is_usable]
  weights   <- weights[is_usable]

  total_weight <- sum(weights)
  if (total_weight <= 1 || length(values) < 2) return(NA_real_)

  weighted_mean         <- sum(weights * values) / total_weight
  weighted_sum_of_sq    <- sum(weights * (values - weighted_mean)^2)
  # Bessel correction uses (total_weight - 1), matching Hmisc with normwt = FALSE.
  weighted_sum_of_sq / (total_weight - 1)
}

#' Valid prior distribution names
#' @noRd
#' @keywords internal
.valid_priors <- c("StdNormal", "Normal", "Cauchy", "StudentT",
                   "DoubleExponential", "Flat", "HalfStdNormal", "HalfNormal",
                   "HalfCauchy", "HalfStudentT", "HalfDoubleExponential",
                   "Gamma", "Weibull", "InvGamma", "LogNormal", "ChiSquare",
                   "Exponential", "Logistic", "Beta", "FlatPos")

#' Valid prior names for positive random variables
#' @noRd
#' @keywords internal
.valid_positive_priors <- c("HalfStdNormal", "HalfNormal", "HalfCauchy",
                            "HalfStudentT", "HalfDoubleExponential",
                            "Gamma", "Weibull", "InvGamma", "LogNormal",
                            "ChiSquare", "Exponential", "Logistic", "Beta",
                            "FlatPos")

#' Is this slot value valid for a strictly-positive parameter?
#'
#' A slot bound to a positive quantity (an SD, rate, amplitude, ...) may hold a
#' prior, a fixed number, or be left empty.  Each case has its own notion of
#' "valid": a prior must be one of the positive-support families, a fixed number
#' must be `> 0`, and an empty slot is always acceptable (the default is used).
#'
#' @param object A `prior_class`, a length-1 numeric, or an empty value.
#' @returns `TRUE`/`FALSE` for priors and numbers, `TRUE` for an empty slot, or
#'   `NULL` for an unrecognised input (so callers can flag it).
#' @noRd
#' @keywords internal
valid_positive_prior <- function(object) {
  if (S7::S7_inherits(object, prior_class)) {
    return(object@name %in% .valid_positive_priors)
  } else if (is.numeric(object) && length(object) == 1) {
    return(object > 0)
  } else if (length(object) < 1) {
    return(TRUE)
  }
  NULL
}

#' Valid delay process names
#' @noRd
#' @keywords internal
.valid_delays <- c("LogNormal", "GeneralizedGamma", "Gamma", "Dirichlet")

#' Pad (or trim) a numeric vector to exactly length 3 with trailing zeros
#'
#' Prior parameters are stored in a fixed-width length-3 slot so the objective
#' can read them positionally regardless of how many parameters a given
#' distribution actually has.
#'
#' @param x A numeric vector of length 0-3 (longer inputs are truncated).
#' @returns A length-3 numeric vector: `x` followed by zeros.
#' @noRd
#' @keywords internal
.pad3 <- function(x) {
  x          <- as.numeric(x)
  n_missing  <- 3 - length(x)
  padded     <- c(x, rep(0, max(0, n_missing)))
  padded[1:3]
}

#' Infer the number of parameters of a custom component
#'
#' `custom_delay()` / `custom_epidemic()` do not ask the user for `n_params`; it
#' is read off whichever of `priors`, `param_names`, or `inits` they supplied.
#' Any that are given must agree on the count.
#'
#' @param priors,param_names,inits The (possibly empty/NULL) component arguments.
#' @returns A single integer: the inferred number of parameters.
#' @noRd
#' @keywords internal
.infer_n_params <- function(priors = list(), param_names = NULL, inits = NULL) {
  candidates <- c(priors = length(priors), param_names = length(param_names),
                  inits = length(inits))
  candidates <- candidates[candidates > 0L]
  if (length(candidates) == 0L)
    cli::cli_abort(c("Cannot infer the number of parameters.",
                     "i" = "Supply a non-empty {.arg priors}, {.arg param_names}, or {.arg inits}."))
  if (length(unique(candidates)) > 1L)
    cli::cli_abort(c("{.arg priors}, {.arg param_names} and {.arg inits} imply different parameter counts.",
                     "i" = "Lengths given: {.val {candidates}}; they must agree."))
  as.integer(candidates[[1L]])
}

#' Require RTMB to be on the search path before taping user-supplied functions
#'
#' A user's `intensity_fn` / `cdf_factory` lives in the global environment, so
#' its arithmetic (`+`, `*`, `exp`, `abs`, `cumsum`, …) only dispatches to
#' RTMB's automatic-differentiation methods when the RTMB package is *attached*
#' (on the search path), not merely loaded.  The built-in delay / epidemic
#' models work without this because their math lives inside this package's
#' namespace (which imports RTMB); only user-supplied functions need RTMB
#' attached.  `diseasenowcasting` keeps RTMB in `Imports` (not `Depends`) to
#' avoid masking base functions like `dnorm`/`pnorm` for users who never write a
#' custom component, so those users must run `library(RTMB)` themselves.  This
#' guard turns the otherwise-cryptic "unimplemented complex function" /
#' "lost class attribute" error into an actionable message.
#'
#' @param what Short label for the feature needing RTMB (used in the message).
#' @returns `TRUE` invisibly if RTMB is attached; otherwise aborts.
#' @noRd
#' @keywords internal
.assert_rtmb_attached <- function(what = "custom delays / processes") {
  if (!"RTMB" %in% .packages()) {
    cli::cli_abort(c(
      "RTMB must be attached to use {what}.",
      "i" = "Run {.code library(RTMB)} first (it is needed only for user-written functions).",
      "x" = "Without it, the arithmetic inside your function cannot be auto-differentiated."
    ))
  }
  invisible(TRUE)
}

#' Class union for parameter slots (a number or a prior)
#' @noRd
#' @keywords internal
.valid_param_slot <- S7::new_union(S7::class_numeric, prior_class)

#' GP kernel name -> integer code
#' @noRd
#' @keywords internal
.gp_kernel_map <- c(
  sq_exp   = 1L, squared_exponential = 1L, SquaredExp = 1L, SqExponential = 1L,
  matern32 = 2L, matern_3_2 = 2L,          Matern3_2  = 2L, Matern_32     = 2L,
  matern52 = 3L, matern_5_2 = 3L,          Matern5_2  = 3L, Matern_52     = 3L
)

#' @noRd
#' @keywords internal
.valid_gp_kernels <- c("1" = "SquaredExp", "2" = "Matern32", "3" = "Matern52")

#' Parse a GP kernel word to its integer code
#' @noRd
#' @keywords internal
.parse_gp_kernel <- function(kernel) {
  key <- tolower(trimws(kernel))
  if (!key %in% names(.gp_kernel_map))
    cli::cli_abort("gp_kernel must be one of: 'sq_exp', 'matern32', 'matern52'. Got: '{kernel}'")
  .gp_kernel_map[[key]]
}

#' HSGP Laplacian eigenbasis (boundary condition) map
#' @noRd
#' @keywords internal
.gp_basis_map <- c(
  dirichlet = 1L, sine = 1L, sin = 1L,
  neumann   = 2L, cosine = 2L, cos = 2L
)

#' Parse a GP basis word/number to its integer code (1 = Dirichlet, 2 = Neumann)
#' @noRd
#' @keywords internal
.parse_gp_basis <- function(basis) {
  # A numeric argument is already the integer code; validate and return it.
  if (is.numeric(basis)) {
    code <- as.integer(basis)
    if (!code %in% c(1L, 2L))
      cli::cli_abort("Numeric gp_basis must be 1 (Dirichlet/sine) or 2 (Neumann/cosine). Got: {basis}")
    return(code)
  }
  # Otherwise look up the word (case/space-insensitive) in the name -> code map.
  key <- tolower(trimws(basis))
  if (!key %in% names(.gp_basis_map))
    cli::cli_abort("gp_basis must be one of: 'dirichlet'/'sine' or 'neumann'/'cosine'. Got: '{basis}'")
  .gp_basis_map[[key]]
}

#' Valid epidemic process names
#' @noRd
#' @keywords internal
.valid_epidemic_processes <- c("HSGP", "AR1", "SIR", "ARIMA", "STS", "ETS",
                               "Theta", "RW", "Naive")

#' Which delay parameters to hard-fix for a given delay family
#'
#' The two-stage path pins the delay distribution by fixing its parameters; this
#' returns the parameter keys to fix for each parametric family.
#'
#' @param delay_id Integer delay-family code: 1 = LogNormal, 2 = Gamma,
#'   3 = Generalized-Gamma (4 = Dirichlet is non-parametric and handled
#'   separately, so it returns no keys).
#' @returns A character vector of parameter-name keys (empty for unknown/Dirichlet).
#' @noRd
#' @keywords internal
.delay_fix_keys <- function(delay_id) {
  switch(as.character(delay_id),
         `1` = c("delay_mu", "delay_sigma"),                      # LogNormal
         `2` = c("delay_mu_gamma", "delay_sigma"),                # Gamma
         `3` = c("delay_mu", "delay_Q", "delay_sigma_gengamma"),  # Generalized-Gamma
         character(0))
}

# =============================================================================
# Holding a parameter at a supplied value
# =============================================================================
# A number in a parameter slot -- `nb_likelihood(phi = 5)`, `ar1_epidemic(phi =
# 0.9)` -- means "hold this here".  The delay side has always honoured that by
# seeding the parameter and mapping it out of the optimisation; these helpers
# let the epidemic and likelihood parameters do the same thing.
#
# The work is all in the transform.  Every one of these parameters is optimised
# on an UNCONSTRAINED scale, so a user-facing value has to be pushed back
# through the constraint map before it can be used as a seed, and a value
# outside the map's domain has to be reported as such rather than turned into a
# silent NaN at the first gradient evaluation.

#' Push a fixed value back through a constraint map
#'
#' @param value The natural-scale value the user supplied.
#' @param argument The argument name, for the error message.
#' @param domain Human-readable description of the admissible set.
#' @param inside Predicate: is the value in the domain?
#' @param transform The map to the unconstrained scale.
#' @keywords internal
#' @noRd
.unconstrain_fixed <- function(value, argument, domain, inside, transform) {
  value <- as.numeric(value)
  if (!length(value) || anyNA(value) || any(!is.finite(value)) || !all(inside(value)))
    cli::cli_abort(c(
      "{.arg {argument}} was fixed at {.val {value}}, which is outside {domain}.",
      "i" = "A fixed value is held exactly as given, so it has to be a value the model can represent."
    ), class = "diseasenowcasting_invalid_fixed_value")
  transform(value)
}

#' @keywords internal
#' @noRd
.unconstrain_positive <- function(value, argument)
  .unconstrain_fixed(value, argument, "the positive line", function(v) v > 0, log)

#' @keywords internal
#' @noRd
.unconstrain_unit <- function(value, argument)
  .unconstrain_fixed(value, argument, "the open interval (0, 1)",
                     function(v) v > 0 & v < 1, stats::qlogis)

#' Inverse of `-0.999 + 1.998 * plogis(x)`, the map AR(1)'s `phi` uses.
#' @keywords internal
#' @noRd
.unconstrain_signed_unit <- function(value, argument)
  .unconstrain_fixed(value, argument, "the open interval (-0.999, 0.999)",
                     function(v) v > -0.999 & v < 0.999,
                     function(v) stats::qlogis((v + 0.999) / 1.998))

#' Inverse of `upper * plogis(x)`, the map the innovation SDs use.
#' @keywords internal
#' @noRd
.unconstrain_bounded_positive <- function(upper) function(value, argument)
  .unconstrain_fixed(value, argument, paste0("the open interval (0, ", format(upper), ")"),
                     function(v) v > 0 & v < upper,
                     function(v) stats::qlogis(v / upper))

#' Inverse of `lower + (upper - lower) * plogis(x)`.
#' @keywords internal
#' @noRd
.unconstrain_bounded_interval <- function(lower, upper) function(value, argument)
  .unconstrain_fixed(value, argument,
                     paste0("the open interval (", format(lower), ", ", format(upper), ")"),
                     function(v) v > lower & v < upper,
                     function(v) stats::qlogis((v - lower) / (upper - lower)))

#' @keywords internal
#' @noRd
.unconstrain_identity <- function(value, argument)
  .unconstrain_fixed(value, argument, "the real line", function(v) TRUE, identity)

#' Resolve one parameter slot into "free" or "held at this seed"
#'
#' Returns the seed on the UNCONSTRAINED scale, recycled to the parameter's
#' length, so the caller can both initialise the parameter there and map it out
#' of the optimisation.  `active = FALSE` (the parameter does not exist in this
#' model) always yields free, so a slot belonging to a component the user did not
#' choose is simply not consulted.
#' @keywords internal
#' @noRd
.resolve_fixed_parameter <- function(prior_entry, argument, transform,
                                     length_out = 1L, active = TRUE) {
  if (!isTRUE(active) || is.null(prior_entry) ||
      !isTRUE(prior_entry$is_constant == 1L))
    return(list(is_fixed = FALSE, seed = NULL))
  supplied <- as.numeric(prior_entry$fixed)
  if (length(supplied) != 1L && length(supplied) != length_out)
    cli::cli_abort(c(
      "{.arg {argument}} was fixed at {length(supplied)} values, but this model has {length_out} {cli::qty(length_out)}{?stratum/strata}.",
      "i" = if (length_out == 1L) "Supply a single value."
            else "Supply one value to share across strata, or {length_out} -- one per stratum."
    ), class = "diseasenowcasting_invalid_fixed_value")
  list(is_fixed = TRUE,
       seed = rep_len(transform(supplied, argument), length_out))
}

#' Put pinned parameters back into a parameter list
#'
#' Reconstruction runs from two different sources.  `obj$env$parList()` carries
#' every parameter, pinned ones included.  A posterior draw does not: a pinned
#' parameter is not in the Laplace precision, so `.split_named_vector()` has no
#' entry for it and everything downstream reads `NULL`.  Rather than teach each
#' of the nine read sites to check, fill the values in once, on the unconstrained
#' scale the reconstruction expects.  Idempotent: where the list already has the
#' parameter the value is the same one.
#'
#' A pinned parameter has no posterior width, which is the point -- the draws
#' vary everything else around it.
#' @keywords internal
#' @noRd
.fill_fixed_parameters <- function(parlist, data, priors) {
  n_strata <- as.integer(data$num_strata %||% 1L)
  epidemic_model <- as.integer(data$epidemic_model)
  uses_ar_trend <- epidemic_model == 2L ||
    (epidemic_model == 3L && isTRUE(data$use_beta_rw_trend == 1L))
  # A hierarchical fit has no `mu_intercept` to pin (it has mu_global + delta),
  # and build_joint_obj() refuses that combination up front.
  is_hierarchical <- !is.null(parlist$mu_global)

  fill <- function(name, entry, argument, transform, length_out, active) {
    if (!active) return(invisible(NULL))
    resolved <- .resolve_fixed_parameter(entry, argument, transform, length_out, TRUE)
    if (resolved$is_fixed) parlist[[name]] <<- resolved$seed
  }
  fill("mu_intercept", priors$mu_intercept, "mu", .unconstrain_identity, n_strata,
       epidemic_model %in% c(1L, 2L, 5L, 6L, 7L) && !is_hierarchical)
  fill("log_phi_nb", priors$phi_nb, "phi", .unconstrain_positive, 1L,
       isTRUE(data$is_negative_binomial == 1L))
  fill("log_gp_alpha", priors$gp_alpha, "alpha", .unconstrain_positive, 1L,
       epidemic_model == 1L)
  fill("log_gp_ell", priors$gp_ell, "ell", .unconstrain_positive, 1L,
       epidemic_model == 1L)
  fill("ar_phi_unc", priors$ar_phi, "phi", .unconstrain_signed_unit, n_strata,
       uses_ar_trend)
  fill("log_ar_sigma_unc", priors$ar_sigma, "sigma",
       .unconstrain_bounded_positive(data$ar_sigma_max), n_strata, uses_ar_trend)
  fill("log_R0", priors$R0, "R0", .unconstrain_positive, n_strata, epidemic_model == 3L)
  fill("u_gamma", priors$gamma_sir, "gamma", .unconstrain_unit, n_strata,
       epidemic_model == 3L)
  fill("u_neff", priors$N_eff, "N_eff", .unconstrain_unit, n_strata,
       epidemic_model == 3L)

  bounded_sigma <- .unconstrain_bounded_positive(data$ar_sigma_max)
  arima_p <- as.integer(data$arima_p %||% 0L)
  arima_q <- as.integer(data$arima_q %||% 0L)
  is_arima <- epidemic_model == 5L
  is_ets <- epidemic_model == 6L
  is_sts <- epidemic_model == 7L
  ets_has_slope <- is_ets && isTRUE(data$ets_has_slope == 1L)
  sts_has_slope <- is_sts && isTRUE(data$sts_has_slope == 1L)
  sts_reverting <- sts_has_slope && isTRUE(data$sts_reverting_slope == 1L)
  # The AR/MA blocks are [order x strata]; a pinned lag profile is shared, so it
  # tiles across the columns the same way the tape seeds it.
  fill_block <- function(name, entry, argument, transform, order, active) {
    if (!active || order < 1L) return(invisible(NULL))
    resolved <- .resolve_fixed_parameter(entry, argument, transform, order, TRUE)
    if (resolved$is_fixed)
      parlist[[name]] <<- matrix(resolved$seed, order, n_strata)
  }
  fill_block("arima_ar_pacf_unc", priors$arima_ar, "ar", .unconstrain_signed_unit,
             arima_p, is_arima)
  fill_block("arima_ma_pacf_unc", priors$arima_ma, "ma", .unconstrain_signed_unit,
             arima_q, is_arima)
  fill("log_arima_sigma_unc", priors$arima_sigma, "sigma", bounded_sigma, n_strata, is_arima)
  fill("arima_drift", priors$arima_drift, "drift", .unconstrain_identity, n_strata,
       is_arima && isTRUE(data$arima_include_drift == 1L))
  fill("log_ets_sigma_unc", priors$ets_sigma, "sigma", bounded_sigma, n_strata, is_ets)
  fill("ets_beta_unc", priors$ets_beta, "beta", .unconstrain_unit, n_strata, ets_has_slope)
  fill("ets_damp_unc", priors$ets_damping, "damping", .unconstrain_unit, n_strata,
       ets_has_slope && isTRUE(data$ets_damped == 1L))
  fill("ets_drift", priors$ets_drift, "drift", .unconstrain_identity, n_strata,
       is_ets && isTRUE(data$ets_include_drift == 1L))
  fill("ets_slope_init", priors$ets_slope_init, "slope_init", .unconstrain_identity,
       n_strata, ets_has_slope)
  fill("log_sts_level_sigma_unc", priors$sts_level_sigma, "level_sigma", bounded_sigma,
       n_strata, is_sts)
  fill("log_sts_slope_sigma_unc", priors$sts_slope_sigma, "slope_sigma", bounded_sigma,
       n_strata, sts_has_slope)
  fill("sts_slope_phi_unc", priors$sts_slope_phi, "slope_phi", .unconstrain_signed_unit,
       n_strata, sts_reverting)
  fill("sts_slope_mean", priors$sts_slope_mean, "slope_mean", .unconstrain_identity,
       n_strata, sts_reverting)
  fill("sts_slope_init", priors$sts_slope_init, "slope_init", .unconstrain_identity,
       n_strata, sts_has_slope)
  parlist
}

#' Check a fixed value against the domain its constructor knows statically
#'
#' The fit-time check in `build_joint_obj()` is the backstop, but it fires deep
#' inside the optimiser's init ladder. Where the admissible set is a property of
#' the parameter rather than of the data -- an autocorrelation in (-1, 1), a
#' probability in (0, 1) -- the constructor can say so immediately, which is
#' where the user can actually see it.
#' @keywords internal
#' @noRd
.check_fixed_domain <- function(value, argument, constructor, domain, inside) {
  if (!is.numeric(value) || !length(value)) return(invisible(NULL))
  value <- as.numeric(value)
  if (anyNA(value) || any(!is.finite(value)) || !all(inside(value)))
    cli::cli_abort(c(
      "{.arg {argument}} in {.fn {constructor}} must be in {domain}, not {.val {value}}.",
      "i" = "A number in a parameter slot holds it at that value, so it has to be one the model can represent.",
      "*" = "Pass a prior instead to estimate it."
    ), class = "diseasenowcasting_invalid_fixed_value")
  # S7 validators must return NULL or a character; anything else is an error
  # about the validator rather than about the object.
  invisible(NULL)
}
