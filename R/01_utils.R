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
.valid_epidemic_processes <- c("HSGP", "AR1", "SIR")

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
