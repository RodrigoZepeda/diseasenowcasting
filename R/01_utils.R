# =============================================================================
# Internal helpers (ported from diseasenowcast2 R/1_utils.R, Stan-free)
# =============================================================================

#' Resolve either a prior, a number, or NULL
#' @noRd
#' @keywords internal
.resolve_prior <- function(object) {
  if (S7::S7_inherits(object, prior_class)) return(sample(object, 1L))
  if (is.numeric(object) && length(object) > 0) return(object)
  NULL
}

#' Weighted median (base-R; matches Hmisc::wtd.quantile(probs = 0.5))
#' @noRd
#' @keywords internal
.wtd_median <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(NA_real_)
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- (cumsum(w) - 0.5 * w) / sum(w)
  stats::approx(cw, x, xout = 0.5, rule = 2, ties = "ordered")$y
}

#' Weighted variance (base-R; matches Hmisc::wtd.var(normwt = FALSE))
#' @noRd
#' @keywords internal
.wtd_var <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  sw <- sum(w)
  if (sw <= 1 || length(x) < 2) return(NA_real_)
  xbar <- sum(w * x) / sw
  sum(w * (x - xbar)^2) / (sw - 1)
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

#' Validate a positive prior or number
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

#' Pad a numeric vector to length 3 with trailing zeros
#' @noRd
#' @keywords internal
.pad3 <- function(x) c(as.numeric(x), rep(0, 3 - length(x)))[1:3]

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
  if (is.numeric(basis)) {
    b <- as.integer(basis)
    if (!b %in% c(1L, 2L))
      cli::cli_abort("Numeric gp_basis must be 1 (Dirichlet/sine) or 2 (Neumann/cosine). Got: {basis}")
    return(b)
  }
  key <- tolower(trimws(basis))
  if (!key %in% names(.gp_basis_map))
    cli::cli_abort("gp_basis must be one of: 'dirichlet'/'sine' or 'neumann'/'cosine'. Got: '{basis}'")
  .gp_basis_map[[key]]
}

#' Valid epidemic process names
#' @noRd
#' @keywords internal
.valid_epidemic_processes <- c("HSGP", "AR1", "SIR")

#' Family-aware delay parameter keys to hard-fix
#' @noRd
#' @keywords internal
.delay_fix_keys <- function(delay_id) {
  switch(as.character(delay_id),
         `1` = c("delay_mu", "delay_sigma"),
         `2` = c("delay_mu_gamma", "delay_sigma"),
         `3` = c("delay_mu", "delay_Q", "delay_sigma_gengamma"),
         character(0))
}
