# =============================================================================
# tidy() -- parameter estimates in tidy data-frame format
# =============================================================================
# Returns a data.frame (tibble-compatible) with one row per estimated parameter:
#   term, estimate, std.error, conf.low, conf.high, type
# Suitable for use with ggplot2, broom-style workflows, and CSV export.
# =============================================================================

#' Tidy parameter estimates from a fitted nowcast
#'
#' Returns all estimated parameters as a long data frame with credible intervals
#' derived from the Laplace approximation posterior precision matrix.
#'
#' @param x A `nowcast_class` object.
#' @param conf.level Credible level for the interval (default 0.95).
#' @param ... Unused.
#' @returns A `data.frame` with columns `term`, `estimate`, `std.error`,
#'   `conf.low`, `conf.high`, `type`.
#' @export
tidy <- function(x, conf.level = 0.95, ...) UseMethod("tidy")

#' @method tidy default
#' @export
tidy.default <- function(x, conf.level = 0.95, ...) {
  cli::cli_abort("No `tidy()` method for objects of class {.cls {class(x)}}.")
}

#' @noRd
S7::method(tidy, nowcast_class) <- function(x, conf.level = 0.95, ...) {
  fit    <- x@fits[[1]]
  data   <- fit$data
  priors <- fit$priors

  # -- posterior precision from the Laplace mode ------------------------------
  # `last.par.best` is the joint mode (fixed + random effects); its names label
  # every estimated parameter, and its values are the point estimates.
  obj             <- fit$obj
  posterior_mode  <- obj$env$last.par.best
  parameter_names <- names(posterior_mode)
  estimates       <- as.numeric(posterior_mode)

  # Posterior SDs are the square roots of the diagonal of the inverse Hessian
  # (the Laplace covariance).  The Hessian can be numerically non-positive-
  # definite at a weakly-identified mode, so we retry the Cholesky factorisation
  # with a progressively larger diagonal ridge until it succeeds.
  std_errors <- tryCatch({
    hessian <- methods::as(obj$he(posterior_mode), "sparseMatrix")
    n_par   <- nrow(hessian)

    cholesky_factor <- NULL
    for (ridge_exponent in c(0, -6:-1)) {
      ridge           <- 1e-8 * 10^ridge_exponent
      cholesky_factor <- tryCatch(
        Matrix::Cholesky(hessian + Matrix::Diagonal(n_par, ridge), super = TRUE),
        error = function(e) NULL)
      if (!is.null(cholesky_factor)) break
    }

    if (is.null(cholesky_factor)) {
      rep(NA_real_, length(estimates))
    } else {
      # diag(H^{-1}) via solving H X = I, then take the diagonal.
      inverse_hessian <- Matrix::solve(cholesky_factor, Matrix::Diagonal(n_par))
      sqrt(diag(inverse_hessian))
    }
  }, error = function(e) rep(NA_real_, length(estimates)))

  # Normal critical value for the requested two-sided credible level.
  critical_z <- stats::qnorm((1 + conf.level) / 2)

  # -- categorise each parameter by which model component it belongs to -------
  classify_parameter <- function(parameter_name) {
    if (grepl("^delay|^simplex|^logit", parameter_name))            "delay"
    else if (grepl("^log_phi|^nb", parameter_name))                 "likelihood"
    else if (grepl("^log_gp|^basis_coefs|^gp", parameter_name))     "epidemic_hsgp"
    else if (grepl("^ar_|^log_ar", parameter_name))                 "epidemic_ar1"
    else if (grepl("^log_R0|^u_gamma|^u_neff", parameter_name))     "epidemic_sir"
    else if (grepl("^mu_intercept|^mu_global|^delta|^log_tau_intercept", parameter_name)) "epidemic_intercept"
    else if (grepl("^gamma", parameter_name))                       "covariate"
    else                                                            "other"
  }

  out <- data.frame(
    term       = parameter_names,
    estimate   = estimates,
    std.error  = as.numeric(std_errors),
    conf.low   = estimates - critical_z * as.numeric(std_errors),
    conf.high  = estimates + critical_z * as.numeric(std_errors),
    type       = vapply(parameter_names, classify_parameter, character(1)),
    stringsAsFactors = FALSE
  )

  # In a two-stage fit the delay is imputed K times, so the single mode reported
  # above is just the first imputation.  Replace the delay point estimates with
  # the average across imputations to reflect the pooled delay.
  has_multiple_imputations <- length(x@fits) > 1L
  has_delay_params <- any(grepl("^delay_mu$|^delay_sigma$|^delay_Q$", out$term))
  if (has_multiple_imputations && has_delay_params) {
    per_imputation_delay <- lapply(x@fits, function(fit_k) {
      list(delay_mu = fit_k$parList$delay_mu, delay_sigma = fit_k$delay_sigma)
    })
    mean_delay_mu    <- mean(vapply(per_imputation_delay,
                                    function(d) d$delay_mu %||% NA_real_, numeric(1)), na.rm = TRUE)
    mean_delay_sigma <- mean(vapply(per_imputation_delay,
                                    function(d) d$delay_sigma %||% NA_real_, numeric(1)), na.rm = TRUE)

    if ("delay_mu" %in% out$term && is.finite(mean_delay_mu))
      out$estimate[out$term == "delay_mu"] <- mean_delay_mu

    # delay_sigma is reported on the log "excess" scale, so convert the averaged
    # natural-scale SD back the same way (matching how the fit stores it).
    is_sigma_row <- grepl("delay_sigma|log_delay_sigma", out$term)
    if (any(is_sigma_row) && is.finite(mean_delay_sigma))
      out$estimate[is_sigma_row][1] <- log(max(mean_delay_sigma - 0.01, 1e-6))
  }

  out <- out[order(out$type, out$term), ]
  rownames(out) <- NULL
  out
}
