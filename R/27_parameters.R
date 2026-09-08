# =============================================================================
# parameters() -- parameter estimates in tidy data-frame format
# =============================================================================
# NOT called tidy().  `tidy()` is the broom generic, and tbl.now defines a method
# on it that returns a NOWCAST (the predicted counts) rather than a parameter
# table.  Owning the generic here made a bare `tidy()` return the parameter table
# instead, which is not what a broom user expects; tbl.now's `.onLoad()` takes the
# generic over as soon as this package stops registering a method for it.
# Returns a data.frame (tibble-compatible) with one row per estimated parameter:
#   term, estimate, std.error, conf.low, conf.high, type
# Suitable for use with ggplot2, broom-style workflows, and CSV export.
# =============================================================================

#' Parameter estimates from a fitted nowcast
#'
#' Returns all estimated parameters as a long data frame with credible intervals
#' derived from the Laplace approximation posterior precision matrix.
#'
#' `tidy()` on a nowcast gives you the **nowcast** -- the predicted counts, via
#' `tbl.now`'s method for the broom generic.  This function gives you the
#' **parameters**.  They are different questions, and this package used to answer
#' the second one under the first one's name.
#'
#' @param x A `nowcast_class` object.
#' @param conf.level Credible level for the interval (default 0.95).
#' @param ... Unused.
#' @returns A `data.frame` with columns `term`, `estimate`, `std.error`,
#'   `conf.low`, `conf.high`, `type`.
#' @examples
#' if (requireNamespace("tbl.now", quietly = TRUE)) {
#'   # nc <- nowcast(data, model())
#'   # parameters(nc)
#' }
#' @export
parameters <- function(x, conf.level = 0.95, ...) UseMethod("parameters")

#' @method parameters default
#' @export
parameters.default <- function(x, conf.level = 0.95, ...) {
  cli::cli_abort("No `parameters()` method for objects of class {.cls {class(x)}}.")
}

#' @noRd
S7::method(parameters, nowcast_class) <- function(x, conf.level = 0.95, ...) {
  fit    <- x@fits[[1]]
  data   <- fit$data
  priors <- fit$priors

  # -- posterior precision from the Laplace mode ------------------------------
  # `last.par.best` is the joint mode (fixed + random effects); its names label
  # every estimated parameter, and its values are the point estimates.
  obj             <- fit$obj
  # A saved/loaded fit has no live tape: use the stored Laplace mode + precision.
  posterior_mode  <- if (is.null(obj)) fit$mode else obj$env$last.par.best
  parameter_names <- names(posterior_mode)
  estimates       <- as.numeric(posterior_mode)

  # Posterior SDs are the square roots of the diagonal of the inverse Hessian
  # (the Laplace covariance).  The Hessian can be numerically non-positive-
  # definite at a weakly-identified mode, so we retry the Cholesky factorisation
  # with a progressively larger diagonal ridge until it succeeds.
  std_errors <- tryCatch({
    hessian <- if (is.null(obj)) fit$precision else methods::as(obj$he(posterior_mode), "sparseMatrix")
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
      # diag(H^{-1}) via solving H X = I, then take the diagonal.  NOTE:
      # `Matrix::diag()`, not `diag()` -- the solve returns a sparse Matrix, and
      # base::diag() on one silently misreads it and errors out ("long vectors not
      # supported"), which used to leave EVERY standard error NA.
      inverse_hessian <- Matrix::solve(cholesky_factor, Matrix::Diagonal(n_par))
      sqrt(Matrix::diag(inverse_hessian))
    }
  }, error = function(e) {
    # Fallback for a precision matrix too large or too ill-conditioned to invert
    # exactly: estimate the marginal SDs from Laplace draws, exactly as predict()
    # samples them.  Approximate, but far better than a column of NAs.
    tryCatch({
      hessian <- if (is.null(obj)) fit$precision else methods::as(obj$he(posterior_mode), "sparseMatrix")
      draws <- .sample_mvnorm_precision(estimates, hessian, 1000L)
      apply(draws, 1, stats::sd)
    }, error = function(e2) rep(NA_real_, length(estimates)))
  })

  # Normal critical value for the requested two-sided credible level.
  critical_z <- stats::qnorm((1 + conf.level) / 2)

  # -- categorise each parameter by which model component it belongs to -------
  classify_parameter <- function(parameter_name) {
    if (grepl("^cumulative_retraction|^log_cumulative_retraction|^movement_|^log_magnitude_size$",
              parameter_name)) "count_cumulative"
    # Resolution first: `logit_confirm_p` would otherwise match the delay rule.
    else if (grepl("^logit_confirm_p|^retract_|^log_retract_|^negative_|^log_negative_",
              parameter_name)) "resolution"
    else if (grepl("^delay|^simplex|^logit", parameter_name))       "delay"
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

  # -- natural-scale rows for the constrained parameters -----------------------
  # `logit_confirm_p` is not what anyone wants to read.  Because plogis() is
  # monotone, transforming the interval ENDPOINTS gives the exact credible interval
  # for p itself (no delta method, no symmetry assumption).  One row per stratum
  # when the fit used `stratified_p`.
  # `p` is the number a practitioner reads, so give it a name that says what it
  # means for the data at hand rather than the internal `confirm_p`.
  natural_name <- switch(x@engine$resolution_label %||% "none",
                         "confirmations" = "prob_confirmed",
                         "confirmations and retractions" = "prob_confirmed",
                         "prob_not_retracted")
  out <- rbind(out, .natural_scale_rows(out, "logit_confirm_p", natural_name,
                                        stats::plogis, x@engine$strata_levels))
  out <- rbind(
    out,
    .natural_scale_rows(
      out, "cumulative_retraction_mass_raw", "retraction_mass",
      stats::plogis
    ),
    .natural_scale_rows(
      out, "log_cumulative_retraction_sigma_excess", "retraction_sigma",
      function(value) 0.01 + exp(value)
    ),
    .natural_scale_rows(
      out, "cumulative_retraction_Q", "retraction_shape_Q",
      function(value) .gengamma_shape_transform(value)$shape_Q
    ),
    .natural_scale_rows(
      out, "log_magnitude_size", "magnitude_size", exp
    )
  )

  # NB: a two-stage fit reports the epidemic + likelihood parameters only.  The
  # reporting delay is fixed during Stage 2 (imputed K times beforehand), so it
  # does not appear in `last.par.best` and therefore has no row here -- the delay
  # is summarised separately, not as a parameters() row.
  out <- out[order(out$type, out$term), ]
  rownames(out) <- NULL
  out
}

#' Back-transformed rows for a constrained parameter
#'
#' Applies a monotone `transform` to the estimate and to both interval endpoints,
#' which for a monotone map is the exact transformed credible interval.  The
#' standard error is left `NA`: on the natural scale it would only be a delta-method
#' approximation, and the interval already carries the uncertainty.  Returns a
#' zero-row frame when the parameter is absent (fixed, or not in this model).
#'
#' @param estimates The frame built from the unconstrained parameters.
#' @param unconstrained_term Name of the unconstrained parameter (may repeat, one
#'   entry per stratum).
#' @param natural_term Name to give the back-transformed rows.
#' @param transform Monotone map from the unconstrained to the natural scale.
#' @param strata_levels Stratum labels, used to name per-stratum rows.
#' @keywords internal
#' @noRd
.natural_scale_rows <- function(estimates, unconstrained_term, natural_term,
                                transform, strata_levels = NULL) {
  rows <- estimates[estimates$term == unconstrained_term, , drop = FALSE]
  if (nrow(rows) == 0L) return(estimates[0L, , drop = FALSE])
  labels <- if (nrow(rows) == 1L) natural_term
            else paste0(natural_term, "[",
                        if (length(strata_levels) == nrow(rows)) strata_levels else seq_len(nrow(rows)),
                        "]")
  data.frame(
    term      = labels,
    estimate  = transform(rows$estimate),
    std.error = NA_real_,
    conf.low  = transform(rows$conf.low),
    conf.high = transform(rows$conf.high),
    type      = rows$type,
    stringsAsFactors = FALSE)
}
