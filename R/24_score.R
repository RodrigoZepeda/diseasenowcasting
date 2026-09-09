# =============================================================================
# fit_check() -- RTMB-specific fit quality, separate from predictive scoring
# =============================================================================

#' Check RTMB optimizer diagnostics for a fitted nowcast
#'
#' Predictive accuracy belongs to [tbl.now::score_nowcast()] and
#' [tbl.now::nowcast_backtest()]. `fit_check()` deliberately reports only
#' diagnostics specific to the RTMB optimization performed by
#' diseasenowcasting.
#'
#' @param object A result from [nowcast()] or [auto_nowcast()]. Native
#'   diseasenowcasting operations unwrap the common result automatically.
#' @param warn If `TRUE`, warn when any retained fit fails the common optimizer
#'   adequacy predicate: finite objective and derivatives, optimizer code zero,
#'   box-constrained KKT residual, positive-definite curvature on the locally
#'   free subspace, and a quadratic objective-gap estimate no larger than
#'   `0.01`.
#'
#' @returns A data frame with one row per retained RTMB fit and columns `fit`,
#'   `rung`, `convergence`, `objective`, raw and projected gradients, quadratic
#'   objective gap, Hessian status, any Laplace-precision regularization used
#'   for prediction, overall status, and diagnostic reasons.
#' @seealso [diseasenowcasting_workflows] for the distinction between native fit
#'   diagnostics and predictive scoring; [nowcast_diagnostic()],
#'   [tbl.now::score_nowcast()], [tbl.now::nowcast_backtest()]
#' @export
fit_check <- function(object, warn = TRUE) {
  native <- .unwrap_nowcast(object, "object")

  out <- do.call(rbind, lapply(seq_along(native@fits), function(i) {
    candidate <- native@fits[[i]]
    summary <- .fit_diagnostic_summary(candidate)
    laplace_fits <- native@fit_diagnostics$laplace_sampling$fits %||% list()
    laplace <- if (length(laplace_fits) >= i) laplace_fits[[i]] else list()
    regularized <- isTRUE(laplace$applied)
    status <- if (regularized) "warning" else summary$status
    reasons <- summary$reasons
    if (regularized) {
      reasons <- c(
        reasons,
        paste0(
          "Laplace precision regularized by ", laplace$method,
          if (isTRUE((laplace$ridge %||% 0) > 0)) {
            paste0(" (ridge ", signif(laplace$ridge, 4), ")")
          } else if (isTRUE((laplace$eigenvalue_floor %||% 0) > 0)) {
            paste0(
              " (eigenvalue floor ",
              signif(laplace$eigenvalue_floor, 4), ")"
            )
          } else {
            ""
          }
        )
      )
    }
    data.frame(
      fit = i,
      rung = native@rung,
      convergence = summary$convergence,
      objective = summary$objective,
      max_gradient = summary$max_gradient,
      projected_gradient = summary$projected_gradient,
      quadratic_gap = summary$quadratic_gap,
      hessian_positive_definite = summary$hessian_positive_definite,
      hessian_status = summary$hessian_status,
      laplace_regularized = as.logical(laplace$applied %||% NA),
      laplace_regularization = as.character(laplace$method %||% "unknown"),
      laplace_ridge = as.numeric(laplace$ridge %||% NA_real_),
      laplace_eigenvalue_floor = as.numeric(
        laplace$eigenvalue_floor %||% NA_real_
      ),
      fit_status = status,
      gradient_status = as.character(candidate$gradient_status %||% "unknown"),
      reasons = paste(reasons, collapse = "; "),
      stringsAsFactors = FALSE
    )
  }))

  if (isTRUE(warn)) {
    applicable <- out$rung != "prior"
    problematic <- applicable & out$fit_status != "pass"
    if (any(problematic)) {
      cli::cli_warn(c(
        "{sum(problematic)} of {nrow(out)} retained RTMB fit{?s} failed the optimizer diagnostic check.",
        "i" = "Inspect {.code fit_check(object, warn = FALSE)} and {.fn nowcast_diagnostic}."
      ))
    }
  }

  out
}
