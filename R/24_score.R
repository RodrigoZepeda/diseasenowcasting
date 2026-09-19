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
#'   objective gap, Hessian status, the headroom between the fitted latent
#'   `log_mean` and its softplus ceiling, any Laplace-precision regularization
#'   used for prediction, overall status, and diagnostic reasons.
#'
#'   Two of those columns report failures the optimizer diagnostics cannot see,
#'   so both leave `optimizer_adequate` alone and only set `fit_status` to
#'   `"warning"`:
#'
#'   * `log_mean_cap_bound` / `log_mean_cap_reportable` -- the fitted latent
#'     incidence is pressed against its softplus ceiling.  `reportable` is
#'     `FALSE` on a count-cumulative stream, where the horizon-0 nowcast is built
#'     by the cohort kernels rather than from `lambda` and a saturated cap moves
#'     the median by well under a percent.  Only the alarm is suppressed there;
#'     the fact is still in `log_mean_cap_bound`.
#'   * `arma_ridge` / `arma_ridge_correlation` -- for an ARMA with `p >= 1` and
#'     `q >= 1`, the largest absolute correlation between an AR and an MA
#'     coordinate in the Laplace covariance.  Near-collinear coefficients mean a
#'     flat ridge: the fit converges with a positive-definite Hessian and the
#'     predictive interval is still poorly determined.  It detects
#'     near-collinearity only -- an ARMA with `q >= 2` can widen its interval by
#'     overfitting instead, which leaves the correlation low, so a quiet check is
#'     not a guarantee that the interval is sound.
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
      optimizer_adequate = summary$adequate,
      log_mean_upper_bound = summary$log_mean_upper_bound,
      log_mean_upper_bound_legacy = summary$log_mean_upper_bound_legacy,
      max_log_mean = summary$max_log_mean,
      log_mean_headroom = summary$log_mean_headroom,
      log_mean_cap_bound = summary$log_mean_cap_bound,
      # FALSE on a count-cumulative fit, where a saturated cap has no
      # predictive consequence -- see `.joint_fit_diagnostic()`.
      log_mean_cap_reportable = summary$log_mean_cap_reportable %||%
        summary$log_mean_cap_bound,
      arma_ridge_correlation = summary$arma_ridge_correlation %||% NA_real_,
      arma_ridge = isTRUE(summary$arma_ridge),
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
    # The two failures are reported separately because they mean different
    # things: one says the optimiser has not arrived, the other says it arrived
    # at a ceiling.  A cap-bound fit routinely passes every optimizer test.
    capped <- applicable & out$log_mean_cap_reportable
    regularized <- !is.na(out$laplace_regularized) & out$laplace_regularized
    problematic <- applicable & (!out$optimizer_adequate | regularized)
    if (any(problematic)) {
      cli::cli_warn(c(
        "{sum(problematic)} of {nrow(out)} retained RTMB fit{?s} failed the optimizer diagnostic check.",
        "i" = "Inspect {.code fit_check(object, warn = FALSE)} and {.fn nowcast_diagnostic}."
      ))
    }
    ridged <- applicable & out$arma_ridge
    if (any(ridged)) {
      .warn_arma_ridge(out$arma_ridge_correlation[ridged], n_fits = nrow(out),
                       context = "retained RTMB fit")
    }
    if (any(capped)) {
      .warn_log_mean_cap(
        out$log_mean_headroom[capped], out$log_mean_upper_bound[capped],
        n_fits = nrow(out), context = "retained RTMB fit",
        legacy_bound = out$log_mean_upper_bound_legacy[capped]
      )
    }
  }

  out
}
