# =============================================================================
# tidy() / model_parameters() -- tidy summaries of a fitted nowcast
# =============================================================================
# tidy() follows the cross-package nowcast contract shared by every engine that
# `tbl.now` normalises: one row per event date per stratum, with the columns
#   event_date, stratum, estimate, conf.low, conf.high, level, engine
# so downstream code can bind tables across engines without special-casing us.
#
# The generic is *re-exported from `generics`* rather than defined here.  A
# locally-defined `tidy` generic would mask `generics::tidy` after
# `library(diseasenowcasting)`, hiding every method other packages register on
# the shared generic.
#
# model_parameters() returns the old `tidy()` table: one row per estimated
# parameter, with credible intervals from the Laplace posterior precision.
# =============================================================================

#' @importFrom generics tidy
#' @export
generics::tidy

# One-shot session state for the `tidy()`-changed-meaning warning.
.tidy_warning_state <- new.env(parent = emptyenv())
.tidy_warning_state$warned <- FALSE

#' Tidy a nowcast into the cross-package nowcast table
#'
#' Returns the posterior nowcast as one row per event date per stratum, in the
#' column layout shared by every engine `tbl.now` normalises.  Called on a
#' fitted [nowcast()] it first draws the posterior predictive via `predict()`;
#' called on the result of `predict()` it summarises the draws it already holds.
#'
#' @param x A `nowcast` (from [nowcast()]) or a `nowcast_prediction` (from
#'   `predict()` on one).
#' @param probs Optional numeric probabilities in `[0, 1]`.  Each adds one
#'   column named `q<probs * 100>` (so `0.05` gives `q5`, `0.025` gives `q2.5`).
#'   Because the object keeps the draws, these quantiles are exact.
#' @param conf.level Width of the credible interval (default 0.95).
#' @param ... Passed to `predict()` when `x` is a fitted nowcast (e.g.
#'   `n_draws`, `seed`); unused otherwise.
#' @returns A [tibble::tibble()] sorted by `stratum` then `event_date`, with
#'   columns `event_date` (Date), `stratum` (character, `"all"` when the fit is
#'   unstratified), `estimate` (posterior median), `conf.low`, `conf.high`,
#'   `level` (the width the interval actually has) and `engine`, plus one
#'   column per entry of `probs`.
#' @seealso [model_parameters()] for the per-parameter table that `tidy()`
#'   used to return.
#' @name tidy.nowcast
#' @examples
#' if (requireNamespace("tbl.now", quietly = TRUE)) {
#'   # nc <- nowcast(data, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()))
#'   # tidy(nc)                                    # one row per event date x stratum
#'   # tidy(predict(nc), probs = c(0.05, 0.95))    # plus exact q5 / q95 columns
#'   # model_parameters(nc)                        # one row per estimated parameter
#' }
NULL

#' @rdname tidy.nowcast
#' @keywords internal
#' @noRd
tidy_nowcast_prediction <- function(x, probs = NULL, conf.level = 0.95, ...) {
  event_dates   <- S7::prop(x, "event_dates")
  strata_draws  <- S7::prop(x, "strata_draws")
  strata_levels <- S7::prop(x, "strata_levels")

  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      is.na(conf.level) || conf.level <= 0 || conf.level >= 1)
    cli::cli_abort("{.arg conf.level} must be a single number strictly between 0 and 1.")

  if (!is.null(probs) && length(probs) > 0L) {
    if (!is.numeric(probs) || anyNA(probs) || any(probs < 0 | probs > 1))
      cli::cli_abort("{.arg probs} must be numeric, non-missing, and within {.val {c(0, 1)}}.")
  }

  if (is.null(event_dates))
    cli::cli_abort(c(
      "This prediction carries no calendar grid, so {.fn tidy} cannot build {.field event_date}.",
      "i" = "Fit with a {.cls tbl_now} so the event dates are known."
    ))
  event_dates <- as.Date(event_dates)

  # Column-wise quantile of a [draws x event-times] matrix.
  column_quantile <- function(draws, p)
    unname(apply(draws, 2L, stats::quantile, probs = p, na.rm = TRUE, names = FALSE))

  tail_probs <- c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2)

  summarise_draws <- function(draws, stratum) {
    out <- tibble::tibble(
      event_date = event_dates,
      stratum    = as.character(stratum),
      estimate   = column_quantile(draws, 0.5),
      conf.low   = column_quantile(draws, tail_probs[1]),
      conf.high  = column_quantile(draws, tail_probs[2]),
      level      = conf.level,
      engine     = "diseasenowcasting"
    )
    if (!is.null(probs) && length(probs) > 0L) {
      quantile_columns <- lapply(probs, function(p) column_quantile(draws, p))
      names(quantile_columns) <- paste0(
        "q", format(probs * 100, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
      )
      out <- dplyr::bind_cols(out, tibble::as_tibble(quantile_columns))
    }
    out
  }

  # A stratified fit carries `strata_draws` = [draws x event-times x stratum];
  # it is NULL when unstratified.  `strata_levels` is *not* the thing to branch
  # on: unstratified fits label their single cell "all" rather than leaving it
  # NULL, so reading it alone would silently pool a stratified fit.
  out <- if (!is.null(strata_draws) && length(strata_levels) > 0L) {
    dplyr::bind_rows(lapply(seq_along(strata_levels), function(k)
      summarise_draws(strata_draws[, , k, drop = TRUE], strata_levels[k])))
  } else {
    summarise_draws(S7::prop(x, "draws"), "all")
  }

  dplyr::arrange(out, .data$stratum, .data$event_date)
}

#' @rdname tidy.nowcast
#' @keywords internal
#' @noRd
tidy_nowcast <- function(x, probs = NULL, conf.level = 0.95, ...) {
  if (!isTRUE(.tidy_warning_state$warned)) {
    .tidy_warning_state$warned <- TRUE
    cli::cli_warn(c(
      "{.fn tidy} on a fitted nowcast now returns the per-date nowcast, not the parameter table.",
      "i" = "Use {.fn model_parameters} for the parameter table (the old return value).",
      "i" = "Shown once per session."
    ))
  }
  tidy_nowcast_prediction(stats::predict(x, ...), probs = probs, conf.level = conf.level)
}

# =============================================================================
# model_parameters() -- one row per estimated parameter
# =============================================================================

#' Parameter estimates from a fitted nowcast
#'
#' Returns every estimated parameter as a long data frame with credible
#' intervals derived from the Laplace approximation posterior precision matrix.
#' This is the table [tidy()] returned before version 2.1.0.
#'
#' @param x A `nowcast` object from [nowcast()].
#' @param conf.level Credible level for the interval (default 0.95).
#' @param ... Unused.
#' @returns A `data.frame` with columns `term`, `estimate`, `std.error`,
#'   `conf.low`, `conf.high`, `type`.
#' @seealso [tidy()] for the per-date nowcast table.
#' @export
model_parameters <- function(x, conf.level = 0.95, ...) UseMethod("model_parameters")

#' @method model_parameters default
#' @export
model_parameters.default <- function(x, conf.level = 0.95, ...) {
  cli::cli_abort("No `model_parameters()` method for objects of class {.cls {class(x)}}.")
}

#' @noRd
S7::method(model_parameters, nowcast_class) <- function(x, conf.level = 0.95, ...) {
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

  # NB: a two-stage fit reports the epidemic + likelihood parameters only.  The
  # reporting delay is fixed during Stage 2 (imputed K times beforehand), so it
  # does not appear in `last.par.best` and therefore has no row here -- the delay
  # is summarised separately, not as a model_parameters() parameter.
  out <- out[order(out$type, out$term), ]
  rownames(out) <- NULL
  out
}
