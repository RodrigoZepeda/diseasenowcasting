# =============================================================================
# save_nowcast() / load_nowcast() -- persist a fitted nowcast to disk
# =============================================================================
# RTMB fit objects hold external pointers (the AD tape) that DO NOT survive
# saveRDS(): a deserialized tape crashes R the moment any tape-touching method
# runs.  So we never serialize the tape.  Instead we save everything that is
# plain R -- the model() spec (including custom closures), the input tbl_now, the
# prepared engine, the priors, and, per fit, the fitted parameters plus the
# Laplace MODE and PRECISION.  predict() only needs the mode + precision (every
# other step -- .joint_reconstruct(), .epidemic_rng() -- is ordinary R), so a
# loaded nowcast predicts with ANY n_draws and needs no RTMB at all (even for
# custom delays/epidemics).  load_nowcast(rebuild = TRUE) additionally re-tapes
# the objective (no re-optimization) for the few things that need a live tape
# (e.g. sdreport for `use_random` marginal fits) or to re-fit on new data.
# =============================================================================

#' Save a fitted nowcast to disk
#'
#' Serializes a [nowcast()] (or [auto_nowcast()]) result to a single `.rds`
#' bundle that can be restored later with [load_nowcast()].  The RTMB autodiff
#' tape is **not** saved (its external pointers cannot be serialized); instead
#' the fitted parameters and the Laplace mode + precision are stored, which is
#' all [predict()] needs.  The input `tbl_now` and the `model()` specification
#' are saved too, so the loaded object can also be re-fit.
#'
#' @param object A `nowcast_class` from [nowcast()] or [auto_nowcast()].
#' @param file Path to write (a single `.rds` file).
#' @returns `file`, invisibly.
#' @seealso [load_nowcast()]
#' @examples
#' \donttest{
#' if (requireNamespace("tbl.now", quietly = TRUE)) {
#'   library(tbl.now)
#'   data(denguedat)
#'   dn <- subset(denguedat, onset_week <= as.Date("1990-12-01") &
#'                           report_week <= as.Date("1990-12-01"))
#'   tn <- tbl_now(dn, event_date = onset_week, report_date = report_week,
#'                 data_type = "linelist", verbose = FALSE)
#'   nc <- nowcast(tn, type = "one_stage", n_draws = 200)
#'   f  <- tempfile(fileext = ".rds")
#'   save_nowcast(nc, f)
#'   nc2 <- load_nowcast(f)
#'   predict(nc2, summary = TRUE)          # works without the original RTMB tape
#' }
#' }
#' @export
save_nowcast <- function(object, file) {
  if (!S7::S7_inherits(object, nowcast_class))
    cli::cli_abort("{.arg object} must be a {.cls nowcast} (from {.fn nowcast} or {.fn auto_nowcast}).")
  if (!is.character(file) || length(file) != 1L)
    cli::cli_abort("{.arg file} must be a single file path.")

  bundle <- list(
    dcast_save_version = 1L,
    saved_with         = as.character(utils::packageVersion("diseasenowcasting")),
    saved_at           = Sys.time(),
    model      = object@model,
    data       = object@data,        # the input tbl_now (for re-fitting)
    now        = object@now,
    type       = object@type,
    rung       = object@rung,
    target     = object@target,
    engine     = object@engine,
    priors     = object@priors,
    phi        = object@phi,
    n_draws    = object@n_draws,
    comparison = object@comparison,   # auto_nowcast() scoreboard, or NULL
    fits       = lapply(object@fits, .serialize_fit)
  )
  saveRDS(bundle, file)
  cli::cli_inform(c("v" = "Saved nowcast to {.path {file}}.",
                    "i" = "Restore with {.code load_nowcast({.str {file}})}."))
  invisible(file)
}

#' Load a nowcast saved with [save_nowcast()]
#'
#' Restores a `nowcast_class` from a bundle written by [save_nowcast()].  The
#' result works with [predict()], [autoplot()], [coef()], [tidy()],
#' [mean()]/[median()]/[quantile()] straight away (sampling from the stored
#' Laplace mode + precision).  To re-fit it -- on the same or new data -- pass the
#' loaded object's `model` to [nowcast()] (the saved `tbl_now` is in the `data`
#' slot).
#'
#' @param file Path to a `.rds` bundle written by [save_nowcast()].
#' @param rebuild If `TRUE`, also re-tape the RTMB objective for each fit (no
#'   re-optimization).  Needed only for `use_random` marginal fits or to inspect
#'   the live tape; custom delays/epidemics require `library(RTMB)`.  Default
#'   `FALSE` -- the stored mode + precision already drive [predict()].
#' @returns A `nowcast_class` object.
#' @seealso [save_nowcast()]
#' @export
load_nowcast <- function(file, rebuild = FALSE) {
  if (!is.character(file) || length(file) != 1L || !file.exists(file))
    cli::cli_abort("{.arg file} must be the path to an existing {.path .rds} file.")
  bundle <- readRDS(file)
  if (!is.list(bundle) || is.null(bundle$dcast_save_version))
    cli::cli_abort(c("{.path {file}} is not a {.fn save_nowcast} bundle.",
                     "i" = "Use the file written by {.fn save_nowcast}."))

  fits <- bundle$fits
  if (isTRUE(rebuild)) fits <- lapply(fits, .rebuild_fit_obj)

  nowcast_class(
    model = bundle$model, data = bundle$data, now = bundle$now,
    type = bundle$type, fits = fits, rung = bundle$rung, target = bundle$target,
    engine = bundle$engine, priors = bundle$priors, phi = bundle$phi,
    n_draws = as.integer(bundle$n_draws), comparison = bundle$comparison)
}

# -- internals ----------------------------------------------------------------

#' Strip the RTMB tape from a fit, keeping the Laplace mode + precision so
#' predict()/tidy() still work after a save/load round-trip.
#' @keywords internal
#' @noRd
.serialize_fit <- function(fit) {
  # prior-only fits already carry precomputed draws (`prior_sims`) and no tape.
  if (isTRUE(fit$prior_only) || is.null(fit$obj)) {
    fit$obj <- NULL; fit$opt <- NULL
    return(fit)
  }
  obj <- fit$obj
  if (isFALSE(fit$use_random %||% FALSE)) {
    mode_vec  <- obj$env$last.par.best
    precision <- methods::as(obj$he(mode_vec), "sparseMatrix")
  } else {
    sd_report <- RTMB::sdreport(obj, getJointPrecision = TRUE)
    precision <- sd_report$jointPrecision
    mode_vec  <- obj$env$last.par.best
    if (is.null(precision)) {
      precision <- methods::as(solve(sd_report$cov.fixed), "sparseMatrix")
      mode_vec  <- sd_report$par.fixed
    }
  }
  fit$mode      <- mode_vec
  fit$precision <- precision
  fit$obj <- NULL; fit$opt <- NULL
  fit
}

#' Re-tape the RTMB objective for a loaded fit (no re-optimization): rebuild from
#' the stored engine + priors + parameter list and pin it at the stored mode.
#' @keywords internal
#' @noRd
.rebuild_fit_obj <- function(fit) {
  if (isTRUE(fit$prior_only) || !is.null(fit$obj)) return(fit)
  rebuilt <- tryCatch({
    built <- build_joint_obj(fit$data, fit$priors, init = fit$parList,
                             use_random = isTRUE(fit$use_random %||% FALSE))
    obj <- built$obj
    if (!is.null(fit$mode)) {
      obj$fn(as.numeric(fit$mode))         # populate internal state at the mode
      obj$env$last.par.best <- fit$mode    # pin the optimum (names preserved)
    }
    obj
  }, error = function(e) {
    cli::cli_warn(c("Could not re-tape the RTMB objective: {conditionMessage(e)}",
                    "i" = "predict() still works from the stored mode + precision."))
    NULL
  })
  if (!is.null(rebuilt)) fit$obj <- rebuilt
  fit
}
