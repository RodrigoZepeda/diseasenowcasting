# =============================================================================
# backtest() -- roll a nowcast across as-of dates and (optionally) several models
# =============================================================================
# For each (as-of date, model) the model is fit as of that date, the
# posterior-predictive nowcast drawn, and the newest-event (d*=0) summary kept
# alongside the eventual truth.  Parallelised over (date x model) with doFuture
# (register a plan(); requires diseasenowcasting installed for multisession workers).
# =============================================================================

#' Fitted backtest object
#' @keywords internal
#' @noRd
backtest_class <- S7::new_class(
  "backtest",
  properties = list(
    data    = S7::class_any,        # the full tbl_now (truth source)
    models  = S7::class_list,       # list of model() objects
    dates   = S7::class_any,        # as-of dates evaluated
    type    = S7::class_character,
    results = S7::class_any,        # data.frame: per (date, model, event) summary + truth
    simulations = S7::class_any     # optional list of pooled draw matrices, or NULL
  )
)

#' Backtest one or more nowcast models across a set of as-of dates
#'
#' @param data A `tbl_now` (the full data; its eventual counts are the truth).
#' @param models A [model()] object or a list of them.  With several models the
#'   backtest can rank them (see [score()]).
#' @param dates A vector of as-of dates.  If `NULL`, a default grid spanning the
#'   observed range (interior points) is sampled.
#' @param type `"two_stage"` (default) or `"one_stage"`.
#' @param n_dates If `dates` is `NULL`, how many to sample. Default 20.
#' @param return_simulations If TRUE, also keep the pooled draw matrix per
#'   (date, model).  Default FALSE (summaries only: mean/median/sd/quantiles).
#' @param n_draws Posterior draws per nowcast.
#' @param K,np_spread Two-stage controls passed through.
#' @param seed Optional base RNG seed.
#' @param ... Passed to [nowcast()].
#' @returns A `backtest_class` object.
#'
#' @details
#' `backtest()` evaluates one nowcast per (as-of date x model) cell and these
#' cells are **embarrassingly parallel**.  The work is dispatched with
#' \pkg{foreach} + \pkg{doFuture}, so parallelism is controlled by the
#' \pkg{future} plan you set *before* calling `backtest()`:
#'
#' ```r
#' library(future)
#' plan(multisession, workers = 4)   # 4 parallel R sessions
#' bt <- backtest(data, models, dates = my_dates)
#' plan(sequential)                  # back to serial when done
#' ```
#'
#' With the default plan (`sequential`) the cells run one at a time.  For a grid
#' of many dates x models, `plan(multisession, workers = N)` (or
#' `plan(multicore)` on Linux/macOS) gives a near-linear speed-up up to the
#' number of physical cores.  Each worker needs the package available, which is
#' automatic for an installed package; with `devtools::load_all()` use
#' `plan(multisession)` so workers re-load it.
#'
#' @examples
#' if (interactive() && requireNamespace("tbl.now", quietly = TRUE)) {
#'   # future::plan(future::multisession, workers = 4)   # opt in to parallelism
#'   # bt <- backtest(my_tbl_now, list(model_a, model_b), dates = my_dates)
#'   # future::plan("sequential")
#' }
#' @export
backtest <- function(data, models = diseasenowcasting::model(), dates = NULL,
                     type = c("two_stage", "one_stage"), n_dates = 20L,
                     return_simulations = FALSE, n_draws = 1000L, K = 25L,
                     np_spread = 1, seed = NULL, ...) {
  type <- match.arg(type)
  if (S7::S7_inherits(models, model_class)) models <- list(models)
  model_labels <- vapply(models, .model_label, character(1))

  event_col  <- tbl.now::get_event_date(data)
  event_unit <- tbl.now::get_event_units(data)
  min_event  <- min(data[[event_col]], na.rm = TRUE)
  report_col <- tbl.now::get_report_date(data)

  if (is.null(dates)) {
    candidate <- sort(unique(data[[event_col]]))
    candidate <- candidate[-c(1, length(candidate))]                 # drop the very ends
    if (!is.null(seed)) set.seed(seed)
    dates <- sort(candidate[round(seq(1, length(candidate), length.out = min(n_dates, length(candidate))))])
  }

  # Eventual truth: total incidence per event-time over ALL reports in `data`.
  truth_inc  <- tbl.now::to_count(data, to = "count-incidence")
  truth_evnum <- .unit_steps(min_event, truth_inc[[event_col]], event_unit)
  truth_count <- as.numeric(truth_inc[[tbl.now::get_case_count(data) %||% "n"]] %||% truth_inc[["n"]])
  truth_final <- tapply(truth_count, truth_evnum, sum)
  truth_by_evnum <- setNames(as.numeric(truth_final), as.integer(names(truth_final)))

  grid <- expand.grid(date_idx = seq_along(dates), model_idx = seq_along(models))

  # Nudge the user toward parallelism when the grid is large and the plan is serial.
  n_cells <- nrow(grid)
  plan_is_sequential <- tryCatch(inherits(future::plan(), "sequential"), error = function(e) TRUE)
  if (plan_is_sequential && n_cells >= 12L) {
    cli::cli_inform(c(
      "i" = "Running {n_cells} backtest cell{?s} sequentially.",
      "*" = "For a large grid, set a parallel plan first: {.code future::plan(future::multisession, workers = N)}."
    ))
  }

  results_list <- foreach::foreach(row = seq_len(nrow(grid)),
                                   .options.future = list(seed = TRUE)) %dofuture% {
    di <- grid$date_idx[row]; mi <- grid$model_idx[row]
    as_of <- dates[di]
    out <- tryCatch({
      nc <- nowcast(data, models[[mi]], type = type, now = as_of,
                    K = K, np_spread = np_spread, n_draws = n_draws, ...)
      pred_summary <- predict(nc, summary = TRUE)              # data.frame per event
      pred_summary$model    <- model_labels[mi]
      pred_summary$date_run <- as_of
      pred_summary$final    <- truth_by_evnum[as.character(pred_summary$.event_num)]
      pred_summary$target   <- nc@target
      list(summary = pred_summary,
           sims = if (return_simulations) predict(nc, n_draws = n_draws)@draws else NULL)
    }, error = function(e) NULL)
    out
  }

  summaries <- do.call(rbind, lapply(results_list, function(x) if (is.null(x)) NULL else x$summary))
  sims <- if (return_simulations) lapply(results_list, function(x) if (is.null(x)) NULL else x$sims) else NULL

  backtest_class(data = data, models = models, dates = dates, type = type,
                 results = summaries, simulations = sims)
}

#' Human-readable label for a model (epidemic / likelihood / delay).
#' @keywords internal
#' @noRd
.model_label <- function(model) {
  paste(model@epidemic@name, model@likelihood@name, model@delay@name, sep = "/")
}

#' Predictions table from a backtest (the per-(date, model, event) summaries)
#' @param object A `backtest_class`.
#' @param ... Unused.
#' @returns The results data.frame (or, if simulations were kept, a list with
#'   `summary` and `simulations`).
#' @noRd
S7::method(predict, backtest_class) <- function(object, ...) {
  if (is.null(object@simulations)) object@results
  else list(summary = object@results, simulations = object@simulations)
}

#' @noRd
S7::method(print, backtest_class) <- function(x, ...) {
  n_models <- length(x@models); n_dates <- length(x@dates)
  cli::cli_text("<backtest> {n_models} model(s) x {n_dates} date(s), {x@type}")
  cli::cli_text("models: {paste(vapply(x@models, .model_label, character(1)), collapse = ', ')}")
  cli::cli_text("score() for WIS/APE/MSE; autoplot() to visualise")
  invisible(x)
}
