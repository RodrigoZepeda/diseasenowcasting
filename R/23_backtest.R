# =============================================================================
# backtest() -- roll a nowcast across as-of dates and (optionally) several models
# =============================================================================
# For each (as-of date, model) the model is fit as of that date, the
# posterior-predictive nowcast drawn, and the newest-event (d*=0) summary kept
# alongside the eventual truth.  Parallelised over (date x model) with
# future.apply (register a plan(); requires diseasenowcasting installed for multisession workers).
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
#' @param type `"two_stage"` (default), `"one_stage"`, or `"auto"` (per delay:
#'   dirichlet one-stage, all other delays two-stage).
#' @param n_dates If `dates` is `NULL`, how many to sample. Default 20.
#' @param max_delay Truth-completeness horizon (in event units).  Event dates
#'   within `max_delay` units of the last report do not yet have a fully
#'   observed eventual count, so the backtest **excludes** them (their "truth"
#'   would still be accruing).  Default `NULL` uses the 99th percentile of the
#'   observed reporting delays.  Pass a number to override, or `Inf` to evaluate
#'   every date regardless of completeness.
#' @param return_simulations If TRUE, also keep the pooled draw matrix per
#'   (date, model).  Default FALSE (summaries only: mean/median/sd/quantiles).
#' @param n_draws Posterior draws per nowcast.
#' @param K,np_spread Two-stage controls passed through.
#' @param recent When `dates` is `NULL`, choose the **most recent** `n_dates`
#'   complete-truth as-of dates rather than spreading them across the whole
#'   history (default `FALSE`).  Useful when the backtest is meant to judge how a
#'   model does on *recent* dynamics (e.g. for model selection ahead of a
#'   present-day nowcast).
#' @param seed Optional base RNG seed.
#' @param ... Passed to [nowcast()].
#' @returns A `backtest_class` object.
#'
#' @details
#' `backtest()` evaluates one nowcast per (as-of date x model) cell and these
#' cells are **embarrassingly parallel**.  The work is dispatched with
#' \pkg{future.apply}, so parallelism is controlled by the
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
                     type = c("two_stage", "one_stage", "auto"), n_dates = 20L,
                     max_delay = NULL,
                     return_simulations = FALSE, n_draws = 1000L, K = 25L,
                     np_spread = 1, recent = FALSE,
                     seed = sample.int(.Machine$integer.max, 1), ...) {
  type <- match.arg(type)
  if (S7::S7_inherits(models, model_class)) models <- list(models)
  model_labels <- vapply(models, .model_label, character(1))

  event_col  <- tbl.now::get_event_date(data)
  event_unit <- tbl.now::get_event_units(data)
  min_event  <- min(data[[event_col]], na.rm = TRUE)
  report_col <- tbl.now::get_report_date(data)

  # ── Truth-completeness horizon ───────────────────────────────────────────
  # An event date only has a complete eventual count once enough time has passed
  # for its late reports to arrive.  Observed delays (in event units):
  ev_event  <- .unit_steps(min_event, data[[event_col]],  event_unit)
  ev_report <- .unit_steps(min_event, data[[report_col]], event_unit)
  delays_u  <- ev_report - ev_event
  delays_u  <- delays_u[is.finite(delays_u) & delays_u >= 0]
  if (is.null(max_delay)) {
    max_delay <- if (length(delays_u) > 0)
      ceiling(stats::quantile(delays_u, 0.99, names = FALSE)) else 0
  }
  last_report_evnum <- if (length(ev_report) > 0) max(ev_report, na.rm = TRUE) else 0
  cutoff_evnum <- last_report_evnum - max_delay      # most-recent fully-observed event-time

  if (is.null(dates)) {
    candidate <- sort(unique(data[[event_col]]))
    candidate <- candidate[-c(1, length(candidate))]                 # drop the very ends
    # Keep only candidates whose truth is (plausibly) complete.
    cand_evnum <- .unit_steps(min_event, candidate, event_unit)
    candidate  <- candidate[cand_evnum <= cutoff_evnum]
    if (length(candidate) == 0L)
      cli::cli_abort(c("No evaluation dates with a complete eventual count.",
                       "i" = "The series is shorter than {.code max_delay} = {max_delay} event units; pass {.code max_delay} explicitly to override."))
    if (!is.null(seed)) set.seed(seed)
    n_keep <- min(n_dates, length(candidate))
    keep_idx <- if (isTRUE(recent)) {
      # The most recent complete-truth dates -- judge the model on recent dynamics.
      seq.int(length(candidate) - n_keep + 1L, length(candidate))
    } else {
      # Spread evenly across the observed history.
      round(seq(1, length(candidate), length.out = n_keep))
    }
    dates <- sort(candidate[keep_idx])
  } else if (is.finite(max_delay)) {
    # Drop user-supplied dates whose truth is not yet complete (with a heads-up).
    dates       <- sort(as(dates, class(data[[event_col]])[1]))
    dates_evnum <- .unit_steps(min_event, dates, event_unit)
    too_recent  <- dates_evnum > cutoff_evnum
    if (any(too_recent)) {
      cli::cli_warn(c(
        "Dropping {sum(too_recent)} evaluation date{?s} within {max_delay} event unit{?s} of the last report (truth not yet complete).",
        "i" = "Pass {.code max_delay = Inf} to keep them."))
      dates <- dates[!too_recent]
    }
    if (length(dates) == 0L) cli::cli_abort("All supplied `dates` were dropped as too recent; pass `max_delay = Inf` to keep them.")
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

  # One worker per (date, model) cell.  Each fits a nowcast as of that date and
  # records its per-event summary joined to the eventual truth.  The fit happens
  # inside the worker because the RTMB object cannot cross processes.
  # `cell_row` is a genuine function argument (not a `foreach` NSE iterator), so
  # it needs no globalVariables() declaration.  `future_lapply` parallelises over
  # the registered `future::plan()` with reproducible per-cell RNG.
  results_list <- future.apply::future_lapply(seq_len(nrow(grid)), function(cell_row) {
    date_index  <- grid$date_idx[cell_row]
    model_index <- grid$model_idx[cell_row]
    as_of       <- dates[date_index]
    tryCatch({
      nc <- nowcast(data, models[[model_index]], type = type, now = as_of,
                    K = K, np_spread = np_spread, n_draws = n_draws, ...)
      pred_summary <- predict(nc, summary = TRUE)              # data.frame, one row per event
      pred_summary$model    <- model_labels[model_index]
      pred_summary$date_run <- as_of
      pred_summary$final    <- truth_by_evnum[as.character(pred_summary$.event_num)]
      pred_summary$target   <- nc@target
      list(summary = pred_summary,
           sims = if (return_simulations) predict(nc, n_draws = n_draws)@draws else NULL)
    }, error = function(e) NULL)   # a failed cell contributes nothing
  }, future.seed = TRUE)

  # Drop failed cells (NULL) and stack the per-cell summaries / simulation draws.
  summaries <- do.call(rbind, lapply(results_list,
    function(cell) if (is.null(cell)) NULL else cell$summary))
  sims <- if (return_simulations)
    lapply(results_list, function(cell) if (is.null(cell)) NULL else cell$sims) else NULL

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
