# =============================================================================
# backtest() -- diseasenowcasting model convenience over tbl.now's backtest
# =============================================================================

#' Backtest one or more diseasenowcasting models
#'
#' `backtest()` translates native [model()] specifications into labelled
#' [tbl.now::engine_diseasenowcasting()] specifications and delegates the full
#' retrospective workflow to [tbl.now::nowcast_backtest()]. The returned object
#' is therefore the common `nowcast_backtest` result used by `tbl.now` for
#' tidying, forecast conversion, scoring, weighting, and ensembling.
#'
#' @param data A [tbl.now::tbl_now] holding the full data, including observations
#'   that arrived after the retrospective nowcast dates.
#' @param models A [model()] or list of models. Names on the list become the
#'   canonical method labels; unnamed models receive labels from their component
#'   names. Labels must be unique.
#' @param dates Retrospective nowcast origins, passed as `now_dates` to
#'   [tbl.now::nowcast_backtest()].
#' @param n_dates Number of automatic retrospective origins. Ignored when
#'   `dates` is supplied. Default `4`.
#' @param type `"two_stage"`, `"one_stage"`, or `"auto"`, passed to every
#'   diseasenowcasting engine.
#' @param horizon Number of time units of hindsight used by `tbl.now` when
#'   `dates = NULL`. `NULL` uses `4` for ordinary data and the largest model
#'   settlement horizon for count-cumulative data (26 for its automatic model).
#' @param n_draws Posterior draws per fit.
#' @param K,np_spread Native two-stage fitting controls passed to every engine.
#' @param seed Optional base seed. `tbl.now` derives a stable seed for each
#'   model/date fit from this value.
#' @param keep_draws Whether the canonical backtest retains posterior draws.
#' @param on_error Either `"warn"` to record and skip failed cells or `"abort"`.
#' @param verbose Whether to report progress.
#' @param truth_axis,truth_type Canonical scoring truth controls passed to
#'   [tbl.now::nowcast_backtest()]. When both are `NULL`, they follow the native
#'   estimand: reported totals without a revision process, confirmed cases for
#'   confirmation/both modes, and still-standing cases for retraction-only mode.
#' @param quantile_levels Quantile probabilities requested from every model.
#' @param ... Additional arguments passed to every [nowcast()] fit through its
#'   engine specification.
#'
#' @returns A [tbl.now::nowcast_backtest] object.
#' @seealso [diseasenowcasting_workflows] for the native/common ownership
#'   boundary; [tbl.now::score_nowcast()], [tbl.now::nowcast_weights()],
#'   [tbl.now::nowcast_ensemble()], [fit_check()]
#' @export
backtest <- function(data, models = diseasenowcasting::model(), dates = NULL,
                     type = c("two_stage", "one_stage", "auto"), horizon = NULL,
                     n_dates = 4L,
                     n_draws = 1000L, K = 25L, np_spread = 1,
                     seed = NULL, keep_draws = FALSE,
                     on_error = c("warn", "abort"), verbose = TRUE,
                     truth_axis = NULL, truth_type = NULL,
                     quantile_levels = tbl.now::nowcast_quantile_levels(), ...) {
  type <- match.arg(type)
  on_error <- match.arg(on_error)
  truth_defaults <- .backtest_truth_defaults(data)
  truth_axis <- truth_axis %||% truth_defaults$axis
  truth_type <- truth_type %||% truth_defaults$type
  truth_axis <- match.arg(truth_axis, c("report", "revision"))

  if (S7::S7_inherits(models, model_class)) {
    models <- list(models)
  }
  if (!is.list(models) || length(models) == 0L ||
      !all(vapply(models, S7::S7_inherits, logical(1L), class = model_class))) {
    cli::cli_abort("{.arg models} must be a {.fn model} or a non-empty list of them.")
  }
  if (is.null(horizon)) {
    horizon <- if (identical(tbl.now::get_data_type(data), "count-cumulative")) {
      max(vapply(models, function(candidate) {
        if (isTRUE(candidate@cumulative@active)) {
          as.numeric(candidate@cumulative@settlement)
        } else {
          26
        }
      }, numeric(1L)))
    } else {
      4
    }
  }

  inferred_labels <- vapply(models, .model_label, character(1L))
  supplied_labels <- names(models)
  labels <- if (is.null(supplied_labels)) {
    inferred_labels
  } else {
    use_supplied <- !is.na(supplied_labels) & nzchar(supplied_labels)
    ifelse(use_supplied, supplied_labels, inferred_labels)
  }
  duplicated_labels <- unique(labels[duplicated(labels)])
  if (length(duplicated_labels) > 0L) {
    cli::cli_abort(c(
      "Every backtested model must have a unique specification.",
      "x" = "Duplicated label{?s}: {.val {duplicated_labels}}.",
      "i" = "Name the model list explicitly when comparing different priors or settings of the same component families."
    ))
  }

  shared_args <- list(...)
  engines <- Map(function(candidate, label) {
    do.call(
      tbl.now::engine_diseasenowcasting,
      c(
        list(
          model = candidate, type = type, n_draws = n_draws,
          K = K, np_spread = np_spread,
          quantile_levels = quantile_levels, label = label
        ),
        shared_args
      )
    )
  }, models, labels)

  do.call(
    tbl.now::nowcast_backtest,
    c(
      list(x = data),
      stats::setNames(engines, labels),
      list(
        now_dates = dates, horizon = horizon, n_dates = n_dates, seed = seed,
        keep_draws = keep_draws, on_error = on_error, verbose = verbose,
        truth_axis = truth_axis, truth_type = truth_type
      )
    )
  )
}

#' Canonical truth controls matching the diseasenowcasting estimand
#' @keywords internal
#' @noRd
.backtest_truth_defaults <- function(data) {
  if (!isTRUE(.tblnow_has_revision(data)) ||
      identical(tbl.now::get_data_type(data), "count-cumulative")) {
    return(list(axis = "report", type = "total"))
  }

  type_col <- .tblnow_get_revision_type(data)
  date_col <- .tblnow_get_revision_date(data)
  mode <- .infer_revision_mode(
    as.character(as.data.frame(data)[[type_col]]),
    as.data.frame(data)[[date_col]]
  )
  if (mode %in% c("confirmation_only", "both")) {
    list(axis = "revision", type = "confirmed")
  } else if (identical(mode, "retraction_only")) {
    list(axis = "report", type = "pending")
  } else {
    list(axis = "report", type = "total")
  }
}

#' Human-readable label for a model
#' @keywords internal
#' @noRd
.model_label <- function(model) {
  base <- paste(model@epidemic@name, model@likelihood@name,
                model@delay@name, sep = "/")
  if (isTRUE(model@cumulative@active)) {
    paste(base, model@cumulative@observation,
          paste0("H", model@cumulative@settlement), sep = "/")
  } else {
    base
  }
}
