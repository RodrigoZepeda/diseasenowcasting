# =============================================================================
# Convert a tbl_now object (+ an as-of "now" date) into engine inputs
# =============================================================================
# The user-facing API takes data as a `tbl_now` (from the tbl.now package).
# This helper reproduces the conversion the benchmark harness did by hand:
# filter to the as-of horizon, aggregate to incidence counts, and build the
# observation matrix `m`, the maximum-observable-delay vector `d_star`, the
# time-grid covariate matrix `X` (from any computed temporal-effect columns),
# and `max_time` -- everything [prepare_data()] needs.
# =============================================================================

#' Build the engine data list from a `tbl_now` as of a given date
#'
#' @param data A `tbl_now` (event/report dates, `.event_num`/`.delay` columns).
#' @param model A [model()] object (delay family etc. drive prepare_data()).
#' @param now As-of date: only events and reports up to `now` are used.  If
#'   `NULL`, uses `get_now(data)` and falls back to the latest report date.
#' @param validation_mode `"none"`, `"confirmation_only"`, `"retraction_only"` or
#'   `"both"`, as resolved by [nowcast()] from the `tbl_now`'s `validation_type`
#'   column.  Anything but `"none"` switches on the validation (cure-model)
#'   observation block; see 31_retraction_likelihood.R.
#' @param validation_censored Optional name of a logical column marking rows whose
#'   validation date is an upper BOUND rather than the exact date.
#' @param ... Passed to [prepare_data()] (e.g. `gp_boundary_frac`).
#' @returns A list: `data` (the prepare_data() engine list), `now`,
#'   `event_col`, `min_event`, `event_unit`, `max_time`.
#' @keywords internal
#' @noRd
prepare_from_tbl_now <- function(data, model, now = NULL, delay_only = FALSE,
                                 validation_mode = "none",
                                 validation_censored = NULL, ...) {
  if (!tbl.now::is_tbl_now(data)) cli::cli_abort("`data` must be a tbl_now (see tbl.now::tbl_now()).")
  event_col   <- tbl.now::get_event_date(data)
  report_col  <- tbl.now::get_report_date(data)
  event_unit  <- tbl.now::get_event_units(data)
  effect_cols <- tbl.now::get_temporal_effect_cols(data)

  now <- now %||% tbl.now::get_now(data)
  report_values <- data[[report_col]]
  if (is.null(now) || all(is.na(now))) now <- max(report_values, na.rm = TRUE)
  now <- as(now, class(data[[event_col]])[1])

  # As-of view: events and reports up to `now`.  `[.tbl_now` preserves the
  # tbl_now class/attributes, so to_count() works directly (no as_tbl_now round
  # trip, which would re-resolve the date columns and can mangle attributes).
  #
  # NOTE: we deliberately do NOT call complete_zeroes().  The censored
  # likelihood handles event-times with zero observed cases natively (they
  # enter via S_k with k = 0), and prepare_data() already fills the full
  # max_time grid with zeros.  The covariate matrix X is built deterministically
  # from the calendar dates below, so it does not need completed rows either.
  keep_rows <- which(data[[event_col]] <= now & data[[report_col]] <= now)
  as_of <- data[keep_rows, , drop = FALSE]
  if (!nrow(as_of)) cli::cli_abort("No observations are available at the requested `now`.")

  strata_cols   <- tbl.now::get_strata(data)
  is_cumulative <- identical(tbl.now::get_data_type(data), "count-cumulative")
  # Preserve ordinary-data indexing for compatibility. For cumulative data,
  # even the time-grid origin must be learned from the as-of view: a historical
  # event first published after `now` is post-origin information.
  min_event <- if (is_cumulative) {
    min(as_of[[event_col]], na.rm = TRUE)
  } else {
    min(data[[event_col]], na.rm = TRUE)
  }
  unit_steps <- function(to) .unit_steps(min_event, to, event_unit)
  covariate_source <- if (is_cumulative) as_of else data
  cumulative_process <- tryCatch(model@count_cumulative, error = function(e) NULL)
  cumulative_settlement <- if (!is.null(cumulative_process) &&
                               isTRUE(cumulative_process@active)) {
    as.integer(cumulative_process@settlement)
  } else 26L
  if (is_cumulative) {
    as_of <- .prepare_count_cumulative_as_of(
      data, now = now, settlement = cumulative_settlement
    )
  }

  # -- linelist retractions -----------------------------------------------------
  # A retraction-date column turns on the cure-model observation block: a case may
  # be reported and then removed from the register.  Two as-of rules are applied
  # here, before anything else touches the data, because both are silent-bug traps
  # (see `.mask_retractions()`): a retraction dated after `now` has not happened
  # yet, so the row is STANDING; a retraction in the same period as its report was
  # never visible at any observation epoch, so the row is dropped outright.
  # VALIDATION mode.  tbl.now records ONE date plus an outcome; the engine below
  # was built around two mirror-image columns (retractions and confirmations), so
  # the two representations are reconciled here rather than duplicating the whole
  # tested path:
  #
  #   confirmation_date := validation_date where validation_type == "confirmed"
  #   retraction_date   := validation_date where validation_type == "retracted"
  #
  # A report resolves either negatively (retracted, we see the negatives) or
  # positively (confirmed, we see the positives).  The two share every downstream
  # code path and differ only in `lag_offset`, which sets the support of the
  # validation lag, and in what a resolved row means for the nowcast target.
  # Mode 2 -- BOTH -- is the full process: modes 0 and 1 see only one of the signs
  # and infer the split from the censoring, while mode 2 sees the sign outright.
  resolution_mode <- switch(validation_mode,
                            "both" = 2L, "confirmation_only" = 1L,
                            "retraction_only" = 0L, 0L)
  has_validation  <- !identical(validation_mode, "none") &&
                     isTRUE(tbl.now::has_validation(data))
  resolution_name <- if (resolution_mode == 1L) "confirmation" else "retraction"

  if (has_validation && is_cumulative && resolution_mode != 0L) {
    # Eq. `noconfirmcum`: a confirmation does not change a cumulative count, so
    # g^val_{D+} = 0 and the confirmation-delay parameters are unidentifiable.
    cli::cli_abort(c(
      "A count-cumulative stream cannot carry {.val confirmed} validations.",
      "i" = "A confirmation does not change a cumulative count, so its delay parameters are unidentifiable.",
      "*" = "Keep the retractions and drop the confirmations, or model the data as {.val count-incidence}."))
  }

  now_step        <- as.integer(unit_steps(now))
  retraction_step <- NULL
  resolution_positive <- NULL
  if (has_validation && !is_cumulative) {
    as_of_frame    <- as.data.frame(as_of)
    validation_col <- tbl.now::get_validation_date(data)
    type_col       <- tbl.now::get_validation_type(data)

    resolution_values <- as_of_frame[[validation_col]]
    outcomes          <- as.character(as_of_frame[[type_col]])
    # A dated row with no outcome cannot enter either lag law.  The mode inference
    # already refused this over the FULL data; re-check here because the as-of view
    # is what actually reaches the likelihood.
    unusable <- !is.na(resolution_values) &
                (is.na(outcomes) | !outcomes %in% c("confirmed", "retracted"))
    if (any(unusable))
      cli::cli_abort(c(
        "{sum(unusable)} row{?s} {?carries/carry} a validation date without a usable {.field validation_type}.",
        "x" = "A resolved report whose outcome is unknown cannot enter either lag law.",
        "i" = "Use {.val confirmed} or {.val retracted}, or clear the date to mark the row {.val pending}."))

    # Mode 2 needs the SIGN of each resolution; modes 0 and 1 see only one sign, so
    # a row carrying the other outcome is not a resolution as far as they are
    # concerned and its date is dropped back to "unresolved".
    if (resolution_mode == 2L) {
      resolution_positive <- !is.na(resolution_values) & outcomes == "confirmed"
    } else {
      wanted <- if (resolution_mode == 1L) "confirmed" else "retracted"
      resolution_values[!is.na(resolution_values) & outcomes != wanted] <- NA
    }
    retract_censored <- .resolve_logical_column(as_of_frame, validation_censored,
                                                "validation_censored")

    # Modes 1 and 2 both allow a same-period resolution (a test can come back the
    # day it is ordered); only retraction-only mode forbids it.
    masked <- .mask_retractions(unit_steps(as_of_frame[[report_col]]),
                                unit_steps(resolution_values),
                                retract_censored, now_step,
                                lag_offset = if (resolution_mode == 0L) 0L else 1L)
    if (masked$dropped > 0)
      cli::cli_inform(c("i" = "Dropped {masked$dropped} case{?s} retracted in the same period as their report (never visible in any data vintage)."))
    as_of            <- as_of[masked$keep, , drop = FALSE]
    retraction_step  <- masked$retraction_step[masked$keep]
    retract_censored <- masked$retraction_censored[masked$keep]
    if (!is.null(resolution_positive)) resolution_positive <- resolution_positive[masked$keep]
    # Nothing has resolved BY `now`, though the data record resolutions later on.
    # Under retraction the cure block would be informed only by its prior and would
    # sit on the p = 1 boundary, which IS the ordinary count model, so the block is
    # dropped.  Under confirmation there is no such reduction -- "nothing confirmed
    # yet" does not mean "nothing will be" -- so the block stays and `p` is carried
    # by its prior, which is what a prior is for.
    if (!any(!is.na(retraction_step))) {
      if (resolution_mode >= 1L) {
        cli::cli_inform(c(
          "i" = "No report is confirmed as of {format(now)}, so {.arg p} is determined by its prior.",
          "*" = "Set it with {.code validation_process(p = beta_prior(...))}, or move `now` later."))
      } else {
        cli::cli_inform(c("i" = "No case is retracted as of {format(now)}; fitting the ordinary count model (`p = 1`)."))
        retraction_step <- NULL
      }
    }
  }

  # Count-cumulative streams de-accumulate to signed updates for the dedicated
  # finite-horizon composite likelihoods. Every other data type gives
  # non-negative incidence counts and the ordinary count model.
  incidence <- if (is_cumulative) {
    .count_cumulative_update_frame(as_of)
  } else {
    as.data.frame(tbl.now::to_count(as_of, to = "count-incidence"))
  }

  max_time <- as.integer(unit_steps(now) + 1L)
  event_num <- as.integer(unit_steps(incidence[[event_col]]))            # 0-indexed time
  delay_num <- as.integer(incidence[[".delay"]])
  counts    <- as.numeric(incidence[[tbl.now::get_case_count(data) %||% "n"]] %||% incidence[["n"]])

  # -- strata cell index (1..num_strata) ---------------------------------------
  # Strata levels must come from the as-of data, never from post-origin rows.
  # A new stratum may therefore appear on update; the warm-start adapter resizes
  # per-stratum blocks when that happens. Unstratified -> one cell.
  # A missing (NA / "") stratum value becomes an explicit "missing" level, so
  # rows with unknown strata form their OWN category rather than being dropped.
  # The model uses independent per-stratum means (each cell has its own
  # intercept + trend, symmetric across cells), so the "missing" cell carries
  # no contrast coefficient relative to any other level -- it is simply one more
  # cell in the K-way product, estimated like the rest and sharing only the
  # delay / overdispersion / GP kernel.
  cell_string <- function(tbl) do.call(paste, c(lapply(strata_cols, function(col) {
    values <- as.character(tbl[[col]]); values[is.na(values) | values == ""] <- "missing"; values
  }), sep = "|"))
  if (length(strata_cols) > 0) {
    cell_levels <- sort(unique(cell_string(as.data.frame(as_of))))
    cell_index  <- match(cell_string(as.data.frame(incidence)), cell_levels)
  } else {
    cell_levels <- "all"; cell_index <- rep(1L, length(event_num))
  }
  num_strata <- length(cell_levels)

  m_all <- cbind(event_num + 1L, counts, delay_num + 1L, cell_index)
  storage.mode(m_all) <- "double"

  # -- censored observations (delay known only up to an upper bound) ------------
  # tbl.now marks these with an is_censored_report column; for such rows the recorded
  # `.delay` is the UPPER BOUND j, and the likelihood uses log G_D(j) (the case
  # arrived with delay <= j) instead of the exact-delay term.  They are split
  # out into `m_censored`; everything else is an exact observation in `m`.
  # NB: renamed in tbl.now 0.29.0 (was `get_is_censored`).  The tryCatch below is
  # deliberately narrow -- it must not swallow a missing accessor, which would
  # silently disable report-side censoring rather than failing loudly.
  cens_col <- tbl.now::get_is_censored_report(data)
  if (is.null(cens_col)) cens_col <- character(0)
  is_cens  <- if (length(cens_col) == 1L && cens_col %in% names(incidence)) {
    v <- as.logical(incidence[[cens_col]]); v[is.na(v)] <- FALSE; v
  } else rep(FALSE, nrow(m_all))

  m          <- m_all[!is_cens, , drop = FALSE]
  m_censored <- m_all[ is_cens, , drop = FALSE]
  if (is_cumulative && any(is_cens)) {
    cli::cli_abort("Report-date censoring is not supported for count-cumulative cells.")
  }
  cumulative_levels <- if (is_cumulative)
    as.numeric(as_of[[".cumulative_level"]])[!is_cens] else NULL
  cumulative_previous <- if (is_cumulative)
    as.numeric(as_of[[".previous_nonzero"]])[!is_cens] else NULL
  if (nrow(m) > 0) {
    m_order <- order(m[, 1], m[, 4], m[, 3])
    m <- m[m_order, , drop = FALSE]
    if (is_cumulative) {
      cumulative_levels <- cumulative_levels[m_order]
      cumulative_previous <- cumulative_previous[m_order]
    }
  }
  if (nrow(m_censored) > 0) m_censored <- m_censored[order(m_censored[, 1], m_censored[, 4], m_censored[, 3]), , drop = FALSE]
  d_star <- matrix(rev(seq_len(max_time)) - 1L, ncol = 1L)

  # Time-grid covariate matrix X computed DETERMINISTICALLY on the full grid.
  # Temporal effects (day-of-week, seasonality, ...) are deterministic functions
  # of the calendar date, so they are well-defined for EVERY event-time on the
  # 1..max_time grid -- including event-times with no observed cases and event-
  # times AFTER the last observation but before `now`.  Computing them from the
  # observed data alone would (incorrectly) leave those rows at zero.
  X_temporal <- .temporal_effect_matrix(
    covariate_source, min_event, event_unit, max_time, effect_cols
  )
  # User covariates (attached via `tbl_now(covariates = ...)` / add_covariates())
  # are event-level values placed on the same 1..max_time grid, then column-bound
  # to the temporal effects.  Both feed the epidemic mean as X %*% gamma, shared
  # by the ordinary and count-cumulative observation models alike.
  X_covariate <- .covariate_matrix(
    covariate_source, event_col, min_event, unit_steps, max_time
  )
  X <- if (is.null(X_temporal) && is.null(X_covariate)) NULL
       else cbind(X_temporal %||% matrix(0.0, max_time, 0L),
                  X_covariate %||% matrix(0.0, max_time, 0L))
  if (!is.null(X) && ncol(X) == 0L) X <- NULL

  # -- retraction sufficient statistics ----------------------------------------
  # Pooled cure tables (retracted rows by lag, standing rows by report age) per
  # stratum, the censoring patterns, and the standing counts the posterior
  # predictive thins.  All computed from the ROWS, independently of the
  # `to_count()` aggregation above, since they are individual-level quantities --
  # which is also why the report-censoring flag is read per row here rather than
  # taken from the aggregated `m_censored`.
  retraction <- NULL
  if (!is.null(retraction_step)) {
    if (isTRUE(delay_only))
      cli::cli_abort("The retraction block needs the joint (non-`delay_only`) engine.")
    as_of_frame <- as.data.frame(as_of)
    report_censored_flag <- if (length(cens_col) == 1L && cens_col %in% names(as_of_frame)) {
      flag <- as.logical(as_of_frame[[cens_col]]); flag[is.na(flag)] <- FALSE; flag
    } else rep(FALSE, nrow(as_of_frame))
    # COUNT-INCIDENCE data carry a case count per (event, report, resolution) row;
    # a linelist has one case per row.  Weighting the tallies is the only
    # difference between the two, so they share this whole path.
    case_count_col <- tbl.now::get_case_count(data)
    row_weights <- if (length(case_count_col) == 1L && case_count_col %in% names(as_of_frame))
      as.numeric(as_of_frame[[case_count_col]]) else rep(1.0, nrow(as_of_frame))
    retraction <- .linelist_retraction_stats(
      event_steps         = unit_steps(as_of_frame[[event_col]]),
      report_steps        = unit_steps(as_of_frame[[report_col]]),
      retraction_step     = retraction_step,
      report_censored     = report_censored_flag,
      retraction_censored = retract_censored,
      cell_index          = if (length(strata_cols) > 0)
                              match(cell_string(as_of_frame), cell_levels) else rep(1L, nrow(as_of_frame)),
      now_step = now_step, max_time = max_time, num_strata = num_strata,
      weights = row_weights,
      lag_offset = if (resolution_mode == 0L) 0L else 1L,
      resolution_positive = resolution_positive)
  }

  engine <- prepare_data(model, m,
                         m_censored = if (nrow(m_censored) > 0) m_censored else NULL,
                         X = X, d_star = d_star, max_time = max_time,
                         num_strata = num_strata, delay_only = delay_only,
                         is_confirmation = FALSE,
                         cumulative_levels = if (is_cumulative)
                           cumulative_levels else NULL,
                         cumulative_previous_nonzero = if (is_cumulative)
                           cumulative_previous else NULL,
                         cumulative_settlement = if (is_cumulative)
                           cumulative_settlement else NULL,
                         retraction = retraction,
                         resolution_mode = resolution_mode, ...)
  engine$resolution_label <- .resolution_label(engine$resolution_mode,
                                               engine$is_linelist_retraction)
  list(data = engine, now = now, event_col = event_col, min_event = min_event,
       event_unit = event_unit, max_time = max_time,
       strata_cols = strata_cols, strata_levels = cell_levels)
}

#' Read a user-named logical column off a data frame, defaulting to all-FALSE.
#'
#' Used for the flags that mark partially observed rows (currently
#' `retraction_censored`).  `NA` is treated as FALSE -- a missing flag means the
#' value is not marked as a bound.
#' @param frame A data frame.
#' @param column_name Column name (character or symbol), or `NULL` for all-FALSE.
#' @param argument_name Name to quote back in the error message.
#' @keywords internal
#' @noRd
.resolve_logical_column <- function(frame, column_name, argument_name) {
  if (is.null(column_name)) return(rep(FALSE, nrow(frame)))
  requested <- if (is.character(column_name)) column_name else deparse(column_name)
  if (!requested %in% names(frame))
    cli::cli_abort(c("`{argument_name}` column {.val {requested}} not found in the data.",
                     "i" = "Available columns: {.val {names(frame)}}."))
  flag <- as.logical(frame[[requested]])
  flag[is.na(flag)] <- FALSE
  flag
}

#' Calendar date of each event-time on the 1..max_time grid (origin `min_event`).
#' @keywords internal
#' @noRd
.grid_event_dates <- function(min_event, event_unit, max_time) {
  unit <- as.character(event_unit)
  idx  <- seq_len(max_time) - 1L
  if (unit %in% c("month", "months")) {
    base <- as.POSIXlt(as.Date(min_event))
    out  <- vapply(idx, function(k) {
      d <- base; d$mon <- d$mon + k; as.numeric(as.Date(d))
    }, numeric(1))
    as.Date(out, origin = "1970-01-01")
  } else {
    mult <- if (unit %in% c("week", "weeks")) 7 else 1
    as.Date(min_event) + idx * mult
  }
}

#' Deterministic temporal-effect covariate matrix over the full event grid.
#'
#' Re-applies the temporal-effect specifications attached to `data` to a complete
#' grid of event dates (`min_event` .. `min_event + (max_time-1) * unit`) and
#' extracts the resulting effect columns.  This guarantees the covariates are
#' correctly defined for every event-time, even those with no observations or
#' those occurring after the last report but before `now`.  Returns `NULL` when
#' the data carries no temporal effects.
#' @keywords internal
#' @noRd
.temporal_effect_matrix <- function(data, min_event, event_unit, max_time, effect_cols) {
  if (length(effect_cols) == 0L) return(NULL)
  specs <- tryCatch(tbl.now::get_temporal_effects(data), error = function(e) NULL)
  if (is.null(specs) || length(specs) == 0L) return(NULL)

  grid_dates <- .grid_event_dates(min_event, event_unit, max_time)
  grid_df <- data.frame(onset = grid_dates, reported = grid_dates)

  built <- tryCatch({
    gtn <- tbl.now::tbl_now(grid_df,
                            event_date  = !!as.symbol("onset"),
                            report_date = !!as.symbol("reported"),
                            data_type = "linelist", verbose = FALSE)
    for (spec in specs) gtn <- tbl.now::add_temporal_effects(gtn, spec$t_effects)
    gtn <- tbl.now::compute_temporal_effects(gtn)
    as.data.frame(gtn)
  }, error = function(e) NULL)

  if (is.null(built)) {
    cli::cli_warn(c("Could not recompute temporal effects on the full grid; using observed-only values.",
                    "i" = conditionMessage(attr(built, "condition") %||% simpleError(""))))
    return(NULL)
  }
  present <- intersect(effect_cols, names(built))
  if (length(present) == 0L) return(NULL)
  # Convert one column at a time. as.matrix.data.frame() first promotes a mixed
  # factor/numeric frame to character, turning day-of-week labels into NA when
  # storage.mode is changed to double.
  X <- vapply(present, function(column) {
    value <- built[[column]][seq_len(max_time)]
    if (is.factor(value)) as.numeric(value) else suppressWarnings(as.numeric(value))
  }, numeric(max_time))
  if (is.null(dim(X))) X <- matrix(X, ncol = 1L)
  X[!is.finite(X)] <- 0
  colnames(X) <- present
  X
}

#' Number of whole event-units from `from` to `to` (0 at `from`).
#' Months are handled by year*12 + month arithmetic; days/weeks via difftime.
#' @keywords internal
#' @noRd
.unit_steps <- function(from, to, event_unit) {
  unit <- as.character(event_unit)
  if (unit %in% c("month", "months")) {
    fl <- as.POSIXlt(from); tl <- as.POSIXlt(to)
    (tl$year - fl$year) * 12 + (tl$mon - fl$mon)
  } else {
    div <- if (unit %in% c("week", "weeks")) 7 else 1
    round(as.numeric(difftime(to, from, units = "days")) / div)
  }
}

#' Event-level covariate matrix over the full event grid.
#'
#' User covariates attached to a `tbl_now` (via `covariates =` / add_covariates())
#' are values carried on each observation row.  This places them on the complete
#' `1..max_time` event-time grid: for each covariate column the value is taken per
#' event date (they are event-level, so constant within an event date), matched to
#' the grid, and event-times with no observation are filled with 0.  Returns `NULL`
#' when the data carry no covariates.
#' @param data A `tbl_now`.
#' @param event_col Event-date column name.
#' @param min_event Grid origin (earliest event date).
#' @param unit_steps Closure mapping a date to its 0-indexed grid position.
#' @param max_time Grid length.
#' @keywords internal
#' @noRd
.covariate_matrix <- function(data, event_col, min_event, unit_steps, max_time) {
  covariate_cols <- tryCatch(tbl.now::get_covariates(data), error = function(e) character(0))
  covariate_cols <- intersect(covariate_cols, names(as.data.frame(data)))
  if (length(covariate_cols) == 0L) return(NULL)

  observations <- as.data.frame(data)
  grid_index   <- as.integer(unit_steps(observations[[event_col]])) + 1L      # 1-indexed event-time
  X <- matrix(0.0, max_time, length(covariate_cols), dimnames = list(NULL, covariate_cols))
  for (covariate_col in covariate_cols) {
    covariate_values <- suppressWarnings(as.numeric(observations[[covariate_col]]))
    valid_rows <- !is.na(covariate_values) & !is.na(grid_index) &
      grid_index >= 1L & grid_index <= max_time
    # One value per event-time (covariates are event-level, so constant within an
    # event date): the last matching row wins.
    X[grid_index[valid_rows], covariate_col] <- covariate_values[valid_rows]
  }
  storage.mode(X) <- "double"
  X
}
