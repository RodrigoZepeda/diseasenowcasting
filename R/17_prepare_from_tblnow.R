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
#' @param ... Passed to [prepare_data()] (e.g. `gp_boundary_frac`).
#' @returns A list: `data` (the prepare_data() engine list), `now`,
#'   `event_col`, `min_event`, `event_unit`, `max_time`.
#' @keywords internal
#' @noRd
prepare_from_tbl_now <- function(data, model, now = NULL, delay_only = FALSE, ...) {
  if (!tbl.now::is_tbl_now(data)) cli::cli_abort("`data` must be a tbl_now (see tbl.now::tbl_now()).")
  event_col   <- tbl.now::get_event_date(data)
  report_col  <- tbl.now::get_report_date(data)
  event_unit  <- tbl.now::get_event_units(data)
  effect_cols <- tbl.now::get_temporal_effect_cols(data)

  now <- now %||% tbl.now::get_now(data)
  report_values <- data[[report_col]]
  if (is.null(now) || all(is.na(now))) now <- max(report_values, na.rm = TRUE)
  now <- as(now, class(data[[event_col]])[1])

  min_event  <- min(data[[event_col]], na.rm = TRUE)
  unit_steps <- function(to) .unit_steps(min_event, to, event_unit)

  # As-of view: events and reports up to `now`.  `[.tbl_now` preserves the
  # tbl_now class/attributes, so to_count() works directly (no as_tbl_now round
  # trip, which would re-resolve the date columns and can mangle attributes).
  keep_rows <- which(data[[event_col]] <= now & data[[report_col]] <= now)
  as_of <- data[keep_rows, , drop = FALSE]
  incidence <- tbl.now::to_count(as_of, to = "count-incidence")
  # Fill zero-count (event, delay) combinations so the covariate time grid is
  # complete and zero-incidence event-times are scored as true zeros.
  incidence <- tryCatch({
    inc2 <- tbl.now::complete_zeroes(incidence)
    # complete_zeroes() adds new zero-count rows but the computed temporal-effect
    # columns (day-of-week dummies etc.) are NA for those rows.  Re-running
    # compute_temporal_effects() fills them in deterministically from the event date.
    te_cols <- tbl.now::get_temporal_effect_cols(inc2)
    if (length(te_cols) > 0) inc2 <- tbl.now::compute_temporal_effects(inc2)
    inc2
  }, error = function(e) incidence)

  max_time <- as.integer(unit_steps(now) + 1L)
  event_num <- as.integer(unit_steps(incidence[[event_col]]))            # 0-indexed time
  delay_num <- as.integer(incidence[[".delay"]])
  counts    <- as.numeric(incidence[[tbl.now::get_case_count(data) %||% "n"]] %||% incidence[["n"]])

  # -- strata cell index (1..num_strata) ---------------------------------------
  # The cell levels are fixed from the FULL data (a stable K-way product of the
  # strata columns), so a cell means the same thing across as-of dates and lines
  # up with the eventual truth.  Unstratified -> a single cell.
  strata_cols <- tbl.now::get_strata(data)
  if (length(strata_cols) > 0) {
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
    cell_levels <- sort(unique(cell_string(as.data.frame(data))))
    cell_index  <- match(cell_string(as.data.frame(incidence)), cell_levels)
  } else {
    cell_levels <- "all"; cell_index <- rep(1L, length(event_num))
  }
  num_strata <- length(cell_levels)

  m <- cbind(event_num + 1L, counts, delay_num + 1L, cell_index)
  m <- m[order(m[, 1], m[, 4], m[, 3]), , drop = FALSE]
  storage.mode(m) <- "double"
  d_star <- matrix(rev(seq_len(max_time)) - 1L, ncol = 1L)

  # Time-grid covariate matrix X from the computed temporal-effect columns:
  # each temporal effect is a deterministic function of the event time, so map
  # event_num -> its effect-column values over the 1..max_time grid.
  X <- NULL
  if (length(effect_cols) > 0) {
    grid_idx <- event_num + 1L                       # 1-indexed event time per row
    X <- matrix(0.0, max_time, length(effect_cols))
    for (j in seq_along(effect_cols)) {
      first_per_time <- tapply(as.numeric(incidence[[effect_cols[j]]]), grid_idx, function(v) v[1])
      slot_idx <- as.integer(names(first_per_time))
      keep <- slot_idx >= 1 & slot_idx <= max_time
      X[slot_idx[keep], j] <- as.numeric(first_per_time)[keep]
    }
  }

  engine <- prepare_data(model, m, X = X, d_star = d_star, max_time = max_time,
                         num_strata = num_strata, delay_only = delay_only, ...)
  list(data = engine, now = now, event_col = event_col, min_event = min_event,
       event_unit = event_unit, max_time = max_time,
       strata_cols = strata_cols, strata_levels = cell_levels)
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
