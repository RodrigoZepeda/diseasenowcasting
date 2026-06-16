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
  #
  # NOTE: we deliberately do NOT call complete_zeroes().  The censored
  # likelihood handles event-times with zero observed cases natively (they
  # enter via S_k with k = 0), and prepare_data() already fills the full
  # max_time grid with zeros.  The covariate matrix X is built deterministically
  # from the calendar dates below, so it does not need completed rows either.
  keep_rows <- which(data[[event_col]] <= now & data[[report_col]] <= now)
  as_of <- data[keep_rows, , drop = FALSE]
  incidence <- tbl.now::to_count(as_of, to = "count-incidence")

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

  m_all <- cbind(event_num + 1L, counts, delay_num + 1L, cell_index)
  storage.mode(m_all) <- "double"

  # -- censored observations (delay known only up to an upper bound) ------------
  # tbl.now marks these with an is_censored column; for such rows the recorded
  # `.delay` is the UPPER BOUND j, and the likelihood uses log G_D(j) (the case
  # arrived with delay <= j) instead of the exact-delay term.  They are split
  # out into `m_censored`; everything else is an exact observation in `m`.
  cens_col <- tryCatch(tbl.now::get_is_censored(data), error = function(e) character(0))
  is_cens  <- if (length(cens_col) == 1L && cens_col %in% names(incidence)) {
    v <- as.logical(incidence[[cens_col]]); v[is.na(v)] <- FALSE; v
  } else rep(FALSE, nrow(m_all))

  m          <- m_all[!is_cens, , drop = FALSE]
  m_censored <- m_all[ is_cens, , drop = FALSE]
  if (nrow(m) > 0)          m          <- m[order(m[, 1], m[, 4], m[, 3]), , drop = FALSE]
  if (nrow(m_censored) > 0) m_censored <- m_censored[order(m_censored[, 1], m_censored[, 4], m_censored[, 3]), , drop = FALSE]
  d_star <- matrix(rev(seq_len(max_time)) - 1L, ncol = 1L)

  # Time-grid covariate matrix X computed DETERMINISTICALLY on the full grid.
  # Temporal effects (day-of-week, seasonality, ...) are deterministic functions
  # of the calendar date, so they are well-defined for EVERY event-time on the
  # 1..max_time grid -- including event-times with no observed cases and event-
  # times AFTER the last observation but before `now`.  Computing them from the
  # observed data alone would (incorrectly) leave those rows at zero.
  X <- .temporal_effect_matrix(data, min_event, event_unit, max_time, effect_cols)

  engine <- prepare_data(model, m,
                         m_censored = if (nrow(m_censored) > 0) m_censored else NULL,
                         X = X, d_star = d_star, max_time = max_time,
                         num_strata = num_strata, delay_only = delay_only, ...)
  list(data = engine, now = now, event_col = event_col, min_event = min_event,
       event_unit = event_unit, max_time = max_time,
       strata_cols = strata_cols, strata_levels = cell_levels)
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
  X <- as.matrix(built[seq_len(max_time), present, drop = FALSE])
  storage.mode(X) <- "double"
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
