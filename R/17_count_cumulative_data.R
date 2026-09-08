# =============================================================================
# Leakage-safe count-cumulative preparation
# =============================================================================

#' Prepare a complete count-cumulative as-of triangle
#'
#' This is intentionally separate from the linelist/count-incidence preparation
#' path.  It trims both event and report dates to the requested origin, updates
#' the `tbl_now` origin, completes cells inside the observable triangle, and only
#' then de-accumulates levels into signed updates.
#'
#' @param data A count-cumulative `tbl_now`.
#' @param now Historical analysis origin.
#' @param settlement Finite settlement horizon in model steps.
#' @returns A `tbl_now` with `.cumulative_level`, `.signed_update`,
#'   `.previous_nonzero`, and `.observed_cell` columns.
#' @keywords internal
#' @noRd
.prepare_count_cumulative_as_of <- function(data, now, settlement = 26L) {
  if (!tbl.now::is_tbl_now(data)) {
    cli::cli_abort("`data` must be a `tbl_now` object.")
  }
  if (!identical(tbl.now::get_data_type(data), "count-cumulative")) {
    cli::cli_abort("Count-cumulative preparation requires `data_type = \"count-cumulative\"`.")
  }
  settlement <- .validate_settlement_horizon(settlement)

  event_col <- tbl.now::get_event_date(data)
  report_col <- tbl.now::get_report_date(data)
  case_count_col <- tbl.now::get_case_count(data)
  if (is.null(case_count_col) || !case_count_col %in% names(data)) {
    cli::cli_abort("Count-cumulative data must carry a case-count column.")
  }

  now <- methods::as(now, class(data[[event_col]])[1L])
  keep <- !is.na(data[[event_col]]) & !is.na(data[[report_col]]) &
    data[[event_col]] <= now & data[[report_col]] <= now
  as_of <- data[which(keep), , drop = FALSE]
  if (!nrow(as_of)) {
    cli::cli_abort("No count-cumulative cells are observable at the requested `now`.")
  }
  if (inherits(now, "Date")) {
    as_of <- tbl.now::change_now(as_of, now = now, verbose = FALSE)
  } else {
    # tbl.now::change_now() currently validates `now` as a Date. Numeric grids
    # are already filtered above, so preserve the numeric origin directly.
    attr(as_of, "now") <- now
  }

  # `complete_zeroes()` is data-type aware: leading cumulative cells are zero
  # and gaps after a published level carry that level forward.  The result stays
  # a `tbl_now`, including its date/strata/count metadata.
  as_of <- tbl.now::complete_zeroes(
    as_of,
    max_delay = settlement,
    until = max(as_of[[event_col]], na.rm = TRUE)
  )

  # Defend the package boundary even if a future tbl.now implementation changes
  # completion behavior: post-origin and post-horizon cells can never enter the
  # fitted triangle.
  keep <- as_of[[event_col]] <= now & as_of[[report_col]] <= now &
    as.integer(as_of[[".delay"]]) <= settlement
  as_of <- as_of[which(keep), , drop = FALSE]

  strata_cols <- tbl.now::get_strata(data)
  ordering_cols <- c(event_col, strata_cols, ".delay", report_col)
  row_order <- do.call(order, unname(as.data.frame(as_of)[ordering_cols]))
  as_of <- as_of[row_order, , drop = FALSE]

  grouping_cols <- c(event_col, strata_cols)
  group_key <- interaction(as.data.frame(as_of)[grouping_cols],
                           drop = TRUE, lex.order = TRUE)
  signed_update <- numeric(nrow(as_of))
  previous_nonzero <- logical(nrow(as_of))
  for (indices in split(seq_len(nrow(as_of)), group_key)) {
    levels <- as.numeric(as_of[[case_count_col]][indices])
    updates <- c(levels[1L], diff(levels))
    signed_update[indices] <- updates
    previous_nonzero[indices] <- c(FALSE, head(updates != 0, -1L))
  }

  as_of[[".cumulative_level"]] <- as.numeric(as_of[[case_count_col]])
  as_of[[".signed_update"]] <- signed_update
  as_of[[".previous_nonzero"]] <- as.numeric(previous_nonzero)
  as_of[[".observed_cell"]] <- TRUE

  stopifnot(
    tbl.now::is_tbl_now(as_of),
    identical(tbl.now::get_data_type(as_of), "count-cumulative"),
    all(as_of[[event_col]] <= now),
    all(as_of[[report_col]] <= now)
  )
  as_of
}

#' Convert prepared cumulative cells to the engine's long update frame
#' @keywords internal
#' @noRd
.count_cumulative_update_frame <- function(cells) {
  event_col <- tbl.now::get_event_date(cells)
  strata_cols <- tbl.now::get_strata(cells)
  out <- as.data.frame(cells)[c(event_col, strata_cols)]
  out[[".delay"]] <- as.integer(cells[[".delay"]])
  out[["n"]] <- as.numeric(cells[[".signed_update"]])
  out
}
