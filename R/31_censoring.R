# =============================================================================
# Censoring helpers -- mark reports whose exact delay is unreliable as censored
# =============================================================================
# A censored report carries only an UPPER BOUND on its delay ("the case arrived
# with delay <= j").  In the likelihood it contributes log G_D(j) instead of the
# exact-delay term log dG_D(j), so an extreme outlier delay stops distorting the
# fitted delay distribution.  nowcast()/prepare_from_tbl_now() read the
# tbl_now's is_censored column automatically.
# =============================================================================

#' Flag reports with an implausibly long delay as censored
#'
#' Marks every report whose reporting delay exceeds `max_delay` (in event units)
#' as *censored*: the recorded delay becomes an upper bound rather than an exact
#' value.  This is the natural response to a "delay too long" surprise (see
#' [surprise()]): instead of letting a 300-day outlier drag the delay
#' distribution, you tell the model only that the case arrived *by* that delay.
#'
#' @param data A `tbl_now` object.
#' @param max_delay Numeric.  Reports with delay (report date minus event date,
#'   in the data's event units) strictly greater than this are flagged.
#' @param quiet If `TRUE`, suppress the informational message.
#' @returns The `tbl_now` with its `is_censored` column updated (created if
#'   absent).  Re-fit with [nowcast()] to use it.
#' @examples
#' if (requireNamespace("tbl.now", quietly = TRUE)) {
#'   df <- data.frame(onset = as.Date("2020-01-01") + c(0, 0, 1, 2),
#'                    reported = as.Date("2020-01-01") + c(1, 5, 2, 300))
#'   tn <- tbl.now::tbl_now(df, event_date = onset, report_date = reported,
#'                          data_type = "linelist", verbose = FALSE)
#'   tn <- censor_delays_above(tn, max_delay = 60)   # the 300-day report -> censored
#' }
#' @export
censor_delays_above <- function(data, max_delay, quiet = FALSE) {
  if (!tbl.now::is_tbl_now(data))
    cli::cli_abort("`data` must be a tbl_now (see tbl.now::tbl_now()).")
  if (!is.numeric(max_delay) || length(max_delay) != 1L || max_delay < 0)
    cli::cli_abort("`max_delay` must be a single non-negative number.")

  event_col  <- tbl.now::get_event_date(data)
  report_col <- tbl.now::get_report_date(data)
  event_unit <- tbl.now::get_event_units(data)
  min_event  <- min(data[[event_col]], na.rm = TRUE)

  delay_u <- .unit_steps(min_event, data[[report_col]], event_unit) -
             .unit_steps(min_event, data[[event_col]],  event_unit)
  flag <- is.finite(delay_u) & delay_u > max_delay

  cens_col <- tryCatch(tbl.now::get_is_censored(data), error = function(e) character(0))
  if (length(cens_col) == 1L && cens_col %in% names(data)) {
    current <- as.logical(data[[cens_col]]); current[is.na(current)] <- FALSE
    data[[cens_col]] <- current | flag
  } else {
    data[[".is_censored"]] <- flag
    data <- tbl.now::add_is_censored(data, ".is_censored")
  }

  if (!quiet)
    cli::cli_inform(c(
      "i" = "Marked {sum(flag)} report{?s} with delay > {max_delay} event unit{?s} as censored.",
      "*" = "Their delay is now an upper bound; re-fit with {.fn nowcast} to use it."))
  data
}
