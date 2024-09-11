# Functions in charge of checking every single argument in `nowcast` functions.

#' Check the onset and report dates
#'
#' @inheritParams nowcast
#' @return The `.disease_data` data.frame filtered for reports after onset.
#' @keywords internal
check_date_columns <- function(.disease_data, onset_date, report_date) {
  # Check that columns are in data
  if (!(onset_date %in% colnames(.disease_data))) {
    cli::cli_abort("{.code onset_date = {.val {onset_date}}} not found in {.code data}")
  } else if (!(report_date %in% colnames(.disease_data))) {
    cli::cli_abort("{.code report_date = {.val {report_date}}} not found in {.code data}")
  }

  # Check that they are dates
  if (!lubridate::is.Date(.disease_data[, onset_date])) {
    cli::cli_abort(
      "{.code onset_date = {.val {onset_date}}} is not a {.emph Date}. Use {.code as.Date} to transform it."
    )
  } else if (!lubridate::is.Date(.disease_data[, report_date])) {
    cli::cli_abort(
      "{.code report_date = {.val {report_date}}} is not a {.emph Date}. Use {.code as.Date} to transform it."
    )
  }

  # Check that none has report before onset
  if (any(.disease_data[, onset_date] > .disease_data[, report_date])) {
    # Get the number of observations before (for message)
    nbefore <- nrow(.disease_data)

    # Filter to keep only those with onset after report
    .disease_data <- .disease_data |>
      dplyr::filter(onset_date <= report_date)

    # Get the number of observations before (for message)
    nafter <- nrow(.disease_data)
    cli::cli_warn(
      "Some rows have a {.code report_date} ocurring before an {.code onset_date}. Dropping {.code n = {nbefore - nafter}} observations."
    )
  }

  return(.disease_data)
}

#' Check the `now` argument to be date
#'
#' @inheritParams nowcast
#' @return (invisibly) TRUE if the `now` date is achievable
#' @keywords internal
check_now <- function(.disease_data, now, onset_date) {
  # Check that now is a date
  if (!is.null(now) && !lubridate::is.Date(now)) {
    cli::cli_abort(
      "{.code now = {.val {now}}} is not a {.emph Date}. Use {.code as.Date} to transform it."
    )
  }

  # Check that now falls between dates
  if (!is.null(now)) {
    if (now <= min(.disease_data[, onset_date]) | now > max(.disease_data[, onset_date])) {
      cli::cli_abort(
        "{.code now = {.val {now}}} is outside the scope of the data's {.code onset_date}."
      )
    }
  }
  invisible(TRUE)
}

#' Check the `units` argument among the options
#'
#' @inheritParams nowcast
#' @return (invisibly) `TRUE` if the units are valid
#' @keywords internal
check_units <- function(units) {
  valid_units <- c("days", "weeks")

  # Check that units is in one of the following:
  if (!is.null(units) && !(units %in% valid_units)) {
    cli::cli_abort(
      "Ivalid {.code units = {.val {units}}}. Specify one of the following: {.val {valid_units}}",
    )
  }

  invisible(TRUE)
}

#' Check the `proportion_reported` argument
#'
#' @inheritParams nowcast
#'
#' @return (invisibly) `TRUE` if the proportion is valid
#'
#' @keywords internal
check_proportion_reported <- function(proportion_reported) {
  # Check that proportion_reported is between 0,1
  if (proportion_reported > 1 | proportion_reported <= 0) {
    cli::cli_abort(
      "{.code proportion_reported = {.val {proportion_reported}}} is not a value in (0,1]"
    )
  }

  invisible(TRUE)
}
