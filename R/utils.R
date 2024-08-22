#' Automatically infer which value is `now`.
#'
#' Function returns the maximum onset date of `.disease_data` if `now = NULL`. Else
#' it check whether now is possible in the data.
#'
#' @inheritParams nowcast
#'
#' @keywords internal
infer_now <- function(.disease_data, now, onset_date) {
  # Check now
  check_now(.disease_data, now = now, onset_date = onset_date)

  # Now should be the last observed moment in time
  if (is.null(now)) {
    now <- max(.disease_data[, onset_date])
  }

  return(now)
}

#' Automatically infer which value is `units`.
#'
#' Function returns whether data is daily, weekly, monthly or yearly by
#' `date_column`.
#'
#' @inheritParams nowcast
#' @param date_column Name of a column of `.disease_data` that contains the dates.
#'
#' @keywords internal
infer_units <- function(.disease_data, units, date_column) {
  # Check units
  check_units(units)

  if (is.null(units)) {
    # Calculate the differences between consecutive dates
    date_diffs <- .disease_data[, date_column] |>
      unique() |>
      sort() |>
      diff()

    # Convert the differences to a period (days)
    min_difference <- date_diffs |>
      min() |>
      as.numeric()

    # Categorize based on the median difference
    valid_units <- c("days", "weeks")
    if (min_difference <= 1) {
      units <- "days"
    } else if (min_difference >= 6 & min_difference <= 8) {
      units <- "weeks"
    } else {
      cli::cli_abort(
        "Cannot infer time units. Specify between {.code units = {.val {valid_units}}}"
      )
    }
  }

  return(units)
}

#' Automatically infer the `data_type`
#'
#' Infers whether the data is line-data or count-data whether it has (or has not)
#' a column named `n`
#'
#' @inheritParams preprocess_for_nowcast
#'
#' @keywords internal
infer_data_type <- function(.disease_data, data_type) {
  # Get the data type
  data_type <- match.arg(data_type, c("auto", "linelist", "count"))

  # Check that there is no column `n` if linedata and that there is if counts
  if (data_type == "auto" & ("n" %in% colnames(.disease_data))) {
    data_type <- "count"
    cli::cli_alert_info(
      "Assuming data is count-data where counts are in column `n`. To change this set {.code data_type = {.val linelist}}"
    )
  } else if (data_type == "auto" & !("n" %in% colnames(.disease_data))) {
    data_type <- "linelist"
    cli::cli_alert_info(
      "Assuming data is linelist-data where each observation is a test. If you are working with count-data set {.code data_type = {.val count}}"
    )
  } else if (data_type == "linelist" & "n" %in% colnames(.disease_data)) {
    cli::cli_warn(
      "Linelist data contains a column named `n` which will be overwritten. If you are working with count-data set {.code data_type = {.val count}}"
    )
  } else if (data_type == "count" & !("n" %in% colnames(.disease_data))) {
    cli::cli_abort(
      "Count data should have a column named `n` with the number of tests per onset-report combination."
    )
  }

  return(data_type)
}
