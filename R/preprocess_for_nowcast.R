#' Preprocess data.frame for generating a nowcast
#'
#' Function that takes a data frame with `true_date` and `report_date` and generates all
#' possible combinations of true_dates and report_dates observable controlling by the covariates
#' specified in `...`
#'
#' @inheritParams nowcast
#'
#' @param data_type Either `linedata` if each row represents a test or `counts` if there
#' is a column named `n` with counts of how many tests had that onset and report dates
#'
#'
#' @returns A `data.frame` with all possible counts for all delay-onset combinations.
#' The new column with the counts is named `n`. Additional columns `.tval` and `.delay`
#' are added where `.tval` codifies the dates as numbers (starting at 0) and delay
#' codifies the difference between onset and report.
#'
#' @examples
#' data(denguedat)
#'
#' # Get counts by onset date and report week consider all possible delays
#' preprocess_for_nowcast(denguedat, "onset_week", "report_week",
#'   units = "weeks", now = as.Date("1990-03-05")
#' )
#'
#' # Complete one date when there was no onset week
#' df <- data.frame(
#'   onset_week  = as.Date(c("1994-09-19", "1994-10-03", "1994-10-03", "1994-10-10")),
#'   report_week = as.Date(c("1994-09-19", "1994-10-03", "1994-10-10", "1994-10-10"))
#' )
#' preprocess_for_nowcast(df, "onset_week", "report_week",
#'   units = "weeks",
#'   now = as.Date("1994-10-10")
#' )
#'
#' # Complete one date when there was no report of delay 3 mostly
#' df <- data.frame(
#'   onset_week  = as.Date(c("1994-09-19", "1994-10-03", "1994-10-03", "1994-10-10")),
#'   report_week = as.Date(c("1994-10-10", "1994-10-03", "1994-10-10", "1994-10-10"))
#' )
#' preprocess_for_nowcast(df, "onset_week", "report_week",
#'   units = "weeks",
#'   now = as.Date("1994-10-10")
#' )
#'
#' # Get counts by onset date and report week stratifying by gender and state
#' df <- data.frame(
#'   onset_week = sample(as.Date(c("1994-09-19", "1994-10-03", "1994-10-10")), 100, replace = TRUE),
#'   gender = sample(c("Male", "Female"), 100, replace = TRUE),
#'   state = sample(c("A", "B", "C", "D"), prob = c(0.5, 0.2, 0.2, 0.1), size = 100, replace = TRUE)
#' )
#' df$report_week <- df$onset_week +
#'   sample(c(lubridate::weeks(1), lubridate::weeks(2)), 100, replace = TRUE)
#' preprocess_for_nowcast(df, "onset_week", "report_week", c("gender", "state"),
#'   units = "weeks",
#'   now = as.Date("1994-09-26")
#' )
#'
#' @export
preprocess_for_nowcast <- function(.disease_data, true_date, report_date, strata = NULL, now, units,
                                   max_delay = Inf, data_type = c("auto", "linelist", "count")) {

  # Get whether data is count or line data
  data_type <- infer_data_type(.disease_data, data_type = data_type)

  # NOTE:
  # This part is done with dplyr otherwise if .disease_data were a tibble
  # it would generate an error.

  # Calculate the delay column
  .disease_data <- .disease_data |>
    dplyr::mutate(.delay = as.numeric(
      difftime(!!as.symbol(report_date), !!as.symbol(true_date), units = !!units)))

  # Get all possible onset dates in data
  min_date <- .disease_data |>
    dplyr::summarise(min_date = min(!!as.symbol(true_date))) |>
    dplyr::pull(min_date)

  # Get the maximum prediction date
  max_date <- .disease_data |>
    dplyr::summarise(max_date = max(!!as.symbol(true_date))) |>
    dplyr::pull(max_date)
  max_date <- min(max_date, now)

  all_onsets <- seq(min_date, max_date, by = units)

  # Get all possible delays in data
  min_delay <- .disease_data |>
    dplyr::summarise(min_delay = min(!!as.symbol(".delay"))) |>
    dplyr::pull(min_delay)

  max_delay <- .disease_data |>
    dplyr::summarise(max_delay = max(!!as.symbol(".delay"))) |>
    dplyr::pull(max_delay)

  all_delays <- seq(min_delay, max_delay, by = 1)

  # Get the last report date observed
  # max_report_date <- .disease_data |>
  #   dplyr::summarise(max_report_date = max(!!as.symbol(report_date))) |>
  #   dplyr::pull(max_report_date)

  # Expand the data frame to include all delays and all onsets
  all_delay_onsets <- tidyr::expand_grid(
    !!as.symbol(true_date) := all_onsets,
    !!as.symbol(".delay") := all_delays
  )

  # If there are covariates extend to include all covariates
  if (length(strata) > 0) {
    for (k in 1:length(strata)){
      all_delay_onsets <- tidyr::expand_grid(
        all_delay_onsets,
        dplyr::distinct_at(.disease_data, strata[k])
      )
    }
  }

  # Create the new data
  if (units == "days") {
    all_delay_onsets <- all_delay_onsets |>
      dplyr::mutate(
        !!as.symbol(report_date) := lubridate::days(!!as.symbol(".delay")) +
          !!as.symbol(true_date)
      )
  } else if (units == "weeks") {
    all_delay_onsets <- all_delay_onsets |>
      dplyr::mutate(
        !!as.symbol(report_date) := lubridate::weeks(!!as.symbol(".delay")) +
          !!as.symbol(true_date)
      )
  }

  # Group data to generate counts
  .disease_data <- .disease_data |>
    dplyr::group_by(!!as.symbol(true_date), !!as.symbol(report_date), !!as.symbol(".delay"))

  if (length(strata) > 0) {
    .disease_data <- .disease_data |>
      dplyr::group_by_at(.vars = strata, .add = TRUE)
  }

  # Count the data aggregating by groups
  if (data_type == "linelist") {
    .disease_data <- .disease_data |>
      dplyr::tally() |>
      dplyr::ungroup()
  } else if (data_type == "count") {
    .disease_data <- .disease_data |>
      dplyr::summarise(!!as.symbol("n") := sum(!!as.symbol("n")), .groups = "drop")
  }

  # Bind the data counts to the delay
  all_delay_onsets <- all_delay_onsets |>
    dplyr::left_join(.disease_data, by = c(true_date, report_date, ".delay", strata)) |>
    dplyr::mutate(
      !!as.symbol("n") := tidyr::replace_na(!!as.symbol("n"), 0)
    )

  # Add a time column,
  # Filter those with reports in the future,
  # Get only the data that is lower than the delay,
  # Sort by date of onset and delay
  all_delay_onsets <- all_delay_onsets |>
    dplyr::mutate(.tval = 1 + as.numeric(difftime(!!as.symbol(true_date), !!min_date, units = !!units))) |>
    dplyr::filter(!!as.symbol(report_date) <= max_date) |>
    dplyr::filter(!!as.symbol(".delay") <= !!max_delay) |>
    dplyr::arrange(!!as.symbol(true_date), !!as.symbol(".delay"))

  all_delay_onsets <- all_delay_onsets |>
    dplyr::select(!!as.symbol("n"), !!as.symbol(".tval"), !!as.symbol(".delay"), tidyr::everything())

  return(all_delay_onsets)
}
