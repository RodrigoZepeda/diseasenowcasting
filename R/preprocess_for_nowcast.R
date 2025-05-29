#' Preprocess data.frame for generating a nowcast
#'
#' Function that takes a data frame with `true_date` and `report_date` and collapses to all
#' observed combinations of true_dates and report_dates controlling by the covariates
#' specified in `...`
#'
#' @inheritParams nowcast
#'
#' @param data_type Either `linedata` if each row represents a test or `counts` if there
#' is a column named `n` with counts of how many tests had that onset and report dates
#'
#' @param verbose Boolean. Whether to print the data type assumptions.
#'
#' @returns A `data.frame` with the counts for all observed delay-onset combinations.
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
#' # Ignores one date when there was no onset week
#' df <- data.frame(
#'   onset_week  = as.Date(c("1994-09-19", "1994-10-03", "1994-10-03", "1994-10-03")),
#'   report_week = as.Date(c("1994-09-19", "1994-10-03", "1994-10-10", "1994-10-10"))
#' )
#' preprocess_for_nowcast(df, "onset_week", "report_week",
#'   units = "weeks",
#'   now = as.Date("1994-10-10")
#' )
#'
#' # Ignores one date when there was no report of delay 3 mostly
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
#' set.seed(642578)
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
                                   max_delay = Inf, data_type = c("auto", "linelist", "count"),
                                   verbose = TRUE) {

  # Get whether data is count or line data
  data_type <- infer_data_type(.disease_data, data_type = data_type, verbose = verbose)

  # Group data to generate counts
  .disease_data <- .disease_data |>
    dplyr::group_by(!!as.symbol(true_date), !!as.symbol(report_date))

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

  # Get all possible onset dates in data
  min_date <- .disease_data |>
    dplyr::summarise(min_date = min(!!as.symbol(true_date))) |>
    dplyr::pull(min_date)

  # Calculate the delay column
  .disease_data <- .disease_data |>
    dplyr::mutate(.delay = as.numeric(
      difftime(!!as.symbol(report_date), !!as.symbol(true_date), units = !!units))) |>
    dplyr::mutate(.tval = 1 + as.numeric(difftime(!!as.symbol(true_date), !!min_date, units = !!units))) |>
    dplyr::filter(!!as.symbol(".delay") <= !!max_delay)


  # Get the maximum prediction date
  max_date_true <- .disease_data |>
    dplyr::summarise(max_date = max(!!as.symbol(true_date))) |>
    dplyr::pull(max_date)

  max_date_report <- .disease_data |>
    dplyr::summarise(max_date = max(!!as.symbol(report_date))) |>
    dplyr::pull(max_date)

  max_date <- min(max(max_date_true, max_date_report), now)

  #Generate d_star the maximum possible delay that could have been observed
  .disease_data <- .disease_data |>
    dplyr::filter(!!as.symbol(report_date) <= max_date) |>
    dplyr::mutate(.dstar = ceiling(as.numeric(
      difftime(max_date, !!as.symbol(true_date), units = !!units)))) |>
    dplyr::arrange(!!as.symbol(true_date), !!as.symbol(".delay"))

  return(.disease_data)
}
