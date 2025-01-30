#' Automatically infer which value is `now`.
#'
#' Function returns the maximum onset date of `.disease_data` if `now = NULL`. Else
#' it check whether now is possible in the data.
#'
#' @inheritParams nowcast
#'
#' @return The `now` value for the [nowcast()] which can be the last date of the data
#' or specified by the user
#'
#' @keywords internal
infer_now <- function(.disease_data, now, true_date, report_date) {
  # Check now
  check_now(.disease_data, now = now, true_date = true_date, report_date = report_date)

  # Now should be the last observed moment in time
  if (is.null(now)) {
    max_report <- .disease_data |>
      dplyr::summarise(!!as.symbol("max") := max(!!as.symbol(report_date))) |>
      dplyr::pull()

    max_true  <- .disease_data |>
      dplyr::summarise(!!as.symbol("max") := max(!!as.symbol(true_date))) |>
      dplyr::pull()

    now <- max(max_report, max_true)
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
#' @return Whether the data's units are `days` or `weeks`
#' @keywords internal
infer_units <- function(.disease_data, units, date_column) {
  # Check units
  check_units(units)

  if (is.null(units)) {
    # Calculate the differences between consecutive dates
    date_diffs <- .disease_data |>
      dplyr::distinct(!!as.symbol(date_column)) |>
      dplyr::arrange(!!as.symbol(date_column)) |>
      dplyr::pull(!!as.symbol(date_column)) |>
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
#' @return Whether the data is `count` or `linelist`
#'
#' @keywords internal
infer_data_type <- function(.disease_data, data_type, verbose = FALSE) {
  # Get the data type
  data_type <- match.arg(data_type, c("auto", "linelist", "count"))

  # Check that there is no column `n` if linedata and that there is if counts
  if (data_type == "auto" & ("n" %in% colnames(.disease_data))) {
    data_type <- "count"
    if (verbose){
      cli::cli_alert_info(
        "Assuming data is count-data where counts are in column `n`. To change this set {.code data_type = {.val linelist}}"
      )
    }
  } else if (data_type == "auto" & !("n" %in% colnames(.disease_data))) {
    data_type <- "linelist"
    if (verbose){
      cli::cli_alert_info(
        "Assuming data is linelist-data where each observation is a test. If you are working with count-data set {.code data_type = {.val count}}"
      )
    }
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

#' Automatically infer the temporal effect based on the units
#'
#' Function that returns a `temporal_effect` object if `t_effect` is `auto`. Else it only
#' checks whether an object is a temporal effect and whether it makes sense
#' given the units.
#'
#' @param units Either "weeks" or "days" for weekly or daily data.
#'
#' @param t_effect Either `"auto"` to infer the temporal effect or a `temporal_effect` object
#' constructed with the [temporal_effects()] function.
#' @param .default A character indicating whether the default should be for delay or epidemic process
#' or for other (an empty effect)
#'
#' @return A `temporal_effect` object for the model
#'
#' @keywords internal
infer_temporal_effect <- function(t_effect, units, .default = c("delay","epidemic","other")) {

  # Check the inputed temporal effect
  check_temporal_effect(t_effect)

  # Check the default
  if (all(t_effect == "auto")){

    #If temporal effect is auto get the default
    .default <- match.arg(.default, c("delay","epidemic","other"))

    #Construct the default
    if (units == "weeks" & .default == "delay"){

      t_effect <- temporal_effects()

    } else if (units == "weeks" & .default == "epidemic"){

      t_effect <- temporal_effects(week_of_year = TRUE)

    } else if (units == "days" & .default == "delay"){

      t_effect <- temporal_effects(day_of_week = TRUE)

    } else if (units == "days" & .default == "epidemic"){

      t_effect <- temporal_effects(week_of_year = TRUE)

    } else  {

      t_effect <- temporal_effects()

    }
  }

  # Check that options aren't weird----

  # Check that weekly data doesn't have day of week effects
  if (units == "weeks"){
    if (unlist(t_effect["day_of_week"]) | unlist(t_effect["weekend"]) | unlist(t_effect["day_of_month"])){
      cli::cli_alert_danger(
        "Daily effects are not suggested for weekly data. Are you sure you are setting the {.code temporal_effects()} correctly?"
      )
    }
  }

  # Check that weekend and day of week effects aren't set up at the same time
  if (unlist(t_effect["day_of_week"]) & unlist(t_effect["weekend"])){
    cli::cli_alert_danger(
      "Weekend effects are already included in day of the week effects. Are you sure you need both in your {.code temporal_effects()}?"
    )
  }

  # Check that week and month effects aren't both included
  if (unlist(t_effect["week_of_year"]) & unlist(t_effect["month_of_year"])){
    cli::cli_alert_danger(
      "Monthly effects are usually included in week of year effects. Are you sure you need both in your {.code temporal_effects()}?"
    )
  }

  return(t_effect)
}


#' Transforms an array into a list of lists
#'
#' Function that takes an array and transforms it into lists of lists
#' this is mainly for interacting with [Rcpp::cppFunction()].
#'
#' @param last_dim_as Either 'vector', 'matrix'  or 'scalar'  depending on what
#' we want for the last dimension
#'
#' @param my_array The array to transform
#'
#' @return A list of lists with the same structure as the array
#'
#' @keywords internal
array_to_list <- function(my_array, last_dim_as = "vector"){

  #Match argument
  .last_dim_as <- match.arg(last_dim_as, c("vector","scalar","matrix"))

  #Get array dimensions
  dims <- dim(my_array)

  lagval <- switch(.last_dim_as,
                   scalar = 0,
                   vector = 1,
                   matrix = 2)

  #Check whether the array warrants it
  if (length(dims) < lagval){
    cli::cli_abort("Cannot convert array of dimensions {length(dims)} to {.last_dim_as}")
  } else if (length(dims) == 1 & lagval == 1){
    return(as.vector(my_array))
  } else if (length(dims) == 2 & lagval == 2){
    return(as.matrix(my_array))
  }

  # Dynamically build the nested lapply structure based on the number of dimensions
  expr_1 <- ""
  expr_2 <- "["
  for (i in 1:(length(dims) - lagval)) {
    expr_1 <- paste0(expr_1, "\nlapply(1:", dims[i], ", function(x_", i, "){")
    expr_2 <- paste0(expr_2, "x_",i, ifelse(i == length(dims), "", ","))
  }

  if (.last_dim_as == "vector"){
    expr_2 <- paste0("\n\tas.vector(my_array",expr_2, "])")
  } else if (.last_dim_as == "matrix") {
    expr_2 <- paste0("\n\tas.matrix(my_array",expr_2, ",])")
  } else {
    expr_2 <- paste0("\n\tmy_array",expr_2, "]")
  }
  expr   <- paste0(expr_1, expr_2, "\n", paste0(rep("})", (length(dims) - lagval)), collapse = ""))

  # Evaluate the generated expression
  return(eval(parse(text = expr)))

}

#' Check whether a date is a weekday vs weekend
#'
#' Function that checks whether a date object is a weekday or weekend.
#'
#' @param date A date object
#'
#' @examples
#' is_weekday(as.Date("2020-04-22"))
#' is_weekday(as.Date("2020-04-19"))
#'
#' @references
#' From https://stackoverflow.com/a/60346779/5067372
#'
#' @export
is_weekday <- function(date){
  lubridate::wday(date, week_start = 1) < 6
}
