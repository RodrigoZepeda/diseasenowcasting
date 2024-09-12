#' Automatically infer which value is `now`.
#'
#' Function returns the maximum onset date of `.disease_data` if `now = NULL`. Else
#' it check whether now is possible in the data.
#'
#' @inheritParams nowcast
#'
#' @return The `now` value for the [nowcasting()] which can be the last date of the data
#' or specified by the user
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
#' @return Whether the data's units are `days` or `weeks`
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
#' @return Whether the data is `count` or `linelist`
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
