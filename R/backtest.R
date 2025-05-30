#' Generate nowcast dates
#'
#' Generates dates for the backtesting procedure given the start_date, end_date and stride
#'
#' @param ncast A nowcaster object generated with [nowcast()]
#'
#' @param start_date start_date
#'
#' @param end_date end_date
#'
#' @param stride the number of time steps between two consecutive nowcasts
#'
#' @param subsample If an integer, how much to subsample the dates; if NULL then runs all dates.
#'
#' @keywords internal
generate_nowcast_dates <- function(start_date, end_date, ncast, stride, subsample = NULL) {

  if(is.null(start_date)) {
    stop("Error: Missing start date.")
  }
  if(is.null(end_date) ) {
    stop("Error: Missing end date.")
  }

  # Check if the start and end dates are valid
  if(as.Date(start_date) > as.Date(end_date)) {
    stop("Error: start date must be earlier than or equal to end date.")
  }

  # Check if the stride is a positive integer
  if(!is.numeric(stride) || stride <= 0 || (stride %% 1 != 0)) {
    stop("Error: stride must be a positive integer.")
  }

  # Generate the sequence of dates
  units     <- ncast[["data"]][["call_parameters"]][["units"]]
  true_date <- ncast[["data"]][["call_parameters"]][["true_date"]]

  possible_date_seq  <- ncast[["data"]][["original_data"]] |>
    dplyr::distinct_at(true_date) |>
    dplyr::filter(!!as.symbol(true_date) >= as.Date(start_date)) |>
    dplyr::filter(!!as.symbol(true_date) <= as.Date(end_date)) |>
    dplyr::pull()

  date_seq <- seq(from = min(possible_date_seq), to = max(possible_date_seq), by = paste(stride, units))

  # Sample
  if (!is.null(subsample)){
    date_seq <- sort(sample(date_seq, size = min(subsample, length(date_seq))))
  }

  return(date_seq)
}

#' Infer the dates for backtesting
#'
#' Infers the start (initial) or  end date one needs to consider for backtesting
#'
#' @inheritParams backtest
#'
#' @return A date from which backtesting is possible to start or end (respectively)
#'
#' @name infer_date
#'
#' @export
infer_start_date <- function(ncast){

  #Get the minimum date
  ncast_min_date <- ncast[["data"]][["preprocessed_data"]] |>
    dplyr::summarise(!!as.symbol("min") := min(!!as.symbol(ncast[["data"]][["call_parameters"]][["true_date"]]))) |>
    dplyr::pull(!!as.symbol("min"))

  #Get the maximum date
  ncast_max_date <- ncast[["data"]][["preprocessed_data"]] |>
    dplyr::summarise(!!as.symbol("max") := max(!!as.symbol(ncast[["data"]][["call_parameters"]][["true_date"]]))) |>
    dplyr::pull(!!as.symbol("max"))

  #Get the maximum delay
  ncast_delay   <- ceiling(ncast[["data"]][["call_parameters"]][["num_delays"]]/2)

  #Get the units
  ncast_units   <- ncast[["data"]][["call_parameters"]][["units"]]

  #Get the initial day to nowcast = max delay + min date
  if (ncast_units == "weeks"){
    start_date <- ncast_min_date + lubridate::weeks(ncast_delay)
  } else if (ncast_units == "days"){
    start_date <- ncast_min_date + lubridate::days(ncast_delay)
  } else {
    cli::cli_abort("Invalid units: {.val {ncast_units}}")
  }

  #Check the initial day is in nowcast
  if (start_date <= ncast_min_date | start_date > ncast_max_date) {
    cli::cli_abort(
      "Unable to automatically infer the initial date for backtesting. Please set up the `start_date` manually."
    )
  }

  return(start_date)

}

#' @rdname infer_date
#' @export
infer_end_date <- function(ncast, start_date){

  #Get the minimum date
  ncast_min_date <- ncast[["data"]][["preprocessed_data"]] |>
    dplyr::summarise(!!as.symbol("min") := min(!!as.symbol(ncast[["data"]][["call_parameters"]][["true_date"]]))) |>
    dplyr::pull(!!as.symbol("min"))

  #Get the maximum date
  ncast_max_date <- ncast[["data"]][["preprocessed_data"]] |>
    dplyr::summarise(!!as.symbol("max") := max(!!as.symbol(ncast[["data"]][["call_parameters"]][["true_date"]]))) |>
    dplyr::pull(!!as.symbol("max"))

  #Get the maximum delay
  ncast_delay   <- ceiling(ncast[["data"]][["call_parameters"]][["num_delays"]]/2)

  #Get the units
  ncast_units   <- ncast[["data"]][["call_parameters"]][["units"]]

  #Get the initial day to nowcast = max delay + min date
  if (ncast_units == "weeks"){
    end_date <- ncast_max_date - lubridate::weeks(ncast_delay)
  } else if (ncast_units == "days"){
    end_date <- ncast_max_date - lubridate::days(ncast_delay)
  } else {
    cli::cli_abort("Invalid units: {.val {ncast_units}}")
  }

  #Check the initial day is in nowcast
  if (end_date <= start_date | end_date > ncast_max_date) {
    cli::cli_abort(
      "Unable to automatically infer the dates for backtesting. Please set up the `start_date` and `end_date` manually."
    )
  }

  return(end_date)

}

#' Backtest
#'
#' Compute historical nowcasts for given time period
#'
#' @param ncast A `nowcaster` object generated by [nowcast()]
#'
#' @param start_date An object of datatype \code{Date} indicating the date at which
#' to start the historical nowcasting (first 'now' date to nowcast).
#'
#' @param end_date An object of datatype \code{Date} indicating the date at which
#' to end the historical nowcasting (last 'now' date to nowcast).
#'
#' @param stride Integer variable indicating the number of time steps between
#' two consecutive nowcasts. Default 1 means nowcasting for each date between
#' start and end,
#'
#' @param dates_to_test Specific values of moments `now` to test the nowcast. If `dates_to_test`
#' is given this overrides `start_date`, `end_date` and `stride`.
#'
#' @param subsample Either NULL or an integer specifying how large of a subsample from the
#' `dates_to_test` to use for the `backtest`. If `NULL` (default) then it uses all of the dates.
#'
#' @param retrain Integer variable indicating the number of iterations for which
#' to retrain the model. Default 1 means retraining the model for each nowcast.
#' NOT IMPLEMENTED YET - currently will be retrained at each time step.
#'
#' @param quantiles list of quantiles between 0 and 1 defining which quantile estimates
#' will be returned
#'
#' @param min_horizon the minimum horizon (value <= 0) to keep from each nowcast estimates
#' (e.g. `min_horizon=-5` means estimates of up to previous 5 time steps from now will be kept)
#'
#' @param model_name A model name that will be used to identify the results of the
#' backtest with the given parameters in a subsequent call to [backtest_metrics()]
#'
#' @param ... Additional arguments to pass to [update.nowcaster()]
#'
#' @inheritParams nowcast
#'
#' @return A tibble with as many rows as `dates_to_test` are given. Each row represents a different `now`
#' with the following columns:
#' * `now`: The date nowcasted
#' * `true_date`: The column corresponding to the `true_date` (has the `true_date`'s name).
#' * `mean`: The mean estimate of the `nowcast`.
#' * `sd`: The standard deviation estimate of the `nowcast`.
#' * `median`: The median estimate of the `nowcast`.
#' * `x%`: Quantile columns corresponding to the estimated `xth` quantile.
#' * `Strata_unified`: A column containing the strata modeled.
#' * `observed`: The value actually obnserved for that date
#' * `model`: The name of the fitted model
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#' # Run a nowcast with very few iterations
#' # change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
#' now <- as.Date("1990-10-01")
#' ncast <- nowcast(denguedat, "onset_week", "report_week", now = now,
#'   method = "optimization", seed = 2495624, iter = 10)
#'
#' # Run a backtest for the model checking the model fit for two dates:
#' btest <- backtest(ncast, dates_to_test = c(as.Date("1990-06-11"), as.Date("1990-06-18")))
#'
#' #The following examples are slow:
#' \dontrun{
#' # Run a backtest for the model automatically selecting 3 possible dates at random
#' btest2 <- backtest(ncast, subsample = 3)
#'
#' # Run a backtest for the model using all possible dates
#' btest3 <- backtest(ncast)
#' }
#' @export
backtest <- function(ncast,
                     start_date    = infer_start_date(ncast),
                     end_date      = infer_end_date(ncast, start_date = start_date),
                     stride        = 1,
                     subsample     = NULL,
                     dates_to_test = generate_nowcast_dates(start_date, end_date, ncast, stride, subsample = subsample),
                     retrain       = 1,
                     quantiles     = c(0.025, 0.05, 0.25, 0.50, 0.75, 0.95, 0.975),
                     min_horizon   = 0,
                     model_name    = NULL,
                     refresh       = 250*rlang::is_interactive(),
                     ...) {

  #Check ncast is a nowcaster
  if (!inherits(ncast, "nowcaster")){
    cli::cli_abort("A {.code nowcaster} object generated by {.help nowcast()} needs to be used.")
  }

  #Default model name
  if (is.null(model_name)){
    model_name <- paste0("model_",format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"),"_", sample.int(.Machine$integer.max,1))
  }

  #Get the true date column
  true_date <- ncast[["data"]][["call_parameters"]][["true_date"]]

  units     <- ncast[["data"]][["call_parameters"]][["units"]]

  pred_table <- NULL
  for(date in dates_to_test) {

    now <- as.Date(date)

    #Refit and use initial values from previous fit
    predictions  <- update.nowcaster(ncast, new_data = ncast[["data"]][["original_data"]], now = now, refresh = refresh, ...)

    pred_summary <- summary.nowcaster(predictions, quantiles=quantiles)

    pred_summary <- dplyr::tibble(now=now, pred_summary)

    pred_summary <- pred_summary |>
      dplyr::mutate(!!as.symbol("horizon") := as.numeric(difftime(!!as.symbol(true_date), !!now, units=!!units))) |>
      dplyr::filter(!!as.symbol("horizon") >= !!min_horizon)

    pred_table   <- rbind(pred_table, pred_summary)
  }

  stratas <- names(ncast[['data']][['strata_dict']][,-c(1,2)])

  cases_per_date <- ncast[["data"]][["original_data"]] |>
    dplyr::group_by(!!as.symbol(true_date), dplyr::all_of(dplyr::pick(stratas))) |>
    dplyr::summarize(!!as.symbol("observed") := dplyr::n(), .groups = "drop")

  backtest_summary <- pred_table |>
    dplyr::left_join(cases_per_date, by = c(true_date,stratas)) |>
    dplyr::mutate(!!as.symbol("model") := !!model_name)

  first_cols <- c("model","now",true_date,"horizon",stratas,"Strata_unified","observed")
  last_cols <- setdiff(colnames(backtest_summary),first_cols)
  backtest_summary <- backtest_summary[,c(first_cols,last_cols)]

  return (backtest_summary)
}


#' aggregate_backtest_summary
#'
#' Aggregate the backtest summary by removing stratas.
#' A utility function for comparing the results of a stratified model to a non stratified model.
#' The results of the stratified model backtest summary should be aggregated before calling backtest_metrics
#' together with the backtest summary of a non-stratified model.
#'
#' @param backtest_summary results of [backtest()]
#' @param remove_strata vector of strata columns found inside backtest_summary to remove
#'
#' @return The aggregated backtest_summary
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#' # Run a nowcast with very few iterations
#' # change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
#' now <- as.Date("1990-10-01")
#' ncast <- nowcast(denguedat, "onset_week", "report_week", now = now,
#'   method = "optimization", seed = 2495624, iter = 10)
#'
#' # Run a backtest for the model checking the model fit for two dates:
#' dates_to_test <- c(as.Date("1990-06-11"), as.Date("1990-06-18"))
#' btest <- backtest(ncast, dates_to_test=dates_to_test , model_name="model_global")
#'
#' # Run a nowcast stratified by gender
#' ncast_strat <- nowcast(denguedat, "onset_week", "report_week", now = now, strata = c("gender"),
#'   method = "optimization", seed = 2495624, iter = 10)
#'
#' # Run a backtest for the stratified model
#' btest_strat <- backtest(ncast_strat, dates_to_test=dates_to_test, model_name="model_strat")
#'
#' # Aggregates the backtest results for the stratified model
#' btest_strat_agg <- aggregate_backtest_summary(btest_strat, "gender")
#'
#' # Compare the metrics of the non-stratified model with the aggregated stratified model
#' metrics = backtest_metrics(btest, btest_strat_agg)
#'
#' @importFrom rlang .data
#'
#' @export
aggregate_backtest_summary <- function(backtest_summary, remove_strata) {

  cols <- colnames(backtest_summary)

  # Find all strata columns - assumes given structure where
  # strata columns are located between horizon and Strata_unified
  start_idx <- which(cols == "horizon")
  end_idx <- which(cols == "Strata_unified")
  # Ensure valid indices
  if (length(start_idx) == 0 | length(end_idx) == 0 | start_idx > end_idx) {
    cli::cli_abort('Unexpected structure of backtest_summary')
  }
  strata <- cols[(start_idx + 1):(end_idx - 1)]

  ## Check that the given strata to remove are strata columns of backtest_summary
  if (!all(remove_strata %in% strata)) {
    cli::cli_abort('Not all given strata to remove are strata columns of backtest_summary')
  }

  kept_strata <- setdiff(strata, remove_strata)
  true_date <- cols[3]
  value_cols <- cols[(end_idx + 1):length(cols)]

  if (length(kept_strata) > 0) {
    backtest_summary_agg <- backtest_summary |>
      dplyr::group_by(.data$now, !!as.symbol(true_date), dplyr::all_of(dplyr::pick(kept_strata))) |>
      dplyr::summarise(
        model = dplyr::first(.data$model),
        horizon = dplyr::first(.data$horizon),
        dplyr::across(value_cols, sum),
        .groups = "drop"
      ) |>
      tidyr::unite(col = "Strata_unified", dplyr::all_of(kept_strata), sep = " - ", remove = FALSE)
  } else {
    backtest_summary_agg <- backtest_summary |>
      dplyr::group_by(.data$now, !!as.symbol(true_date)) |>
      dplyr::summarise(
        model = dplyr::first(.data$model),
        horizon = dplyr::first(.data$horizon),
        Strata_unified = "No strata",
        dplyr::across(value_cols, sum),
        .groups = "drop"
      )
  }

  desc_cols <- c("model", "now", true_date, "horizon", kept_strata, "Strata_unified")
  backtest_summary_agg <- backtest_summary_agg[, c(desc_cols, value_cols)]
  return(backtest_summary_agg)
}


#' Check same columns
#'
#' Checks that the given list of dataframes contains the same columns
#'
#' @param df_list list of dataframes
#'
#' @keywords internal
check_same_columns <- function(df_list) {
  cols <- colnames(df_list[[1]])
  all_same <- all(sapply(df_list, function(df) identical(colnames(df), cols)))
  return(all_same)
}


#' calc_mae
#'
#' Calculates mean absolute error (mae)
#'
#' @param backtest_summary results of [backtest()]
#'
#' @return The Mean Absolute Error for each of the runs in [backtest()].
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#' # Run a nowcast with very few iterations
#' # change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
#' now <- as.Date("1990-10-01")
#' ncast <- nowcast(denguedat, "onset_week", "report_week", now = now,
#'   method = "optimization", seed = 2495624, iter = 10)
#'
#' # Run a backtest for the model checking the model fit for two dates:
#' btest <- backtest(ncast, dates_to_test = c(as.Date("1990-06-11"), as.Date("1990-06-18")))
#'
#' # Get the mean absolute error with the scoringutils package
#' if (requireNamespace("scoringutils", quietly = TRUE)){
#'   calc_mae(btest)
#' }
#'
#' @export
calc_mae <- function(backtest_summary) {

  mae_scores <- NULL
  if (requireNamespace("scoringutils", quietly = TRUE)){
    mae_scores <- backtest_summary |>
      dplyr::select(!!as.symbol("predicted") := !!as.symbol("mean"), dplyr::everything()) |>
      scoringutils::as_forecast_point() |>
      scoringutils::score() |>
      scoringutils::summarise_scores(by = c("horizon","Strata_unified","model","now")) |>
      dplyr::select_at(c("model","now","horizon","Strata_unified","ae_point")) |>
      dplyr::rename(!!as.symbol("mae") := "ae_point")
  } else {
    cli::cli_alert_danger("The `scoringutils` package is required to perform this operation. Please install.")
  }

  return(mae_scores)
}

#' calc_ape
#'
#' Calculates the absolute percent error (ape)
#'
#' @param backtest_summary results of backtest call
#'
#' @return The Absolute Percent Error for each of the runs in [backtest()].
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#' # Run a nowcast with very few iterations
#' # change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
#' now <- as.Date("1990-10-01")
#' ncast <- nowcast(denguedat, "onset_week", "report_week", now = now,
#'   method = "optimization", seed = 2495624, iter = 10)
#'
#' # Run a backtest for the model checking the model fit for two dates:
#' btest <- backtest(ncast, dates_to_test = c(as.Date("1990-06-11"), as.Date("1990-06-18")))
#'
#' # Get the rmse with the scoringutils package
#' if (requireNamespace("scoringutils", quietly = TRUE)){
#'   calc_ape(btest)
#' }
#'
#' @export
calc_ape <- function(backtest_summary) {

  ape_scores <- NULL
  if (requireNamespace("scoringutils", quietly = TRUE)){
    ape_scores <- backtest_summary |>
      dplyr::select(!!as.symbol("predicted") := !!as.symbol("mean"), dplyr::everything()) |>
      dplyr::mutate(sample=1) |>
      scoringutils::as_forecast_point() |>
      scoringutils::score() |>
      scoringutils::summarise_scores(by = c("horizon","Strata_unified","model","now")) |>
      dplyr::select_at(c("model","now","horizon","Strata_unified","ape"))
  } else {
    cli::cli_alert_danger("The `scoringutils` package is required to perform this operation. Please install.")
  }

  return (ape_scores)
}

#' calc_rmse
#'
#' Calculates root mean squared error (rmse)
#'
#' @param backtest_summary results of backtest call
#'
#' @return The Root Mean Squared Error for each of the runs in [backtest()].
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#' # Run a nowcast with very few iterations
#' # change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
#' now <- as.Date("1990-10-01")
#' ncast <- nowcast(denguedat, "onset_week", "report_week", now = now,
#'   method = "optimization", seed = 2495624, iter = 10)
#'
#' # Run a backtest for the model checking the model fit for two dates:
#' btest <- backtest(ncast, dates_to_test = c(as.Date("1990-06-11"), as.Date("1990-06-18")))
#'
#' # Get the rmse with the scoringutils package
#' if (requireNamespace("scoringutils", quietly = TRUE)){
#'   calc_rmse(btest)
#' }
#'
#' @export
calc_rmse <- function(backtest_summary) {

  rmse_scores <- NULL
  if (requireNamespace("scoringutils", quietly = TRUE)){
    rmse_scores <- backtest_summary |>
      dplyr::select(!!as.symbol("predicted") := !!as.symbol("mean"), dplyr::everything()) |>
      dplyr::mutate(sample=1) |>
      scoringutils::as_forecast_point() |>
      scoringutils::score() |>
      scoringutils::summarise_scores(by = c("horizon","Strata_unified","model","now")) |>
      dplyr::select_at(c("model","now","horizon","Strata_unified","se_point")) |>
      dplyr::mutate(!!as.symbol("rmse") := sqrt(!!as.symbol("se_point"))) |>
      dplyr::select(-!!as.symbol("se_point"))
  } else {
    cli::cli_alert_danger("The `scoringutils` package is required to perform this operation. Please install.")
  }

  return (rmse_scores)
}


#' get_quantile_metrics
#'
#' Returns vector of supported quantile metrics based on \code{\link[scoringutils]{get_metrics.forecast_quantile}}
#' from [scoringutils::get_metrics()].
#'
#' @export
get_quantile_metrics <- function() {

  metrics <-  c("wis","overprediction","underprediction","dispersion","bias",
                "interval_coverage_50","interval_coverage_90","ae_median")
  return (metrics)
}


#' calc_quantile_scores
#'
#' Calculates quantile-based scores
#'
#' @param backtest_summary results of backtest call
#'
#' @param metrics the quantile metrics to calculate - see \code{\link{get_quantile_metrics}}
#'
#' @return The weighted interval score for each of the runs in [backtest()].
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#' # Run a nowcast with very few iterations
#' # change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
#' now <- as.Date("1990-10-01")
#' ncast <- nowcast(denguedat, "onset_week", "report_week", now = now,
#'   method = "optimization", seed = 2495624, iter = 10)
#'
#' # Run a backtest for the model checking the model fit for two dates:
#' btest <- backtest(ncast, dates_to_test = c(as.Date("1990-06-11"), as.Date("1990-06-18")))
#'
#' # Get the rmse with the scoringutils package
#' if (requireNamespace("scoringutils", quietly = TRUE)){
#'   metrics = c("wis","dispersion","bias")
#'   calc_quantile_scores(btest,metrics=metrics)
#' }
#'
#' @export
calc_quantile_scores <- function(backtest_summary, metrics) {

  #Get the quantiles
  quantile_cols <- colnames(backtest_summary)[grepl(".*\\%", colnames(backtest_summary))]

  #Check median is included
  if (!("50%" %in% quantile_cols)){
    backtest_summary <- backtest_summary |>
      dplyr::rename(!!as.symbol("50%") := !!as.symbol("median"))

    quantile_cols <- c(quantile_cols, "50%")
  }

  quantile_metrics <- get_quantile_metrics()
  non_valid_metrics <- setdiff(metrics,quantile_metrics)
  if(length(non_valid_metrics)>0) {
    cli::cli_abort("Unsupported quantile metrics: {.val {non_valid_metrics}}.")
  }

  quantile_scores <- NULL
  if (requireNamespace("scoringutils", quietly = TRUE)){
    quantile_scores <- backtest_summary |>
      tidyr::pivot_longer(cols = quantile_cols, names_to = 'quantile_level', values_to = 'predicted') |>
      dplyr::mutate(!!as.symbol("quantile_level") := as.numeric(stringr::str_remove_all(!!as.symbol("quantile_level"), "\\%"))/100)  |>
      scoringutils::as_forecast_quantile() |>
      scoringutils::score() |>
      scoringutils::summarise_scores(by = c("horizon","Strata_unified","model","now")) |>
      dplyr::select_at(c("model","now","horizon","Strata_unified",metrics))

  } else {
    cli::cli_alert_danger("The `scoringutils` package is required to perform this operation. Please install.")
  }

  return (quantile_scores)
}

#' backtest_metrics
#'
#' Compute metrics for given backtest results
#'
#' @param ... Results of calls to backtest.
#'
#' @param metrics list of metrics which should be calculated.
#' Currently supporting: 'mae' (mean absolute error), 'rmse' (root mean squared error),
#' 'ape' (absolute percent error) and 'quantile' for all quantile-based metrics or input specific quantile-base metrics from \code{\link{get_quantile_metrics}}.
#'
#' @param horizons vector of horizons for which the metrics be calculated.
#' Default is to calculate metrics only for horizon 0 (the nowcast).
#'
#' @return A backtest_metrics object - a table with the computed metrics.
#'
#' @examples
#' #These examples require the `scoringutils` function
#' if (requireNamespace("scoringutils", quietly = TRUE)){
#'
#' # Load the data
#' data(denguedat)
#'
#' # In this example we will test two models
#' now    <- as.Date("1990-10-01")
#' ncast1 <- nowcast(denguedat, "onset_week", "report_week", now = now,
#'   method = "optimization", seed = 2495624, iter = 10)
#'
#' ncast2 <- nowcast(denguedat, "onset_week", "report_week", now = now,
#'   method = "optimization", seed = 2495624, iter = 10, dist = "Poisson")
#'
#' # Run a backtest for each of the models
#' btest1 <- backtest(ncast1, dates_to_test = as.Date("1990-06-11"), model_name = "Classic")
#' btest2 <- backtest(ncast2, dates_to_test = as.Date("1990-06-11"), model_name = "Poisson")
#'
#' # Compare the models to select the best model
#' backtest_metrics(btest1, btest2)
#' }
#' @export
backtest_metrics <- function(..., metrics = c("mae","rmse","ape","quantile"), horizons = 0){

  ##Check the models are compatible
  if(!check_same_columns(list(...))){
    cli::cli_abort('Models cannot be compared - different column names in elements of backtest_summary')
  }

  backtest_summary <- dplyr::bind_rows(...)

  #Check the summary
  backtest_summary <- backtest_summary |>
    dplyr::filter(!!as.symbol("horizon") %in% horizons)


  #Get the different strata and models
  now     <- backtest_summary |> dplyr::distinct(!!as.symbol("now")) |> dplyr::pull()
  models  <- backtest_summary |> dplyr::distinct(!!as.symbol("model")) |> dplyr::pull()
  stratas <- backtest_summary |> dplyr::distinct(!!as.symbol("Strata_unified")) |> dplyr::pull()

  metrics_table <- tidyr::expand_grid(horizon=horizons, Strata_unified=stratas, model=models,now=now)

  quantile_metrics <- get_quantile_metrics()

  for(metric in metrics) {

    #Calculate the metric
    if(metric=="mae")
      metric_results = calc_mae(backtest_summary)
    else if(metric=="rmse")
      metric_results = calc_rmse(backtest_summary)
    else if(metric=="ape")
      metric_results = calc_ape(backtest_summary)
    else if(metric=="quantile")
      metric_results = calc_quantile_scores(backtest_summary, quantile_metrics)
    else if(metric %in% quantile_metrics)
      metric_results = calc_quantile_scores(backtest_summary, metric)
    else
        cli::cli_abort("Unsupported metric: {.val {metric}}.")

    metrics_table <- dplyr::left_join(metrics_table, metric_results, by=c('horizon','Strata_unified','model','now'))
  }

  metrics_table <- metrics_table |>
    dplyr::arrange(!!as.symbol("model"),!!as.symbol("horizon"))

  desc_cols <- c("model","now","horizon","Strata_unified")
  value_cols <- setdiff(colnames(metrics_table),desc_cols)
  metrics_table <- metrics_table[,c(desc_cols,value_cols)]

  metrics_table <- structure(metrics_table, class = c("backtest_metrics", "data.frame"))

  return(metrics_table)
}

