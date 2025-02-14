
#' generate_nowcast_dates
#'
#' Generates dates for the backtesting procedure given the start_date, end_date and stride
#'
#' @param start_date start_date
#'
#' @param end_date end_date
#'
#' @param units units
#'
#' @param stride stride
#'
#' @param stride the number of time steps between two consecutive nowcasts
#'
generate_nowcast_dates <- function(start_date, end_date, units, stride) {

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
  date_seq <- seq(from = as.Date(start_date), to = as.Date(end_date), by = paste(stride, units))

  return(date_seq)
}

#' backtest
#'
#' Compute historical nowcasts for given time period
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
#' @param retrain Integer variable indicating the number of iterations for which
#' to retrain the model. Default 1 means retraining the model for each nowcast.
#' NOT IMPLEMENTED YET - currently will be retrained at each time step.
#'
#' @param quantiles list of quantiles in [0,1] defining which quantile estimates
#' will be returned
#'
#' @param min_horizon the minimum horizon in [-Inf,0] to keep from each nowcast estimates
#' (e.g. min_horizon=-5 means estimates of up to previous 5 time steps from now will be kept)
#'
#' @param model_name A model name that will be used to identify the results of the
#' backtest with the given parameters in a subsequent call to [backtest_metrics()]
#'
#' @inheritParams nowcast
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#' # Run a backtest
#' # change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
# backtest(.disease_data=denguedat,
#          onset_date="onset_week", report_date="report_week",
#          start_date = as.Date('1990-01-22'), end_date = as.Date('1990-05-22'),
#          stride = 4, min_horizon = -3,
#          method = "optimization", dist = "Normal",
#          refresh=0, model_name='model_Normal')
#' @export
backtest <- function(start_date = NULL,
                     end_date = NULL,
                     stride = 1,
                     retrain = 1,
                     quantiles = c(0.025,0.05,0.25,0.50,0.75,0.95,0.975),
                     min_horizon=0,
                     model_name  = NULL,
                     .disease_data, onset_date, report_date,
                     strata = NULL,
                     dist   = c("NegativeBinomial","Poisson","Normal","Student"),
                     units = NULL,
                     max_delay = Inf,
                     prior_only = FALSE,
                     proportion_reported = 1,
                     refresh = 250*rlang::is_interactive(),
                     control = control_default(),
                     method  = c("sampling","variational","optimization"),
                     priors  = set_priors(),
                     ...) {

  units  <- infer_units(.disease_data, units = units, date_column = onset_date)

  dates <- generate_nowcast_dates(start_date, end_date, units, stride)

  if(is.null(model_name))
    stop('Error: No model name was given')

  pred_table <- NULL
  for(date in dates) {

    now <- as.Date(date)

    predictions <- nowcast(.disease_data=.disease_data,
                           onset_date=onset_date, report_date=report_date, strata=strata,
                           dist=dist, now=now, units=units, max_delay=max_delay,
                           prior_only=prior_only, proportion_reported=proportion_reported,
                           refresh=refresh, control=control, method=method, priors=priors)

    pred_summary <- summary_nowcast(predictions, quantiles=quantiles)

    pred_summary <- data.frame(now=now,pred_summary)

    pred_summary$horizon <- as.numeric(difftime(pred_summary$onset_week, now, units=units))

    pred_summary <- pred_summary[pred_summary$horizon>=min_horizon,]

    pred_table <- rbind(pred_table,pred_summary)
  }

  stratas <- names(ncast[['data']][['strata_dict']][,-c(1,2)])

  cases_per_date <- ncast[["data"]][["original_data"]] |>
    dplyr::group_by(!!as.symbol(true_date), !!!syms(stratas)) |>
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
#' btest <- backtest(ncast, dates_to_test = c(as.Date("1990-06-11"), as.Date("1990-06-18")), model_name="model_global")
#'
#' # Run a nowcast stratified by gender
#' ncast_strat <- nowcast(denguedat, "onset_week", "report_week", now = now, strata = c("gender"),
#'   method = "optimization", seed = 2495624, iter = 10)
#'
#' # Run a backtest for the stratified model
#' btest_strat <- backtest(ncast_strat, dates_to_test = c(as.Date("1990-06-11"), as.Date("1990-06-18")), model_name="model_strat")
#'
#' # Aggregates the backtest results for the stratified model
#' btest_strat_agg <- aggregate_backtest_summary(btest_strat, "gender")
#'
#' # Compare the metrics of the non-stratified model with the aggregated stratified model
#' metrics = backtest_metrics(btest, btest_strat_agg)
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

  ##Check that the given strata to remove are starta columns of backtest_summary
  if(!all(remove_strata %in% strata)) {
    cli::cli_abort('Not all given strata to remove are strata columns of backtest_summary')
  }

  kept_strata <- setdiff(strata, remove_strata)
  true_date <- cols[3]
  value_cols <- cols[(end_idx+1):length(cols)]

  backtest_summary_agg <- backtest_summary %>%
    group_by(now, !!as.symbol(true_date), !!!syms(kept_strata)) %>%
    summarise(
      model = first(model),
      horizon = first(horizon),
      Strata_unified = "No strata",
      across(value_cols, sum),
      .groups = "drop"
    )

  if(length(kept_strata)>0) {
    backtest_summary_agg <- backtest_summary_agg |>
      tidyr::unite(col = "Strata_unified", dplyr::all_of(kept_strata), sep = " - ", remove = FALSE)
  }

  desc_cols <- c("model","now",true_date,"horizon",kept_strata,"Strata_unified")
  backtest_summary_agg <- backtest_summary_agg[,c(desc_cols,value_cols)]
  return (backtest_summary_agg)
}


#' check_same_columns
#'
#' Checks that the given list of dataframes contains the same columns
#'
#' @param df_list list of dataframes
#'
check_same_columns <- function(df_list) {
  cols <- colnames(df_list[[1]])
  all_same <- all(sapply(df_list, function(df) identical(colnames(df), cols)))
  return(all_same)
}

#' calc_mae
#'
#' Calculates mean absolute error (mae)
#'
#' @param backtest_summary results of backtest call
#'
#' @return The mean absolute error for each of the runs in [backtest()].
#'
#' @export
#'
calc_mae <- function(backtest_summary) {
  mae_vals <- backtest_summary |>
    dplyr::select(!!as.symbol("predicted") := !!as.symbol("mean"), dplyr::everything()) |>
    dplyr::mutate(sample=1) |>
    scoringutils::as_forecast_point() |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("horizon","strata","model")) |>
    dplyr::select(!!as.symbol("horizon"),
                  !!as.symbol("strata"),
                  !!as.symbol("model"),
                  !!as.symbol("mae"):='ae_point')
  return (mae_vals)
}

#' calc_ape
#'
#' Calculates absolute percent error (ape)
#'
#' @param backtest_summary results of backtest call
#'
#' @return The absolute percent error for each of the runs in [backtest()].
#'
#' @export
calc_ape <- function(backtest_summary) {

  ape_vals <- NULL
  if (requireNamespace("scoringutils", quietly = TRUE)){
    ape_vals <- backtest_summary |>
      dplyr::select(!!as.symbol("predicted") := !!as.symbol("mean"), dplyr::everything()) |>
      dplyr::mutate(sample=1) |>
      scoringutils::as_forecast_point() |>
      scoringutils::score() |>
      scoringutils::summarise_scores(by = c("horizon","Strata_unified","model","now")) |>
      dplyr::select_at(c("model","now","horizon","Strata_unified","ape"))
  } else {
    cli::cli_alert_danger("The `scoringutils` package is required to perform this operation. Please install.")
  }

  return (ape_vals)
}

#' calc_rmse
#'
#' Calculates root mean squared error (rmse)
#'
#' @param backtest_summary results of backtest call
#'
#' @return The root mean squared error for each of the runs in [backtest()].
#'
#' @export
#'
calc_rmse <- function(backtest_summary) {
  rmse_vals <- backtest_summary |>
    dplyr::select(!!as.symbol("predicted") := !!as.symbol("mean"), dplyr::everything()) |>
    dplyr::mutate(sample=1) |>
    scoringutils::as_forecast_point() |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("horizon","strata","model")) |>
    dplyr::select(!!as.symbol("horizon"),
                  !!as.symbol("strata"),
                  !!as.symbol("model"),
                  !!as.symbol("rmse"):='se_point') |>
    dplyr::mutate(rmse=sqrt(!!as.symbol("rmse")))
  return (rmse_vals)
}

#' calc_wis
#'
#' Calculates weighted interval score (wis)
#'
#' @param backtest_summary results of backtest call
#'
#' @return The weighted interval score for each of the runs in [backtest()].
#'
#' @export
#'
calc_wis <- function(backtest_summary) {
  if(!('X50.' %in% colnames(backtest_summary)))
    backtest_summary$X50. <- backtest_summary$mean
  quantile_cols <- colnames(backtest_summary)[grepl("^X.*\\.$", colnames(backtest_summary))]

  wis_vals <- backtest_summary |>
    tidyr::pivot_longer(cols=quantile_cols,names_to='quantile_level',values_to='predicted') |>
    dplyr::mutate(!!as.symbol("quantile_level"):=as.numeric(sub("^X(.*)\\.$", "\\1", !!as.symbol("quantile_level")))/100)  |>
    scoringutils::as_forecast_quantile() |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("horizon","strata","model")) |>
    dplyr::select(!!as.symbol("horizon"),
                  !!as.symbol("strata"),
                  !!as.symbol("model"),
                  !!as.symbol("wis"))

  return (wis_vals)
}

#' backtest_metrics
#'
#' Compute metrics for given backtest results
#'
#' @param backtest_summary Results of call to backtest or a list of results of backtest calls.
#'
#' @param metrics list of metrics which should be calculated.
#' Currently supporting: 'mae' (mean absolute error), 'rmse' (root mean squared error),
#' 'ape' (absolute percent error) and 'wis' (weighted interval score)
#'
#' @param horizons list of horizons for which the metrics be calculated.
#' Default is to calculate metrics only for horizon 0.
#'
#' @export
backtest_metrics <- function(..., metrics = c("mae","rmse","ape","wis"), horizons = 0){

  ##Check the models are compatible
  if(!check_same_columns(list(...))){
    cli::cli_abort('Models cannot be compared - different column names in elements of backtest_summary')
  }

  backtest_summary <- backtest_summary |>
    dplyr::filter(!!as.symbol("horizon") %in% horizons) |>
    dplyr::rename(!!as.symbol("strata") := !!as.symbol("Strata_unified"))

  models <- unique(backtest_summary$model)
  stratas <- unique(backtest_summary$strata)
  metrics_table <- expand.grid(horizon=horizons, strata=stratas, model=models)

  for(metric in metrics) {
    metric_results <- switch(
      metric,
      mae  = calc_mae(backtest_summary),
      rmse = calc_rmse(backtest_summary),
      ape = calc_ape(backtest_summary),
      wis  = calc_wis(backtest_summary),
      cli::cli_abort("Unsupported metric: {.val {metric}}.")
    )
    metrics_table <- merge(metrics_table, metric_results, by=c('horizon','strata','model'))
  }
  metrics_table <- metrics_table |>
    dplyr::arrange(!!as.symbol("model"),!!as.symbol("horizon"))

  desc_cols <- c("model","now","horizon","Strata_unified")
  value_cols <- setdiff(colnames(metrics_table),desc_cols)
  metrics_table <- metrics_table[,c(desc_cols,value_cols)]

  return(metrics_table)
}
