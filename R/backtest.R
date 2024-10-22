
#' generate_nowcast_dates
#'
#' Generates dates for the backtesting procedure given the start_date, end_date and stride
#'
#' @param start_date start_date
#'
#' @param end_date end_date
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

  cases_per_date <- .disease_data |>
                    dplyr::group_by(!!dplyr::sym(onset_date)) |>
                    dplyr::summarize(reported=dplyr::n())
  backtest_summary <- merge(pred_table, cases_per_date, by = onset_date, all.x = TRUE)
  backtest_summary$model <- model_name
  return (backtest_summary)
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

# calc_mae <- function(backtest_summary) {
#   backtest_summary$ae <- abs(backtest_summary$reported-backtest_summary$mean)
#   mae_vals <- backtest_summary |> dplyr::group_by(horizon,strata,model) |> dplyr::summarize(MAE = mean(ae),.groups='drop')
#   return (mae_vals)
# }
#
# calc_rmse <- function(backtest_summary) {
#   backtest_summary$se <- (backtest_summary$reported-backtest_summary$mean)^2
#   rmse_vals <- backtest_summary |> dplyr::group_by(horizon,strata,model) |> dplyr::summarize(RMSE = sqrt(mean(se)),.groups='drop')
#   return (rmse_vals)
# }

#' calc_mae
#'
#' Calculates mean absolute error (mae)
#'
#' @param backtest_summary results of backtest call
#'
calc_mae <- function(backtest_summary) {
  mae_vals <- backtest_summary |>
    dplyr::select(true_value=reported, prediction=mean, everything()) |>
    dplyr::mutate(sample=1) |>
    scoringutils::score(metrics='ae_median') |>
    scoringutils::summarise_scores(by = c("horizon","strata","model")) |>
    dplyr::select(horizon,strata,model,mae='ae_median')
  return (mae_vals)
}

#' calc_rmse
#'
#' Calculates root mean squared error (rmse)
#'
#' @param backtest_summary results of backtest call
#'
calc_rmse <- function(backtest_summary) {
  rmse_vals <- backtest_summary |>
    dplyr::select(true_value=reported, prediction=mean, everything()) |>
    dplyr::mutate(sample=1) |>
    scoringutils::score(metrics='se_mean') |>
    scoringutils::summarise_scores(by = c("horizon","strata","model")) |>
    dplyr::select(horizon,strata,model,rmse='se_mean') |>
    dplyr::mutate(rmse=sqrt(rmse))
  return (rmse_vals)
}

#' calc_wis
#'
#' Calculates weighted interval score (wis)
#'
#' @param backtest_summary results of backtest call
#'
calc_wis <- function(backtest_summary) {
  if(!('X50.' %in% colnames(backtest_summary)))
    backtest_summary$X50. <- backtest_summary$mean
  quantile_cols <- colnames(backtest_summary)[grepl("^X.*\\.$", colnames(backtest_summary))]

  wis_vals <- backtest_summary |>
    tidyr::pivot_longer(cols=quantile_cols,names_to='quantile',values_to='prediction') |>
    dplyr::mutate(quantile=as.numeric(sub("^X(.*)\\.$", "\\1", quantile))/100)  |>
    dplyr::select(true_value=reported, everything()) |>
    scoringutils::check_forecasts() |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("horizon","strata","model")) |>
    dplyr::select(horizon,strata,model,wis='interval_score')

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
#' and 'wis' (weighted interval score)
#'
#' @param horizons list of horizons for which the metrics be calculated.
#' Default is to calculate metrics only for horizon 0.
#'
#' @export
backtest_metrics <- function(backtest_summary, metrics, horizons=c(0))
{
  if(is.list(backtest_summary)) {
    if(!check_same_columns(backtest_summary))
      stop('Error: models cannot be compared - differnt column names in elements of backtest_summary')
    backtest_summary <- dplyr::bind_rows(backtest_summary)
  }

  backtest_summary <- backtest_summary |>
                      dplyr::filter(horizon %in% horizons) |>
                      dplyr::rename(strata=Strata_unified)

  models <- unique(backtest_summary$model)
  stratas <- unique(backtest_summary$strata)
  metrics_table <- expand.grid(horizon=horizons, strata=stratas, model=models)

  for(metric in metrics) {
    metric_results <- switch(
      metric,
      mae=calc_mae(backtest_summary),
      rmse=calc_rmse(backtest_summary),
      wis=calc_wis(backtest_summary),
      stop("Error: unsupported metric - " +metric +".")
    )
    metrics_table <- merge(metrics_table, metric_results, by=c('horizon','strata','model'))
  }
  metrics_table <- metrics_table |>
                   dplyr::arrange(model,horizon)
  return (metrics_table)
}
