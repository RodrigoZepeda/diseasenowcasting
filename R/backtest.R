library(dplyr)
library(tidyr)
library(scoringutils)

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
#' @param probs list of probabilities in [0,1] defining which quantile estimates
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
#' @export
backtest <- function(start_date = NULL,
                     end_date = NULL,
                     stride = 1,
                     retrain = 1,
                     probs = c(0.025,0.05,0.25,0.50,0.75,0.95,0.975),
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

    pred_summary <- summary_nowcast(predictions)

    pred_summary <- data.frame(now=now,pred_summary)

    pred_summary$horizon <- as.numeric(difftime(pred_summary$onset_week, now, units=units))

    pred_summary <- pred_summary[pred_summary$horizon>=min_horizon,]

    pred_table <- rbind(pred_table,pred_summary)
  }

  cases_per_date <- .disease_data %>% group_by(!!sym(onset_date)) %>% summarize(reported=n())
  backtest_summary <- merge(pred_table, cases_per_date, by = onset_date, all.x = TRUE)
  backtest_summary$model <- model_name
  return (backtest_summary)
}


check_same_columns <- function(df_list) {
  cols <- colnames(df_list[[1]])
  all_same <- all(sapply(df_list, function(df) identical(colnames(df), cols)))
  return(all_same)
}

calc_mae <- function(backtest_summary) {
  backtest_summary$ae <- abs(backtest_summary$reported-backtest_summary$Mean)
  mae_vals <- backtest_summary %>% group_by(horizon,model) %>% summarize(MAE = mean(ae),.groups='drop')
  return (mae_vals)
}

calc_rmse <- function(backtest_summary) {
  backtest_summary$se <- (backtest_summary$reported-backtest_summary$Mean)^2
  rmse_vals <- backtest_summary %>% group_by(horizon,model) %>% summarize(RMSE = sqrt(mean(se)),.groups='drop')
  return (rmse_vals)
}

calc_wis <- function(backtest_summary) {
  if(!('q50' %in% colnames(backtest_summary)))
    backtest_summary$q50 <- backtest_summary$Mean
  quantile_cols <- colnames(backtest_summary)[startsWith(colnames(backtest_summary), "q")]
  df <- backtest_summary %>%
    pivot_longer(cols=quantile_cols,names_to='quantile',values_to='prediction') %>%
    mutate(quantile=as.numeric(sub("^q", "", quantile))/100) %>%
    select(true_value=reported, everything())

  wis_vals <- df %>%
              check_forecasts() %>%
              score() %>%
              summarise_scores(by = c("horizon","model")) %>%
              select(horizon,model,WIS='interval_score')
  return (wis_vals)
}

#' backtest_metrics
#'
#' Compute metrics for given backtest results
#'
#' @param backtest_summary Results of call to backtest or a list of results of backtest calls.
#'
#' @param metrics list of metrics which should be calculated.
#' Currently supporting:MAE(mean absolute error), RMSE (root mean squared error),
#' and WIS (weighted interval score)
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
    backtest_summary <- bind_rows(backtest_summary)
  }

  models <- unique(backtest_summary$model)

  backtest_summary <- backtest_summary %>%
    filter(horizon %in% horizons)

  metrics_table <- expand.grid(horizon=horizons, model=models)
  for(metric in metrics) {
    metric_results <- switch(
      metric,
      MAE=calc_mae(backtest_summary),
      RMSE=calc_rmse(backtest_summary),
      WIS=calc_wis(backtest_summary),
      stop("Error: unsupported metric - " +metric +".")
    )
    metrics_table <- merge(metrics_table, metric_results, by=c('horizon','model'))
  }
  metrics_table <- metrics_table %>%
                    arrange(model,horizon)
  return (metrics_table)
}
