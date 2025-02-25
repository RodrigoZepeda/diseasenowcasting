

library(ggrepel)
library(tidyr)
library(dplyr)

data(denguedat)

#Use just a subsample of the data for the example
denguedat <- denguedat |>
  filter(report_week <= as.Date("1990/08/01", format = "%Y/%m/%d"))

#Run the nowcast model globally (not stratified) using NB model (default)
ncast1 <- nowcast(denguedat, true_date = "onset_week", report_date = "report_week",
                  method = "optimization", iter = 10)

#Run the nowcast model globally (not stratified) using Poisson model
ncast2 <- nowcast(denguedat, true_date = "onset_week", report_date = "report_week",
                  method = "optimization", dist = "Poisson", iter = 10)

#Run the nowcast model stratified by gender using NB model (default)
ncast3 <- nowcast(denguedat, true_date = "onset_week", report_date = "report_week", strata = c("gender"),
                  method = "optimization", iter = 10)

#Run the nowcast model stratified by gender using Poisson model
ncast4 <- nowcast(denguedat, true_date = "onset_week", report_date = "report_week",strata = c("gender"),
                  method = "optimization", dist = "Poisson", iter = 10)

#Run backtest procedures using nowcasts parameters defined above
backtest_summary1 <- backtest(ncast1,
                              start_date = as.Date('1990-01-22'),
                              end_date = as.Date('1990-07-22'),
                              stride = 4,
                              min_horizon = -3,
                              model_name='model_NB')

backtest_summary2 <- backtest(ncast2,
                              start_date = as.Date('1990-01-22'),
                              end_date = as.Date('1990-07-22'),
                              stride = 4,
                              min_horizon = -3,
                              model_name='model_Poisson')

backtest_summary3 <- backtest(ncast3,
                              start_date = as.Date('1990-01-22'),
                              end_date = as.Date('1990-07-22'),
                              stride = 4,
                              min_horizon = -3,
                              model_name='model_NB_strat')

backtest_summary4 <- backtest(ncast4,
                              start_date = as.Date('1990-01-22'),
                              end_date = as.Date('1990-07-22'),
                              stride = 4,
                              min_horizon = -3,
                              model_name='model_Poisson_strat')

#Compare metrics of backtest results for stratified models
metrics_comp_strat <- backtest_metrics(backtest_summary3,
                                       backtest_summary4, horizons =c(-1,0))

#Compare metrics of backtest results for all models -
#stratified models results are aggregated to allow the comparison
metrics_comp_all <- backtest_metrics(backtest_summary1,
                                     backtest_summary2,
                                     aggregate_backtest_summary(backtest_summary3,"gender"),
                                     aggregate_backtest_summary(backtest_summary4,"gender"), horizons = c(-1,0))

summary.backtest_metrics(long_mtr)

plot.backtest_metrics(metrics_comp_strat)
plot.backtest_metrics(metrics_comp_all)


