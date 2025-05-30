% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backtest.R
\name{aggregate_backtest_summary}
\alias{aggregate_backtest_summary}
\title{aggregate_backtest_summary}
\usage{
aggregate_backtest_summary(backtest_summary, remove_strata)
}
\arguments{
\item{backtest_summary}{results of \code{\link[=backtest]{backtest()}}}

\item{remove_strata}{vector of strata columns found inside backtest_summary to remove}
}
\value{
The aggregated backtest_summary
}
\description{
Aggregate the backtest summary by removing stratas.
A utility function for comparing the results of a stratified model to a non stratified model.
The results of the stratified model backtest summary should be aggregated before calling backtest_metrics
together with the backtest summary of a non-stratified model.
}
\examples{
# Load the data
data(denguedat)

# Run a nowcast with very few iterations
# change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
now <- as.Date("1990-10-01")
ncast <- nowcast(denguedat, "onset_week", "report_week", now = now,
  method = "optimization", seed = 2495624, iter = 10)

# Run a backtest for the model checking the model fit for two dates:
dates_to_test <- c(as.Date("1990-06-11"), as.Date("1990-06-18"))
btest <- backtest(ncast, dates_to_test=dates_to_test , model_name="model_global")

# Run a nowcast stratified by gender
ncast_strat <- nowcast(denguedat, "onset_week", "report_week", now = now, strata = c("gender"),
  method = "optimization", seed = 2495624, iter = 10)

# Run a backtest for the stratified model
btest_strat <- backtest(ncast_strat, dates_to_test=dates_to_test, model_name="model_strat")

# Aggregates the backtest results for the stratified model
btest_strat_agg <- aggregate_backtest_summary(btest_strat, "gender")

# Compare the metrics of the non-stratified model with the aggregated stratified model
metrics = backtest_metrics(btest, btest_strat_agg)

}
