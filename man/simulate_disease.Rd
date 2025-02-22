% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.R
\name{simulate_disease}
\alias{simulate_disease}
\title{Simulate a process}
\usage{
simulate_disease(
  num_steps = 10,
  num_delays = 8,
  num_strata = 2,
  initial_day = NULL,
  warmup_steps = 50,
  units = c("daily", "weekly"),
  priors = random_priors(has_cycle = FALSE),
  autoregresive = AR(),
  moving_average = MA(),
  ...
)
}
\arguments{
\item{num_steps}{Integer. Number of time steps to simulate. Default is 10.}

\item{num_delays}{Integer. Number of delay strata. Default is 8.}

\item{num_strata}{Integer. Number of strata for the population. Default is 2.}

\item{initial_day}{Date. If the simulation is to start on a specific day.}

\item{warmup_steps}{Initial steps on model (to discard)}

\item{units}{Either \code{"daily"} (default) or \code{"weekly"}.}

\item{priors}{A list of all of the nowcast priors. You can use \code{\link[=set_priors]{set_priors()}} to change
the priors of the function (see details)}

\item{autoregresive}{An \code{\link[=AR]{AR()}} object with the autoregresive components for the epidemic
and delay processes.}

\item{moving_average}{A \code{\link[=MA]{MA()}} object with the moving average component for the epidemic
and delay processes.}

\item{...}{Additional arguments to pass to \code{\link[=nowcast]{nowcast()}}}
}
\value{
A tibble with simulated state-space process results, including the onset and
report dates, strata, delays, and the number of observed cases per time step.
}
\description{
This function simulates a state-space process for testing purposes in a model with
multiple delays and strata. It estimates the process over a specified number of steps,
with options for trend degrees, priors, and error distributions.
}
\examples{
# Simulate a disease for 20 time steps with delay of maximum 10 and 3 strata
set.seed(48672)
sims <- simulate_disease(num_steps = 20, num_delays = 3, num_strata = 3)
}
