#' Nowcasting
#'
#' Function that uses the `brms` engine to generate nowcasts.
#'
#' @param .disease_data A time series of reporting data in aggregated line list format
#' such that each row has a column for onset date, report date, and
#'
#' @param now An object of datatype \code{Date} indicating the date at which
#' to perform the nowcast.
#'
#' @param units Time scale of reporting. Options: "1 day", "1 week".
#'
#' @param dist Distribution. Either "NegativeBinomial"  or "Poisson"
#'
#' @param onset_date In quotations, the name of the column of datatype
#' \code{Date} designating the date of case onset. e.g. "onset_week"
#'
#' @param report_date In quotations, the name of the column of datatype
#' \code{Date} designating the date of case report. e.g. "report_week"
#'
#' @param moving_window Size of moving window for estimation of cases (numeric).
#' The moving window size should be specified in the same date units as the
#' reporting data (i.e. specify 7 to indicate 7 days, 7 weeks, etc).
#' Default: NULL, i.e. takes all historical dates into consideration.
#'
#' @param max_delay Maximum possible delay observed or considered for estimation
#' of the delay distribution (numeric). Default: `Inf`
#'
#' @param proportion_reported A decimal greater than 0 and less than or equal to
#' 1 representing the proportion of all cases expected to be reported.
#' Default: 1, e.g. 100 percent of all cases will eventually be reported.
#' For asymptomatic diseases where not all cases will ever be reported,
#' or for outbreaks in which severe under-reporting is expected, change this
#'  to less than 1.
#'
#' @param ... Additional arguments to pass to [rstan::sampling()]
#'
#' @examples
#' # Load the data
#' data(denguedat)
#' now <- as.Date("1990-10-01")
#' nowcast(denguedat, "onset_week", "report_week")
#' @export
nowcast <- function(.disease_data, onset_date, report_date,
                    covariates = NULL,
                    dist = c("NegativeBinomial", "Poisson"),
                    now = NULL,
                    units = NULL,
                    seed = NULL,
                    moving_window = NULL,
                    max_delay = Inf,
                    prior_only = FALSE,
                    proportion_reported = 1, ...) {

  # Check that the columns of onset and report are columns of data and are dates
  .disease_data <- check_date_columns(.disease_data, onset_date = onset_date, report_date = report_date)

  # Check proportion reported
  check_proportion_reported(proportion_reported)

  # Set seed if null
  if (is.null(seed)) {
    seed <- ceiling(1000000 * runif(1))
  }

  # Get `now` as the last date by default
  now <- infer_now(.disease_data, now = now, onset_date = onset_date)

  # Infer the units whether it is daily, weekly, monthly or yearly
  units <- infer_units(.disease_data, units = units, date_column = onset_date)

  # Match the distribution whether negative binomial or poisson
  dist <- match.arg(dist, c("NegativeBinomial", "Poisson"))

  # Print message to user
  cli::cli_alert_info(
    "Computing a nowcast for {.emph {now}} per {.emph {units}}"
  )

  # Get the data for processing
  .disease_data <- preprocess_for_nowcast(.disease_data,
      onset_date = onset_date,
      report_date = report_date, now = now, units = units,
      max_delay = max_delay
  )

  nowcast.rstan(.disease_data, onset_date, dist = dist, ...)

}

#' Nowcasting with specific engines
#'
#' @inheritParams nowcast
#' @param ... Additional arguments to pass to [rstan::sample()]
#'
#' @examples
#' # Load the data
#' data(denguedat)
#' now <- as.Date("1990-10-01")
#' nowcast(denguedat, onset_dat)
#' @export
nowcast.rstan <- function(.disease_data, onset_date,
                          covariates = NULL,
                          dist = c("NegativeBinomial", "Poisson"),
                          prior_only = FALSE,
                          ...) {

  #Get maximum time for model
  max_time <- .disease_data |>
    summarise(max_time = max(.tval)) |>
    pull(max_time)

  #Get maximum delay for model
  max_delays <- .disease_data |>
    summarise(max_delays = max(.delay)) |>
    pull(max_delays)

  #Number of strata
  num_strata <- 1

  #Number of covariates
  num_covariates <- 0

  #Nmatrix
  Nmat <- .disease_data |>
    dplyr::select(-!!as.symbol(report_date), -!!as.symbol(onset_date)) |>
    dplyr::select(!!as.symbol("n"), !!as.symbol(".tval"), !!as.symbol(".delay"), tidyr::everything())

  #Distribution
  is_negative_binomial <- as.numeric(dist == "NegativeBinomial")

  #Sample only from the prior
  prior_only <- as.numeric(prior_only)

  dispersion_prior_shape <- 0.001
  dispersion_prior_rate  <- 0.001

  beta_mean_prior <- 0
  beta_sd_prior   <- 1

  alpha_mean_prior <- 0
  alpha_sd_prior   <- 1.0 / sqrt(0.001)

  alphat_shape_prior <- 0.001
  alphat_rate_prior  <- 0.001

  stan_data <- list(
    max_time = max_time,
    max_delays = max_delays,
    num_strata = num_strata,
    num_covariates = num_covariates,
    nobs = nrow(Nmat),
    Nmat = as.matrix(Nmat),
    is_negative_binomial = is_negative_binomial,
    prior_only = prior_only,
    dispersion_prior_shape = dispersion_prior_shape,
    dispersion_prior_rate = dispersion_prior_rate,
    beta_mean_prior = beta_mean_prior,
    beta_sd_prior = beta_sd_prior,
    alpha_mean_prior = alpha_mean_prior,
    alpha_sd_prior = alpha_sd_prior,
    alphat_shape_prior = alphat_shape_prior,
    alphat_rate_prior = alphat_rate_prior
  )

  #model <- rstan::stan_model("inst/stan/nowcast.stan")
  out   <- rstan::sampling(stanmodels$nowcast, data = stan_data, init = 0, ...)

  return(out)

}
