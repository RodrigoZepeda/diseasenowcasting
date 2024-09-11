#' Nowcasting
#'
#' Function that uses the [rstan::sampling()] engine to generate nowcasts.
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
#' @param strata Character vector of names of the strata included in the data.
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
#' @param prior_only Boolean variable indicating whether to compute only the prior distribution
#'
#' @param ... Additional arguments to pass to [rstan::sampling()]
#'
#' @examples
#' # Load the data
#' data(denguedat)
#' now <- as.Date("1990-10-01")
#'
#' # Run a nowcast with very few iterations
#' # change to 4 chains and 2000 iter when doing inference
#' # or just run the command without the `iter = 10`, `chains = 1` thing.
#' nowcast(denguedat, "onset_week", "report_week",
#'           iter = 100, chains = 1, seed = 12334)
#'
#' # You can also run the nowcast stratifying by several variables
#' nowcast(denguedat, "onset_week", "report_week", strata = c("gender"),
#'           iter = 100, chains = 1, seed = 12334)
#' @export
nowcast <- function(.disease_data, onset_date, report_date,
                    strata = NULL,
                    dist = c("NegativeBinomial", "Poisson"),
                    now = NULL,
                    units = NULL,
                    max_delay = Inf,
                    prior_only = FALSE,
                    proportion_reported = 1,
                    refresh = 10*interactive(),
                    control = control_default(),
                    ...) {

  # Check that the columns of onset and report are columns of data and are dates
  .disease_data <- check_date_columns(.disease_data, onset_date = onset_date, report_date = report_date)

  # Check proportion reported
  check_proportion_reported(proportion_reported)

  # Get `now` as the last date by default
  now <- infer_now(.disease_data, now = now, onset_date = onset_date)

  # Infer the units whether it is daily, weekly, monthly or yearly
  units <- infer_units(.disease_data, units = units, date_column = onset_date)

  # Match the distribution whether negative binomial or poisson
  dist <- match.arg(dist, c("NegativeBinomial", "Poisson"))

  # Print message to user
  cli::cli_alert_info(
    "Computing a nowcast for {.val {now}} per {.val {units}}"
  )

  # Get the data for processing
  .disease_data <- preprocess_for_nowcast(.disease_data,
    onset_date = onset_date,
    report_date = report_date,
    strata = strata,
    now = now,
    units = units,
    max_delay = max_delay
  )

  nowcast.rstan(.disease_data, onset_date, report_date, strata = strata, dist = dist,
                prior_only = prior_only, refresh = refresh, control = control, ...)
}

#' Nowcasting with the `rstan` engine
#'
#' @inheritParams nowcast
#' @param ... Additional arguments to pass to [rstan::sample()]
#'
#' @keywords internal
nowcast.rstan <- function(.disease_data, onset_date,
                          report_date,
                          strata = NULL,
                          dist = c("NegativeBinomial", "Poisson"),
                          mu_degree = 2,
                          nu_degree = 1,
                          mu_is_constant = FALSE,
                          nu_is_constant = TRUE,
                          prior_only = FALSE,
                          mu_shape_prior = 0.0,
                          mu_rate_prior = 1.0,
                          nu_shape_prior = 0.0,
                          nu_rate_prior = 1.0,
                          dispersion_prior_shape = 0.0,
                          dispersion_prior_rate = 0.001,
                          mean_mu_0_prior = log(mean(.disease_data$n, na.rm = T)),
                          mean_nu_0_prior = 0.0,
                          sigma_mu_0_prior = 0.01,
                          sigma_nu_0_prior = 0.01,
                          mu_prior_name = "standard_normal",
                          nu_prior_name = "standard_normal",
                          r_prior_name  = "normal",
                          control = control_default(),
                          refresh = 250,
                          ...) {

  #Get the specified prior distributions
  mu_prior <- get_prior_code_stan(mu_prior_name)
  nu_prior <- get_prior_code_stan(nu_prior_name)
  r_prior  <- get_prior_code_stan(r_prior_name)

  # Get maximum time for model
  num_steps <- .disease_data |>
    dplyr::summarise(max_time = max(!!as.symbol(".tval"))) |>
    dplyr::pull(max_time)

  # Get maximum delay for model
  num_delays <- .disease_data |>
    dplyr::summarise(max_delays = 1 + max(!!as.symbol(".delay"))) |>
    dplyr::pull(max_delays)

  # Number of covariates
  num_covariates <- 0

  # Nmatrix
  Nmat <- .disease_data |>
    dplyr::select(-!!as.symbol(report_date), -!!as.symbol(onset_date)) |>
    dplyr::select(!!as.symbol("n"), !!as.symbol(".tval"), !!as.symbol(".delay"), tidyr::everything())

  #Create the strata column
  if (ncol(Nmat) == 3){
    Nmat <- Nmat |>
      dplyr::mutate(!!as.symbol(".strata") := "No strata")
  }
  Nmat <- Nmat |>
    tidyr::unite(col = ".strata", 4:dplyr::last_col(), sep = " - ") |>
    dplyr::mutate_at(".strata", function(x) as.numeric(as.factor(x))) |>
    dplyr::mutate_at(".delay", function(x) x + 1)

  #Return the number of strata
  num_strata <- Nmat |>
    dplyr::distinct(!!as.symbol(".strata")) |>
    dplyr::tally() |>
    dplyr::pull()

  # Distribution
  is_negative_binomial <- as.numeric(dist == "NegativeBinomial")

  # Sample only from the prior
  prior_only <- as.numeric(prior_only)

  stan_data <- list(
    #Data information
    num_steps  = num_steps,
    num_delays = num_delays,
    num_strata = num_strata,
    n_rows  = nrow(Nmat),
    N_cases = as.matrix(Nmat),

    #Whether to compute only the prior
    prior_only = prior_only,

    #Trend specification
    mu_degree = mu_degree,
    nu_degree = nu_degree,
    mu_is_constant = mu_is_constant,
    nu_is_constant = nu_is_constant,

    #Distribution information
    is_negative_binomial = is_negative_binomial,
    mu_prior = mu_prior,
    nu_prior = nu_prior,
    r_prior  = r_prior,

    #Prior parameters
    dispersion_prior_shape = dispersion_prior_shape,
    dispersion_prior_rate = dispersion_prior_rate,
    mu_shape_prior = mu_shape_prior,
    mu_rate_prior  = mu_rate_prior,
    nu_shape_prior = nu_shape_prior,
    nu_rate_prior  = nu_rate_prior,

    mean_mu_0_prior  = mean_mu_0_prior,
    mean_nu_0_prior  = mean_nu_0_prior,
    sigma_mu_0_prior = sigma_mu_0_prior,
    sigma_nu_0_prior = sigma_nu_0_prior
  )

  # model <- rstan::stan_model("inst/stan/nowcast.stan")
  out <- rstan::sampling(stanmodels$nowcast_v2, data = stan_data,
                         control = control,
                         refresh = refresh,
                         ...)

  return(out)
}
