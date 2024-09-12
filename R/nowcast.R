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
#' @param control Control parameter for [rstan::sampling()]
#'
#' @param refresh Refresh parameter for [rstan::sampling()]
#'
#' @param mu_degree Integer. Degree of the epidemic trend. Default is 2.
#'
#' @param nu_degree Integer. Degree of the delay trend. Default is 1.
#'
#' @param mu_is_constant Logical. Indicates whether the epidemic trend is constant. Default is FALSE.
#'
#' @param nu_is_constant Logical. Indicates whether the delay trend is constant. Default is TRUE.
#'
#' @param mu_error_prior Character. Prior for the epidemic trend error. Default is "normal".
#'
#' @param nu_error_prior Character. Prior for the delay trend error. Default is "normal".
#'
#' @param mu_param_1 Numeric. First parameter for the epidemic trend error. Default is 0.0.
#'
#' @param mu_param_2 Numeric. Second parameter for the epidemic trend error. Default is 1.0.
#'
#' @param nu_param_1 Numeric. First parameter for the delay trend error. Default is 0.0.
#'
#' @param nu_param_2 Numeric. Second parameter for the delay trend error. Default is 1.0.
#'
#' @param mu_0_prior Character. Prior for the initial epidemic trend distribution. Default is "normal".
#'
#' @param nu_0_prior Character. Prior for the initial delay trend distribution. Default is "normal".
#'
#' @param mu_0_param_1 Numeric. First parameter for the initial epidemic trend distribution. Default is 0.0.
#'
#' @param mu_0_param_2 Numeric. Second parameter for the initial epidemic trend distribution. Default is 1.0.
#'
#' @param nu_0_param_1 Numeric. First parameter for the initial delay trend distribution. Default is 0.0.
#'
#' @param nu_0_param_2 Numeric. Second parameter for the initial delay trend distribution. Default is 1.0.
#'
#' @param r_prior Character. Prior for the negative binomial precision parameter. Default is "normal".
#'
#' @param r_param_1 Numeric. First parameter for the dispersion prior if negative binomial. Default is 0.0.
#'
#' @param r_param_2 Numeric. Second parameter for the dispersion prior if negative binomial. Default is 1.0.
#'
#' @param ... Additional arguments to pass to [rstan::sampling()]
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#' # Create a fake disease process
#' sims <- simulate_process_for_testing()
#'
#' # Run a nowcast with very few iterations
#' # change to 4 chains and 2000 iter when doing inference
#' nowcast(sims, "onset_date", "report_date", iter = 100, chains = 1, seed = 2524)
#' @export
nowcast <- function(.disease_data, onset_date, report_date,
                    strata = NULL,
                    dist = c("NegativeBinomial", "Poisson"),
                    now = NULL,
                    units = NULL,
                    max_delay = Inf,
                    prior_only = FALSE,
                    proportion_reported = 1,
                    refresh = 250*interactive(),
                    control = control_default(),
                    mu_degree = 2,               #Trend degree specification for epidemic
                    nu_degree = 1,               #Trend degree specification for delays (1 = constant)
                    mu_is_constant = FALSE,      #Whether epidemic pattern is constant
                    nu_is_constant = TRUE,       #Whether delay pattern is constant
                    mu_error_prior = "normal",   #Prior for the epidemic trend error
                    nu_error_prior = "normal",   #Prior for the delay trend error
                    mu_param_1 = 0.0,            #First parameter for degree error in epidemic trend
                    mu_param_2 = 0.1,            #Second parameter for degree error in epidemic trend
                    nu_param_1 = 0.0,            #First parameter for degree error in delay trend
                    nu_param_2 = 0.1,            #Second parameter for degree error in delay trend
                    mu_0_prior = "normal",       #Prior for the initial epidemic distribution
                    nu_0_prior = "normal",       #Prior for the initial delay distribution
                    mu_0_param_1 = "auto", #First parameter for degree error in epidemic trend
                    mu_0_param_2 = 0.01,          #Second parameter for degree error in epidemic trend
                    nu_0_param_1 = 0.0,          #First parameter for degree error in delay trend
                    nu_0_param_2 = 0.01,          #Second parameter for degree error in delay trend
                    r_prior  = "normal",         #Prior for the negative binomial precision
                    r_param_1 = 0.0,             #First parameter for dispersion prior if negative binomial
                    r_param_2 = 1.0,             #Second parameter for dispersion prior if negative binomial
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

  #Get the log mean of disease data and the sd
  log_mean <- .disease_data |>
    dplyr::summarise(log_mean = mean(log1p(!!as.symbol("n")), na.rm = T)) |>
    dplyr::pull(log_mean)

  #Get the log mean of disease data and the sd
  log_sd <- .disease_data |>
    dplyr::summarise(log_sd = sd(log1p(!!as.symbol("n")), na.rm = T)) |>
    dplyr::pull(log_sd)

  nowcast.rstan(.disease_data, onset_date, report_date, strata = strata, dist = dist,
                prior_only = prior_only, refresh = refresh, control = control,
                mu_degree = mu_degree,
                nu_degree = nu_degree,
                mu_is_constant = mu_is_constant,
                nu_is_constant = nu_is_constant,
                mu_error_prior = mu_error_prior,
                nu_error_prior = nu_error_prior,
                mu_param_1 = mu_param_1,
                mu_param_2 = mu_param_2,
                nu_param_1 = nu_param_1,
                nu_param_2 = nu_param_2,
                mu_0_prior = mu_0_prior,
                nu_0_prior = nu_0_prior,
                mu_0_param_1 = ifelse(mu_0_param_1 == "auto", log_mean, mu_0_param_1),
                mu_0_param_2 = mu_0_param_2,
                nu_0_param_1 = nu_0_param_1,
                nu_0_param_2 = nu_0_param_2,
                r_prior  = r_prior,
                r_param_1 = r_param_1,
                r_param_2 = r_param_2,
                ...)
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
                          prior_only = FALSE,
                          control = control_default(),
                          refresh = 250,
                          mu_degree = 2,               #Trend degree specification for epidemic
                          nu_degree = 1,               #Trend degree specification for delays (1 = constant)
                          mu_is_constant = FALSE,      #Whether epidemic pattern is constant
                          nu_is_constant = TRUE,       #Whether delay pattern is constant
                          mu_error_prior = "normal",   #Prior for the epidemic trend error
                          nu_error_prior = "normal",   #Prior for the delay trend error
                          mu_param_1 = 0.0,            #First parameter for degree error in epidemic trend
                          mu_param_2 = 1.0,            #Second parameter for degree error in epidemic trend
                          nu_param_1 = 0.0,            #First parameter for degree error in delay trend
                          nu_param_2 = 1.0,            #Second parameter for degree error in delay trend
                          mu_0_prior = "normal",       #Prior for the initial epidemic distribution
                          nu_0_prior = "normal",       #Prior for the initial delay distribution
                          mu_0_param_1 = log(mean(.disease_data$n, na.rm = T)), #First parameter for degree error in epidemic trend
                          mu_0_param_2 = 0.01,          #Second parameter for degree error in epidemic trend
                          nu_0_param_1 = 0.0,          #First parameter for degree error in delay trend
                          nu_0_param_2 = 0.01,          #Second parameter for degree error in delay trend
                          r_prior  = "normal",         #Prior for the negative binomial precision
                          r_param_1 = 0.0,             #First parameter for dispersion prior if negative binomial
                          r_param_2 = 1.0,             #Second parameter for dispersion prior if negative binomial
                          ...) {


  #FIXME: Implement
  if (mu_0_prior != "normal"){
    cli::cli_alert_warning("This functionality has not yet been implemented defaulting to {.code mu_0_prior = {.val normal}}")
  }

  if (nu_0_prior != "normal"){
    cli::cli_alert_warning("This functionality has not yet been implemented defaulting to {.code nu_0_prior = {.val normal}}")
  }

  #Get the specified prior distributions
  mu_prior <- get_prior_code_stan(mu_error_prior)
  nu_prior <- get_prior_code_stan(nu_error_prior)
  r_prior  <- get_prior_code_stan(r_prior)

  # Get maximum time for model
  num_steps <- .disease_data |>
    dplyr::summarise(num_steps = max(!!as.symbol(".tval"))) |>
    dplyr::pull(num_steps)

  # Get maximum delay for model
  num_delays <- .disease_data |>
    dplyr::summarise(num_delays = 1 + max(!!as.symbol(".delay"))) |>
    dplyr::pull(num_delays)

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
    dispersion_prior_shape = r_param_1,
    dispersion_prior_rate = r_param_2,
    mu_shape_prior = mu_param_1,
    mu_rate_prior  = mu_param_2,
    nu_shape_prior = nu_param_1,
    nu_rate_prior  = nu_param_2,

    mean_mu_0_prior  = mu_0_param_1,
    mean_nu_0_prior  = nu_0_param_1,
    sigma_mu_0_prior = mu_0_param_2,
    sigma_nu_0_prior = nu_0_param_2
  )

  # model <- rstan::stan_model("inst/stan/nowcast.stan")
  out <- rstan::sampling(stanmodels$nowcast_v2, data = stan_data,
                         control = control,
                         refresh = refresh,
                         ...)

  return(out)
}
