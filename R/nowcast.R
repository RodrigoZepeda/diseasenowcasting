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
#' @param method Fitting method either `sampling` (recommended for inference), `variational`
#' (recommended for testing) or `optimization`. The `sampling` method calls [rstan::sampling()] while the
#' `variational` calls [rstan::vb()] and `optimization` calls [rstan::optimizing()]
#'
#' @param priors A list of all of the nowcast priors. You can use [set_priors()] to change
#' the priors of the function (see details)
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
#' nowcast(sims, "onset_date", "report_date", method = "optimization", seed = 2524)
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
                    method  = c("sampling","variational","optimization"),
                    priors  = set_priors(),
                    ...) {

  # Check that the columns of onset and report are columns of data and are dates
  .disease_data <- check_date_columns(.disease_data, onset_date = onset_date, report_date = report_date)

  # Check proportion reported
  # FIXME: The proportion reported currently doesn't do anything
  check_proportion_reported(proportion_reported)

  # Get `now` as the last date by default
  now <- infer_now(.disease_data, now = now, onset_date = onset_date)

  # Infer the units whether it is daily, weekly, monthly or yearly
  units <- infer_units(.disease_data, units = units, date_column = onset_date)

  # Match the distribution whether negative binomial or poisson
  dist <- match.arg(dist, c("NegativeBinomial", "Poisson"))

  # Method
  method <- match.arg(method, c("sampling", "variational","optimization"))

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

  stan_list <- nowcast.rstan(.disease_data,
                onset_date,
                report_date,
                strata = strata,
                dist = dist,
                prior_only = prior_only,
                refresh = refresh,
                control = control,
                method = method,
                priors = priors,
                ...)

  #Get the call values
  call_parameters = list(
    onset_date = onset_date,
    report_date = report_date,
    strata = strata,
    now = now,
    units = units,
    max_delay = max_delay
  )

  # Add to stan list
  stan_list$data <- stan_list$data |>
    append(
      list(
        preprocessed_data = .disease_data,
        call_parameters   = call_parameters
      )
    )

 return(stan_list)

}

#' Nowcasting with the `rstan` engine
#'
#' @inheritParams nowcast
#' @param ... Additional arguments to pass to [rstan::sample()]
#'
#' @keywords internal
nowcast.rstan <- function(.disease_data, onset_date, report_date, strata, dist,
                          prior_only, control, refresh, method, priors, ...) {


  #Get the specified prior distributions
  priors$mu_sd_prior          <- get_prior_code_stan(priors$mu_sd_prior)
  priors$nu_sd_prior          <- get_prior_code_stan(priors$nu_sd_prior)
  priors$mu_0_mean_hyperprior <- get_prior_code_stan(priors$mu_0_mean_hyperprior)
  priors$nu_0_mean_hyperprior <- get_prior_code_stan(priors$nu_0_mean_hyperprior)
  priors$mu_0_sd_hyperprior   <- get_prior_code_stan(priors$mu_0_sd_hyperprior)
  priors$nu_0_sd_hyperprior   <- get_prior_code_stan(priors$nu_0_sd_hyperprior)
  priors$r_prior              <- get_prior_code_stan(priors$r_prior)
  priors$phi_AR_prior         <- get_prior_code_stan(priors$phi_AR_prior)

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
    tidyr::unite(col = ".strata_unified", 4:dplyr::last_col(), sep = " - ") |>
    dplyr::mutate(!!as.symbol(".strata") := as.numeric(as.factor(!!as.symbol(".strata_unified")))) |>
    dplyr::mutate_at(".delay", function(x) x + 1)

  #Get the strata dictionary
  strata_dict <- Nmat |>
    dplyr::distinct(!!as.symbol(".strata_unified"),!!as.symbol(".strata"))

  Nmat <- Nmat |>
    dplyr::select(-!!as.symbol(".strata_unified"))

  #Return the number of strata
  num_strata <- Nmat |>
    dplyr::distinct(!!as.symbol(".strata")) |>
    dplyr::tally() |>
    dplyr::pull()

  # Distribution
  is_negative_binomial <- as.numeric(dist == "NegativeBinomial")

  # Sample only from the prior
  prior_only <- as.numeric(prior_only)

  #Get the log mean of disease data and the sd
  log_mean <- .disease_data |>
    dplyr::summarise(log_mean = mean(log1p(!!as.symbol("n")), na.rm = T)) |>
    dplyr::pull(log_mean)

  #Get the log mean of disease data and the sd
  log_sd <- .disease_data |>
    dplyr::summarise(log_sd = sd(log1p(!!as.symbol("n")), na.rm = T)) |>
    dplyr::pull(log_sd)

  stan_data <- list(

    #Data information
    num_steps  = num_steps,
    num_delays = num_delays,
    num_strata = num_strata,
    n_rows     = nrow(Nmat),
    N_cases    = as.matrix(Nmat),

    #Whether to compute only the prior
    prior_only     = prior_only,

    #Trend specification
    mu_degree      = priors$mu_degree,
    nu_degree      = priors$nu_degree,
    mu_is_constant = priors$mu_is_constant,
    nu_is_constant = priors$nu_is_constant,

    #ARMA specification
    p              = priors$p,
    phi_AR_param_1 = priors$phi_AR_param_1,
    phi_AR_param_2 = priors$phi_AR_param_2,
    phi_AR_prior   = priors$phi_AR_prior,

    #Distribution information
    is_negative_binomial = is_negative_binomial,
    mu_sd_prior = priors$mu_sd_prior,
    nu_sd_prior = priors$nu_sd_prior,
    r_prior     = priors$r_prior,

    #Prior parameters
    r_param_1     = priors$r_param_1,
    r_param_2     = priors$r_param_2,
    mu_sd_param_1 = priors$mu_sd_param_1,
    mu_sd_param_2 = priors$mu_sd_param_2,
    nu_sd_param_1 = priors$nu_sd_param_1,
    nu_sd_param_2 = priors$nu_sd_param_2,

    mu_0_mean_param_1    = ifelse(priors$mu_0_mean_param_1 == "auto", log_mean, priors$mu_0_mean_param_1),
    mu_0_mean_param_2    = priors$mu_0_mean_param_2,
    mu_0_sd_param_1      = ifelse(priors$mu_0_sd_param_1 == "auto", log_sd, priors$mu_0_sd_param_1),
    mu_0_sd_param_2      = priors$mu_0_sd_param_2,
    nu_0_mean_param_1    = priors$nu_0_mean_param_1,
    nu_0_mean_param_2    = priors$nu_0_mean_param_2,
    nu_0_sd_param_1      = priors$nu_0_sd_param_1,
    nu_0_sd_param_2      = priors$nu_0_sd_param_2,
    mu_0_mean_hyperprior = priors$mu_0_mean_hyperprior,
    nu_0_mean_hyperprior = priors$nu_0_mean_hyperprior,
    mu_0_sd_hyperprior   = priors$mu_0_sd_hyperprior,
    nu_0_sd_hyperprior   = priors$nu_0_sd_hyperprior
  )

  if (method[1] == "sampling"){
    stan_fit <- rstan::sampling(stanmodels$nowcast, data = stan_data,
                           control = control,
                           refresh = refresh,
                           ...)
    draws    <- as.matrix(stan_fit)
  } else if (method[1] == "variational") {
    stan_fit <- rstan::vb(stanmodels$nowcast, data = stan_data, ...)
    draws    <- as.matrix(stan_fit)
  } else if (method[1] == "optimization") {
    stan_fit <- rstan::optimizing(stanmodels$nowcast, data = stan_data, ...)

    # Work around to get draws from the optimized params for gq
    draws           <- matrix(rep(stan_fit$par, 1000), ncol = length(stan_fit$par), byrow = TRUE)
    colnames(draws) <- names(stan_fit$par)

  } else {
    cli::cli_abort("Invalid method. Please select between {.val {c('sampling', 'variational','optimization')}}")
  }

  #Get the generated quantities
  generated_quantities <- rstan::gqs(stanmodels$generated_quantities, data = stan_data, draws = draws)

  return(
    list(
      data         = list(stan_data = stan_data),
      dict         = list(strata_dict = strata_dict),
      generated_quantities  = generated_quantities,
      model        = stan_fit
    )
  )
}
