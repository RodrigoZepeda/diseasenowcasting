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
#' @param dist Distribution. Either "NegativeBinomial", "Poisson", "Normal", or "Student"
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
#' # Run a nowcast with very few iterations
#' # change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
#' now <- as.Date("1990-10-01")
#' nowcast(denguedat, "onset_week", "report_week", now = now,
#'   method = "optimization", seed = 2495624, iter = 10)
#' @export
nowcast <- function(.disease_data,
                    onset_date,
                    report_date,
                    strata = NULL,
                    dist   = c("NegativeBinomial", "Poisson","Normal","Student"),
                    now = NULL,
                    units = NULL,
                    max_delay = Inf,
                    prior_only = FALSE,
                    proportion_reported = 1,
                    refresh = 250*rlang::is_interactive(),
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
  now    <- infer_now(.disease_data, now = now, onset_date = onset_date)

  # Infer the units whether it is daily, weekly, monthly or yearly
  units  <- infer_units(.disease_data, units = units, date_column = onset_date)

  # Match the distribution whether negative binomial or poisson
  dist   <- match.arg(dist, c("NegativeBinomial", "Poisson","Normal","Student"))

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

  # Get maximum time for model
  num_steps <- .disease_data |>
    dplyr::summarise(num_steps = max(!!as.symbol(".tval"))) |>
    dplyr::pull(num_steps)

  # Get maximum delay for model
  num_delays <- .disease_data |>
    dplyr::summarise(num_delays = 1 + max(!!as.symbol(".delay"))) |>
    dplyr::pull(num_delays)

  #Get the specified prior distributions
  #priors        <- priors_to_numeric(.disease_data, priors, dist)

  #Get the strata
  strata_list   <- preprocess_strata(.disease_data, strata)
  .disease_data <- strata_list$.disease_data
  num_strata    <- strata_list$num_strata

  stan_list <- nowcast.rstan(.disease_data,
                onset_date,
                report_date,
                num_steps  = num_steps,
                num_delays = num_delays,
                num_strata = num_strata,
                dist       = dist,
                prior_only = prior_only,
                refresh    = refresh,
                control    = control,
                method     = method,
                priors     = priors,
                ...)

  #Get the call values
  call_parameters = list(
    onset_date  = onset_date,
    report_date = report_date,
    strata      = strata,
    now         = now,
    units       = units,
    max_delay   = max_delay,
    num_delays  = num_delays,
    num_steps   = num_steps,
    num_strata  = num_strata
  )

  # Add to stan list
  stan_list$data <- stan_list$data |>
    append(
      list(
        preprocessed_data = .disease_data,
        call_parameters   = call_parameters,
        strata_dict       = strata_list$.strata_dict
      )
    )

 return(stan_list)

}

#' Nowcasting with the `rstan` engine
#'
#' @inheritParams nowcast
#' @param num_steps  Number of time steps to run in the model
#' @param num_delays Number of delays to consider in the model
#' @param num_strata Number of strata in the model
#'
#' @keywords internal
nowcast.rstan <- function(.disease_data, onset_date, report_date, num_steps, num_delays, num_strata,
                          dist, prior_only, control, refresh, method, priors, ...) {

  #Keep only the columns n, .tval, .delay, .strata
  .disease_data <- .disease_data |>
    dplyr::select(!!as.symbol("n"), !!as.symbol(".tval"), !!as.symbol(".delay"), !!as.symbol(".strata"))

  # Distribution
  distribution <- get_distribution_number(dist)

  # If cases are normal or student normalize the cases and fit the model to the normalized version
  if (dist == "Normal" || dist == "Student"){

    #Get the mean and variance per delay-strata
    .disease_data_normalization_ct <- .disease_data |>
      dplyr::group_by(!!as.symbol(".strata"), !!as.symbol(".delay")) |>
      dplyr::summarise(
        !!as.symbol("mu") := mean(!!as.symbol("n"), na.rm = TRUE),
        !!as.symbol("sd") := sd(!!as.symbol("n"), na.rm = TRUE), .groups = "drop"
      ) |>
      #Reconvert sd = 1 and mu = 0 if we have only one observation as it will not make sense to divide by mu
      dplyr::mutate(!!as.symbol("sd") := dplyr::if_else(!!as.symbol("sd") == 0, 1, !!as.symbol("sd")))

    .disease_data <- .disease_data |>
      dplyr::left_join(.disease_data_normalization_ct, by = c(".strata",".delay")) |>
      dplyr::mutate(!!as.symbol("n") := (!!as.symbol("n") - !!as.symbol("mu"))/!!as.symbol("sd")) |>
      dplyr::arrange(!!as.symbol(".strata"), !!as.symbol(".delay"))

    #Create the mu_case matrix
    mu_cases <- .disease_data_normalization_ct |>
      tidyr::pivot_wider(id_cols = ".strata", names_from = ".delay", values_from = "mu") |>
      dplyr::select(-!!as.symbol(".strata")) |>
      as.matrix()

    sd_cases <- .disease_data_normalization_ct |>
      tidyr::pivot_wider(id_cols = ".strata", names_from = ".delay", values_from = "sd") |>
      dplyr::select(-!!as.symbol(".strata")) |>
      as.matrix()


  } else {
    #There shouldn't be a scaling for the Poisson and Negative Binomial
    mu_cases <- matrix(0.0, nrow = num_strata, ncol = num_delays)
    sd_cases <- matrix(1.0, nrow = num_strata, ncol = num_delays)
  }

  # Cases and positions handled separately
  N_cases <- as.matrix(.disease_data)

  stan_data <- priors |>
    append(
      list(

        #Data information
        num_steps   = num_steps,
        num_delays  = num_delays,
        num_strata  = num_strata,
        n_rows      = nrow(.disease_data),
        case_idx    = N_cases[,2:4],
        cases       = N_cases[,1],

        #Whether to compute only the prior
        prior_only  = as.numeric(prior_only),

        #For generated quantities
        mu_cases = mu_cases,
        sd_cases = sd_cases
  ))

  #Select the model
  if (dist %in% c("NegativeBinomial","Poisson")){
    model_stan <- stanmodels$nowcasting_v2
    gq_stan    <- stanmodels$generated_quantities
  } else {
    model_stan <- stanmodels$nowcasting_v2
    gq_stan    <- stanmodels$generated_quantities
  }

  if (method[1] == "sampling"){
    stan_fit <- rstan::sampling(model_stan, data = stan_data,
                           control = control,
                           refresh = refresh,
                           ...)
    draws    <- as.matrix(stan_fit)
  } else if (method[1] == "variational") {
    stan_fit <- rstan::vb(model_stan, data = stan_data, refresh = refresh,...)
    draws    <- as.matrix(stan_fit)
  } else if (method[1] == "optimization") {
    stan_fit <- rstan::optimizing(model_stan, data = stan_data, refresh = refresh, ...)

    # Work around to get draws from the optimized params for gq
    draws           <- matrix(rep(stan_fit$par, 1000), ncol = length(stan_fit$par), byrow = TRUE)
    colnames(draws) <- names(stan_fit$par)

  } else {
    cli::cli_abort("Invalid method. Please select between {.val {c('sampling', 'variational','optimization')}}")
  }

  #Get the generated quantities
  generated_quantities <- rstan::gqs(gq_stan, data = stan_data, draws = draws)

  # flag <- generated_quantities |>
  #   posterior::as_draws() |>
  #   posterior::subset_draws("lambda_higher_than_maxval_flag") |>
  #   posterior::summarise_draws(max) |>
  #   dplyr::pull(max)

  # if (flag >= 1){
  #   cli::cli_alert_warning(
  #     "Some values of lambda have been truncated as they are too large. This might be attributed
  #     to a high variance of your data or on the prior. Consider reducing the prior variance."
  #   )
  # }

  return(
    list(
      data         = list(stan_data = stan_data),
      generated_quantities  = generated_quantities,
      model        = stan_fit
    )
  )
}
