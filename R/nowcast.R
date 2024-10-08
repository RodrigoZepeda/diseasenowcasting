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
#' (recommended for testing) or `optimization`. The `sampling` method calls [rstan::sampling()]
#' while the `variational` calls [rstan::vb()] and `optimization` calls [rstan::optimizing()]
#'
#' @param priors A list of all of the nowcast priors. You can use [set_priors()] to change
#' the priors of the function (see details)
#'
#' @param link_x Link function for the epidemic process (see section On links).
#'
#' @param link_y Link function for the data (see section On links).
#'
#' @param normalize_data Whether the data `y` should be normalized (substracted its mean and divided
#' by standard deviation) for fitting. This option is only valid if using a continuous model
#' (Normal or Student).
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
                    link_x = default_x_link(dist),
                    link_y = default_y_link(dist),
                    now = NULL,
                    units = NULL,
                    max_delay = Inf,
                    prior_only = FALSE,
                    proportion_reported = 1,
                    normalize_data = (dist %in% c("Normal","Student")),
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

  #Link
  link_x <- match.arg(link_x, c("log","identity","softplus","dhyperbolic"))
  link_y <- match.arg(link_y, c("log","identity","softplus","dhyperbolic"))

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
                link_x     = link_x,
                link_y     = link_y,
                normalize_data = normalize_data,
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
                          dist, link_x, link_y, normalize_data,
                          prior_only, control, refresh, method, priors, ...) {

  #Keep only the columns n, .tval, .delay, .strata
  .disease_data <- .disease_data |>
    dplyr::select(!!as.symbol("n"), !!as.symbol(".tval"), !!as.symbol(".delay"), !!as.symbol(".strata")) |>
    dplyr::arrange(!!as.symbol(".tval"), !!as.symbol(".strata"), !!as.symbol(".delay")) #IMPORTANT! Requirement for Stan model to be arranged by time, strata and delay in that order

  # Distribution
  distribution <- get_distribution_number(dist)

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
        cases_real  = N_cases[,1],
        cases_int   = as.integer(N_cases[,1]),

        #Whether to compute only the prior
        prior_only  = as.numeric(prior_only),
        dist        = distribution,
        link_x      = get_link_number(link_x),
        link_y      = get_link_number(link_y),
        normalize_data = normalize_data

  ))


  if (method[1] == "sampling"){
    stan_fit <- rstan::sampling(stanmodels$nowcasting, data = stan_data,
                           control = control,
                           refresh = refresh,
                           ...)
    draws    <- as.matrix(stan_fit)
  } else if (method[1] == "variational") {
    stan_fit <- rstan::vb(stanmodels$nowcasting, data = stan_data, refresh = refresh, ...)
    draws    <- as.matrix(stan_fit)
  } else if (method[1] == "optimization") {
    stan_fit <- rstan::optimizing(stanmodels$nowcasting, data = stan_data, refresh = refresh, ...)

    # Work around to get draws from the optimized params for gq
    draws           <- matrix(rep(stan_fit$par, 1000), ncol = length(stan_fit$par), byrow = TRUE)
    colnames(draws) <- names(stan_fit$par)

  } else {
    cli::cli_abort("Invalid method. Please select between {.val {c('sampling', 'variational','optimization')}}")
  }

  #Get the generated quantities
  generated_quantities <- rstan::gqs(stanmodels$generated_quantities, data = stan_data, draws = draws)

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
