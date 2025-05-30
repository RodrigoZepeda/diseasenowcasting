#' Simulate a process
#'
#' This function simulates a state-space process for testing purposes in a model with
#' multiple delays and strata. It estimates the process over a specified number of steps,
#'  with options for trend degrees, priors, and error distributions.
#'
#' @param num_steps Integer. Number of time steps to simulate. Default is 10.
#' @param num_delays Integer. Number of delay strata. Default is 8.
#' @param num_strata Integer. Number of strata for the population. Default is 2.
#' @param initial_day Date. If the simulation is to start on a specific day.
#' @param units Either `"daily"` (default) or `"weekly"`.
#' @param warmup_steps Initial steps on model (to discard)
#' @param ... Additional arguments to pass to [nowcast()]
#' @inheritParams nowcast
#'
#' @return A tibble with simulated state-space process results, including the onset and
#' report dates, strata, delays, and the number of observed cases per time step.
#'
#' @export
#'
#' @examples
#' # Simulate a disease for 20 time steps with delay of maximum 10 and 3 strata
#' set.seed(48672)
#' sims <- simulate_disease(num_steps = 20, num_delays = 3, num_strata = 3)
simulate_disease <- function(num_steps    = 10,
                             num_delays   = 8,
                             num_strata   = 2,
                             initial_day  = NULL,
                             warmup_steps = 50,
                             units        = c("daily", "weekly"),
                             priors       = random_priors(has_cycle = FALSE),
                             autoregresive  = AR(),
                             moving_average = MA(),
                             ...){


  #Check the warm-up steps
  warmup_steps <- ifelse(!is.numeric(warmup_steps) | warmup_steps < 0,
                         cli::cli_abort("Invalid warmup_steps. Set to an integer >= 0"),
                         ceiling(warmup_steps))

  #Check the units are either daily or weekly
  units     <- match.arg(units, c("daily", "weekly"))

  #Set the scale according to units
  scale_val <- ifelse(units[1] == "weekly", 7, 1)

  if (is.null(initial_day)){
    initial_day <- lubridate::today() - scale_val*(num_steps + warmup_steps) - scale_val*num_delays - 1
  }

  if (!lubridate::is.Date(initial_day)){
    cli::cli_abort("Invalid initial_day. Set it to a Date or to NULL")
  }

  #Create a fake dataset
  disease_data <- tidyr::expand_grid(
    .tval      =  seq(0, num_steps + warmup_steps - 1, by = 1),
    .delay     =  seq(0, num_delays - 1, by = 1),
    .strata    =  paste0("s", seq(1, num_strata)),
  ) |>
    dplyr::mutate(!!as.symbol("true_date")  := !!initial_day +
                    lubridate::days(!!scale_val*!!as.symbol(".tval"))) |>
    dplyr::mutate(!!as.symbol("report_date") := !!as.symbol("true_date")  +
                    lubridate::days(!!scale_val*!!as.symbol(".delay"))) |>
    dplyr::mutate(!!as.symbol("n") := !!1)

  #Generate fake dataset
  ss_process <- nowcast(disease_data, true_date = "true_date", report_date = "report_date",
                        strata = ".strata",
                        priors = priors,
                        prior_only = F,
                        algorithm = "Fixed_param",
                        method = "sampling",
                        chains = 1,
                        normalize_data = FALSE,
                        autoregresive = autoregresive,
                        moving_average = moving_average,
                        iter = 1000,
                        #init = get_priors_from_init(priors = priors,
                        #                            num_strata = num_strata,
                        #                            num_delays = num_delays,
                        #                            num_steps = num_steps + warmup_steps,
                        #                            autoregresive = autoregresive,
                        #                            moving_average = moving_average),
                        ...)

  #Create the simulation tibble
  simulations <- ss_process$generated_quantities |>
    posterior::as_draws() |>
    posterior::subset_draws(c("N_mat_predict")) |>
    posterior::summarise_draws() |>
    dplyr::mutate(!!as.symbol(".pos")  := as.numeric(stringr::str_remove_all(!!as.symbol("variable"),".*\\[.*,|\\]"))) |>
    dplyr::mutate(!!as.symbol(".tval") := as.numeric(stringr::str_remove_all(!!as.symbol("variable"),".*\\[|,.*\\]"))) |>
    dplyr::filter(!!as.symbol(".tval") >= !!warmup_steps) |>
    dplyr::select(!!as.symbol(".tval"), !!as.symbol(".pos"), !!as.symbol("median")) |>
    dplyr::group_by(!!as.symbol(".tval"), !!as.symbol(".pos")) |>
    dplyr::summarise(!!as.symbol("median") := sum(!!as.symbol("median")), .groups = "drop") |>
    dplyr::left_join(
      tidyr::expand_grid(
        .delay     =  seq(0, num_delays - 1, by = 1),
        .strata    =  paste0("s", seq(1, num_strata)),
      ) |>
        dplyr::mutate(!!as.symbol(".pos") := 1:dplyr::n()),
      by = ".pos"
    ) |>
    dplyr::mutate(!!as.symbol("true_date")  := !!initial_day +
                    lubridate::days(!!scale_val*!!as.symbol(".tval"))) |>
    dplyr::mutate(!!as.symbol("report_date") := !!as.symbol("true_date")  +
                    lubridate::days(!!scale_val*!!as.symbol(".delay"))) |>
    dplyr::rename(!!as.symbol("n") := !!as.symbol("median")) |>
    dplyr::select(-!!as.symbol(".tval"), -!!as.symbol(".pos"), -!!as.symbol(".delay")) |>
    dplyr::mutate_at("n", ceiling) |>
    dplyr::mutate_at("n", abs)


  return(simulations)

}
