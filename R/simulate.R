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
#' @param initial_n Real. The initial expected average value of cases.
#' @param units Either `"daily"` (default) or `"weekly"`.
#' @inheritParams nowcast
#'
#' @return A tibble with simulated state-space process results, including the onset and
#' report dates, strata, delays, and the number of observed cases per time step.
#'
#' @export
#'
#' @examples
#' simulate_process_for_testing(num_steps = 20, num_delays = 5, num_strata = 3)
simulate_process_for_testing <- function(num_steps  = 10,
                                         num_delays = 8,
                                         num_strata = 2,
                                         initial_day = NULL,
                                         initial_n   = 100,
                                         dist   = c("NegativeBinomial", "Poisson"),
                                         units = c("daily", "weekly"),
                                         priors = set_priors()){


  cli::cli_alert_warning("This function will disappear in future versions please call `simulate_disease`")
  simulate_disease(num_steps, num_delays, num_strata, initial_day, initial_n, dist, units, priors)

}

#' Simulate a process
#'
#' This function simulates a state-space process for testing purposes in a model with
#' multiple delays and strata. It estimates the process over a spesupcified number of steps,
#'  with options for trend degrees, priors, and error distributions.
#'
#' @param num_steps Integer. Number of time steps to simulate. Default is 10.
#' @param num_delays Integer. Number of delay strata. Default is 8.
#' @param num_strata Integer. Number of strata for the population. Default is 2.
#' @param initial_day Date. If the simulation is to start on a specific day.
#' @param initial_n Real. The initial expected average value of cases.
#' @param units Either `"daily"` (default) or `"weekly"`.
#' @inheritParams nowcast
#'
#' @return A tibble with simulated state-space process results, including the onset and
#' report dates, strata, delays, and the number of observed cases per time step.
#'
#' @export
#'
#' @examples
#' simulate_process_for_testing(num_steps = 20, num_delays = 5, num_strata = 3)
simulate_disease <- function(num_steps  = 10, num_delays = 8, num_strata = 2,
                             initial_day = NULL, initial_n   = 100,
                             dist   = c("NegativeBinomial", "Poisson"),
                             units = c("daily", "weekly"),
                             priors = set_priors()){


  scale_val <- ifelse(units[1] == "weekly", 7, 1)

  if (is.null(initial_day) || !lubridate::is.Date(initial_day)){
    initial_day <- lubridate::today() - scale_val*num_steps - scale_val*num_delays - 1
  }

  #Create a fake dataset
  disease_data <- tidyr::expand_grid(
    .tval      =  seq(0, num_steps - 1, by = 1),
    .delay     =  seq(0, num_delays - 1, by = 1),
    .strata    =  paste0("s", seq(1, num_strata)),
  ) |>
    dplyr::mutate(!!as.symbol("onset_date")  := !!initial_day +
                    lubridate::days(!!scale_val*!!as.symbol(".tval"))) |>
    dplyr::mutate(!!as.symbol("report_date") := !!as.symbol("onset_date")  +
                    lubridate::days(!!scale_val*!!as.symbol(".delay"))) |>
    dplyr::mutate(!!as.symbol("n") := !!initial_n)

  #Generate fake dataset
  ss_process <- nowcast(disease_data, onset_date = "onset_date", report_date = "report_date",
                        strata = ".strata", prior_only = TRUE, priors = priors,
                        algorithm = "Fixed_param", dist = dist, chains = 1)

  #Create the simulation tibble
  simulations <- ss_process$generated_quantities |>
    posterior::as_draws() |>
    posterior::subset_draws("N_mat_predict") |>
    posterior::summarise_draws() |>
    dplyr::mutate(!!as.symbol(".pos")  := as.numeric(stringr::str_remove_all(!!as.symbol("variable"),".*\\[.*,|\\]"))) |>
    dplyr::mutate(!!as.symbol(".tval") := as.numeric(stringr::str_remove_all(!!as.symbol("variable"),".*\\[|,.*\\]"))) |>
    dplyr::select(!!as.symbol(".tval"), !!as.symbol(".pos"), !!as.symbol("median")) |>
    dplyr::left_join(
      tidyr::expand_grid(
        .delay     =  seq(0, num_delays - 1, by = 1),
        .strata    =  paste0("s", seq(1, num_strata)),
      ) |>
        dplyr::mutate(!!as.symbol(".pos") := 1:dplyr::n()),
      by = ".pos"
    ) |>
    dplyr::mutate(!!as.symbol("onset_date")  := !!initial_day +
                    lubridate::days(!!scale_val*!!as.symbol(".tval"))) |>
    dplyr::mutate(!!as.symbol("report_date") := !!as.symbol("onset_date")  +
                    lubridate::days(!!scale_val*!!as.symbol(".delay"))) |>
    dplyr::rename(!!as.symbol("n") := !!as.symbol("median")) |>
    dplyr::select(-!!as.symbol(".tval"), -!!as.symbol(".pos"), -!!as.symbol(".delay"))

  return(simulations)

}
