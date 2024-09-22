#' Simulate a process
#'
#' This function simulates a state-space process for testing purposes in a model with
#' multiple delays and strata. It estimates the process over a specified number of steps,
#'  with options for trend degrees, priors, and error distributions.
#'
#' @param num_steps Integer. Number of time steps to simulate. Default is 10.
#' @param num_delays Integer. Number of delay strata. Default is 8.
#' @param num_strata Integer. Number of strata for the population. Default is 2.
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
                                         dist   = c("NegativeBinomial", "Poisson"),
                                         priors = set_priors()
){

  # Match the distribution whether negative binomial or poisson
  dist <- match.arg(dist, c("NegativeBinomial", "Poisson"))
  is_negative_binomial <- (dist == "NegativeBinomial")





  #Create the simulation tibble
  sim <- dplyr::as_tibble(ss_process, .name_repair = "unique") |>
    dplyr::mutate(!!as.symbol(".strata") := rep(1:num_strata, num_delays)) |>
    dplyr::mutate(!!as.symbol(".delay") := rep(1:num_delays, each = num_strata)) |>
    tidyr::pivot_longer(cols = c(dplyr::everything(), -!!as.symbol(".delay"), -!!as.symbol(".strata")),
                        names_to = ".tval", values_to = "n", names_transform = as.numeric) |>
    dplyr::mutate(!!as.symbol("lambda")  := exp(!!as.symbol("n"))) |>
    dplyr::rowwise() |>
    dplyr::mutate(!!as.symbol("n") := nfun(!!as.symbol("lambda"), !!rval)) |>
    dplyr::ungroup() |>
    dplyr::select(-!!as.symbol("lambda")) |>
    dplyr::mutate(!!as.symbol("onset_date") := as.Date(Sys.time()) - max(!!as.symbol(".tval")) + !!as.symbol(".tval") - 1) |>
    dplyr::mutate(!!as.symbol("report_date") := !!as.symbol("onset_date") + !!as.symbol(".delay") - 1)

  return(sim)

}
