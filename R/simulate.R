#' Simulate a process
#'
#' This function simulates a state-space process for testing purposes in a model with
#' multiple delays and strata. It estimates the process over a specified number of steps,
#'  with options for trend degrees, priors, and error distributions.
#'
#' @param num_steps Integer. Number of time steps to simulate. Default is 10.
#' @param num_delays Integer. Number of delay strata. Default is 8.
#' @param num_strata Integer. Number of strata for the population. Default is 2.
#' @param is_negative_binomial Logical. Indicates whether the data is modeled using a negative binomial distribution. Default is TRUE.
#' @inheritParams nowcast
#'
#' @return A tibble with simulated state-space process results, including the onset and
#' report dates, strata, delays, and the number of observed cases per time step.
#'
#' @export
#'
#' @examples
#' simulate_process_for_testing(num_steps = 20, num_delays = 5, num_strata = 3)
simulate_process_for_testing <- function(num_steps  = 10, num_delays = 8, num_strata = 2,
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
                                         mu_0_param_1 = log(100),          #First parameter for degree error in epidemic trend
                                         mu_0_param_2 = 0.01,          #Second parameter for degree error in epidemic trend
                                         nu_0_param_1 = log(6),          #First parameter for degree error in delay trend
                                         nu_0_param_2 = 0.01,          #Second parameter for degree error in delay trend
                                         r_prior  = "normal",         #Prior for the negative binomial precision
                                         is_negative_binomial = TRUE, #Whether data is negative binomial
                                         r_param_1 = 0.0,             #First parameter for dispersion prior if negative binomial
                                         r_param_2 = 1.0              #Second parameter for dispersion prior if negative binomial
){

  #Get the trend matrices using the Stan functions
  A_mu       <- create_trend_matrix_block_A(mu_degree, rstan::get_stream())
  A_nu       <- create_trend_matrix_block_A(nu_degree, rstan::get_stream())
  L_mu       <- create_trend_vector_block_L(mu_degree, rstan::get_stream())
  L_nu       <- create_trend_vector_block_L(nu_degree, rstan::get_stream())
  R_mu       <- create_trend_matrix_block_R(mu_degree, FALSE, rstan::get_stream())
  R_nu       <- create_trend_matrix_block_R(nu_degree, TRUE, rstan::get_stream())

  #Function for simulating
  rmu_0 <- get_prior_code_sim_R(mu_0_prior)
  mu_0  <- array(rmu_0(num_delays*num_strata*nrow(A_mu), mu_0_param_1, mu_0_param_2),
                 dim = c(num_strata, num_delays, nrow(A_mu)))
  rnu_0 <- get_prior_code_sim_R(nu_0_prior)
  nu_0  <- array(rnu_0(num_delays*num_strata*nrow(A_nu), nu_0_param_1, nu_0_param_2),
                 dim = c(num_strata, num_delays, nrow(A_nu)))

  B_cnt <- matrix(0, nrow = 1, ncol = 1)
  X_cnt <- matrix(0, nrow = 1, ncol = 1)

  rmu    <- get_prior_code_sim_R(mu_error_prior)
  xi_mu  <- array(rmu(num_delays*num_strata*(num_steps - 1)*ncol((R_mu)), mu_param_1, mu_param_2),
                  dim = c(num_steps - 1, num_strata*num_delays, ncol(R_mu)))

  rmu    <- get_prior_code_sim_R(nu_error_prior)
  xi_nu  <- array(rnorm(num_delays*num_strata*(num_steps - 1)*ncol(R_nu), nu_param_1, nu_param_2),
                  dim = c(num_steps - 1, num_strata*num_delays, ncol(R_nu)))

  #Compute the process in stan and compare with R
  ss_process <- state_space_process_v3(
    num_steps  = num_steps,
    num_delays = num_delays,
    num_strata = num_strata,
    A_mu = A_mu,
    A_nu = A_nu,
    R_mu = R_mu,
    R_nu = R_nu,
    L_mu = L_mu,
    L_nu = L_nu,
    mu_0  = matrix(mu_0, nrow = num_strata*num_delays, ncol = nrow(A_mu)),
    xi_mu = array_to_list(xi_mu, "matrix"),
    nu_0  = matrix(nu_0, nrow = num_strata*num_delays, ncol = nrow(A_nu)),
    xi_nu = array_to_list(xi_nu, "matrix"),
    B_cnt = B_cnt,
    X_cnt = X_cnt,
    pstream__ = rstan::get_stream())

  #Post format
  colnames(ss_process) <- 1:num_steps

  #Get the function to simulate R
  rfun <- get_prior_code_sim_R(r_prior)
  rval <- dplyr::if_else(is_negative_binomial, abs(rfun(1, r_param_1, r_param_2)), 0.0)

  #Get the function for simulating the data
  nfun <- ifelse(is_negative_binomial,
                 function(param_1, param_2) rnbinom(1, mu = param_1, size = param_2),
                 function(param_1, param_2) rpois(1, lambda = param_1))

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
    dplyr::mutate(!!as.symbol("onset_date") := as.Date(Sys.time()) - max(!!as.symbol(".tval")) + !!as.symbol(".tval") - 1) |>
    dplyr::mutate(!!as.symbol("report_date") := !!as.symbol("onset_date") + !!as.symbol(".delay") - 1)

  return(sim)

}
