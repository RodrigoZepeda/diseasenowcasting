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
                                         dist = c("NegativeBinomial", "Poisson"),
                                         mu_degree = 1,
                                         nu_degree = 1,
                                         mu_is_constant = FALSE,
                                         nu_is_constant = TRUE,
                                         mu_sd_prior = "normal",
                                         nu_sd_prior = "normal",
                                         mu_sd_param_1 = 0.0,
                                         mu_sd_param_2 = 0.1,
                                         nu_sd_param_1 = 0.0,
                                         nu_sd_param_2 = 0.1,
                                         mu_0_mean_param_1 = log(100),
                                         mu_0_mean_param_2 = 0.01,
                                         mu_0_sd_param_1 = 0.01,
                                         mu_0_sd_param_2 = 0.01,
                                         nu_0_mean_param_1 = 0.0,
                                         nu_0_mean_param_2 = 0.01,
                                         nu_0_sd_param_1 = 0.00,
                                         nu_0_sd_param_2 = 0.01,
                                         mu_0_mean_hyperprior = "normal",
                                         nu_0_mean_hyperprior = "normal",
                                         mu_0_sd_hyperprior = "normal",
                                         nu_0_sd_hyperprior = "normal",
                                         r_prior  = "normal",
                                         r_param_1 = 0.0,
                                         r_param_2 = 1.0
){

  # Match the distribution whether negative binomial or poisson
  dist <- match.arg(dist, c("NegativeBinomial", "Poisson"))
  is_negative_binomial <- (dist == "NegativeBinomial")

  #Get the trend matrices using the Stan functions
  A_mu       <- create_trend_matrix_block_A(mu_degree, rstan::get_stream())
  A_nu       <- create_trend_matrix_block_A(nu_degree, rstan::get_stream())
  L_mu       <- create_trend_vector_block_L(mu_degree, rstan::get_stream())
  L_nu       <- create_trend_vector_block_L(nu_degree, rstan::get_stream())
  R_mu       <- create_trend_matrix_block_R(mu_degree, FALSE, rstan::get_stream())
  R_nu       <- create_trend_matrix_block_R(nu_degree, TRUE, rstan::get_stream())

  #Function for simulating
  rmu_0_mean <- get_prior_code_sim_R(mu_0_mean_hyperprior)
  rmu_0_sd   <- get_prior_code_sim_R(mu_0_sd_hyperprior)

  #Generate arrays of means and standard deviations for mu_0 and nu_0
  mu_0_mean  <- rmu_0_mean(1, mu_0_mean_param_1, mu_0_mean_param_2)
  mu_0_sd    <- rmu_0_sd(1, mu_0_sd_param_1, mu_0_sd_param_2) |> abs()
  mu_0       <- array(rnorm(num_delays*num_strata*nrow(A_mu), mu_0_mean, mu_0_sd),
                      dim = c(num_strata, num_delays, nrow(A_mu)))

  #Repeat process with nu
  rnu_0_mean <- get_prior_code_sim_R(nu_0_mean_hyperprior)
  rnu_0_sd   <- get_prior_code_sim_R(nu_0_sd_hyperprior)

  nu_0_mean  <- rnu_0_mean(1, nu_0_mean_param_1, nu_0_mean_param_2)
  nu_0_sd    <- rnu_0_sd(1, nu_0_sd_param_1, nu_0_sd_param_2) |> abs()
  nu_0       <- array(rnorm(num_delays*num_strata*nrow(A_nu), nu_0_mean, nu_0_sd),
                      dim = c(num_strata, num_delays, nrow(A_nu)))

  #Get the covariates
  B_cnt <- matrix(0, nrow = 1, ncol = 1)
  X_cnt <- matrix(0, nrow = 1, ncol = 1)

  #Get the simulations for the errors
  rmu_sd <- get_prior_code_sim_R(mu_sd_prior)
  mu_sd  <- rmu_sd(1, mu_sd_param_1, mu_sd_param_2)  |> abs()

  rnu_sd <- get_prior_code_sim_R(nu_sd_prior)
  nu_sd  <- rnu_sd(1, nu_sd_param_1, nu_sd_param_2) |> abs()

  #Create the error matrices
  xi_mu  <- array(rnorm(num_delays*num_strata*(num_steps - 1)*ncol((R_mu)), 0.0, mu_sd),
                  dim = c(num_steps - 1, num_strata*num_delays, ncol(R_mu)))
  xi_nu  <- array(rnorm(num_delays*num_strata*(num_steps - 1)*ncol(R_nu), 0.0, nu_sd),
                  dim = c(num_steps - 1, num_strata*num_delays, ncol(R_nu)))

  #Compute the process in stan and compare with R
  ss_process <- state_space_process(
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
    dplyr::select(-!!as.symbol("lambda")) |>
    dplyr::mutate(!!as.symbol("onset_date") := as.Date(Sys.time()) - max(!!as.symbol(".tval")) + !!as.symbol(".tval") - 1) |>
    dplyr::mutate(!!as.symbol("report_date") := !!as.symbol("onset_date") + !!as.symbol(".delay") - 1)

  return(sim)

}
