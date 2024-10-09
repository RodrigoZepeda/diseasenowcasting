#' Get nowcast priors not controlling by distribution
#'
#' Function to return the default values for the [nowcast()] function
#' as a list.
#'
#' @param mu_p Integer. Degree of the epidemic trend.
#'
#' @param mu_q Integer. Degree of the errors in the epidemic trend.
#'
#' @param nu_p Integer. Degree of the delay trend
#'
#' @param mu_0_param_1 Real. Mean of the initial value of the epidemic trend.
#'
#' @param mu_0_param_2 Positive Real. Variance of the initial value of the epidemic trend.
#'
#' @param nu_0_param_1 Real. Mean of the initial value of the disease trend.
#'
#' @param nu_0_param_2 Positive Real. Variance of the initial value of the disease trend.
#'
#' @param mu_intercept_param_1 Real. Mean of the epidemic intercept.
#'
#' @param mu_intercept_param_2 Positive real. Variance of the epidemic intercept.
#'
#' @param nu_intercept_param_1 Real. Mean of the delay intercept.
#'
#' @param nu_intercept_param_2 Positive real. Variance of the delay intercept.
#'
#' @param sd_mu_param_1 Positive real. Initial value for the mean of the standard deviation of the epidemic trend.
#'
#' @param sd_mu_param_2 Positive real. Initial value for the variance of the standard deviation of the epidemic trend.
#'
#' @param sd_nu_param_1 Positive real. Initial value for the mean of the standard deviation of the disease trend.
#'
#' @param sd_nu_param_2 Positive real. Initial value for the variance of the standard deviation of the disease trend.
#'
#' @param sd_m_param_1 Positive real. Initial value for the mean of the standard deviation of the observed cases (not considered if distribution is Poisson)
#'
#' @param sd_m_param_2 Positive real. Initial value for the variance of the standard deviation of the observed cases (not considered if distribution is Poisson)
#'
#' @param dof Degrees of freedom if the distribution used in [nowcast()] is Student (default = 7).
#'
#' @param control_k_transform Parameter for the `dhyperbolic` or `softplus` link functions.
#'
#' @param control_c_transform Parameter for the `dhyperbolic` link function.
#'
#' @return A list with all the priors for the [nowcast()] function.
#'
#' @examples
#' #Get the default priors
#' set_priors()
#'
#' #Change the priors
#' set_priors(mu_intercept_param_1 = 2, mu_q = 3)
#'
#' @export
set_priors <- function(
    mu_p = 1,
    mu_q = 1,
    nu_p = 1,
    mu_intercept_param_1 = 0,
    mu_intercept_param_2 = 1,
    mu_0_param_1 = 0,
    mu_0_param_2 = 1,
    nu_intercept_param_1 = 0,
    nu_intercept_param_2 = 1,
    nu_0_param_1 = 0,
    nu_0_param_2 = 1,
    sd_mu_param_1 = 0,
    sd_mu_param_2 = 1,
    sd_nu_param_1 = 0,
    sd_nu_param_2 = 1,
    sd_m_param_1 = 0,
    sd_m_param_2 = 1,
    dof = 7, #Degrees of freedom for student t
    control_k_transform = 2,
    control_c_transform = 0.5
  ) {

  #Get the priors
  priors <- as.list(environment())

  return(priors)

}

#' Function to randomly set the priors
#'
#' @description
#' Assign random priors. This is mostly used with the [simulate_process()] function.
#'
#' @param ... Any parameter used in [set_priors()] that will remain fixed (not random).
#' @examples
#' #Get random priors
#' random_priors()
#'
#' #Get random priors except for mu_0_param_1 which is fixed at 1 and nu_0_param_2 fixed at 0.5
#' random_priors(mu_0_param_1 = 1, nu_0_param_2 = 0.5)
#'
#' @export
random_priors <- function(...) {

  constant_priors <- c("mu_p", "mu_q", "nu_p", "dof", "control_k_transform", "control_c_transform")

  priors_means    <- set_priors()

  #Get the priors
  priors_set      <- list(...)

  #Randomly select the prior means
  random_priors   <- priors_means[which(!(names(priors_means) %in% names(priors_set)))]
  random_priors   <- priors_means[which(!(names(priors_means) %in% constant_priors))]
  constant_priors <- constant_priors[which(!(constant_priors %in% names(priors_set)))]

  #Remove the distribution specification
  for (pr in names(random_priors)){
    random_priors[pr] <- rnorm(1, as.numeric(random_priors[pr]), 1)

    #Make sd's positive
    if (grepl("sd|param_2", pr)){
      random_priors[pr] <- abs(as.numeric(random_priors[pr]))
    }
  }

  #Combine everything together
  random_priors |>
    append(priors_set) |>
    append(
      priors_means[which(names(priors_means) %in% constant_priors)]
    )

}

#' Function for setting the distribution from words to numner
#'
#' @inheritParams nowcast
#'
#' @return A number for the distribution code for Stan's `data`.
#' @keywords internal
get_distribution_number <- function(dist){
  switch(dist,
         "Normal"           = 0,
         "Student"          = 1,
         "Poisson"          = 2,
         "NegativeBinomial" = 3,
         cli::cli_abort("Invalid distribution {dist}"))
}

#' Function for sets the link required
#'
#' @inheritParams nowcast
#'
#' @description
#' The following links are implemented:
#' * identity: g(x) = x
#' * log: g(x) = log(x)
#' * softplus: g(x) = k*log(1 + exp(x/k))
#' * dhyperbolic: g(x) = c*x/2 + sqrt( (c*x/2)^2 + k)
#'
#' @return A number for the distribution code for Stan's `data`.
#' @keywords internal
get_link_number <- function(link){
  switch(link,
         "identity"             = 0,
         "log"                  = 1,
         "softplus"             = 2,
         "dhyperbolic"          = 3,
         cli::cli_abort("Invalid link {link}"))
}

#' Function for setting the initial values from priors for simulation
#'
#' @inheritParams simulate_disease
#'
#' @return An init function that generates initial values based upon the priors to send to
#' any `rstan` sampling or optimization algorithm.
#'
#' @keywords internal
get_priors_from_init <- function(priors, num_strata, num_delays, num_steps){
  #Add the priors
  initfun <- function(...) {

    #Normalize phi_ar
    if (priors$mu_p > 0){
      phi_mu <- list(stats::runif(priors$mu_p, -1, 1))
    } else {
      phi_mu <- as.numeric()
    }

    if (priors$mu_q > 0){
      theta_mu <- list(stats::runif(priors$mu_q, -1, 1))
    } else {
      theta_mu <- as.numeric()
    }

    if (priors$nu_p > 0){
      phi_nu <- list(stats::runif(priors$nu_p, -1, 1))
    } else {
      phi_nu <- as.numeric()
    }

    #Prior variance
    rval <- list(as.vector(abs(stats::rnorm(1, priors$sd_m_param_1, priors$sd_m_param_2))))

    list(
      phi_mu                = phi_mu,
      theta_mu              = theta_mu,
      phi_nu                = phi_nu,
      mu_intercept_centered = rnorm(num_strata*num_delays),
      nu_intercept_centered = rnorm(num_strata),
      mu_init_centered      = rnorm(num_strata*num_delays),
      nu_init_centered      = rnorm(num_strata),
      xi_mu                 = matrix(rnorm(num_strata*num_delays*(num_steps - 1)), ncol = num_steps - 1),
      xi_nu                 = matrix(rnorm(num_strata*(num_delays - 1)), ncol = num_delays - 1),
      sd_mu                 = rnorm(1, priors$sd_mu_param_1, priors$sd_mu_param_2) |> abs(),
      sd_nu                 = rnorm(1, priors$sd_nu_param_1, priors$sd_nu_param_2) |> abs(),
      sd_m                  = rval
    )
  }
  return(initfun)
}
