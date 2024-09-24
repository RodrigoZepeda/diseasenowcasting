#' Get nowcast priors
#'
#' Function to return the default values for the [nowcast()] function
#' as a list.
#'
#' @param mu_degree Integer. Degree of the epidemic trend.
#'
#' @param nu_degree Integer. Degree of the delay trend.
#'
#' @param p Integer. The number of lags to consider for an autocorrelated AR(p) model
#'
#' @param q Integer. The number of lags to consider for a moving average MA(q) model
#'
#' @param mu_is_constant Logical. Indicates whether the epidemic trend is constant.
#'
#' @param nu_is_constant Logical. Indicates whether the delay trend is constant.
#'
#' @param mu_sd_prior Character. Prior for the epidemic trend error.
#'
#' @param nu_sd_prior Character. Prior for the delay trend error.
#'
#' @param mu_sd_param_1 Numeric. First parameter for the epidemic trend error.
#'
#' @param mu_sd_param_2 Numeric. Second parameter for the epidemic trend error.
#'
#' @param nu_sd_param_1 Numeric. First parameter for the delay trend error.
#'
#' @param nu_sd_param_2 Numeric. Second parameter for the delay trend error.
#'
#' @param mu_0_mean_hyperprior Prior distribution for the mean of the epidemic trend.
#'
#' @param mu_0_sd_hyperprior Prior distribution for the standard deviation of the epidemic trend.
#'
#' @param mu_0_mean_param_1 Character. Prior for the initial epidemic trend mean's first parameter.
#'
#' @param mu_0_mean_param_2 Character. Prior for the initial epidemic trend mean's second parameter.
#'
#' @param mu_0_sd_param_1 Character. Prior for the initial epidemic trend standard deviation's first parameter.
#'
#' @param mu_0_sd_param_2 Character. Prior for the initial epidemic trend standard deviation's second parameter.
#'
#' @param nu_0_mean_hyperprior Prior distribution for the mean of the delay trend. Default is normal.
#'
#' @param nu_0_sd_hyperprior Prior distribution for the standard deviation of the delay trend.
#'
#' @param nu_0_mean_param_1 Character. Prior for the initial delay trend mean's first parameter.
#'
#' @param nu_0_mean_param_2 Character. Prior for the initial delay trend mean's second parameter.
#'
#' @param nu_0_sd_param_1 Character. Prior for the initial delay trend standard deviation's first parameter.
#'
#' @param nu_0_sd_param_2 Character. Prior for the initial delay trend standard deviation's second parameter.
#'
#' @param r_prior Character. Prior for the negative binomial precision parameter.
#'
#' @param r_param_1 Numeric. First parameter for the dispersion prior if negative binomial.
#'
#' @param r_param_2 Numeric. Second parameter for the dispersion prior if negative binomial.
#'
#' @param phi_AR_param_1 Numeric. Prior for the AR coefficients' first parameter.
#'
#' @param phi_AR_param_2 Numeric. Prior for the AR coefficients' second parameter.
#'
#' @param phi_AR_prior String Prior distirbution name for the AR coefficients.
#'
#' @param theta_MA_param_1 Numeric. Prior for the MA coefficients' first parameter.
#'
#' @param theta_MA_param_2 Numeric. Prior for the MA coefficients' second parameter.
#'
#' @param theta_MA_prior String Prior distirbution name for the MA coefficients.
#'
#' @param xi_sd_param_1 Numeric. Prior for the error (in log-scale) first parameter
#'
#' @param xi_sd_param_2 Numeric. Prior for the error (in log-scale) second parameter
#'
#' @param xi_sd_prior String Prior distribution name for the errors
#'
#' @return A list with all the priors for the [nowcast()] function.
#'
#' @examples
#' #Get the default priors
#' set_priors()
#'
#' #Change the distribution of the priors sd for the epidemic trend as well as the degree of the delays
#' set_priors(nu_degree = 2, mu_sd_prior = "cauchy")
#'
#' @export
set_priors <- function(mu_degree            = 1,
                       nu_degree            = 1,
                       p                    = 3,
                       q                    = 0,
                       mu_is_constant       = FALSE,
                       nu_is_constant       = TRUE,
                       mu_sd_prior          = "standard_normal",
                       nu_sd_prior          = "standard_normal",
                       mu_sd_param_1        = 0.0,
                       mu_sd_param_2        = 0.1,
                       nu_sd_param_1        = 0.0,
                       nu_sd_param_2        = 0.1,
                       phi_AR_param_1       = 0.0,
                       phi_AR_param_2       = 0.01,
                       phi_AR_prior         = "cauchy",
                       theta_MA_param_1     = 0.0,
                       theta_MA_param_2     = 0.1,
                       theta_MA_prior       = "cauchy",
                       xi_sd_param_1        = 0.0,
                       xi_sd_param_2        = 0.1,
                       xi_sd_prior          = "standard_normal",
                       mu_0_mean_param_1    = "auto",
                       mu_0_mean_param_2    = 0.01,
                       mu_0_sd_param_1      = "auto",
                       mu_0_sd_param_2      = 0.01,
                       nu_0_mean_param_1    = 0.0,
                       nu_0_mean_param_2    = 0.01,
                       nu_0_sd_param_1      = 0.00,
                       nu_0_sd_param_2      = 0.01,
                       mu_0_mean_hyperprior = "standard_normal",
                       nu_0_mean_hyperprior = "standard_normal",
                       mu_0_sd_hyperprior   = "standard_normal",
                       nu_0_sd_hyperprior   = "standard_normal",
                       r_prior              = "standard_normal",
                       r_param_1            = 0.0,
                       r_param_2            = 1.0) {

  return(as.list(environment()))

}


#' Function for setting the priors to numeric
#'
#' @inheritParams nowcast
#'
#' @return The same list of priors but with the numeric codes RStan requires for the distributions
#' @keywords internal
priors_to_numeric <- function(.disease_data, priors){

  #1) SUBSTITUTE THE MU_0 MEAN AND SD PRIOR FOR THE AVERAGE NUMBER OF CASES
  #Get the log mean of disease data and the sd
  log_mean <- .disease_data |>
    dplyr::summarise(log_mean = mean(log1p(!!as.symbol("n")), na.rm = T)) |>
    dplyr::pull(log_mean)

  #Get the log mean of disease data and the sd
  log_sd <- .disease_data |>
    dplyr::summarise(log_sd = sd(log1p(!!as.symbol("n")), na.rm = T)) |>
    dplyr::pull(log_sd)

  priors$mu_0_mean_param_1 <- ifelse(priors$mu_0_mean_param_1 == "auto", log_mean, priors$mu_0_mean_param_1)
  priors$mu_0_sd_param_1   <- ifelse(priors$mu_0_sd_param_1 == "auto", log_sd, priors$mu_0_sd_param_1)

  #1) SUBSTITUTE THE HYPERPRIOR DISTRIBUTIONS FROM WORDS TO NUMERIC
  #Set the priors that are in words to numeric
  priors$mu_sd_prior          <- get_prior_code_stan(priors$mu_sd_prior)
  priors$nu_sd_prior          <- get_prior_code_stan(priors$nu_sd_prior)
  priors$mu_0_mean_hyperprior <- get_prior_code_stan(priors$mu_0_mean_hyperprior)
  priors$nu_0_mean_hyperprior <- get_prior_code_stan(priors$nu_0_mean_hyperprior)
  priors$mu_0_sd_hyperprior   <- get_prior_code_stan(priors$mu_0_sd_hyperprior)
  priors$nu_0_sd_hyperprior   <- get_prior_code_stan(priors$nu_0_sd_hyperprior)
  priors$r_prior              <- get_prior_code_stan(priors$r_prior)
  priors$phi_AR_prior         <- get_prior_code_stan(priors$phi_AR_prior)
  priors$theta_MA_prior       <- get_prior_code_stan(priors$theta_MA_prior)
  priors$xi_sd_prior          <- get_prior_code_stan(priors$xi_sd_prior)

  return(priors)
}

#' Function for setting the initial values from priors for simulation
#'
#' @inheritParams simulate_disease
#'
#' @return An init function that generates initial values based upon the priors to send to
#' any `rstan` sampling or optimization algorithm.
#' @keywords internal
get_priors_from_init <- function(priors, num_strata, num_delays, num_steps, dist){
  #Add the priors
  #FIXME: Simulate stuff with rnorm to give more movement
  initfun <- function(...) {

    #Normalize phi_ar
    if (priors$p > 0){
      phi_ar <- rnorm(priors$p, priors$phi_AR_param_1, priors$phi_AR_param_2)
      phi_ar <- phi_ar/(max(abs(phi_ar)) + min(abs(phi_ar)))
    } else {
      phi_ar <- as.numeric()
    }

    if (priors$q > 0){
      theta_ma <- rnorm(priors$q, priors$theta_MA_param_1, priors$theta_MA_param_2)
      theta_ma <- theta_ma/(max(abs(theta_ma)) + min(abs(theta_ma)))
    } else {
      theta_ma <- as.numeric()
    }

    if (dist == "Poisson"){
      rval <- list()
    } else  {
      rval <- as.vector(abs(rnorm(1, priors$r_param_1, priors$r_param_2)))
    }

    list(
      mu_0_mean = ifelse(priors$mu_0_mean_param_1 == "auto", rnorm(1), rnorm(1, priors$mu_0_mean_param_1, priors$mu_0_mean_param_2)),
      nu_0_mean = ifelse(priors$nu_0_mean_param_1 == "auto", rnorm(1), rnorm(1, priors$nu_0_mean_param_1, priors$nu_0_mean_param_2)),
      mu_0_sd   = ifelse(priors$mu_0_sd_param_1 == "auto", abs(rnorm(1)), abs(rnorm(1, priors$mu_0_sd_param_1, priors$mu_0_sd_param_2))),
      nu_0_sd   = ifelse(priors$nu_0_sd_param_1 == "auto", abs(rnorm(1)), abs(rnorm(1, priors$nu_0_sd_param_1, priors$nu_0_sd_param_2))),
      xi_sd     = rnorm(1, priors$xi_sd_param_1, priors$xi_sd_param_2) |> abs(),
      xi_mu_sd  = rnorm(1, priors$mu_sd_param_1, priors$mu_sd_param_2) |> abs(),
      xi_nu_sd  = rnorm(1, priors$nu_sd_param_1, priors$nu_sd_param_2) |> abs(),
      phi_AR    = phi_ar,
      theta_MA  = theta_ma,
      r         = list(rval)
    )
  }
  return(initfun)
}
