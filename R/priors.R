#' Get nowcast priors not controlling by distribution
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

set_priors <- function(
     mu_degree            = 1,
     nu_degree            = 1,
     p                    = 3,
     q                    = 0,
     mu_is_constant       = FALSE,
     nu_is_constant       = TRUE,
     mu_sd_prior          = "cauchy",
     nu_sd_prior          = "cauchy",
     mu_sd_param_1        = 0.0,
     mu_sd_param_2        = 1.0,
     nu_sd_param_1        = 0.0,
     nu_sd_param_2        = 1.0,
     phi_AR_param_1       = 0.0,
     phi_AR_param_2       = 1.0,
     phi_AR_prior         = "cauchy",
     theta_MA_param_1     = 0.0,
     theta_MA_param_2     = 1.0,
     theta_MA_prior       = "cauchy",
     xi_sd_param_1        = 0.0,
     xi_sd_param_2        = 1.0,
     xi_sd_prior          = "student_t",
     mu_0_mean_param_1    = "auto",
     mu_0_mean_param_2    = 1.0,
     mu_0_sd_param_1      = "auto",
     mu_0_sd_param_2      = 1.0,
     nu_0_mean_param_1    = 0.0,
     nu_0_mean_param_2    = 1.0,
     nu_0_sd_param_1      = 0.00,
     nu_0_sd_param_2      = 1.0,
     mu_0_mean_hyperprior = "cauchy",
     nu_0_mean_hyperprior = "cauchy",
     mu_0_sd_hyperprior   = "cauchy",
     nu_0_sd_hyperprior   = "cauchy",
     r_prior              = "cauchy",
     r_param_1            = 0.0,
     r_param_2            = 1.0) {

  return(as.list(environment()))

}

#' Get nowcast priors for the discrete case
#'
#' Function to return the priors when the data is assumed to follow a discrete distribution
#'
#' @param ... Name of the parameters for the priors to pass to [set_priors()].
#'
#' @return A list of the default priors for the nowcasting model when the distribution is discrete.
#'
#' @keywords internal
default_discrete_priors <- function(...){

  #Get the parameters passed in function
  vals      <<- list(...)
  elem_vals <- names(vals)

  #Get the other priors
  defaults <- set_priors()
  elem_default <- names(defaults)

  #Check that all passed elements are valid elements
  not_priors <- which(!(elem_vals %in% elem_default))
  if (length(not_priors) > 0){
    cli::cli_abort(
      "Elements {.val {elem_vals[not_priors]}} not valid arguments for `set_priors()`. Please check."
    )
  }

  #Get the defaults not in elem_vals
  not_in_vals <- which(!(elem_default %in% elem_vals))
  if (length(not_in_vals) > 0){
    defaults <- defaults[which(!(elem_default %in% elem_vals))]
  } else {
    defaults <- list()
  }

  #Append and return
  defaults |> append(vals)

}

#' Get nowcast priors for the continuous case
#'
#' Function to return the priors when the data is assumed to follow a continuous distribution
#'
#' @inheritParams default_continuous_priors
#'
#' @return A list of the default priors for the nowcasting model when the distribution is continuous
#'
#' @keywords internal
default_continuous_priors <- function(...){

  #Get the parameters passed in function
  vals      <<- list(...)
  elem_vals <- names(vals)

  #Get the other priors
  defaults <- set_priors()
  elem_default <- names(defaults)

  #Check that all passed elements are valid elements
  not_priors <- which(!(elem_vals %in% elem_default))
  if (length(not_priors) > 0){
    cli::cli_abort(
      "Elements {.val {elem_vals[not_priors]}} not valid arguments for `set_priors()`. Please check."
    )
  }

  #Get the defaults not in elem_vals
  not_in_vals <- which(!(elem_default %in% elem_vals))
  if (length(not_in_vals) > 0){
    defaults <- defaults[which(!(elem_default %in% elem_vals))]
  } else {
    defaults <- list()
  }

  #Append and return
  defaults |> append(vals)

}


#' Get nowcast priors for the continuous case
#'
#' Function to return the priors when the data is assumed to follow a continuous distribution
#'
#' @inheritParams nowcast
#' @inheritParams default_continuous_priors
#'
#' @return A list of the default priors for the nowcasting model when the distribution is discrete.
#' @examples
#'
#' #Use inside nowcasting as dollows:
#' data(denguedat)
#'
#' # Running a quick nowcast with almost zero iterations
#' now <- as.Date("1990-10-01")
#' nowcast(denguedat, "onset_week", "report_week", now = now,
#'   method = "optimization", seed = 2495624, iter = 10,
#'   priors = default_priors("Normal", p = 0, q = 1)) #Change whatever you want from the defaults
#'
#'
#' @export
default_priors <- function(dist, ...){

  dist <- match.arg(dist, c("NegativeBinomial","Normal","Student","Poisson"))

  if (dist %in% c("Normal", "Student")){
    return(
      default_continuous_priors(
        mu_0_mean_param_1 = "mean",
        mu_0_sd_param_1 = "sd"
      )
    )
  } else {
    return(
      default_discrete_priors(
        mu_0_mean_param_1 = "logmean",
        mu_0_sd_param_1 = "logsd"
      )
    )
  }

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
  if (priors$mu_0_mean_param_1 == "logmean"){
    log_mean <- .disease_data |>
      dplyr::summarise(log_mean = mean(log1p(!!as.symbol("n")), na.rm = T)) |>
      dplyr::pull(log_mean)
  } else if (priors$mu_0_mean_param_1 == "mean" || priors$mu_0_mean_param_1 == "auto"){
    log_mean <- .disease_data |>
      dplyr::summarise(log_mean = mean(!!as.symbol("n"), na.rm = T)) |>
      dplyr::pull(log_mean)
  }

  if (priors$mu_0_sd_param_1 == "logsd"){
    log_sd <- .disease_data |>
      dplyr::summarise(log_sd = sd(log1p(!!as.symbol("n")), na.rm = T)) |>
      dplyr::pull(log_sd)
  } else if (priors$mu_0_sd_param_1 == "sd" | priors$mu_0_sd_param_1 == "auto"){
    log_sd <- .disease_data |>
      dplyr::summarise(log_sd = sd(!!as.symbol("n"), na.rm = T)) |>
      dplyr::pull(log_sd)
  }

  #Get the log mean of disease data and the sd
  priors$mu_0_mean_param_1 <- ifelse(priors$mu_0_mean_param_1 %in% c("mean","logmean","auto"), log_mean, priors$mu_0_mean_param_1)
  priors$mu_0_sd_param_1   <- ifelse(priors$mu_0_sd_param_1 %in% c("sd","logsd","auto"), log_sd, priors$mu_0_sd_param_1)

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
