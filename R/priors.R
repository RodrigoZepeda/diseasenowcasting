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
set_priors <- function(mu_degree      = 1,
                       nu_degree      = 1,
                       p              = 0,
                       mu_is_constant = FALSE,
                       nu_is_constant = TRUE,
                       mu_sd_prior    = "standard_normal",
                       nu_sd_prior    = "standard_normal",
                       mu_sd_param_1  = 0.0,
                       mu_sd_param_2  = 0.1,
                       nu_sd_param_1  = 0.0,
                       nu_sd_param_2  = 0.1,
                       phi_AR_param_1 = 0.0,
                       phi_AR_param_2 = 1.0,
                       phi_AR_prior   = "cauchy",
                       mu_0_mean_param_1 = "auto",
                       mu_0_mean_param_2 = 0.01,
                       mu_0_sd_param_1 = "auto",
                       mu_0_sd_param_2 = 0.01,
                       nu_0_mean_param_1 = 0.0,
                       nu_0_mean_param_2 = 0.01,
                       nu_0_sd_param_1 = 0.00,
                       nu_0_sd_param_2 = 0.01,
                       mu_0_mean_hyperprior = "standard_normal",
                       nu_0_mean_hyperprior = "standard_normal",
                       mu_0_sd_hyperprior = "standard_normal",
                       nu_0_sd_hyperprior = "standard_normal",
                       r_prior = "standard_normal",
                       r_param_1 = 0.0,
                       r_param_2 = 1.0) {
  list(
    mu_degree            = mu_degree,
    nu_degree            = nu_degree,
    p                    = p,
    mu_is_constant       = mu_is_constant,
    nu_is_constant       = nu_is_constant,
    mu_sd_prior          = mu_sd_prior,
    nu_sd_prior          = nu_sd_prior,
    mu_sd_param_1        = mu_sd_param_1,
    mu_sd_param_2        = mu_sd_param_2,
    nu_sd_param_1        = nu_sd_param_1,
    nu_sd_param_2        = nu_sd_param_2,
    phi_AR_param_1       = phi_AR_param_1,
    phi_AR_param_2       = phi_AR_param_2,
    phi_AR_prior         = phi_AR_prior,
    mu_0_mean_param_1    = mu_0_mean_param_1,
    mu_0_mean_param_2    = mu_0_mean_param_2,
    mu_0_sd_param_1      = mu_0_sd_param_1,
    mu_0_sd_param_2      = mu_0_sd_param_2,
    nu_0_mean_param_1    = nu_0_mean_param_1,
    nu_0_mean_param_2    = nu_0_mean_param_2,
    nu_0_sd_param_1      = nu_0_sd_param_1,
    nu_0_sd_param_2      = nu_0_sd_param_2,
    mu_0_mean_hyperprior = mu_0_mean_hyperprior,
    nu_0_mean_hyperprior = nu_0_mean_hyperprior,
    mu_0_sd_hyperprior   = mu_0_sd_hyperprior,
    nu_0_sd_hyperprior   = nu_0_sd_hyperprior,
    r_prior              = r_prior,
    r_param_1            = r_param_1,
    r_param_2            = r_param_2
  )
}
