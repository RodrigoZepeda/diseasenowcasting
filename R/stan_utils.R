#' Function for mapping the priors to numbers
#'
#' Takes a prior from one of the listed in `priors.stan` and
#' returns the number that codes it
#'
#' @param prior_name (character) The name of the prior distribution. Can be one of
#' the following: `standard_normal`, `normal`, `student_t`, `cauchy`, `exponential`,
#' `gamma`, `inverse_gamma`, `lognormal`, `weibull`, `frechet`,
#' `double_exponential`, `rayleigh`, `loglogistic`, `gumbel`.
#'
#' @keywords internal
get_prior_code_stan <- function(prior_name){
  dist_list <- c("standard_normal", "normal", "student_t", "cauchy", "exponential",
                 "gamma", "inverse_gamma", "lognormal", "weibull", "frechet",
                 "double_exponential", "rayleigh", "loglogistic", "gumbel")
  switch(tolower(prior_name),
         "standard_normal"    = 1,
         "normal"             = 2,
         "student_t"          = 3,
         "cauchy"             = 4,
         "exponential"        = 5,
         "gamma"              = 6,
         "inverse_gamma"      = 7,
         "lognormal"          = 8,
         "weibull"            = 9,
         "frechet"            = 10,
         "double_exponential" = 11,
         "rayleigh"           = 12,
         "loglogistic"        = 13,
         "gumbel"             = 14,
         cli::cli_abort("Distribution {.val {character}} not found. Choose one of the following: {.val {dist_list}}"))
}

#' Function for mapping the priors to numbers
#'
#' Takes a prior from one of the listed in `priors.stan` and
#' returns a function for generating random numbers from it
#'
#' @inheritParams get_prior_code_stan
#'
#' @keywords internal
get_prior_code_sim_R <- function(prior_name){
  dist_list <- c("standard_normal", "normal", "student_t", "cauchy", "exponential",
                 "gamma", "inverse_gamma", "lognormal", "weibull", "frechet",
                 "double_exponential", "rayleigh", "loglogistic", "gumbel")
  switch(tolower(prior_name),
         "standard_normal"    = function(x, param1, param2) rnorm(x, 0, 1),
         "normal"             = rnorm,
         "student_t"          = rt,
         "cauchy"             = rcauchy,
         "exponential"        = function(x, param1, param2) rexp(x, param1),
         "gamma"              = rgamma,
         "inverse_gamma"      = function(x, param1, param2) 1/rgamma(x, param1, param2),
         "lognormal"          = rlnorm,
         "weibull"            = rweibull,
         #"frechet"            = 10,
         #"double_exponential" = 11,
         #"rayleigh"           = 12,
         #"loglogistic"        = 13,
         #"gumbel"             = 14,
         cli::cli_abort("Distribution {.val {character}} not found. Choose one of the following: {.val {dist_list}}"))
}

#' Return the default `STAN` control parameters for nowcasting
#'
#' The function estimates the default control parameters for running [rstan::sampling()].
#'
#' @return A list with the default control parameters for running the sampling algorithm.
#'
#' @keywords internal
control_default <- function(){
  list(adapt_delta = 0.95, max_treedepth = 12)
}
