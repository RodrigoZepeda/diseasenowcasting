#' Function for mapping the priors to numbers
#'
#' Takes a prior from one of the listed in `priors.stan` and
#' returns the number that codes it
#'
#' @param prior_name (character) The name of the prior distribution. Can be one of
#' the following: `jeffreys` (non-informative), `standard_normal`, `normal`, `student_t`,
#' `cauchy`, `exponential`, `gamma`, `inverse_gamma`, `lognormal`, `weibull`, `frechet`,
#' `double_exponential`, `rayleigh`, `loglogistic`, `gumbel`.
#'
#' @keywords internal
get_prior_code_stan <- function(prior_name){

  dist_list <- c("jeffreys", "standard_normal", "normal", "student_t", "cauchy", "exponential",
                 "gamma", "inverse_gamma", "lognormal", "weibull", "frechet",
                 "double_exponential","logistic","rayleigh", "loglogistic", "gumbel","uniform")

  if (!is.numeric(prior_name)){
    return(
      switch(
        tolower(prior_name),
           "jeffreys"           = 0,
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
            "logistic"          = 12,
           "rayleigh"           = 13,
           "loglogistic"        = 14,
           "gumbel"             = 15,
           "uniform"            = 16,
           cli::cli_abort("Distribution {.val {character}} not found. Choose one of the following: {.val {dist_list}}"))
    )
  } else {
    return(prior_name)
  }
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

#' Return the default links for the model
#'
#' The functions return the corresponding default links for the data model
#' @inheritParams nowcast
#' @rdname default_link
#' @return The name of the default link for a distribution
#' @keywords internal

#' @title Default y link
#' @keywords internal
default_y_link <- function(dist){
  if (dist[1] %in% c("Normal","Student")){
    return ("identity")
  } else {
    return ("identity")
  }
}

#' @title Default x link
#' @keywords internal
default_x_link <- function(dist){
  if (dist[1] %in% c("Normal","Student")){
    return ("identity")
  } else {
    return ("dhyperbolic")
  }
}

