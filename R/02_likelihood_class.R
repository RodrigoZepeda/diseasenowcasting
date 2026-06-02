# =============================================================================
# Likelihood classes
# =============================================================================

#' Likelihood related classes
#' @name likelihood_classes
#' @keywords internal
#' @noRd
NULL

#' @rdname likelihood_classes
#' @keywords internal
#' @noRd
likelihood_class <- S7::new_class(
  "likelihood_class",
  properties = list(
    name   = S7::class_character,
    num_id = S7::class_numeric
  )
)

#' @rdname likelihood_classes
#' @keywords internal
#' @noRd
poisson_likelihood_class <- S7::new_class(
  name   = "poisson_likelihood_class",
  parent = likelihood_class,
  properties = list(
    mu = .valid_param_slot   # log-scale mean intercept
  ),
  constructor = function(mu = numeric(0)) {
    S7::new_object(S7::S7_object(), name = "poisson", num_id = 0L, mu = mu)
  }
)

#' @rdname likelihood_classes
#' @keywords internal
#' @noRd
nb_likelihood_class <- S7::new_class(
  name   = "nb_likelihood_class",
  parent = likelihood_class,
  properties = list(
    mu  = .valid_param_slot,  # log-scale mean intercept
    phi = .valid_param_slot   # NB overdispersion (> 0)
  ),
  constructor = function(mu = numeric(0), phi = lognormal_prior(log(20), 0.5)) {
    S7::new_object(S7::S7_object(), name = "nb", num_id = 1L, mu = mu, phi = phi)
  }
)

#' Likelihood for the Bayesian Nowcast
#'
#' Count observation model for the (truncation-corrected) case counts.
#'
#' @param mu  Log-scale mean intercept prior (or fixed numeric).
#' @param phi Negative-binomial overdispersion prior (or fixed numeric);
#'   NB only.  Defaults to `lognormal_prior(log(20), 0.5)`.  This is the *only*
#'   place to set the overdispersion prior — [nowcast()] reads it from the model
#'   and does not accept its own `phi` argument.
#'
#' @returns A `likelihood_class` object.
#'
#' @examples
#' poisson_likelihood()
#' nb_likelihood()
#' # Wider overdispersion (heavier-tailed counts) -- set via the likelihood:
#' nb_likelihood(phi = lognormal_prior(log(5), 0.5))
#'
#' @name likelihood
NULL

#' @rdname likelihood
#' @export
poisson_likelihood <- function(mu = numeric(0)) {
  poisson_likelihood_class(mu = mu)
}

#' @rdname likelihood
#' @export
nb_likelihood <- function(mu = numeric(0), phi = lognormal_prior(log(20), 0.5)) {
  nb_likelihood_class(mu = mu, phi = phi)
}
