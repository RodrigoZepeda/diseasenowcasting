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
    S7::new_object(likelihood_class(name = "poisson", num_id = 0L), mu = mu)
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
    phi = .valid_param_slot   # NB dispersion 1/size (> 0)
  ),
  constructor = function(mu = numeric(0), phi = lognormal_prior(log(0.1), 1.5)) {
    S7::new_object(likelihood_class(name = "nb", num_id = 1L), mu = mu, phi = phi)
  },
  validator = function(self) {
    .check_fixed_domain(self@phi, "phi", "nb_likelihood", "the positive line",
                        function(v) v > 0)
  }
)

#' Likelihood for the Bayesian Nowcast
#'
#' Count observation model for the (truncation-corrected) case counts.
#'
#' @param mu  Log-scale mean intercept prior, or a number to hold it there.
#' @param phi Negative-binomial overdispersion prior, or a number to hold it
#'   there (> 0); NB only.  `phi` is the dispersion, the reciprocal of the NB
#'   size: counts with mean \eqn{\mu} have variance \eqn{\mu + \phi \mu^2},
#'   i.e. `rnbinom(size = 1 / phi, mu = mu)`.  Larger `phi` means more
#'   overdispersion and wider intervals; `phi -> 0` is the Poisson.  This is the
#'   *only* place to set the overdispersion prior — [nowcast()] reads it from
#'   the model and does not accept its own `phi` argument.
#'
#' @section Default priors:
#' `phi ~ lognormal_prior(log(0.1), 1.5)`: a median size of 10 with a 95%
#' interval for the size of roughly 0.5 to 190.  On long series the data
#' dominate this prior; on short series (a few weeks of data) it sets the
#' interval width.  The default up to version 2.5.0 was
#' `lognormal_prior(log(20), 0.5)`, which centred the size on 0.05 (see
#' `NEWS.md`).
#'
#' @returns A `likelihood_class` object.
#'
#' @examples
#' poisson_likelihood()
#' nb_likelihood()
#' # More overdispersion (heavier-tailed counts, size around 2):
#' nb_likelihood(phi = lognormal_prior(log(0.5), 0.5))
#' # Close to Poisson (size around 100):
#' nb_likelihood(phi = lognormal_prior(log(0.01), 0.5))
#' # A number holds the parameter at that value instead of estimating it
#' # (phi = 0.05 is size 20):
#' nb_likelihood(phi = 0.05)
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
nb_likelihood <- function(mu = numeric(0), phi = lognormal_prior(log(0.1), 1.5)) {
  nb_likelihood_class(mu = mu, phi = phi)
}
