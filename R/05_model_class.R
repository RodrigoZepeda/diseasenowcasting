# =============================================================================
# model_class — top-level model specification
# =============================================================================

#' @keywords internal
#' @noRd
model_class <- S7::new_class(
  "model_class",
  properties = list(
    likelihood      = likelihood_class,
    epidemic        = epidemic_process_class,
    delay           = delay_process_class,
    covariate_prior = prior_class
  ),
  constructor = function(likelihood      = nb_likelihood(),
                         epidemic        = hsgp_epidemic(),
                         delay           = dirichlet_delay(),
                         covariate_prior = std_normal_prior()) {
    S7::new_object(S7::S7_object(),
                   likelihood = likelihood, epidemic = epidemic,
                   delay = delay, covariate_prior = covariate_prior)
  }
)

#' Bayesian Nowcast Model
#'
#' Combines a likelihood, an epidemic process, and a delay distribution into a
#' model object.  Arguments are positional: the first is the likelihood, the
#' second the epidemic process, the third the delay.  Any argument can be
#' omitted to use its default.
#'
#' @param likelihood      A `likelihood_class` ([poisson_likelihood()] /
#'   [nb_likelihood()]).  Default: [nb_likelihood()].
#' @param epidemic        An `epidemic_process_class`.  Default: [hsgp_epidemic()].
#' @param delay           A `delay_process_class`.  Default: [dirichlet_delay()].
#' @param covariate_prior A `prior_class` applied to all covariate coefficients.
#'   Default: [std_normal_prior()].
#'
#' @returns A `model_class` object.
#'
#' @examples
#' model()
#' model(poisson_likelihood(), hsgp_epidemic(gp_kernel = "matern52"))
#' model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
#'
#' @export
model <- function(likelihood      = nb_likelihood(),
                  epidemic        = hsgp_epidemic(),
                  delay           = dirichlet_delay(),
                  covariate_prior = std_normal_prior()) {
  model_class(likelihood = likelihood, epidemic = epidemic,
              delay = delay, covariate_prior = covariate_prior)
}
