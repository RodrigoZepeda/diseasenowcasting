# =============================================================================
# model_class -- top-level model specification
# =============================================================================

#' @keywords internal
#' @noRd
model_class <- S7::new_class(
  "model_class",
  properties = list(
    likelihood      = likelihood_class,
    epidemic        = epidemic_process_class,
    delay           = delay_process_class,
    covariate_prior = prior_class,
    strata_pooling  = S7::class_character   # "independent" | "hierarchical"
  ),
  constructor = function(likelihood      = nb_likelihood(),
                         epidemic        = hsgp_epidemic(),
                         delay           = dirichlet_delay(),
                         covariate_prior = std_normal_prior(),
                         strata_pooling  = "independent") {
    S7::new_object(S7::S7_object(),
                   likelihood = likelihood, epidemic = epidemic,
                   delay = delay, covariate_prior = covariate_prior,
                   strata_pooling = strata_pooling)
  },
  validator = function(self) {
    if (!self@strata_pooling %in% c("independent", "hierarchical"))
      cli::cli_abort("`strata_pooling` must be \"independent\" or \"hierarchical\".")
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
#' @param delay           A `delay_process_class`.  Default: [lognormal_delay()].
#' @param covariate_prior A `prior_class` applied to all covariate coefficients.
#'   Default: [std_normal_prior()].
#' @param strata_pooling  `"independent"` (default) fits fully separate intercepts
#'   per stratum.  `"hierarchical"` pools intercepts via a shared mean and a
#'   half-normal prior on the between-stratum SD:
#'   \eqn{\mu_0^{(s)} = \mu_{\text{global}} + \tau \cdot \delta^{(s)}},
#'   \eqn{\delta^{(s)} \sim \mathcal{N}(0,1)},
#'   \eqn{\tau \sim \text{HalfNormal}(0,1)}.
#'   Only relevant when `num_strata > 1`.
#'
#' @returns A `model_class` object.
#'
#' @examples
#' model()
#' model(poisson_likelihood(), hsgp_epidemic(gp_kernel = "matern52"))
#' model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
#' model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
#'       strata_pooling = "hierarchical")
#'
#' @export
model <- function(likelihood      = nb_likelihood(),
                  epidemic        = hsgp_epidemic(),
                  delay           = lognormal_delay(),
                  covariate_prior = std_normal_prior(),
                  strata_pooling  = "independent") {
  model_class(likelihood = likelihood, epidemic = epidemic,
              delay = delay, covariate_prior = covariate_prior,
              strata_pooling = strata_pooling)
}
