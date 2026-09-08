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
    revision       = revision_process_class,    # revision layer (inert by default)
    covariate_prior = prior_class,
    strata_pooling  = S7::class_character,  # "independent" | "hierarchical"
    cumulative = cumulative_process_class
  ),
  constructor = function(likelihood      = nb_likelihood(),
                         epidemic        = hsgp_epidemic(),
                         delay           = dirichlet_delay(),
                         revision       = no_revision(),
                         covariate_prior = std_normal_prior(),
                         strata_pooling  = "independent",
                         cumulative = no_cumulative()) {
    S7::new_object(S7::S7_object(),
                   likelihood = likelihood, epidemic = epidemic,
                   delay = delay, revision = revision,
                   covariate_prior = covariate_prior,
                   strata_pooling = strata_pooling,
                   cumulative = cumulative)
  },
  validator = function(self) {
    if (!self@strata_pooling %in% c("independent", "hierarchical"))
      cli::cli_abort("`strata_pooling` must be \"independent\" or \"hierarchical\".")
  }
)

#' Bayesian Nowcast Model
#'
#' Combines a likelihood, an epidemic process, a reporting-delay distribution,
#' and an optional revision process into a model object. Arguments are
#' positional: likelihood, epidemic process, reporting delay, then revision
#' process. Any argument can be omitted by naming the later components.
#' @param likelihood      A `likelihood_class` ([poisson_likelihood()] /
#'   [nb_likelihood()]).  Default: [nb_likelihood()].
#' @param epidemic        An `epidemic_process_class`.  Default: [hsgp_epidemic()].
#' @param delay           A `delay_process_class`.  Default: [lognormal_delay()].
#' @param revision      A `revision_process_class` ([revision_process()])
#'   for report-level confirmation/retraction outcomes in linelist or
#'   count-incidence data. Default: inert. Count-cumulative revisions use the
#'   separate `cumulative` component.
#' @param covariate_prior A `prior_class` applied to all covariate coefficients.
#'   Default: [std_normal_prior()].
#' @param strata_pooling  `"independent"` (default) fits fully separate intercepts
#'   per stratum.  `"hierarchical"` pools intercepts via a shared mean and a
#'   half-normal prior on the between-stratum SD:
#'   \eqn{\mu_0^{(s)} = \mu_{\text{global}} + \tau \cdot \delta^{(s)}},
#'   \eqn{\delta^{(s)} \sim \mathcal{N}(0,1)},
#'   \eqn{\tau \sim \text{HalfNormal}(0,1)}.
#'   Only relevant when `num_strata > 1`.
#' @param cumulative Dedicated count-cumulative observation configuration
#'   from [cumulative_process()]. It is inert by default for linelist and
#'   count-incidence data; count-cumulative data use the hurdle--ZTNB default
#'   unless configured explicitly.
#'
#' @returns A `model_class` object.
#'
#' @examples
#' model()
#' model(poisson_likelihood(), hsgp_epidemic(gp_kernel = "matern52"))
#' model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
#' model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
#'       revision_process())
#' model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
#'       strata_pooling = "hierarchical")
#'
#' @export
model <- function(likelihood      = nb_likelihood(),
                  epidemic        = hsgp_epidemic(),
                  delay           = lognormal_delay(),
                  revision       = no_revision(),
                  covariate_prior = std_normal_prior(),
                  strata_pooling  = "independent",
                  cumulative = no_cumulative()) {
  model_class(likelihood = likelihood, epidemic = epidemic,
              delay = delay, revision = revision,
              covariate_prior = covariate_prior,
              strata_pooling = strata_pooling,
              cumulative = cumulative)
}
