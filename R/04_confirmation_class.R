# =============================================================================
# Confirmation / retraction process class
# =============================================================================
# A confirmation process describes the *down-revision* side of a count-cumulative
# surveillance stream: a fraction `(1 - p)` of reports are erroneous and are later
# retracted, after a retraction delay `g_C`.  It is an OPTIONAL model component,
# attached to a model() alongside the epidemic and (appearance-)delay processes.
#
# When the data handed to nowcast() is count-cumulative, the observation model
# switches automatically to the signed-increment Skellam / SkNB likelihood
# (28_confirmation_likelihood.R); this component supplies the retraction structure
# that likelihood needs.  With no confirmation process (`p = 1`) the model reduces
# exactly to the standard right-censored count model.
# =============================================================================

#' Confirmation / retraction process
#'
#' Describes reports that are later retracted (count-cumulative streams that revise
#' downward as well as upward).  Attach it to a [model()] via the `confirmation`
#' argument; [nowcast()] then uses the signed-increment Skellam / SkNB likelihood
#' when the data are count-cumulative.
#'
#' @param retract_delay A `delay_process_class` (e.g. [lognormal_delay()],
#'   [gamma_delay()]) describing the retraction delay `g_C` (onset of a report to
#'   its retraction).  Default a geometric-like short lognormal.
#' @param p The confirmation probability (probability that a report is genuine and
#'   never retracted), specified the same way as a delay parameter: either a
#'   `prior_class` (estimated with that prior) or a single numeric in `(0, 1]`
#'   (held fixed).  **Left unset (the default)**, [default_priors()] builds a
#'   *data-informed, strongly-concentrated* Beta prior centred at the empirical
#'   retraction rate -- exactly as the lognormal delay's `mu` default is
#'   data-informed.  The strong prior is deliberate: retractions are empirically
#'   rare (~1-2% of reports), and with a weak prior the Skellam variance abuses the
#'   retraction stream as an overdispersion knob and `p` collapses.  Pass a fixed
#'   value (e.g. `p = 0.98`) to hold it constant, or your own `beta_prior()` to
#'   estimate it under a prior of your choosing.
#'
#' @details
#' **Default priors.** `retract_delay` inherits the default priors of its delay
#' family (see [delay_process]); `p` gets a strongly-concentrated Beta centred at
#' the empirical retraction rate (see [default_priors()]).  At `p = 1` the
#' confirmation layer is inert and the model is the ordinary count model.
#'
#' @returns A `confirmation_process_class` object.
#'
#' @examples
#' # A confirmation process with a lognormal retraction delay
#' confirmation_process(retract_delay = lognormal_delay())
#'
#' # Attach to a model for a count-cumulative stream
#' model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
#'       confirmation = confirmation_process())
#'
#' @seealso [model()], [delay_process], [nowcast()]
#' @export
confirmation_process <- function(retract_delay = lognormal_delay(),
                                 p = numeric(0)) {
  confirmation_process_class(retract_delay = retract_delay, p = p)
}

#' Confirmation process S7 class
#' @keywords internal
#' @noRd
confirmation_process_class <- S7::new_class(
  "confirmation_process_class",
  properties = list(
    retract_delay = delay_process_class,
    p             = .valid_param_slot,   # prior_class or fixed numeric in (0, 1]
    active        = S7::class_logical    # FALSE for the inert (p = 1) default
  ),
  constructor = function(retract_delay = lognormal_delay(),
                         p             = numeric(0),
                         active        = TRUE) {
    S7::new_object(S7::S7_object(),
                   retract_delay = retract_delay, p = p, active = active)
  },
  validator = function(self) {
    # `p` is unset (length-0 numeric -> data-informed default), a fixed number in
    # (0, 1], or a prior on (0, 1) -- mirroring the delay parameter slots.
    if (is.numeric(self@p) && length(self@p) == 1L) {
      if (self@p <= 0 || self@p > 1)
        cli::cli_abort("Fixed confirmation probability `p` must be in (0, 1]. Got {self@p}.")
    } else if (is.numeric(self@p) && length(self@p) == 0L) {
      invisible(NULL)                       # unset -> resolved in default_priors()
    } else if (!S7::S7_inherits(self@p, prior_class)) {
      cli::cli_abort("`p` must be a prior_class object, a single numeric in (0, 1], or unset.")
    }
  }
)

#' The inert confirmation process (`p = 1`, no retractions).
#'
#' Used as the model() default so the confirmation layer is off unless the user
#' opts in.  Detecting count-cumulative data in nowcast() promotes this to an
#' active default when the user has not supplied one.
#' @keywords internal
#' @noRd
no_confirmation <- function() {
  confirmation_process_class(retract_delay = lognormal_delay(), p = 1, active = FALSE)
}
