# =============================================================================
# Dedicated count-cumulative process configuration
# =============================================================================

#' Count-cumulative observation process
#'
#' Configures models for revision streams that publish cumulative levels. The
#' target is finite-horizon database retention `C_t(H)`, not biological truth.
#' The retraction mechanism is the collapsed kernel
#' `h_R(l) = retraction_mass * g_R(l)`; it does not separately identify a truth
#' probability and a conditional validation-delay law.
#'
#' @param observation Observation composite likelihood. `"cumulative"` uses
#'   cumulative Poisson or negative-binomial marginals according to the model's
#'   [likelihood]. `"hurdle_ztnb"` uses signed hurdle updates with a
#'   zero-truncated-negative-binomial magnitude. `"hurdle_ztpoisson"` uses the
#'   corresponding zero-truncated-Poisson magnitude.
#' @param retraction_delay Parametric delay family for retraction ages `1:H`.
#'   Lognormal, gamma, and generalized gamma are supported.
#' @param settlement Positive integer settlement horizon `H`, in model steps.
#' @param retraction_mass Prior or fixed value in `[0, 1]` for the finite-horizon
#'   mass of `h_R`. A Beta prior is used by default.
#' @param movement_intercept,movement_age,movement_previous Priors or fixed
#'   values for the bounded movement-probability regression. Set
#'   `movement_previous = 0` to disable previous-movement dependence.
#' @param magnitude_size Positive prior or fixed value for the ZTNB magnitude
#'   size. It is used only by `"hurdle_ztnb"`.
#'
#' @returns A `count_cumulative_process_class` object for
#'   `model(count_cumulative = )`.
#'
#' @examples
#' count_cumulative_process()
#' count_cumulative_process(observation = "cumulative", settlement = 52L)
#' count_cumulative_process(observation = "hurdle_ztpoisson", settlement = 6L)
#'
#' @export
count_cumulative_process <- function(
    observation = c("hurdle_ztnb", "hurdle_ztpoisson", "cumulative"),
    retraction_delay = lognormal_delay(),
    settlement = 26L,
    retraction_mass = beta_prior(1.5, 20),
    movement_intercept = normal_prior(-1, 2),
    movement_age = normal_prior(0, 1),
    movement_previous = normal_prior(0, 1),
    magnitude_size = NULL) {
  observation <- match.arg(observation)
  if (identical(observation, "hurdle_ztnb") && is.null(magnitude_size))
    magnitude_size <- lognormal_prior(0, 1.5)
  if (identical(observation, "hurdle_ztpoisson") &&
      !is.null(magnitude_size)) {
    cli::cli_abort("`hurdle_ztpoisson` has no magnitude-dispersion parameter; omit `magnitude_size`.")
  }
  count_cumulative_process_class(
    observation = observation,
    retraction_delay = retraction_delay,
    settlement = as.numeric(settlement),
    retraction_mass = retraction_mass,
    movement_intercept = movement_intercept,
    movement_age = movement_age,
    movement_previous = movement_previous,
    magnitude_size = magnitude_size,
    active = TRUE
  )
}

#' Count-cumulative process S7 class
#' @keywords internal
#' @noRd
count_cumulative_process_class <- S7::new_class(
  "count_cumulative_process_class",
  properties = list(
    observation = S7::class_character,
    retraction_delay = delay_process_class,
    settlement = S7::class_numeric,
    retraction_mass = .valid_param_slot,
    movement_intercept = .valid_param_slot,
    movement_age = .valid_param_slot,
    movement_previous = .valid_param_slot,
    magnitude_size = S7::class_any,
    active = S7::class_logical
  ),
  constructor = function(
      observation = "hurdle_ztnb",
      retraction_delay = lognormal_delay(),
      settlement = 26,
      retraction_mass = beta_prior(1.5, 20),
      movement_intercept = normal_prior(-1, 2),
      movement_age = normal_prior(0, 1),
      movement_previous = normal_prior(0, 1),
      magnitude_size = NULL,
      active = TRUE) {
    S7::new_object(
      S7::S7_object(),
      observation = observation,
      retraction_delay = retraction_delay,
      settlement = settlement,
      retraction_mass = retraction_mass,
      movement_intercept = movement_intercept,
      movement_age = movement_age,
      movement_previous = movement_previous,
      magnitude_size = magnitude_size,
      active = active
    )
  },
  validator = function(self) {
    if (!isTRUE(self@active)) return(NULL)
    if (!self@observation %in%
        c("cumulative", "hurdle_ztnb", "hurdle_ztpoisson")) {
      cli::cli_abort("Unsupported count-cumulative observation model {.val {self@observation}}.")
    }
    .validate_settlement_horizon(self@settlement)
    if (!self@retraction_delay@num_id %in% 1:3) {
      cli::cli_abort(c(
        "Unsupported count-cumulative retraction-delay family {.val {self@retraction_delay@name}}.",
        "i" = "Use a lognormal, gamma, or generalized-gamma delay."
      ))
    }
    if (is.numeric(self@retraction_mass)) {
      if (length(self@retraction_mass) != 1L ||
          !is.finite(self@retraction_mass) ||
          self@retraction_mass < 0 || self@retraction_mass > 1) {
        cli::cli_abort("Fixed `retraction_mass` must be one finite value in [0, 1].")
      }
    } else if (!S7::S7_inherits(self@retraction_mass, prior_class) ||
               !identical(self@retraction_mass@name, "Beta")) {
      cli::cli_abort("`retraction_mass` must be a Beta prior or a fixed value in [0, 1].")
    }
    movement_values <- list(
      movement_intercept = self@movement_intercept,
      movement_age = self@movement_age,
      movement_previous = self@movement_previous
    )
    for (slot_name in names(movement_values)) {
      value <- movement_values[[slot_name]]
      if (is.numeric(value) &&
          (length(value) != 1L || !is.finite(value))) {
        cli::cli_abort("Fixed `{slot_name}` must be one finite numeric value.")
      }
    }
    if (identical(self@observation, "hurdle_ztnb") &&
        !isTRUE(valid_positive_prior(self@magnitude_size))) {
      cli::cli_abort("`magnitude_size` must be a positive prior or fixed positive value.")
    }
    if (identical(self@observation, "hurdle_ztpoisson") &&
        !is.null(self@magnitude_size)) {
      cli::cli_abort("`hurdle_ztpoisson` has no magnitude-dispersion parameter.")
    }
  }
)

#' Inert count-cumulative configuration for non-cumulative data
#' @keywords internal
#' @noRd
no_count_cumulative <- function() {
  count_cumulative_process_class(active = FALSE)
}
