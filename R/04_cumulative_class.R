# =============================================================================
# Dedicated count-cumulative process configuration
# =============================================================================

#' Count-cumulative observation process
#'
#' Configures models for revision streams that publish cumulative levels. The
#' target is finite-horizon database retention `C_t(H)`, not biological truth.
#' The retraction mechanism is the collapsed kernel
#' `h_R(l) = retraction_mass * g_R(l)`; it does not separately identify a truth
#' probability and a conditional revision-delay law.
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
#'   size of the first update of each event (delay 0). It is used only by
#'   `"hurdle_ztnb"`.
#' @param revision_magnitude_size Positive prior or fixed value for the ZTNB
#'   magnitude size of later updates (delays `1:H`). Initial reports and later
#'   revisions have very different dispersion relative to their mean: a single
#'   shared size is pulled to the heavy-tailed revisions and then leaves the
#'   initial reports too little weight to identify the epidemic trajectory. It
#'   is used only by `"hurdle_ztnb"`.
#' @param initial_report How the hurdle models treat the first published level
#'   `C_t(0)`. `"hurdle"` treats it as one more signed update. `"offset"` gives
#'   each event a Gamma(`initial_size`, `initial_size`) effect `Xi_t` shared by
#'   all its updates: `C_t(0)` is Poisson with mean `Xi_t mu_t q_C(0)`, hence
#'   negative binomial, and the later hurdle updates use
#'   `mu_t E[Xi_t | C_t(0)] = mu_t (initial_size + C_t(0)) /
#'   (initial_size + mu_t q_C(0))`. As `initial_size` goes to zero the published
#'   `C_t(0) / q_C(0)` becomes an offset for the later updates; as it grows the
#'   model returns to `mu_t`. `E[C_t(H)] = mu_t q_C(H)` holds exactly. Ignored by
#'   `"cumulative"`.
#' @param initial_size Positive prior or fixed value for the size `kappa` of the
#'   `"offset"` effect. It replaces `magnitude_size`, which the offset does not
#'   use. The default fixes it at 100. The weekly effect and the epidemic trend
#'   both explain week-to-week variation in `C_t(0)`, and only the trend's
#'   autocorrelation separates them; estimated `kappa` can fall to about one,
#'   leaving the trend flat while the effect absorbs the epidemic.
#'
#' @returns A `cumulative_process_class` object for
#'   `model(cumulative = )`.
#'
#' @examples
#' cumulative_process()
#' cumulative_process(observation = "cumulative", settlement = 52L)
#' cumulative_process(observation = "hurdle_ztpoisson", settlement = 6L)
#' cumulative_process(initial_report = "offset")
#'
#' @export
cumulative_process <- function(
    observation = c("hurdle_ztnb", "hurdle_ztpoisson", "cumulative"),
    retraction_delay = lognormal_delay(),
    settlement = 26L,
    retraction_mass = beta_prior(1.5, 20),
    movement_intercept = normal_prior(-1, 2),
    movement_age = normal_prior(0, 1),
    movement_previous = normal_prior(0, 1),
    magnitude_size = NULL,
    revision_magnitude_size = NULL,
    initial_report = c("hurdle", "offset"),
    initial_size = NULL) {
  observation <- match.arg(observation)
  initial_report <- match.arg(initial_report)
  is_offset <- identical(initial_report, "offset") &&
    observation %in% c("hurdle_ztnb", "hurdle_ztpoisson")
  if (is_offset && !is.null(magnitude_size)) {
    cli::cli_abort(c(
      "`initial_report = \"offset\"` models `C_t(0)` with `initial_size`, not `magnitude_size`.",
      "i" = "Omit `magnitude_size`."
    ))
  }
  if (!is_offset && !is.null(initial_size)) {
    cli::cli_abort("`initial_size` is used only with a hurdle observation and `initial_report = \"offset\"`.")
  }
  if (is_offset && is.null(initial_size))
    initial_size <- 100
  if (identical(observation, "hurdle_ztnb") && !is_offset &&
      is.null(magnitude_size))
    magnitude_size <- lognormal_prior(0, 1.5)
  if (identical(observation, "hurdle_ztnb") &&
      is.null(revision_magnitude_size))
    revision_magnitude_size <- lognormal_prior(0, 1.5)
  if (identical(observation, "hurdle_ztpoisson") &&
      (!is.null(magnitude_size) || !is.null(revision_magnitude_size))) {
    cli::cli_abort("`hurdle_ztpoisson` has no magnitude-dispersion parameter; omit `magnitude_size` and `revision_magnitude_size`.")
  }
  cumulative_process_class(
    observation = observation,
    retraction_delay = retraction_delay,
    settlement = as.numeric(settlement),
    retraction_mass = retraction_mass,
    movement_intercept = movement_intercept,
    movement_age = movement_age,
    movement_previous = movement_previous,
    magnitude_size = magnitude_size,
    revision_magnitude_size = revision_magnitude_size,
    initial_report = initial_report,
    initial_size = initial_size,
    active = TRUE
  )
}

#' Count-cumulative process S7 class
#' @keywords internal
#' @noRd
cumulative_process_class <- S7::new_class(
  "cumulative_process_class",
  properties = list(
    observation = S7::class_character,
    retraction_delay = delay_process_class,
    settlement = S7::class_numeric,
    retraction_mass = .valid_param_slot,
    movement_intercept = .valid_param_slot,
    movement_age = .valid_param_slot,
    movement_previous = .valid_param_slot,
    magnitude_size = S7::class_any,
    revision_magnitude_size = S7::class_any,
    initial_report = S7::class_character,
    initial_size = S7::class_any,
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
      revision_magnitude_size = NULL,
      initial_report = "hurdle",
      initial_size = NULL,
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
      revision_magnitude_size = revision_magnitude_size,
      initial_report = initial_report,
      initial_size = initial_size,
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
    if (!self@initial_report %in% c("hurdle", "offset")) {
      cli::cli_abort("`initial_report` must be \"hurdle\" or \"offset\".")
    }
    is_offset <- identical(self@initial_report, "offset") &&
      self@observation %in% c("hurdle_ztnb", "hurdle_ztpoisson")
    if (is_offset && !isTRUE(valid_positive_prior(self@initial_size))) {
      cli::cli_abort("`initial_size` must be a positive prior or fixed positive value.")
    }
    if (identical(self@observation, "hurdle_ztnb") && !is_offset &&
        !isTRUE(valid_positive_prior(self@magnitude_size))) {
      cli::cli_abort("`magnitude_size` must be a positive prior or fixed positive value.")
    }
    if (identical(self@observation, "hurdle_ztnb") &&
        !isTRUE(valid_positive_prior(self@revision_magnitude_size))) {
      cli::cli_abort("`revision_magnitude_size` must be a positive prior or fixed positive value.")
    }
    if (identical(self@observation, "hurdle_ztpoisson") &&
        (!is.null(self@magnitude_size) ||
           !is.null(self@revision_magnitude_size))) {
      cli::cli_abort("`hurdle_ztpoisson` has no magnitude-dispersion parameter.")
    }
  }
)

#' Inert count-cumulative configuration for non-cumulative data
#' @keywords internal
#' @noRd
no_cumulative <- function() {
  cumulative_process_class(active = FALSE)
}
