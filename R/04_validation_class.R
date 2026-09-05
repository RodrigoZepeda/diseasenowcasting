# =============================================================================
# Validation process class
# =============================================================================
# A validation process describes what happens to a report AFTER it is filed: a
# laboratory result comes back, and the report is either CONFIRMED (a real case)
# or RETRACTED (struck from the register).  It is an OPTIONAL model component,
# attached to a model() alongside the epidemic and (appearance-)delay processes.
#
# This component is only for linelist / count-incidence data with a validation
# date and outcome per report; its likelihood is the mixture-cure block in
# 31_retraction_likelihood.R.  Aggregate count-cumulative streams use the
# distinct count_cumulative_process() component because they do not identify a
# report-level confirmation probability.
#
# With no validation process (`p = 1`) the model reduces exactly to the standard
# right-censored count model.
# =============================================================================

#' Validation process: reports that are later confirmed or retracted
#'
#' A report is rarely a case outright.  It is provisional, and **resolved exactly
#' once**: a test comes back, and the report is either *confirmed* (a real case)
#' or *retracted* (removed from the register).  A validation process models that
#' second step, so the nowcast targets the settled count rather than the raw
#' report count.
#'
#' Attach it with `model(validation = validation_process(...))`.  [nowcast()]
#' switches it on automatically when the data carry it -- see **Detection** below.
#'
#' @param validation_delay A `delay_process_class` describing the validation lag
#'   `g_C` (report to result).  Any delay family works; the `*_validation()`
#'   constructors ([lognormal_validation()] and friends) are aliases that read
#'   more naturally in this slot.  Default a short lognormal.
#' @param p Probability that a report resolves **positive** -- that it is a real
#'   case and is never retracted.  Either a `prior_class` (estimated under that
#'   prior) or a single numeric in `(0, 1]` (held fixed).  **Left unset (the
#'   default)** the behaviour depends on the data, because the two cases identify
#'   `p` through the report-level cure block:
#'
#'   * **linelist / count-incidence** -- a weak data-informed Beta.  The cure
#'     block pins `p` directly: a report standing unresolved for a long time is
#'     evidence about the cure fraction, so the likelihood is informative and the
#'     prior only has to keep `p` on the interval.
#'
#' @param stratified_p If `TRUE`, estimate a **separate** `p` per stratum instead
#'   of one shared value.  Only meaningful for per-report validation data with
#'   more than one stratum; the validation lag `g_C` stays shared either way (it
#'   is usually a property of the verification workflow, whereas `p` reflects how
#'   often a given group is misclassified).  Each stratum's `p` gets the same
#'   prior.  Default `FALSE` -- with sparse strata the shared `p` is safer.
#' @param negative_delay Optional second lag distribution for the **negative**
#'   resolutions, turning the model into a competing-risks fit in which positives
#'   and negatives come back on different timescales.  Needs data recording both
#'   outcomes (`mode = "both"`), since one sign alone cannot identify two lag
#'   laws.  Left `NULL` (the default) the two share one law, and the age of a
#'   pending report says nothing about which way it will go.
#' @param mode Which outcomes the data record.  `"auto"` (the default) infers it
#'   from the `validation_type` column; the others assert it.  See **Modes**.
#'
#' @section Modes:
#' What differs between surveillance systems is only which resolutions get
#' recorded, and all three possibilities share one likelihood:
#'
#' | | `retraction_only` | `confirmation_only` | `both` |
#' |---|---|---|---|
#' | outcomes recorded | the negatives | the positives | both signs |
#' | a missing outcome means | not retracted **yet** | not confirmed **yet** | not resolved **yet** |
#' | target | reports never retracted | reports eventually confirmed | reports resolving positive |
#' | lag support | `{1, 2, ...}` | `{0, 1, ...}` | `{0, 1, ...}` |
#'
#' The lag support differs only because a *retraction* in the same period as its
#' report describes a case never visible in any data vintage, whereas a test
#' coming back the day it was ordered is ordinary.
#'
#' `"auto"` reads `unique(validation_type)` over the **full** data, not the as-of
#' view, so the mode is a stable property of the data source and does not flip
#' between backtest dates.
#'
#' @section Detection:
#' [nowcast()] attaches a validation process when the `tbl_now` carries
#' `validation_date` / `validation_type` (see
#' `tbl.now::add_validation_date()`).
#'
#' Count-cumulative data instead use [count_cumulative_process()], whose primitive
#' retraction object is the finite-age kernel `h_R`; it does not estimate `p`.
#'
#' @section Count-cumulative data:
#' Do not use this component for an aggregate cumulative stream.  Configure its
#' down-revisions with [count_cumulative_process()].  Without individual report
#' outcomes, `p` and a conditional validation-delay law are not separately
#' identified.
#'
#' @section Default priors:
#' `validation_delay` inherits the default priors of its delay family (see
#' [delay_process]).  `p` is described under its argument above.  At `p = 1` the
#' validation layer is inert and the model is the ordinary count model.
#'
#' @returns A `validation_process_class` object, for `model(validation = )`.
#'
#' @examples
#' # Results come back on one timescale, whatever the answer:
#' validation_process(lognormal_validation())
#'
#' # Negatives come back faster than positives (competing risks):
#' validation_process(lognormal_validation(), negative_delay = lognormal_validation())
#'
#' # Attach to a model:
#' model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
#'       validation = validation_process(
#'         validation_delay = dirichlet_validation(bins = 10),
#'         p                = beta_prior(20, 3),
#'         stratified_p     = TRUE))
#'
#' @seealso [model()], [validation_delay], [delay_process], [nowcast()]
#' @export
validation_process <- function(validation_delay = lognormal_delay(),
                               p = numeric(0), stratified_p = FALSE,
                               negative_delay = NULL,
                               mode = c("auto", "confirmation_only",
                                        "retraction_only", "both")) {
  mode <- match.arg(mode)
  validation_process_class(validation_delay = validation_delay, p = p,
                           stratified_p = isTRUE(stratified_p),
                           negative_delay = negative_delay %||% list(),
                           mode = mode)
}

#' Validation process S7 class
#' @keywords internal
#' @noRd
validation_process_class <- S7::new_class(
  "validation_process_class",
  properties = list(
    validation_delay = delay_process_class,
    p                = .valid_param_slot,   # prior_class or fixed numeric in (0, 1]
    stratified_p     = S7::class_logical,   # one p per stratum instead of a shared one
    negative_delay   = S7::class_any,       # second lag law -> competing risks (or list())
    mode             = S7::class_character, # auto / confirmation_only / retraction_only / both
    active           = S7::class_logical    # FALSE for the inert (p = 1) default
  ),
  constructor = function(validation_delay = lognormal_delay(),
                         p                = numeric(0),
                         stratified_p     = FALSE,
                         negative_delay   = list(),
                         mode             = "auto",
                         active           = TRUE) {
    S7::new_object(S7::S7_object(),
                   validation_delay = validation_delay, p = p,
                   stratified_p = isTRUE(stratified_p),
                   negative_delay = negative_delay, mode = mode, active = active)
  },
  validator = function(self) {
    # `p` is unset (length-0 numeric -> data-dependent default), a fixed number in
    # (0, 1], or a prior on (0, 1) -- mirroring the delay parameter slots.
    if (is.numeric(self@p) && length(self@p) == 1L) {
      if (self@p <= 0 || self@p > 1)
        cli::cli_abort("Fixed validation probability `p` must be in (0, 1]. Got {self@p}.")
    } else if (is.numeric(self@p) && length(self@p) == 0L) {
      invisible(NULL)                       # unset -> resolved in default_priors()
    } else if (!S7::S7_inherits(self@p, prior_class)) {
      cli::cli_abort("`p` must be a prior_class object, a single numeric in (0, 1], or unset.")
    }
    if (!self@mode %in% c("auto", "confirmation_only", "retraction_only", "both"))
      cli::cli_abort(paste0("`mode` must be one of \"auto\", \"confirmation_only\", ",
                            "\"retraction_only\" or \"both\". Got {.val {self@mode}}."))
  }
)

# =============================================================================
# Validation-lag constructors
# =============================================================================
# `validation_delay` is an ordinary delay distribution used in a different role,
# so these are thin aliases of the `*_delay()` constructors.  They exist so that a
# model reads as what it is -- `validation_process(lognormal_validation())` -- and
# so the support convention has a documented home.

#' Validation-lag distributions
#'
#' The distribution `g_C` of the **validation lag**: how long after a case is
#' reported its result comes back.  Pass one to [validation_process()] as
#' `validation_delay`.
#'
#' These are aliases of the corresponding [delay_process] constructors -- a
#' validation lag is an ordinary non-negative delay, only measured from the
#' *report* rather than from the event -- so the parameters, priors and
#' `r lifecycle::badge('experimental')` behaviour are identical.  The one
#' difference is the support, and it depends on the mode: under
#' `retraction_only` the lag lives on `{1, 2, ...}` (a retraction lands strictly
#' after the report it withdraws, so a case retracted in the same period is
#' dropped by [nowcast()] -- it was never visible in any data vintage), while
#' under `confirmation_only` and `both` it lives on `{0, 1, ...}`, since a test
#' can come back the day it was ordered.
#'
#' @inheritParams delay_process
#'
#' @returns A `delay_process_class` object, for the `validation_delay` slot of
#'   [validation_process()].
#'
#' @section Which one to use:
#' `dirichlet_validation()` is the safest default when the counts are large.  The
#' correction applied to a pending report of age `j` is
#' `rho(j) = p / (p + (1 - p) * (1 - G_C(j)))`, so at high counts a *shape* error
#' in `g_C` biases the nowcast by more than its Monte-Carlo noise: on a COVID
#' series of ~8000 cases/day a lognormal `g_C` fitted to a `1 + Poisson(2)` lag
#' left a ~0.9% bias and lost nominal coverage, while the Dirichlet recovered
#' `rho` to four decimals.  At low counts the parametric families are fine and
#' estimate fewer parameters.
#'
#' @examples
#' validation_process(lognormal_validation())
#' validation_process(dirichlet_validation(bins = 10))
#'
#' # Held-fixed validation lag, e.g. from an external study
#' validation_process(gamma_validation(shape = log(3), rate = 2))
#'
#' @seealso [validation_process()], [delay_process], [nowcast()]
#' @name validation_delay
NULL

#' @rdname validation_delay
#' @export
lognormal_validation <- function(mu = numeric(0), sigma = numeric(0)) {
  lognormal_delay(mu = mu, sigma = sigma)
}

#' @rdname validation_delay
#' @export
gamma_validation <- function(shape = numeric(0), rate = numeric(0)) {
  gamma_delay(shape = shape, rate = rate)
}

#' @rdname validation_delay
#' @export
generalized_gamma_validation <- function(mu = numeric(0), sigma = numeric(0), Q = numeric(0)) {
  generalized_gamma_delay(mu = mu, sigma = sigma, Q = Q)
}

#' @rdname validation_delay
#' @export
dirichlet_validation <- function(alpha = numeric(0), bins = numeric(0)) {
  dirichlet_delay(alpha = alpha, bins = bins)
}

#' The inert validation process (`p = 1`, nothing is ever retracted).
#'
#' Used as the model() default so the validation layer is off unless the user opts
#' in.  nowcast() promotes this to an active default when the data carry a
#' report-level validation process.
#' @keywords internal
#' @noRd
no_validation <- function() {
  validation_process_class(validation_delay = lognormal_delay(), p = 1,
                           negative_delay = list(), mode = "auto", active = FALSE)
}

# =============================================================================
# Mode inference
# =============================================================================

#' Infer the validation mode from the outcomes recorded in the data.
#'
#' Reads `unique(validation_type)` over the FULL data rather than an as-of view, so
#' the mode is a stable property of the data source and cannot flip between
#' backtest dates.  A dated row whose outcome is `NA` is an error: the row IS
#' resolved but its sign is unknown, so it cannot enter either lag law.
#'
#' @param validation_type Character vector of outcomes (`"confirmed"`,
#'   `"retracted"`, `"pending"`, or `NA`).
#' @param validation_date The matching dates, used only to tell an unresolved row
#'   (no date) from a resolved one whose sign is missing.
#' @returns One of `"confirmation_only"`, `"retraction_only"` or `"both"`.
#' @keywords internal
#' @noRd
.infer_validation_mode <- function(validation_type, validation_date) {
  outcomes <- as.character(validation_type)
  dated    <- !is.na(validation_date)

  # A date without an outcome is unusable: the report resolved, but we cannot say
  # into which lag law it goes.  tbl.now warns at construction; this is the second
  # and final ask.
  # A dated row must say WHICH way it resolved.  `NA` is the obvious failure, but an
  # unrecognised label is the dangerous one: it would fall through every
  # `== "confirmed"` test and be silently counted as a retraction in `both` mode.
  recognised <- c("confirmed", "retracted")
  unusable   <- dated & (is.na(outcomes) | !outcomes %in% recognised)
  if (any(unusable)) {
    offending <- unique(outcomes[dated & !is.na(outcomes) & !outcomes %in% recognised])
    cli::cli_abort(c(
      "{sum(unusable)} row{?s} {?carries/carry} a validation date without a usable {.field validation_type}.",
      "x" = "A resolved report whose outcome is unknown cannot enter either lag law.",
      if (length(offending))
        c("x" = "Unrecognised outcome{?s}: {.val {offending}}.") else
        c("x" = "The outcome is missing."),
      "i" = "Use {.val confirmed} or {.val retracted}, or clear the date to mark the row {.val pending}."))
  }

  observed <- unique(outcomes[dated])
  has_confirmed <- "confirmed" %in% observed
  has_retracted <- "retracted" %in% observed

  # Nothing resolved anywhere in the data: the mode is genuinely unknowable, and
  # the honest answer is that there is no validation process to fit.  `NA` rather
  # than an error, because under retraction that IS the documented reduction --
  # nothing retracted means p = 1, i.e. the ordinary count model.  An ASSERTED
  # mode is still refused (see `.resolve_validation_mode()`); only inference falls
  # back.
  if (has_confirmed && has_retracted) "both"
  else if (has_confirmed)             "confirmation_only"
  else if (has_retracted)             "retraction_only"
  else                                NA_character_
}

#' Resolve the validation mode for a `tbl_now`, honouring an asserted one.
#'
#' `validation_process(mode = )` asserts; `"auto"` infers.  An asserted mode is
#' checked against the data, because asserting `both` on a stream that only
#' records retractions would silently fit an unidentifiable model.
#'
#' @param data A `tbl_now` carrying a validation process.
#' @param validation The model's `validation_process_class` component.
#' @returns `"confirmation_only"`, `"retraction_only"` or `"both"`.
#' @keywords internal
#' @noRd
.resolve_validation_mode <- function(data, validation) {
  type_col <- tbl.now::get_validation_type(data)
  date_col <- tbl.now::get_validation_date(data)
  if (is.null(type_col) || !type_col %in% names(data))
    cli::cli_abort(c(
      "The data carry a validation date but no {.field validation_type} column.",
      "i" = "A date alone cannot say whether the report was confirmed or retracted.",
      "*" = "Set one with {.code tbl.now::change_validation_date(x, <date>, validation_type = <outcome>)}."))

  # Inferred from the FULL data, not the as-of view, so the mode is a property of
  # the data source and cannot flip between backtest dates.
  observed_mode <- .infer_validation_mode(data[[type_col]], data[[date_col]])
  asserted <- validation@mode %||% "auto"
  inferring <- identical(asserted, "auto") || !isTRUE(validation@active)

  if (is.na(observed_mode)) {
    # No row has resolved anywhere in the data.
    if (inferring) {
      cli::cli_inform(c(
        "i" = "No report has resolved, so there is nothing for a validation process to learn.",
        "*" = "Fitting the ordinary count model ({.code p = 1})."))
      return("none")
    }
    # An ASSERTED mode with nothing resolved is not an error: this is a Bayesian
    # model, and a parameter the data say nothing about is exactly what a prior is
    # for.  The user has told us the process exists; the posterior on `p` is then
    # its prior, which is the honest answer rather than a refusal.
    cli::cli_inform(c(
      "i" = "No report has resolved yet, so {.arg p} is determined by its prior.",
      "*" = "Set it with {.code validation_process(p = beta_prior(...))}, or use {.code mode = \"auto\"} to fit the ordinary count model instead."))
    return(asserted)
  }
  if (inferring) return(observed_mode)

  if (!identical(asserted, observed_mode)) {
    # Asserting a mode the data cannot support is an error; asserting a NARROWER
    # one than the data offer throws information away, so it is a warning.
    if (identical(observed_mode, "both"))
      cli::cli_warn(c(
        "{.code validation_process(mode = {.val {asserted}})} ignores outcomes the data record.",
        "i" = "The data carry both confirmations and retractions; seeing both signs is strictly more informative."))
    else
      cli::cli_abort(c(
        "{.code validation_process(mode = {.val {asserted}})} does not match the data.",
        "x" = "The data record only {.val {observed_mode}} outcomes.",
        "i" = "Use {.val auto}, or {.val {observed_mode}}."))
  }
  asserted
}
