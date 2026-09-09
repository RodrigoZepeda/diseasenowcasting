# =============================================================================
# Revision process class
# =============================================================================
# A revision process describes what happens to a report AFTER it is filed: a
# laboratory result comes back, and the report is either CONFIRMED (a real case)
# or RETRACTED (struck from the register).  It is an OPTIONAL model component,
# attached to a model() alongside the epidemic and (appearance-)delay processes.
#
# This component is only for linelist / count-incidence data with a revision
# date and outcome per report; its likelihood is the mixture-cure block in
# 31_retraction_likelihood.R.  Aggregate count-cumulative streams use the
# distinct cumulative_process() component because they do not identify a
# report-level confirmation probability.
#
# With no revision process (`p = 1`) the model reduces exactly to the standard
# right-censored count model.
# =============================================================================

#' Revision process: reports that are later confirmed or retracted
#'
#' A report is rarely a case outright.  It is provisional, and **resolved exactly
#' once**: a test comes back, and the report is either *confirmed* (a real case)
#' or *retracted* (removed from the register).  A revision process models that
#' second step, so the nowcast targets the settled count rather than the raw
#' report count.
#'
#' Attach it with `model(revision = revision_process(...))`.  [nowcast()]
#' switches it on automatically when the data carry it -- see **Detection** below.
#'
#' @param revision_delay A `delay_process_class` describing the revision lag
#'   `g_C` (report to result).  Any delay family works; the `*_revision()`
#'   constructors ([lognormal_revision()] and friends) are aliases that read
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
#'   of one shared value.  Only meaningful for per-report revision data with
#'   more than one stratum; the revision lag `g_C` stays shared either way (it
#'   is usually a property of the verification workflow, whereas `p` reflects how
#'   often a given group is misclassified).  Each stratum's `p` gets the same
#'   prior.  Default `FALSE` -- with sparse strata the shared `p` is safer.
#' @param mode Which outcomes the data record.  `"auto"` (the default) infers it
#'   from the `revision_type` column; the others assert it.  See **Modes**.
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
#' In `mode = "both"`, `revision_delay` is one shared law for positive and
#' negative resolutions. This first prototype deliberately does not fit separate
#' competing-risk lag laws. In a one-outcome mode, the same argument denotes the
#' lag for the outcome that is recorded: confirmation or retraction respectively.
#'
#' `"auto"` reads `unique(revision_type)` over the **full** data, not the as-of
#' view, so the mode is a stable property of the data source and does not flip
#' between backtest dates.
#'
#' @section Detection:
#' [nowcast()] attaches a revision process when the `tbl_now` carries
#' `revision_date` / `revision_type` (see
#' `tbl.now::add_revision_date()`).
#'
#' Count-cumulative data instead use [cumulative_process()], whose primitive
#' retraction object is the finite-age kernel `h_R`; it does not estimate `p`.
#'
#' @section Count-cumulative data:
#' Do not use this component for an aggregate cumulative stream.  Configure its
#' down-revisions with [cumulative_process()].  Without individual report
#' outcomes, `p` and a conditional revision-delay law are not separately
#' identified.
#'
#' @section Default priors:
#' `revision_delay` inherits the default priors of its delay family (see
#' [delay_process]).  `p` is described under its argument above.  At `p = 1` the
#' revision layer is inert and the model is the ordinary count model.
#'
#' @returns A `revision_process_class` object, for `model(revision = )`.
#'
#' @examples
#' # Results come back on one shared timescale, whatever the answer:
#' revision_process(lognormal_revision())
#'
#' # Attach to a model:
#' model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
#'       revision = revision_process(
#'         revision_delay = dirichlet_revision(bins = 10),
#'         p                = beta_prior(20, 3),
#'         stratified_p     = TRUE))
#'
#' @seealso [model()], [revision_distributions], [delay_process], [nowcast()]
#' @export
revision_process <- function(revision_delay = lognormal_delay(),
                               p = numeric(0), stratified_p = FALSE,
                               mode = c("auto", "confirmation_only",
                                        "retraction_only", "both")) {
  mode <- match.arg(mode)
  revision_process_class(revision_delay = revision_delay, p = p,
                           stratified_p = isTRUE(stratified_p),
                           mode = mode)
}

#' Revision process S7 class
#' @keywords internal
#' @noRd
revision_process_class <- S7::new_class(
  "revision_process_class",
  properties = list(
    revision_delay = delay_process_class,
    p                = .valid_param_slot,   # prior_class or fixed numeric in (0, 1]
    stratified_p     = S7::class_logical,   # one p per stratum instead of a shared one
    mode             = S7::class_character, # auto / confirmation_only / retraction_only / both
    active           = S7::class_logical    # FALSE for the inert (p = 1) default
  ),
  constructor = function(revision_delay = lognormal_delay(),
                         p                = numeric(0),
                         stratified_p     = FALSE,
                         mode             = "auto",
                         active           = TRUE) {
    S7::new_object(S7::S7_object(),
                   revision_delay = revision_delay, p = p,
                   stratified_p = isTRUE(stratified_p),
                   mode = mode, active = active)
  },
  validator = function(self) {
    # `p` is unset (length-0 numeric -> data-dependent default), a fixed number in
    # (0, 1], or a prior on (0, 1) -- mirroring the delay parameter slots.
    if (is.numeric(self@p) && length(self@p) == 1L) {
      if (self@p <= 0 || self@p > 1)
        cli::cli_abort("Fixed revision probability `p` must be in (0, 1]. Got {self@p}.")
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
# Revision-lag constructors
# =============================================================================
# `revision_delay` is an ordinary delay distribution used in a different role,
# so these are thin aliases of the `*_delay()` constructors.  They exist so that a
# model reads as what it is -- `revision_process(lognormal_revision())` -- and
# so the support convention has a documented home.

#' Revision-lag distributions
#'
#' The distribution `g_C` of the **revision lag**: how long after a case is
#' reported its result comes back.  Pass one to [revision_process()] as
#' `revision_delay`.
#'
#' These are aliases of the corresponding [delay_process] constructors -- a
#' revision lag is an ordinary non-negative delay, only measured from the
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
#' @returns A `delay_process_class` object, for the `revision_delay` slot of
#'   [revision_process()].
#'
#' @section Which one to use:
#' `dirichlet_revision()` is the safest default when the counts are large.  The
#' correction applied to a pending report of age `j` is
#' `rho(j) = p / (p + (1 - p) * (1 - G_C(j)))`, so at high counts a *shape* error
#' in `g_C` biases the nowcast by more than its Monte-Carlo noise: on a COVID
#' series of ~8000 cases/day a lognormal `g_C` fitted to a `1 + Poisson(2)` lag
#' left a ~0.9% bias and lost nominal coverage, while the Dirichlet recovered
#' `rho` to four decimals.  At low counts the parametric families are fine and
#' estimate fewer parameters.
#'
#' @examples
#' revision_process(lognormal_revision())
#' revision_process(dirichlet_revision(bins = 10))
#'
#' # Held-fixed revision lag, e.g. from an external study
#' revision_process(gamma_revision(shape = log(3), rate = 2))
#'
#' @seealso [revision_process()], [delay_process], [nowcast()]
#' @name revision_distributions
NULL

#' @rdname revision_distributions
#' @export
lognormal_revision <- function(mu = numeric(0), sigma = numeric(0)) {
  lognormal_delay(mu = mu, sigma = sigma)
}

#' @rdname revision_distributions
#' @export
gamma_revision <- function(shape = numeric(0), rate = numeric(0)) {
  gamma_delay(shape = shape, rate = rate)
}

#' @rdname revision_distributions
#' @export
generalized_gamma_revision <- function(mu = numeric(0), sigma = numeric(0), Q = numeric(0)) {
  generalized_gamma_delay(mu = mu, sigma = sigma, Q = Q)
}

#' @rdname revision_distributions
#' @export
dirichlet_revision <- function(alpha = numeric(0), bins = numeric(0)) {
  dirichlet_delay(alpha = alpha, bins = bins)
}

#' The inert revision process (`p = 1`, nothing is ever retracted).
#'
#' Used as the model() default so the revision layer is off unless the user opts
#' in.  nowcast() promotes this to an active default when the data carry a
#' report-level revision process.
#' @keywords internal
#' @noRd
no_revision <- function() {
  revision_process_class(revision_delay = lognormal_delay(), p = 1,
                           mode = "auto", active = FALSE)
}

# =============================================================================
# tbl.now revision metadata adapter
# =============================================================================

# Read current tbl.now revision metadata in one place.
.tblnow_has_revision <- function(data) {
  tbl.now::has_revision(data)
}

.tblnow_get_revision_date <- function(data) {
  tbl.now::get_revision_date(data)
}

.tblnow_get_revision_type <- function(data) {
  tbl.now::get_revision_type(data)
}

.tblnow_get_is_censored_revision <- function(data) {
  tbl.now::get_is_censored_revision(data)
}

# =============================================================================
# Mode inference
# =============================================================================

#' Infer the revision mode from the outcomes recorded in the data.
#'
#' Reads `unique(revision_type)` over the FULL data rather than an as-of view, so
#' the mode is a stable property of the data source and cannot flip between
#' backtest dates.  A dated row whose outcome is `NA` is an error: the row IS
#' resolved but its sign is unknown, so it cannot enter either lag law.
#'
#' @param revision_type Character vector of outcomes (`"confirmed"`,
#'   `"retracted"`, `"pending"`, or `NA`).
#' @param revision_date The matching dates, used only to tell an unresolved row
#'   (no date) from a resolved one whose sign is missing.
#' @returns One of `"confirmation_only"`, `"retraction_only"` or `"both"`.
#' @keywords internal
#' @noRd
.infer_revision_mode <- function(revision_type, revision_date) {
  outcomes <- as.character(revision_type)
  dated    <- !is.na(revision_date)

  # A date without an outcome is unusable: the report resolved, but we cannot say
  # into which lag law it goes. tbl.now warns at construction; this is the final
  # defensive check before the values reach the likelihood.
  # A dated row must say WHICH way it resolved.  `NA` is the obvious failure, but an
  # unrecognised label is the dangerous one: it would fall through every
  # `== "confirmed"` test and be silently counted as a retraction in `both` mode.
  recognised <- c("confirmed", "retracted")
  unusable   <- dated & (is.na(outcomes) | !outcomes %in% recognised)
  if (any(unusable)) {
    offending <- unique(outcomes[dated & !is.na(outcomes) & !outcomes %in% recognised])
    cli::cli_abort(c(
      "{sum(unusable)} row{?s} {?carries/carry} a revision date without a usable {.field revision_type}.",
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
  # the honest answer is that there is no revision process to fit.  `NA` rather
  # than an error, because under retraction that IS the documented reduction --
  # nothing retracted means p = 1, i.e. the ordinary count model.  An ASSERTED
  # mode can still retain the process under its prior (see
  # `.resolve_revision_mode()`); only automatic inference falls back.
  if (has_confirmed && has_retracted) "both"
  else if (has_confirmed)             "confirmation_only"
  else if (has_retracted)             "retraction_only"
  else                                NA_character_
}

#' Resolve the revision mode for a `tbl_now`, honouring an asserted one.
#'
#' `revision_process(mode = )` asserts; `"auto"` infers.  An asserted mode is
#' checked against the data, because asserting `both` on a stream that only
#' records retractions would silently fit an unidentifiable model.
#'
#' @param data A `tbl_now` carrying a revision process.
#' @param revision The model's `revision_process_class` component.
#' @returns `"confirmation_only"`, `"retraction_only"` or `"both"`.
#' @keywords internal
#' @noRd
.resolve_revision_mode <- function(data, revision) {
  type_col <- .tblnow_get_revision_type(data)
  date_col <- .tblnow_get_revision_date(data)
  if (is.null(type_col) || !type_col %in% names(data))
    cli::cli_abort(c(
      "The data carry a revision date but no {.field revision_type} column.",
      "i" = "A date alone cannot say whether the report was confirmed or retracted.",
      "*" = "Set one with {.code tbl.now::change_revision_date(x, <date>, revision_type = <outcome>)}."))

  # Inferred from the FULL data, not the as-of view, so the mode is a property of
  # the data source and cannot flip between backtest dates.
  observed_mode <- .infer_revision_mode(data[[type_col]], data[[date_col]])
  asserted <- revision@mode %||% "auto"
  inferring <- identical(asserted, "auto") || !isTRUE(revision@active)

  if (is.na(observed_mode)) {
    # No row has resolved anywhere in the data.
    if (inferring) {
      cli::cli_inform(c(
        "i" = "No report has resolved, so there is nothing for a revision process to learn.",
        "*" = "Fitting the ordinary count model ({.code p = 1})."))
      return("none")
    }
    # An ASSERTED mode with nothing resolved is not an error: this is a Bayesian
    # model, and a parameter the data say nothing about is exactly what a prior is
    # for.  The user has told us the process exists; the posterior on `p` is then
    # its prior, which is the honest answer rather than a refusal.
    cli::cli_inform(c(
      "i" = "No report has resolved yet, so {.arg p} is determined by its prior.",
      "*" = "Set it with {.code revision_process(p = beta_prior(...))}, or use {.code mode = \"auto\"} to fit the ordinary count model instead."))
    return(asserted)
  }
  if (inferring) return(observed_mode)

  if (!identical(asserted, observed_mode)) {
    # Asserting a mode the data cannot support is an error; asserting a NARROWER
    # one than the data offer throws information away, so it is a warning.
    if (identical(observed_mode, "both"))
      cli::cli_warn(c(
        "{.code revision_process(mode = {.val {asserted}})} ignores outcomes the data record.",
        "i" = "The data carry both confirmations and retractions; seeing both signs is strictly more informative."))
    else
      cli::cli_abort(c(
        "{.code revision_process(mode = {.val {asserted}})} does not match the data.",
        "x" = "The data record only {.val {observed_mode}} outcomes.",
        "i" = "Use {.val auto}, or {.val {observed_mode}}."))
  }
  asserted
}
