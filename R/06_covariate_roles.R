# =============================================================================
# Process roles for covariates
# =============================================================================

#' Tag covariates for a nowcasting process
#'
#' These helpers attach a lightweight S3 class to a covariate without changing
#' its values or removing any of its existing classes. `diseasenowcasting`
#' reads the tags when it prepares a `tbl_now` object:
#'
#' - event covariates affect the latent event/incidence process;
#' - delay covariates affect event-to-report timing;
#' - revision covariates affect report-to-revision timing.
#'
#' Untagged columns registered by `tbl.now::get_covariates()` remain event
#' covariates for backward compatibility. A vector may carry more than one tag.
#'
#' @section What the coefficients mean:
#' Delay and revision covariates enter a discrete-time hazard regression, not the
#' delay's location parameter: `logit h(k) = logit h0(k) + eta`, where `h0` is the
#' chosen delay family's own conditional hazard. A coefficient is therefore a log
#' hazard-odds ratio -- how much more likely a still-unreported case is to be
#' reported on a flagged date -- and it changes *timing*, not the eventual number
#' of cases and not the confirmation probability `p`. At zero coefficients the
#' model is exactly the stationary one.
#'
#' @section Eligibility:
#' The two roles are knowable at different times, so they are held to different
#' standards.
#'
#' A **delay** covariate is a property of the latent reporting risk set, which
#' includes the cases that have not been reported yet. It must therefore be
#' constant within each event-time by stratum cell; a value that varies between
#' the reports that happen to have arrived says nothing about the ones that have
#' not, and is refused. This is also what keeps the marks independent and
#' identically distributed within a cell, so that a single
#' `1 - G_R,t(d*)` is still the probability of being unobserved there.
#'
#' A **revision** covariate only has to be known once the report exists, so it may
#' differ from report to report. The likelihood is then *conditional* on those
#' values: they are treated as fixed, which is only meaningful if they are
#' unrelated to whether the report was filed at all. A variable recorded only when
#' the revision happens must not be used, because it is structurally missing for
#' exactly the pending reports the correction is about.
#'
#' Calendar effects are not covariates and are exempt from the first rule:
#' `tbl.now` can generate them for every possible destination date, including
#' dates with no observations, so report-date and revision-date temporal effects
#' are the right tool for operational rhythms such as weekends and holidays.
#'
#' @section Stability across fits:
#' The fitted design -- reference levels, contrasts, centring constants and the
#' surviving columns -- is pinned as a schema on the fitted object, so
#' `delay_beta[2]` keeps meaning the same contrast when the model is re-fitted or
#' updated. Declare tagged categorical columns as a `factor` with explicit
#' `levels`: a character column can only take its reference level from whatever
#' sorts first in the current as-of view, and `diseasenowcasting` warns when it
#' has to do that. A backtest deliberately rebuilds the schema at each as-of date,
#' because replaying a later one would use information that date did not have.
#'
#' @section Cost:
#' A reporting regression makes the delay law cohort-specific, so the objective
#' builds one hazard path per event time and stratum instead of one shared
#' distribution. Taping is roughly quadratic in the number of event times and
#' linear in the strata: on this machine a 365-step daily series takes a few
#' seconds to tape against well under one for the stationary model. Both
#' `type = "one_stage"` and `type = "two_stage"` are supported; under two-stage,
#' Stage 1 fits the baseline and the coefficients together as a censored
#' regression on the recent window and Stage 2 fixes the whole vector, so the
#' reporting data is read once.
#'
#' The helpers can be used on a vector, typically inside `dplyr::mutate()`, or
#' on a data frame / `tbl_now` with tidy-select expressions in `...`. When a
#' `tbl_now` is supplied without selections, all columns registered as
#' covariates are tagged.
#'
#' @param x A vector to tag, or a data frame / `tbl_now` containing covariates.
#' @param ... For data-frame inputs, tidy-select expressions identifying the
#'   columns to tag.
#'
#' @return `x`, with the requested process-role S3 class added.
#' @examples
#' x <- data.frame(day = 1:3, capacity = c(10, 12, 11))
#' x$capacity <- as_delay_covariates(x$capacity)
#' inherits(x$capacity, "delay_covariates")
#'
#' x <- as_revision_covariates(x, capacity)
#' inherits(x$capacity, "revision_covariates")
#' @name covariate_roles
NULL

.tag_covariate_role <- function(x, role) {
  marker <- paste0(role, "_covariates")
  old_class <- class(x)
  if (!marker %in% old_class) class(x) <- c(marker, old_class)
  x
}

.as_process_covariates <- function(x, ..., role) {
  dots <- rlang::enquos(...)
  if (!is.data.frame(x)) {
    if (length(dots)) {
      cli::cli_abort("Tidy-select expressions in `...` are only supported when `x` is a data frame.")
    }
    return(.tag_covariate_role(x, role))
  }

  columns <- if (length(dots)) {
    names(dplyr::select(as.data.frame(x), !!!dots))
  } else if (tbl.now::is_tbl_now(x)) {
    tbl.now::get_covariates(x) %||% character(0)
  } else {
    names(x)
  }
  if (!length(columns)) {
    cli::cli_abort("No covariate columns were selected to tag.")
  }
  for (column in columns) x[[column]] <- .tag_covariate_role(x[[column]], role)
  x
}

#' @rdname covariate_roles
#' @export
as_event_covariates <- function(x, ...) {
  .as_process_covariates(x, ..., role = "event")
}

#' @rdname covariate_roles
#' @export
as_delay_covariates <- function(x, ...) {
  .as_process_covariates(x, ..., role = "delay")
}

#' @rdname covariate_roles
#' @export
as_revision_covariates <- function(x, ...) {
  .as_process_covariates(x, ..., role = "revision")
}

#' Resolve the process role of every ordinary covariate column
#' @keywords internal
#' @noRd
.covariate_roles <- function(data) {
  frame <- as.data.frame(data)
  registered <- tryCatch(tbl.now::get_covariates(data), error = function(e) character(0))
  registered <- intersect(registered %||% character(0), names(frame))
  tagged <- lapply(c("event", "delay", "revision"), function(role) {
    marker <- paste0(role, "_covariates")
    names(frame)[vapply(frame, inherits, logical(1), what = marker)]
  })
  names(tagged) <- c("event", "delay", "revision")

  explicitly_tagged <- unique(unlist(tagged, use.names = FALSE))
  tagged$event <- unique(c(tagged$event, setdiff(registered, explicitly_tagged)))
  tagged
}

.restore_covariate_role_classes <- function(value, source) {
  markers <- intersect(
    class(source),
    c("event_covariates", "delay_covariates", "revision_covariates")
  )
  class(value) <- unique(c(markers, class(value)))
  value
}

.strip_covariate_role_classes <- function(value) {
  class(value) <- setdiff(
    class(value),
    c("event_covariates", "delay_covariates", "revision_covariates")
  )
  value
}

# vctrs uses double dispatch for join keys. A tagged stratum can therefore meet
# the corresponding untagged factor/character generated by tbl.now while it
# completes an observation grid. Preserve the role on the common type without
# changing the underlying factor levels or character values.
.covariate_role_ptype2_left <- function(x, y, ...) {
  .restore_covariate_role_classes(
    vctrs::vec_ptype2(.strip_covariate_role_classes(x), y, ...), x
  )
}
.covariate_role_ptype2_right <- function(x, y, ...) {
  .restore_covariate_role_classes(
    vctrs::vec_ptype2(x, .strip_covariate_role_classes(y), ...), y
  )
}
.covariate_role_cast_left <- function(x, to, ...) {
  .restore_covariate_role_classes(
    vctrs::vec_cast(
      .strip_covariate_role_classes(x),
      .strip_covariate_role_classes(to), ...
    ), to
  )
}
.covariate_role_cast_right <- function(x, to, ...) {
  vctrs::vec_cast(.strip_covariate_role_classes(x), to, ...)
}
.covariate_role_ptype2_both <- function(x, y, ...) {
  value <- vctrs::vec_ptype2(
    .strip_covariate_role_classes(x),
    .strip_covariate_role_classes(y), ...
  )
  .restore_covariate_role_classes(
    .restore_covariate_role_classes(value, x), y
  )
}
.covariate_role_cast_both <- function(x, to, ...) {
  .restore_covariate_role_classes(
    vctrs::vec_cast(
      .strip_covariate_role_classes(x),
      .strip_covariate_role_classes(to), ...
    ), to
  )
}

#' @export
vec_ptype2.event_covariates.event_covariates <- .covariate_role_ptype2_both
#' @export
vec_cast.event_covariates.event_covariates <- .covariate_role_cast_both
#' @export
vec_ptype2.delay_covariates.delay_covariates <- .covariate_role_ptype2_both
#' @export
vec_cast.delay_covariates.delay_covariates <- .covariate_role_cast_both
#' @export
vec_ptype2.revision_covariates.revision_covariates <- .covariate_role_ptype2_both
#' @export
vec_cast.revision_covariates.revision_covariates <- .covariate_role_cast_both

#' @export
vec_ptype2.event_covariates.factor <- .covariate_role_ptype2_left
#' @export
vec_ptype2.factor.event_covariates <- .covariate_role_ptype2_right
#' @export
vec_cast.event_covariates.factor <- .covariate_role_cast_left
#' @export
vec_cast.factor.event_covariates <- .covariate_role_cast_right
#' @export
vec_ptype2.delay_covariates.factor <- .covariate_role_ptype2_left
#' @export
vec_ptype2.factor.delay_covariates <- .covariate_role_ptype2_right
#' @export
vec_cast.delay_covariates.factor <- .covariate_role_cast_left
#' @export
vec_cast.factor.delay_covariates <- .covariate_role_cast_right
#' @export
vec_ptype2.revision_covariates.factor <- .covariate_role_ptype2_left
#' @export
vec_ptype2.factor.revision_covariates <- .covariate_role_ptype2_right
#' @export
vec_cast.revision_covariates.factor <- .covariate_role_cast_left
#' @export
vec_cast.factor.revision_covariates <- .covariate_role_cast_right

#' @export
vec_ptype2.event_covariates.character <- .covariate_role_ptype2_left
#' @export
vec_ptype2.character.event_covariates <- .covariate_role_ptype2_right
#' @export
vec_cast.event_covariates.character <- .covariate_role_cast_left
#' @export
vec_cast.character.event_covariates <- .covariate_role_cast_right
#' @export
vec_ptype2.delay_covariates.character <- .covariate_role_ptype2_left
#' @export
vec_ptype2.character.delay_covariates <- .covariate_role_ptype2_right
#' @export
vec_cast.delay_covariates.character <- .covariate_role_cast_left
#' @export
vec_cast.character.delay_covariates <- .covariate_role_cast_right
#' @export
vec_ptype2.revision_covariates.character <- .covariate_role_ptype2_left
#' @export
vec_ptype2.character.revision_covariates <- .covariate_role_ptype2_right
#' @export
vec_cast.revision_covariates.character <- .covariate_role_cast_left
#' @export
vec_cast.character.revision_covariates <- .covariate_role_cast_right

# Keep the lightweight role class through base vector operations. Existing
# classes (Date, factor, ordered, and so on) still receive their own methods via
# NextMethod().
#' @export
`[.event_covariates` <- function(x, ...) {
  .restore_covariate_role_classes(NextMethod(), x)
}
#' @export
`[.delay_covariates` <- function(x, ...) {
  .restore_covariate_role_classes(NextMethod(), x)
}
#' @export
`[.revision_covariates` <- function(x, ...) {
  .restore_covariate_role_classes(NextMethod(), x)
}
