# =============================================================================
# reporting_fraction() -- how much of each cohort the fit believes has arrived
# =============================================================================

#' How much of each event-time has been reported, and by how much the nowcast
#' multiplies it
#'
#' Every nowcast is, at bottom, one number per event-time: the fraction of that
#' cohort's cases that has arrived by the as-of date. The engine calls it
#' `Gstar`, and it is the fitted reporting distribution evaluated at the
#' cohort's age,
#'
#' \deqn{G^*(t) = F_D(d^*_t + 1),}
#'
#' where \eqn{F_D} is the fitted delay CDF and \eqn{d^*_t} is the largest delay
#' that could have been observed for event-time \eqn{t}. The expected settled
#' count is the observed count divided by `reporting_fraction`, so
#' `inflation = 1 / reporting_fraction` is exactly the multiplier the nowcast is
#' applying --- the whole of the claim it makes, expressed as one dimensionless
#' number per cohort.
#'
#' It is worth reading directly, because it is the quantity most nowcast errors
#' travel through and it is not visible in the predictive summary. On the
#' `covid_colombia` stream at `now = 2020-08-01`, every model in a seven-way
#' sweep put the horizon-0 inflation at 132--208 --- while the settled truth for
#' that cohort was 9,171 against 309 reported, an inflation of 29.7. No epidemic
#' process can be right when it is handed a reporting fraction five times too
#' small, and each one lands somewhere different on the resulting ridge.
#'
#' @section What the numbers mean:
#'
#' `reporting_fraction` lies in \eqn{(0, 1]} and `inflation` in
#' \eqn{[1, \infty)}. They are *fitted* quantities, not observed ones: the model
#' extrapolates the delay distribution past the data.
#'
#' - `inflation` near 1 means the cohort is settled and the nowcast is little
#'   more than the observed count.
#' - `inflation` grows sharply towards the as-of date, where the cohort is
#'   youngest. **A very large value at horizon 0 is normal, not a fault** --- on
#'   early-2020 `covid_us` the correct inflation is 37--83, and a fit reporting
#'   anything near 1 there would be badly wrong. There is no threshold that is
#'   universally suspicious, which is why nothing here warns.
#' - What *is* informative is comparing it against the settled inflation you can
#'   compute retrospectively, or against its own stability: a delay law that is
#'   drifting over calendar time (`covid_colombia` runs 15.8 in March, 103.2 in
#'   June, 5.1 in October) makes a single stationary estimate wrong in both
#'   directions at different dates.
#'
#' A `reporting_fraction` of exactly 0 is returned as an `inflation` of `Inf`
#' rather than being dropped: it means the fitted delay law puts no mass at all
#' within the cohort's age, which is a statement worth seeing.
#'
#' @param object A result from [nowcast()] or [auto_nowcast()].
#' @param summary If `TRUE` (default), pool the retained fits and return one row
#'   per (event-time, stratum) with the median across fits plus the range. If
#'   `FALSE`, return one row per fit as well, in a `fit` column.
#'
#' @returns A `data.frame` with columns `event_date` (or `.event_num` when the
#'   calendar is unavailable), `stratum`, `horizon` (event-time units back from
#'   the as-of date; `0` is the most recent cohort), `observed`,
#'   `reporting_fraction` and `inflation`. With `summary = TRUE` the
#'   `reporting_fraction_low` / `reporting_fraction_high` columns give the range
#'   across the retained fits, which for a `type = "two_stage"` fit is the delay
#'   uncertainty the cascade propagates.
#'
#' @seealso [fit_check()] for optimizer diagnostics and the `log_mean` ceiling,
#'   [nowcast_diagnostic()], [predict()]
#' @examples
#' \dontrun{
#' nc <- nowcast(tn, model(), type = "two_stage")
#' rf <- reporting_fraction(nc)
#' head(rf[order(rf$horizon), ])          # the youngest cohorts inflate most
#' subset(rf, horizon == 0)$inflation     # what the nowcast is multiplying by
#' }
#' @export
reporting_fraction <- function(object, summary = TRUE) {
  native <- .unwrap_nowcast(object, "object")
  fits <- native@fits
  if (!length(fits)) {
    cli::cli_abort("{.arg object} carries no retained fits to read a reporting fraction from.")
  }

  engine <- native@engine
  n_strata <- as.integer(engine$num_strata %||% 1L)
  strata_levels <- engine$strata_levels %||% NULL

  gstar_of <- function(fit) {
    g <- fit$Gstar
    if (is.null(g)) return(NULL)
    if (!is.matrix(g)) matrix(g, ncol = n_strata) else g
  }
  matrices <- Filter(Negate(is.null), lapply(fits, gstar_of))
  if (!length(matrices)) {
    cli::cli_abort(c(
      "No fit in {.arg object} carries a `Gstar` reporting fraction.",
      "i" = "Cumulative and validation streams reach the likelihood through cohort kernels instead."
    ))
  }

  n_time <- nrow(matrices[[1]])
  event_dates <- tryCatch({
    min_event <- engine$min_event
    if (is.null(min_event)) NULL else
      seq(as.Date(min_event), by = as.character(engine$event_unit), length.out = n_time)
  }, error = function(e) NULL)

  observed <- engine$case_counts
  if (!is.matrix(observed)) observed <- matrix(observed, n_time, n_strata)

  # The most recent event-time is horizon 0; `target` is that index.
  target <- as.integer(native@target %||% n_time)

  grid <- expand.grid(time = seq_len(n_time), stratum = seq_len(n_strata),
                      KEEP.OUT.ATTRS = FALSE)
  label <- function(index) {
    if (is.null(strata_levels) || length(strata_levels) < n_strata) {
      if (n_strata == 1L) "Total" else as.character(index)
    } else as.character(strata_levels[index])
  }

  base <- data.frame(
    .event_num = grid$time - 1L,
    stratum = vapply(grid$stratum, label, character(1)),
    horizon = target - grid$time,
    observed = observed[cbind(grid$time, grid$stratum)],
    stringsAsFactors = FALSE
  )
  if (!is.null(event_dates)) {
    base <- cbind(event_date = event_dates[grid$time], base)
  }

  # `inflation` is 1 / fraction, with an exactly-zero fraction reported as Inf
  # rather than silently dropped -- see the documentation.
  inflate <- function(fraction) ifelse(fraction > 0, 1 / fraction, Inf)

  if (!isTRUE(summary)) {
    out <- do.call(rbind, lapply(seq_along(matrices), function(i) {
      fraction <- matrices[[i]][cbind(grid$time, grid$stratum)]
      cbind(fit = i, base, reporting_fraction = fraction,
            inflation = inflate(fraction))
    }))
    rownames(out) <- NULL
    return(out)
  }

  stacked <- vapply(matrices, function(m) m[cbind(grid$time, grid$stratum)],
                    numeric(nrow(grid)))
  if (!is.matrix(stacked)) stacked <- matrix(stacked, nrow = nrow(grid))
  fraction <- apply(stacked, 1L, stats::median)
  out <- cbind(
    base,
    reporting_fraction = fraction,
    reporting_fraction_low = apply(stacked, 1L, min),
    reporting_fraction_high = apply(stacked, 1L, max),
    inflation = inflate(fraction)
  )
  rownames(out) <- NULL
  out
}
