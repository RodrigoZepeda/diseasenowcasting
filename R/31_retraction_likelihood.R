# =============================================================================
# Linelist retractions: the confirmation (cure-model) observation block
# =============================================================================
# A linelist may carry a RETRACTION DATE: present when the case was eventually
# removed from the register (reclassified, corrected, duplicate) and missing when
# it never was.  A missing retraction date does NOT mean "this case is genuine" --
# it means "this case has not been retracted YET", so the retraction lag of a
# still-standing row is RIGHT-CENSORED at the age of its report, `now - report`.
#
# The generative model is the one the count-cumulative confirmation model already
# uses (see 28_confirmation_likelihood.R and section "Linelist data with
# retractions" of the Mathematics vignette): each report is genuine with
# probability `p`, otherwise transient and retracted a further `C ~ g_C` periods
# after it appeared.  Poisson colouring makes the observable trajectory-type
# counts independent Poissons, and the one-origin log-likelihood splits into
#
#   (i)   count block : the ordinary right-censored count likelihood on the number
#                       of rows k_t, with the epidemic mean lambda_t replaced by
#                       the GROSS report rate mu_t = lambda_t / p,
#   (ii)  delay block : sum_i log g_D(D_i) -- UNCHANGED (every row, retracted or
#                       not, appeared with the appearance delay g_D),
#   (iii) retraction  : the block implemented here.
#
# For rows whose report delay AND retraction lag are both known exactly, block
# (iii) collapses to a Berkson-Gage mixture-CURE likelihood with cure fraction `p`,
#
#   R log(1 - p) + sum_c r_c log g_C(c) + sum_j u_j log[ p + (1 - p) Sbar_C(j) ],
#
# with sufficient statistics R (retracted rows), r_c (retracted rows whose lag was
# c >= 1) and u_j (STANDING rows whose report is j periods old).
#
# PARTIALLY OBSERVED ROWS.  Censoring coarsens the observable partition: instead of
# knowing which trajectory type a row is, we know only that it is one of a set.
# Poisson colouring still applies -- the count over a union of types is Poisson with
# the summed intensity -- so such a row contributes the log of a SUM.  With the
# appearance delay known only to lie in `[a_lo, a_hi]`:
#
#   standing                        log sum_{a in [a_lo, a_hi]} g_D(a) h(d* - a)
#   retracted at withdrawal delay b log(1-p) + log sum_{a <= min(a_hi, b-1)} g_D(a) g_C(b - a)
#   retracted by withdrawal delay B log(1-p) + log sum_{a <= min(a_hi, B-1)} g_D(a) G_C(B - a)
#
# where the "withdrawal delay" is measured from the EVENT (b = retraction - event),
# which is the natural coordinate when the report date is itself uncertain.  The
# three published cases -- censored report with an exact retraction, exact report
# with a censored retraction, and both censored -- are all instances of these two
# kernels, and each reduces to the exact-row term when the interval is a point.
# The exposure term is untouched: summed over ALL types it is mu_t G_D(d*)
# regardless of how the types are grouped.
#
# Two consequences worth stating, because they are what makes this branch much
# simpler than the Skellam one:
#   * the NB gamma frailty scales every trajectory-type mean by the same factor,
#     so it cancels out of the multinomial split of the rows -- blocks (ii) and
#     (iii) are frailty-free and the count block is the plain NB.  No quadrature.
#   * `p` is aliased with the epidemic intercept in block (i), so ALL information
#     about `p` comes from the cure block.  The overdispersion-knob pathology that
#     forces a strongly-concentrated prior in the count-cumulative path cannot
#     happen here, and a weak data-informed prior is enough.
#
# `p` may be shared across strata (the default) or estimated per stratum
# (`revision_process(stratified_p = TRUE)`); `g_C` is always shared.
# =============================================================================

#' Mask and drop retraction dates that the as-of view must not see
#'
#' Two rules, both of which are silent-bug traps if skipped:
#'   * a retraction dated **after** `now` has not happened yet at the analysis
#'     date, so the row is STANDING -- mask the date rather than dropping the row;
#'   * a retraction landing in the **same** event-unit period as its report has a
#'     lag of 0, meaning the case was never visible at any observation epoch.  The
#'     model (like the count-cumulative one) puts `g_C` on `{1, 2, ...}`, so those
#'     rows are dropped entirely.
#'
#' A row whose retraction date is an upper BOUND is masked on the same rule (the
#' bound has to be within the as-of view for the retraction to be known at all),
#' but the same-period test uses the *latest possible* report, so a bound that
#' cannot be distinguished from its report is dropped too.
#'
#' @param report_steps,retraction_steps Integer grid positions (`NA` allowed).
#' @param retraction_censored Logical: the retraction date is an upper bound.
#' @param now_step Integer grid position of `now`.
#' @returns `list(retraction_step = <integer, NA when standing>, keep = <logical>)`.
#' @keywords internal
#' @noRd
.mask_retractions <- function(report_steps, retraction_steps, retraction_censored,
                              now_step, lag_offset = 0L) {
  retraction_steps[!is.na(retraction_steps) & retraction_steps > now_step] <- NA

  # A resolution dated BEFORE its report is impossible in either mode.  A
  # SAME-PERIOD resolution is impossible only under retraction (`g_C` starts at 1,
  # and such a case was never visible in any data vintage); a confirmation may
  # perfectly well land in the period the case was reported, so `g_K` starts at 0.
  minimum_lag <- 1L - lag_offset
  impossible  <- !is.na(retraction_steps) &
    (retraction_steps - report_steps) < minimum_lag
  retraction_steps[impossible] <- NA
  retraction_censored[is.na(retraction_steps)] <- FALSE
  # Only the retraction mode drops the rows: under confirmation an "impossible"
  # row is a data error, not a structurally invisible case, so it is kept and
  # simply treated as unresolved.
  list(retraction_step = retraction_steps, retraction_censored = retraction_censored,
       keep = if (lag_offset == 1L) rep(TRUE, length(impossible)) else !impossible,
       dropped = if (lag_offset == 1L) 0L else sum(impossible))
}

#' Retraction sufficient statistics from an as-of linelist view
#'
#' Splits the rows into the exact ones -- whose contribution collapses into two
#' pooled tables per stratum -- and the partially observed ones, which are grouped
#' into distinct censoring *patterns* carrying a count.  Also returns the standing
#' counts the posterior predictive thins, again split into an exact table (indexed
#' by report age, so `rho` is a cheap lookup) and a censored one.
#'
#' Every row handed in must already have passed the as-of filter and the retraction
#' masking / same-period drop of `.mask_retractions()`.
#'
#' @param event_steps,report_steps Integer grid positions of each row's dates.
#' @param retraction_step Integer grid position of the retraction (`NA` = standing).
#' @param report_censored,retraction_censored Logical, per row.
#' @param cell_index 1-indexed stratum cell per row.
#' @param now_step Integer grid position of `now`.
#' @param max_time Length of the event-time grid.
#' @param num_strata Number of stratum cells.
#' @returns A list of sufficient statistics; see the field comments below.
#' @keywords internal
#' @noRd
.linelist_retraction_stats <- function(event_steps, report_steps, retraction_step,
                                       report_censored, retraction_censored,
                                       cell_index, now_step, max_time, num_strata,
                                       weights = NULL, lag_offset = 0L,
                                       resolution_positive = NULL) {
  event_steps  <- as.integer(event_steps)
  report_steps <- as.integer(report_steps)
  cell_index   <- as.integer(cell_index)
  is_retracted <- !is.na(retraction_step)
  # One row per case for a linelist; the case count for count-incidence data,
  # where a row stands for `n` identical cases.  Everything below is a weighted
  # tally, so the two data types share this code path exactly.
  if (is.null(weights)) weights <- rep(1.0, length(event_steps))
  weights <- as.numeric(weights)
  # Which sign a resolved row carries.  Modes 0 and 1 record a single sign, so the
  # whole resolved set is negative (retraction) or positive (confirmation); mode 2
  # records both and supplies the vector.
  if (is.null(resolution_positive))
    resolution_positive <- rep(lag_offset == 1L, length(event_steps))
  resolution_positive <- as.logical(resolution_positive) & is_retracted

  horizon      <- as.integer(now_step - event_steps)              # d*_t
  appear_upper <- as.integer(report_steps - event_steps)          # exact delay, or its bound
  appear_lower <- ifelse(report_censored, 0L, appear_upper)
  withdraw     <- as.integer(retraction_step - event_steps)       # b (exact) or B (bound)
  is_exact     <- !report_censored & (!is_retracted | !retraction_censored)

  # A small grouped-count helper: one row per distinct key combination.  The last
  # argument is the row mask, so the weights line up with the keys.
  tally <- function(..., mask) {
    keys <- lapply(list(...), function(key) key[mask])
    if (length(keys[[1]]) == 0L) return(NULL)
    grouped <- stats::aggregate(list(count = weights[mask]), by = keys, FUN = sum)
    as.matrix(grouped)
  }

  # --- exact rows: the two pooled cure tables, per stratum ---------------------
  exact_retracted <- is_exact & is_retracted
  exact_standing  <- is_exact & !is_retracted
  retract_table <- tally(stratum = cell_index, lag = withdraw - appear_upper,
                         mask = exact_retracted)
  standing_table <- tally(stratum = cell_index, age = horizon - appear_upper,
                          mask = exact_standing)
  # `rowsum()` on an empty selection returns a ZERO-ROW matrix, not a vector of
  # zeros, so an all-FALSE mask would silently yield numeric(0) and index to NA
  # downstream.  Retraction-only mode has no positives at all, so that case is the
  # norm rather than an edge case.
  by_stratum <- function(mask) {
    totals <- numeric(num_strata)
    if (!any(mask)) return(totals)
    grouped <- rowsum(weights[mask], factor(cell_index[mask], levels = seq_len(num_strata)))
    as.numeric(grouped[, 1])
  }
  n_retracted_by_stratum <- by_stratum(exact_retracted)
  n_positive_by_stratum  <- by_stratum(exact_retracted &  resolution_positive)
  n_negative_by_stratum  <- by_stratum(exact_retracted & !resolution_positive)

  # --- censored rows: one pattern per distinct (stratum, bounds, horizon) ------
  # `retracted` distinguishes the two kernels; `withdraw_censored` picks G_C over
  # g_C inside the retracted one.  Standing patterns carry `withdraw = 0`, which
  # the kernel ignores.
  censored <- !is_exact
  censored_patterns <- tally(
    stratum           = cell_index,
    retracted         = as.integer(is_retracted),
    appear_lower      = appear_lower,
    appear_upper      = appear_upper,
    withdraw          = ifelse(is.na(withdraw), 0L, withdraw),
    withdraw_censored = as.integer(retraction_censored & is_retracted),
    positive          = as.integer(resolution_positive),
    horizon           = horizon,
    mask              = censored)

  # --- standing counts for the predictive -------------------------------------
  # `cell` is the column-major index into the [max_time x num_strata] matrix, so the
  # draws aggregate with rowsum().  Exact standing rows are keyed by report age (a
  # cheap `rho` lookup); censored ones keep their bounds, because their genuine
  # probability has to average `rho` over the possible appearance delays.
  cell <- (cell_index - 1L) * max_time + event_steps + 1L
  standing_rows <- tally(cell = cell, stratum = cell_index,
                         age = horizon - appear_upper, mask = exact_standing)
  censored_standing <- censored & !is_retracted
  standing_censored_rows <- tally(
    cell = cell, stratum = cell_index, appear_lower = appear_lower,
    appear_upper = appear_upper, horizon = horizon, mask = censored_standing)

  # --- resolved counts, for the CONFIRMATION predictive ------------------------
  # A confirmed case is already in the target and enters the nowcast with weight 1;
  # a retracted one is gone and enters with weight 0.  The matrix is built either
  # way so the mode can be decided downstream.
  accumulate_by_cell <- function(cells, values) {
    out <- matrix(0.0, max_time, num_strata)
    if (!length(cells)) return(out)
    by_cell <- rowsum(values, cells, reorder = FALSE)
    index   <- as.integer(rownames(by_cell))
    out[index] <- as.numeric(by_cell)
    out
  }
  # Only a POSITIVE resolution is already part of the target: a confirmed case is
  # in, a retracted one is out.  Under retraction-only mode nothing is positive, so
  # this is the zero matrix and the predictive falls back to the thinning alone.
  resolved_counts <- accumulate_by_cell(cell[resolution_positive],
                                        weights[resolution_positive])

  standing_counts <- matrix(0.0, max_time, num_strata)
  for (table_rows in list(standing_rows, standing_censored_rows)) {
    if (is.null(table_rows)) next
    # A cell appears once per age/pattern, so the counts must be ACCUMULATED
    # (`x[i] <- x[i] + v` would silently keep only the last of each repeated index).
    by_cell <- rowsum(table_rows[, "count"], table_rows[, "cell"], reorder = FALSE)
    cells   <- as.integer(rownames(by_cell))
    standing_counts[cells] <- standing_counts[cells] + as.numeric(by_cell)
  }

  list(retract_table          = retract_table,
       standing_table         = standing_table,
       censored_patterns      = censored_patterns,
       n_retracted_by_stratum = n_retracted_by_stratum,
       n_positive_by_stratum  = n_positive_by_stratum,
       n_negative_by_stratum  = n_negative_by_stratum,
       n_positive             = sum(weights[resolution_positive]),
       n_negative             = sum(weights[is_retracted & !resolution_positive]),
       n_retracted            = sum(weights[is_retracted]),
       n_standing             = sum(weights[!is_retracted]),
       n_censored             = sum(weights[censored]),
       resolved_counts        = resolved_counts,
       standing_rows          = standing_rows,
       standing_censored_rows = standing_censored_rows,
       standing_counts        = standing_counts,
       max_report_age         = if (length(horizon)) max(horizon) else 0L,
       max_grid               = max(c(0L, horizon, withdraw[is_retracted])))
}

# =============================================================================
# Objective-side evaluation
# =============================================================================

#' Discretised pmf / cdf grids for a delay, indexed from delay 0
#'
#' Returns `cdf[k + 1] = F(k)` for `k = 0 .. grid_max + 1` and
#' `pmf[k + 1] = F(k + 1) - F(k)` for `k = 0 .. grid_max`, with `F(0) := 0` set
#' explicitly.  The CDF is never evaluated at 0: for a lognormal that would put a
#' `log(0)` on the AD tape, whose derivative is `NaN` even though its value is fine.
#'
#' @param cdf_fn A CDF closure (AD in the delay parameters).
#' @param grid_max Largest delay needed.
#' @returns `list(cdf = , pmf = )`, both AD vectors.
#' @keywords internal
#' @noRd
.delay_grid <- function(cdf_fn, grid_max) {
  cdf_positive <- cdf_fn(seq_len(grid_max + 1L))
  # Lead with an ADVECTOR zero: a plain-numeric first argument makes c() dispatch
  # to base and strip the advector class.
  cdf_from_zero <- c(cdf_positive[1] * 0, cdf_positive)
  list(cdf = cdf_from_zero,
       pmf = cdf_from_zero[-1] - cdf_from_zero[-length(cdf_from_zero)])
}

#' Discretised pmf / cdf grids for the RESOLUTION LAG, indexed from lag 0
#'
#' The delays in this model are discretised on DIFFERENT conventions, and mixing
#' them is an easy off-by-one:
#'
#'   appearance delay   g_D(a) = F(a + 1) - F(a)    (delay `a` falls in bin `a + 1`)
#'   confirmation lag   g_K(c) = F(c + 1) - F(c)    (lag `c >= 0` -- a case may be
#'                                                   confirmed the day it is reported)
#'   retraction lag     g_C(c) = F(c) - F(c - 1)    (lag `c >= 1`, with F(0) := 0 --
#'                                                   a retraction lands strictly
#'                                                   after the report it withdraws)
#'
#' The retraction convention is what `.discretised_delay_loglik()` evaluates for
#' the exactly observed lags, so the censored kernels have to match it or the two
#' paths disagree.  The two are one `lag_offset` apart, which is the only place
#' the confirmation and retraction modes differ in the lag algebra.
#'
#' @param cdf_fn A CDF closure for the lag (AD in the resolution parameters).
#' @param grid_max Largest lag needed.
#' @param lag_offset `1L` for a confirmation lag (support from 0), `0L` for a
#'   retraction lag (support from 1).
#' @returns `list(cdf = , pmf = )` of length `grid_max + 1`, with
#'   `cdf[j + 1] = P(R <= j)` and `pmf[c + 1] = P(R = c)`.
#' @keywords internal
#' @noRd
.resolution_lag_grid <- function(cdf_fn, grid_max, lag_offset = 0L) {
  cdf_positive <- cdf_fn(seq_len(grid_max + 1L))
  # Lead with an ADVECTOR zero: a plain-numeric first argument makes c() dispatch
  # to base and strip the advector class.  `cdf_from_zero[k + 1] = F(k)`.
  cdf_from_zero <- c(cdf_positive[1] * 0, cdf_positive)
  upper <- cdf_from_zero[-1]
  lower <- cdf_from_zero[-length(cdf_from_zero)]
  if (lag_offset == 1L) {
    list(cdf = upper, pmf = upper - lower)                    # P(R <= j) = F(j + 1)
  } else {
    list(cdf = lower,                                          # P(R <= j) = F(j)
         pmf = c(lower[1] * 0, (upper - lower)[-length(upper)]))
  }
}

#' Plain-English name for what the resolution model is doing
#'
#' `resolution_mode` is an integer internally; nobody reading a `print()` or a
#' `parameters()` should have to know that.  This is the phrase shown instead.
#' @param resolution_mode 0, 1 or 2.
#' @param is_active Whether the resolution block is switched on at all.
#' @returns A single string.
#' @keywords internal
#' @noRd
.resolution_label <- function(resolution_mode, is_active = TRUE) {
  if (!isTRUE(is_active == 1L) && !isTRUE(is_active)) return("none")
  switch(as.character(as.integer(resolution_mode)),
         "0" = "retractions",
         "1" = "confirmations",
         "2" = "confirmations and retractions",
         "resolutions")
}

#' Probability that a report belongs to a class whose resolution is OBSERVED
#'
#' The single switch between the three resolution modes:
#'   0 retraction only    -- the recorded dates are the negatives, so `pi = 1 - p`
#'   1 confirmation only  -- the recorded dates are the positives, so `pi = p`
#'   2 both recorded      -- every resolution is seen, so `pi = 1`
#' Mode 2 makes the unresolved weight `h(j) = Sbar_R(j)`, free of `p`: `p` is then
#' identified purely by the +/- split of the resolved rows.
#' @keywords internal
#' @noRd
.resolved_probability <- function(confirm_p, resolution_mode) {
  if (resolution_mode == 2L) 1 + 0 * confirm_p
  else if (resolution_mode == 1L) confirm_p
  else 1 - confirm_p
}

#' Retraction-lag grid (the `lag_offset = 0` case), kept for readability.
#' @keywords internal
#' @noRd
.retraction_lag_grid <- function(cdf_fn, grid_max) {
  .resolution_lag_grid(cdf_fn, grid_max, lag_offset = 0L)
}

#' Retraction (cure) log-likelihood for one stratum, AD-safe
#'
#' Evaluates `R log(1 - p) + sum_c r_c log g_C(c) + sum_j u_j log h(j)` over the
#' exact rows of a single stratum, with `h(j) = p + (1 - p) * Sbar_C(j)`.  Rows
#' whose report age is `0` are dropped by the caller: `h(0) = 1` contributes
#' exactly zero, and a delay CDF evaluated at `0` would put a `log(0)` on the tape.
#'
#' @param retract_fns Delay closures for `g_C` -- either the parametric bundle from
#'   `.delay_distribution_functions()` (`log_cdf` / `log_survival`) or the
#'   non-parametric bundle from `.nonparametric_delay_functions()`
#'   (`log_pmf_raw` / `survival`).
#' @param is_nonparametric `1L` when `retract_fns` is the Dirichlet bundle.
#' @param retract_lags,retract_lag_counts The `r_c` table (lags >= 1).
#' @param standing_ages,standing_age_counts The `u_j` table (ages >= 1).
#' @param n_retracted The scalar `R`.
#' @param confirm_p The confirmation probability `p` for this stratum (AD).
#' @param split_lag Lower-tail / survival-tail split for the parametric pmf.
#' @keywords internal
#' @noRd
.loglik_retraction <- function(retract_fns, is_nonparametric,
                               retract_lags, retract_lag_counts,
                               standing_ages, standing_age_counts,
                               n_retracted, confirm_p, split_lag,
                               lag_offset = 0L, resolution_mode = 0L,
                               n_resolved_positive = numeric(0),
                               n_resolved_negative = numeric(0)) {
  # `pi` is the probability a report belongs to a class whose resolution is
  # OBSERVED: the retracted ones (1 - p) when only retractions are recorded, the
  # confirmed ones (p) when only confirmations are, and ALL of them (1) when both
  # are.  Everything else in this block is mode-independent.
  resolved_probability <- .resolved_probability(confirm_p, resolution_mode)

  # `n_retracted` splits into the two signs.  Modes 0 and 1 put the whole count on
  # one side and leave the other at zero, so this single expression covers all
  # three.  Written as branches rather than `n * log(.)` so that an empty side is 0
  # rather than `0 * -Inf = NaN` at the boundary (p = 1 under retraction).
  n_positive <- if (length(n_resolved_positive)) n_resolved_positive else 0
  n_negative <- if (length(n_resolved_negative)) n_resolved_negative else n_retracted
  loglik <- 0 * confirm_p
  if (n_positive > 0) loglik <- loglik + n_positive * log(confirm_p)
  if (n_negative > 0) loglik <- loglik + n_negative * log1p(-confirm_p)

  # Shifting the lag by `lag_offset` turns the retraction convention
  # `F(c) - F(c-1)` into the confirmation one `F(c+1) - F(c)`; see
  # `.resolution_lag_grid()`.
  if (length(retract_lags) > 0) {
    shifted_lags <- retract_lags + lag_offset
    loglik <- loglik + if (is_nonparametric == 1L) {
      sum(retract_lag_counts * retract_fns$log_pmf_raw(shifted_lags))
    } else {
      .discretised_delay_loglik(shifted_lags, retract_lag_counts, split_lag + lag_offset,
                                retract_fns$log_cdf, retract_fns$log_survival)
    }
  }

  if (length(standing_ages) > 0) {
    shifted_ages <- standing_ages + lag_offset
    survival_at_age <- if (is_nonparametric == 1L) retract_fns$survival(shifted_ages)
                       else exp(retract_fns$log_survival(shifted_ages))
    loglik <- loglik +
      sum(standing_age_counts *
            log((1 - resolved_probability) + resolved_probability * survival_at_age))
    # Mode 2 note: pi = 1 collapses that to log Sbar_R(j) -- an unresolved row says
    # only that its resolution has not landed, and nothing about which way it will
    # go, because the lag law is shared between the two signs.
  }

  loglik
}

#' Log-likelihood of the partially observed (censored) rows, AD-safe
#'
#' One term per censoring pattern, each the log of a sum over the appearance delays
#' the row is compatible with.  See the file header for the two kernels.  All loop
#' bounds come from the pattern table, which is data, so the tape length is fixed.
#'
#' @param patterns Matrix of censoring patterns (columns `retracted`,
#'   `appear_lower`, `appear_upper`, `withdraw`, `withdraw_censored`, `horizon`,
#'   `count`), already restricted to one stratum.
#' @param appearance_pmf `g_D(a)` for `a = 0, 1, ...` (AD).
#' @param retract_pmf,retract_cdf `g_C(c)` and `G_C(c)` for `c = 0, 1, ...` (AD).
#' @param confirm_p The confirmation probability for this stratum (AD).
#' @keywords internal
#' @noRd
.loglik_retraction_censored <- function(patterns, appearance_pmf,
                                        retract_pmf, retract_cdf, confirm_p,
                                        lag_offset = 0L, resolution_mode = 0L) {
  if (is.null(patterns) || nrow(patterns) == 0L) return(0 * confirm_p)
  # See `.loglik_retraction()`: `pi` is the probability of a class whose
  # resolution is observed, and h(j) = (1 - pi) + pi * Sbar_R(j).
  resolved_probability <- .resolved_probability(confirm_p, resolution_mode)
  unresolved_weight <- (1 - resolved_probability) +
    resolved_probability * (1 - retract_cdf)

  total <- 0 * confirm_p
  for (pattern_index in seq_len(nrow(patterns))) {
    lower     <- as.integer(patterns[pattern_index, "appear_lower"])
    upper     <- as.integer(patterns[pattern_index, "appear_upper"])
    horizon   <- as.integer(patterns[pattern_index, "horizon"])
    count     <- patterns[pattern_index, "count"]
    resolved  <- as.integer(patterns[pattern_index, "retracted"]) == 1L

    if (!resolved) {
      # An appearance later than the horizon is not observable at all.
      upper <- min(upper, horizon)
      if (upper < lower) next
      appear <- lower:upper
      term <- sum(appearance_pmf[appear + 1L] * unresolved_weight[horizon - appear + 1L])
    } else {
      withdraw <- as.integer(patterns[pattern_index, "withdraw"])
      # A retraction lands strictly after the report it withdraws (a <= b - 1); a
      # confirmation may land in the same period (a <= b).
      upper <- min(upper, withdraw - 1L + lag_offset)
      if (upper < lower) next
      appear <- lower:upper
      lag_weight <- if (as.integer(patterns[pattern_index, "withdraw_censored"]) == 1L)
        retract_cdf[withdraw - appear + 1L]      # resolved BY B: G_R(B - a)
      else
        retract_pmf[withdraw - appear + 1L]      # resolved AT b: g_R(b - a)
      # With both signs recorded, a resolved row's class weight is its OWN sign's
      # probability, not the pooled pi (which is 1 in that mode).
      sign_weight <- if (resolution_mode == 2L) {
        if (as.integer(patterns[pattern_index, "positive"]) == 1L) confirm_p else 1 - confirm_p
      } else resolved_probability
      term <- sign_weight * sum(appearance_pmf[appear + 1L] * lag_weight)
    }
    total <- total + count * log(term + 1e-300)
  }
  total
}

# =============================================================================
# Prediction-side helpers
# =============================================================================

#' Probability that a standing row of age `j` is genuine
#'
#' `rho(j) = p / h(j) = p / (p + (1 - p) Sbar_C(j))` -- the posterior probability
#' that a report which has stood unretracted for `j` periods will never be
#' retracted.  `rho(0) = p` (a brand-new report carries no evidence) and
#' `rho(j) -> 1` as the report matures.  Plain numeric (prediction-side only).
#'
#' @param ages Integer report ages `0, 1, ..., max_age`.
#' @param confirm_p The fitted confirmation probability.
#' @param survival_fn Function returning `Sbar_C(j)` for `j >= 1`.
#' @returns Numeric vector of the same length as `ages`.
#' @keywords internal
#' @noRd
.retraction_genuine_probability <- function(ages, confirm_p, survival_fn,
                                            lag_offset = 0L, resolution_mode = 0L) {
  shifted_ages <- ages + lag_offset
  survival_at_age <- rep(1.0, length(ages))     # Sbar_R(0) = 1 when the lag starts at 1
  evaluable <- shifted_ages >= 1
  if (any(evaluable))
    survival_at_age[evaluable] <- as.numeric(survival_fn(shifted_ages[evaluable]))
  survival_at_age <- pmin(pmax(survival_at_age, 0), 1)

  if (resolution_mode == 2L) {
    # BOTH SIGNS RECORDED: an unresolved row will resolve one way or the other, and
    # because the lag law is shared its age says nothing about which -- so
    # rho(j) = p exactly, flat in the age under the prototype's shared lag law.
    rep(confirm_p, length(ages))
  } else if (lag_offset == 1L) {
    # CONFIRMATION: an unresolved row is one not yet confirmed.  It still counts
    # toward the target if it is destined to be confirmed later --
    #   rho(j) = p Sbar_K(j) / [(1 - p) + p Sbar_K(j)]
    # -- which DECREASES with age: the longer a report sits unconfirmed, the more
    # likely it never will be.  (Resolved rows count with probability 1; they are
    # handled separately, since they are already confirmed.)
    numerator <- confirm_p * survival_at_age
    numerator / (numerator + (1 - confirm_p))
  } else {
    # RETRACTION: an unresolved row is one not yet retracted, and it counts unless
    # a retraction is still coming --
    #   rho(j) = p / [p + (1 - p) Sbar_C(j)]
    # -- which INCREASES with age.  (Resolved rows count 0: they are gone.)
    confirm_p / (confirm_p + (1 - confirm_p) * survival_at_age)
  }
}

#' Genuine probability for a standing row whose appearance delay is censored
#'
#' With the appearance delay known only to lie in `[a_lo, a_hi]`, the report age is
#' uncertain too, so `rho` has to be averaged over the possible delays weighted by
#' how likely each is to have produced a standing row:
#'
#'   rho = p * sum_a g_D(a)  /  sum_a g_D(a) h(d* - a).
#'
#' Reduces to `rho(d* - a)` when the interval is a single point.
#'
#' @param patterns Matrix with columns `appear_lower`, `appear_upper`, `horizon`.
#' @param confirm_p The fitted confirmation probability for the row's stratum.
#' @param appearance_pmf Numeric `g_D(a)` for `a = 0, 1, ...`.
#' @param retract_cdf Numeric `G_C(c)` for `c = 0, 1, ...`.
#' @returns Numeric vector, one entry per pattern row.
#' @keywords internal
#' @noRd
.retraction_genuine_probability_censored <- function(patterns, confirm_p,
                                                     appearance_pmf, retract_cdf,
                                                     lag_offset = 0L,
                                                     resolution_mode = 0L) {
  if (is.null(patterns) || nrow(patterns) == 0L) return(numeric(0))
  if (resolution_mode == 2L) return(rep(confirm_p, nrow(patterns)))
  resolved_probability <- .resolved_probability(confirm_p, resolution_mode)
  survival          <- 1 - retract_cdf
  unresolved_weight <- (1 - resolved_probability) + resolved_probability * survival
  # Numerator: the share of the unresolved mass that still counts toward the
  # target.  Under retraction that is the genuine reports (weight p, independent of
  # the age); under confirmation it is the not-yet-confirmed ones still to come
  # (weight p * Sbar_K(j), which decays with the age).
  target_weight <- if (lag_offset == 1L) confirm_p * survival
                   else rep(confirm_p, length(survival))

  vapply(seq_len(nrow(patterns)), function(pattern_index) {
    lower   <- as.integer(patterns[pattern_index, "appear_lower"])
    horizon <- as.integer(patterns[pattern_index, "horizon"])
    upper   <- min(as.integer(patterns[pattern_index, "appear_upper"]), horizon)
    if (upper < lower) return(confirm_p)
    appear      <- lower:upper
    appear_mass <- appearance_pmf[appear + 1L]
    lag_index   <- horizon - appear + 1L
    denominator <- sum(appear_mass * unresolved_weight[lag_index])
    if (!is.finite(denominator) || denominator <= 0) return(confirm_p)
    min(max(sum(appear_mass * target_weight[lag_index]) / denominator, 0), 1)
  }, numeric(1))
}

#' Crude but censoring-aware estimate of the confirmation probability
#'
#' The naive rate `R / n` **under-estimates** `1 - p`, because recent reports have
#' not had time to be retracted.  This uses only rows with enough follow-up -- an
#' age at or beyond the 90th percentile of the observed retraction lags, where
#' almost every retraction that will happen already has -- and falls back to the
#' naive rate when too few such rows exist.  Used to centre the (weak) default
#' Beta prior and to seed the optimiser, never as an estimator in its own right.
#'
#' @param retract_lags,retract_lag_counts The `r_c` table.
#' @param standing_ages,standing_age_counts The `u_j` table (ages >= 0).
#' @returns A confirmation probability in `[0.5, 0.999]`.
#' @keywords internal
#' @noRd
.empirical_confirmation_probability <- function(retract_lags, retract_lag_counts,
                                                standing_ages, standing_age_counts) {
  total_retracted <- sum(retract_lag_counts)
  total_standing  <- sum(standing_age_counts)
  total_rows      <- total_retracted + total_standing
  if (total_rows <= 0) return(0.98)

  naive_rate <- total_retracted / total_rows
  mature_rate <- NA_real_
  if (total_retracted > 0) {
    # 90th percentile of the observed lag distribution: beyond it, a transient
    # report has almost surely already been caught.
    lag_order      <- order(retract_lags)
    cumulative     <- cumsum(retract_lag_counts[lag_order]) / total_retracted
    follow_up_need <- retract_lags[lag_order][which(cumulative >= 0.9)[1]]
    mature_standing  <- sum(standing_age_counts[standing_ages >= follow_up_need])
    mature_retracted <- sum(retract_lag_counts[retract_lags <= follow_up_need])
    if (mature_standing + mature_retracted >= 30)
      mature_rate <- mature_retracted / (mature_standing + mature_retracted)
  }

  retraction_rate <- if (is.finite(mature_rate)) mature_rate else naive_rate
  max(0.5, min(1 - retraction_rate, 0.999))
}

#' Split a stratum-keyed statistics matrix into a per-stratum list of row indices
#'
#' The pooled tables carry a `stratum` column; the objective slices them with plain
#' integer indices (data, never AD), so the split is done once when the tape is
#' built rather than inside it.
#' @keywords internal
#' @noRd
.split_rows_by_stratum <- function(table_rows, num_strata) {
  if (is.null(table_rows) || nrow(table_rows) == 0L)
    return(replicate(num_strata, integer(0), simplify = FALSE))
  strata <- as.integer(table_rows[, "stratum"])
  lapply(seq_len(num_strata), function(stratum) which(strata == stratum))
}
