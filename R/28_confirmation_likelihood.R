# =============================================================================
# Confirmation / retraction observation likelihood (count-cumulative streams)
# =============================================================================
# When a surveillance stream is reported as a *cumulative* count that revises
# both up (late reports) and down (retractions / reclassifications), the weekly
# increments m_t^d = C_t(d) - C_t(d-1) are SIGNED integers.  The censored signed
# marked point process (see CENSORED_POINT_PROCESS_MATHEMATICS.md and
# CENSORED_POINT_PROCESS_NEGBIN.md) gives the exact marginal of each increment:
#
#   m_t^d ~ Skellam(alpha_d, beta_d)                       (Poisson streams)
#   m_t^d ~ SkNB(alpha_d, beta_d, r)                       (NB / gamma-frailty)
#
# with alpha_d = mu_t * g_D(d)   (gross additions at delay d)
#      beta_d  = eta_t * g_W(d)   (retractions at delay d, g_W = g_D * g_C)
#      mu_t = lambda_t / p, eta_t = (1 - p) * lambda_t,  p = P(report is genuine).
#
# These helpers evaluate the log-likelihood of one increment, AD-safe under RTMB,
# for use inside the joint objective.  They are the count-cumulative analogue of
# the truncated Poisson / negative-binomial count likelihood.
# =============================================================================

# --- numeric constants -------------------------------------------------------

# Gamma-frailty quadrature nodes for the SkNB likelihood.  The NB increment law
# is a gamma mixture of Skellams: SkNB(a, b, r) = E_u[Skellam(a*u, b*u)] with
# u ~ Gamma(r, r).  With r floored (see the confirmation prior) the mixing density
# concentrates well inside [0, ~4], so a 24-point Gauss-Legendre rule on
# [1e-3, 4.5] integrates it to ~1e-3 -- matching the exact convolution while
# keeping the RTMB tape linear.  Computed once at load.
.confirmation_gl_nodes <- local({
  lower_bound <- 1e-3
  upper_bound <- 4.5
  gauss_rule  <- statmod::gauss.quad(24L, kind = "legendre")     # nodes on [-1, 1]
  nodes   <- (upper_bound - lower_bound) / 2 * gauss_rule$nodes +
             (upper_bound + lower_bound) / 2
  log_weights <- log(gauss_rule$weights) + log((upper_bound - lower_bound) / 2)
  list(nodes = nodes, log_weights = log_weights, u_max = upper_bound)
})

#' Numerically stable log(exp(log_left) + exp(log_right)), AD-safe.
#'
#' The maximum is taken via a smooth `sqrt` rather than `pmax()` so the derivative
#' is defined even when the two arguments are equal (a plain `abs()` kinks at ties
#' and yields NaN gradients under RTMB when two log-terms coincide).
#' @keywords internal
#' @noRd
.logspace_add <- function(log_left, log_right) {
  smooth_max <- (log_left + log_right + sqrt((log_left - log_right)^2 + 1e-12)) / 2
  smooth_max + log(exp(log_left - smooth_max) + exp(log_right - smooth_max))
}

#' Fold `.logspace_add()` across a vector: log(sum(exp(log_values))), AD-safe.
#' @keywords internal
#' @noRd
.logsumexp <- function(log_values) {
  accumulated <- log_values[1]
  for (value_index in seq_along(log_values)[-1]) {
    accumulated <- .logspace_add(accumulated, log_values[value_index])
  }
  accumulated
}

#' Discrete convolution g_W = g_D * g_C on the delay grid (fixed length, AD-safe).
#'
#' `g_D` is the appearance-delay pmf on delays `0, 1, ..., Dmax`; `g_C` is the
#' retraction-delay pmf on the same grid with `g_C[1] = g_C(0) = 0` (a retraction
#' always lands strictly after its report).  Returns the withdrawal-delay pmf
#' `g_W(w) = sum_a g_D(a) g_C(w - a)` indexed so that entry `d + 1` holds `g_W(d)`.
#' @keywords internal
#' @noRd
.convolve_delays <- function(g_D, g_C) {
  "[<-" <- RTMB::ADoverload("[<-")          # in-place AD writes (falls back to base for numeric)
  n_appearance <- length(g_D)
  n_retraction <- length(g_C)
  # g_W(w) = sum_a g_D(a) g_C(w - a).  Scalar indexing + in-place accumulation:
  # `g_W[1] * 0` seeds an advector (or numeric) vector of zeros so the writes keep
  # the advector class without depending on the caller's environment.
  g_W <- rep(g_D[1] * 0, n_appearance + n_retraction - 1L)
  for (appearance_index in seq_len(n_appearance)) {
    for (retraction_index in seq_len(n_retraction)) {
      out_index <- appearance_index + retraction_index - 1L
      g_W[out_index] <- g_W[out_index] + g_D[appearance_index] * g_C[retraction_index]
    }
  }
  g_W
}

#' Log modified Bessel function I_nu(z) via its ascending series, in log-space.
#'
#' `log I_nu(z) = log sum_k (z/2)^(2k+nu) / (k! (k+nu)!)`, summed in log-space so it
#' never underflows.  We use this (never R's `besselI()`) throughout the Skellam
#' increment density: for tiny `z` (the usual confirmation regime, where retraction
#' intensity `beta` is small) `besselI()` underflows to exactly 0 and `log(0)` both
#' returns `-Inf` and poisons the AD gradient with `NaN`, whereas the series stays
#' finite and differentiable.  The dominant term sits near `k* ~ (z/2)^2 / nu`, so
#' the caller sizes `n_terms` from the order (data, hence a fixed tape length) to
#' cover the peak with a comfortable margin.  AD-safe.
#' @keywords internal
#' @noRd
.log_bessel_i_series <- function(z, order, n_terms) {
  term_index <- 0:n_terms
  .logsumexp((2 * term_index + order) * log(z / 2) -
               lgamma(term_index + 1) - lgamma(term_index + order + 1))
}

#' Number of ascending-series terms needed for `.log_bessel_i_series()`.
#'
#' Derived from the order `nu = |increment|` alone (data, never a parameter), so the
#' series loop length is fixed when the tape is built.  The ascending series peaks
#' near `k* ~ (z/2)^2 / nu`; in the confirmation regime `z` stays small (`beta`
#' small), so `0.3 * nu + 30` terms cover the peak with wide margin even up to
#' `z ~ nu`.
#' @keywords internal
#' @noRd
.bessel_series_terms <- function(order) {
  max(60L, as.integer(ceiling(0.3 * order)) + 30L)
}

#' Log-pmf of a single signed increment under the Skellam law.
#'
#' `bin_type` is decided OUTSIDE the RTMB tape from the integer delay index, so no
#' comparison ever touches an AD value:
#'   0 = pure addition    (delay 0: `beta` is structurally 0)     -> Poisson(alpha)
#'   2 = pure retraction   (delay beyond the appearance support)   -> -Poisson(beta)
#'   1 = mixed             (both streams active)                   -> Bessel form
#' The mixed case is the exact Skellam log-pmf
#' `-(alpha+beta) + (m/2) log(alpha/beta) + log I_|m|(2 sqrt(alpha beta))`, with the
#' Bessel factor evaluated by the log-space ascending series (never R's `besselI()`,
#' which underflows to 0 for the small arguments typical here and breaks the AD
#' gradient).  Tiny floors on `alpha`, `beta` keep `sqrt`/`log`/ratio finite -- and,
#' crucially, floor the Poisson mean for the pure-addition/retraction cases so an
#' observed increment in a delay cell the delay pmf has driven to ~0 is a large
#' *finite* penalty (which pushes the optimizer to widen the delay) rather than a
#' fatal `-Inf`.
#' @keywords internal
#' @noRd
.log_skellam_increment <- function(increment, alpha, beta, bin_type) {
  if (bin_type == 0L) return(dpois(increment, alpha + 1e-8, log = TRUE))
  if (bin_type == 2L) return(dpois(-increment, beta + 1e-8, log = TRUE))
  alpha <- alpha + 1e-8
  beta  <- beta  + 1e-8
  bessel_argument <- 2 * sqrt(alpha * beta)
  log_bessel <- .log_bessel_i_series(bessel_argument, abs(increment),
                                     .bessel_series_terms(abs(increment)))
  -(alpha + beta) + (increment / 2) * log(alpha / beta) + log_bessel
}

#' Log-likelihood of a full increment path under the Poisson-Skellam model.
#'
#' Sums the per-delay Skellam log-pmf over the observed horizon.  `increments`,
#' `alpha`, `beta`, `bin_type` are aligned vectors over delays `0 .. d_star`.
#' @keywords internal
#' @noRd
.loglik_skellam_path <- function(increments, alpha, beta, bin_type) {
  total <- 0
  for (delay_index in seq_along(increments)) {
    total <- total + .log_skellam_increment(increments[delay_index], alpha[delay_index],
                                             beta[delay_index], bin_type[delay_index])
  }
  total
}

#' Log-likelihood of a full increment path under the NB (gamma-frailty) SkNB model.
#'
#' Evaluates L = int prod_d Skellam(m_d; alpha_d u, beta_d u) Gamma(u; r, r) du by
#' fixed Gauss-Legendre quadrature over the shared frailty `u` -- the increments of
#' one origin share a single gamma frailty, so the quadrature is *outside* the
#' product over delays.  Equivalent to the exact convolution but with a linear
#' tape.
#' @keywords internal
#' @noRd
.loglik_sknb_path <- function(increments, alpha, beta, bin_type, nb_size) {
  nodes       <- .confirmation_gl_nodes$nodes
  log_weights <- .confirmation_gl_nodes$log_weights
  accumulated <- NULL
  for (node_index in seq_along(nodes)) {
    frailty <- nodes[node_index]
    node_loglik <- dgamma(frailty, nb_size, nb_size, log = TRUE) + log_weights[node_index]
    for (delay_index in seq_along(increments)) {
      node_loglik <- node_loglik +
        .log_skellam_increment(increments[delay_index],
                               alpha[delay_index] * frailty,
                               beta[delay_index]  * frailty,
                               bin_type[delay_index])
    }
    accumulated <- if (is.null(accumulated)) node_loglik else .logspace_add(accumulated, node_loglik)
  }
  accumulated
}

# =============================================================================
# Count-cumulative -> signed-increment de-accumulation
# =============================================================================

#' De-accumulate a count-cumulative as-of view into signed delay increments.
#'
#' tbl.now does not implement `to_count(count-cumulative -> count-incidence)`
#' (de-accumulating a cumulative total can go negative, which the ordinary count
#' model cannot represent).  This helper does it for the confirmation model: for
#' each (event, stratum) it orders the cumulative observations by report delay,
#' keeps the latest value at each integer delay, and differences them into signed
#' weekly increments `m_t^d = C_t(d) - C_t(d - 1)`.  Returns a long data frame with
#' the SAME shape `to_count("count-incidence")` would produce -- columns
#' `<event_col>`, `.delay`, `n` (signed), plus any strata columns -- so the rest of
#' `prepare_from_tbl_now()` is unchanged.
#'
#' @param as_of A `tbl_now` filtered to the as-of view (events and reports <= now).
#' @param event_col,report_col,event_unit,min_event Date-grid metadata from the
#'   caller.
#' @param strata_cols Character vector of stratum column names (possibly empty).
#' @returns A data frame of signed increments per (event, delay, stratum).
#' @keywords internal
#' @noRd
.deaccumulate_to_increments <- function(as_of, event_col, report_col,
                                        event_unit, min_event, strata_cols) {
  observations   <- as.data.frame(as_of)
  case_count_col <- tbl.now::get_case_count(as_of) %||% "n"
  if (!case_count_col %in% names(observations)) {
    cli::cli_abort("Count-cumulative data must carry a case-count column.")
  }

  # Integer reporting delay in event units (report grid position minus event grid
  # position); drop the (structurally impossible) negative delays.
  observations <- observations |>
    dplyr::mutate(
      .event_step  = .unit_steps(min_event, !!as.symbol(event_col),  event_unit),
      .report_step = .unit_steps(min_event, !!as.symbol(report_col), event_unit),
      .delay       = as.integer(round(.data$.report_step - .data$.event_step))
    ) |>
    dplyr::filter(.data$.delay >= 0L)

  # One group per (event-time, stratum): each holds that origin's cumulative curve.
  grouping_cols      <- c(event_col, strata_cols)
  group_keys         <- interaction(observations[grouping_cols], drop = TRUE, lex.order = TRUE)
  observation_groups <- split(observations, group_keys)

  # De-accumulate each origin's cumulative curve into signed increments m_t^d.  This
  # is a genuinely multi-step per-group operation (order the reports, keep the latest
  # value at each delay, then difference), so an explicit loop reads more clearly than
  # a dense grouped pipeline.
  increment_frames <- vector("list", length(observation_groups))
  for (group_index in seq_along(observation_groups)) {
    group_observations <- observation_groups[[group_index]]
    if (nrow(group_observations) == 0L) next

    # Keep the latest reported cumulative value at each integer delay, in delay order.
    group_observations <- group_observations[
      order(group_observations$.delay, group_observations$.report_step), , drop = FALSE
    ]
    latest_per_delay <- group_observations[
      !duplicated(group_observations$.delay, fromLast = TRUE), , drop = FALSE
    ]
    latest_per_delay <- latest_per_delay[order(latest_per_delay$.delay), , drop = FALSE]

    # Difference the cumulative curve into signed weekly increments (may be negative).
    cumulative_curve  <- as.numeric(latest_per_delay[[case_count_col]])
    signed_increments <- c(cumulative_curve[1], diff(cumulative_curve))

    increment_frames[[group_index]] <- data.frame(
      latest_per_delay[rep(1L, length(signed_increments)), grouping_cols, drop = FALSE],
      .delay = latest_per_delay$.delay,
      n      = signed_increments,
      row.names = NULL, check.names = FALSE
    )
  }

  all_increments <- do.call(rbind, increment_frames)
  rownames(all_increments) <- NULL
  all_increments
}
