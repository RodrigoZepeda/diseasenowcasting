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
#      mu_t = lambda_t / p, eta_t = (1 - p) * mu_t,  p = P(report is genuine).
#
# Both rates are on the GROSS scale: a fraction (1 - p) of the gross reports is
# erroneous, not a fraction of the genuine ones.  See article eqs. alphasimplified
# and omegadef, and the note on `eta_stream` in R/14_objective_joint.R.
#
# These helpers evaluate the log-likelihood of one increment, AD-safe under RTMB,
# for use inside the joint objective.  They are the count-cumulative analogue of
# the truncated Poisson / negative-binomial count likelihood.
# =============================================================================

# --- numeric constants -------------------------------------------------------

#' Gauss-Legendre quadrature rule on `[-1, 1]` via the Golub-Welsch algorithm.
#'
#' Returns `n` nodes and weights (weights sum to 2) for integrating against the
#' constant weight on `[-1, 1]`.  The nodes are the eigenvalues of the symmetric
#' tridiagonal Jacobi matrix of the Legendre three-term recurrence, and the weights
#' are `2 * (first component of each eigenvector)^2` (Golub & Welsch, 1969).  This
#' is a base-R replacement for `statmod::gauss.quad(n, "legendre")` -- it matches it
#' to machine precision -- so the package needs no extra dependency for the single
#' fixed rule the SkNB frailty integral uses.
#' @param n Number of quadrature points.
#' @keywords internal
#' @noRd
.gauss_legendre_rule <- function(n) {
  # Off-diagonal entries of the Jacobi matrix for the Legendre recurrence.
  off_diagonal_indices <- seq_len(n - 1L)
  off_diagonal <- off_diagonal_indices / sqrt(4 * off_diagonal_indices^2 - 1)

  jacobi_matrix <- matrix(0, n, n)
  for (index in off_diagonal_indices) {
    jacobi_matrix[index, index + 1L] <- off_diagonal[index]
    jacobi_matrix[index + 1L, index] <- off_diagonal[index]
  }

  decomposition <- eigen(jacobi_matrix, symmetric = TRUE)
  list(nodes   = decomposition$values,                 # eigenvalues are the nodes
       weights = 2 * decomposition$vectors[1, ]^2)      # Golub-Welsch weights
}

# Gamma-frailty quadrature nodes for the SkNB likelihood.  The NB increment law
# is a gamma mixture of Skellams: SkNB(a, b, r) = E_u[Skellam(a*u, b*u)] with
# u ~ Gamma(r, r).  With r floored (see the confirmation prior) the mixing density
# concentrates well inside [0, ~4], so a 24-point Gauss-Legendre rule on
# [1e-3, 4.5] integrates it to ~1e-3 -- matching the exact convolution while
# keeping the RTMB tape linear.  Computed once at load.
.confirmation_gl_nodes <- local({
  lower_bound <- 1e-3
  upper_bound <- 4.5
  gauss_rule  <- .gauss_legendre_rule(24L)                       # nodes on [-1, 1]
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

#' Log-pmf of a single signed increment under the Skellam law.
#'
#' `bin_type` is decided OUTSIDE the RTMB tape from the integer delay index, so no
#' comparison ever touches an AD value:
#'   0 = pure addition    (delay 0: `beta` is structurally 0)     -> Poisson(alpha)
#'   2 = pure retraction   (delay beyond the appearance support)   -> -Poisson(beta)
#'   1 = mixed             (both streams active)                   -> the form below
#' Tiny floors on `alpha`, `beta` keep `sqrt`/`log`/ratio finite -- and, crucially,
#' floor the Poisson mean for the pure-addition/retraction cases so an observed
#' increment in a delay cell the delay pmf has driven to ~0 is a large *finite*
#' penalty (which pushes the optimizer to widen the delay) rather than a fatal `-Inf`.
#'
#' **The mixed case: saddlepoint, with the exact series where it is cheap.**
#' Article eq. skellampmf writes the mixed case with a modified Bessel function,
#' `-(alpha+beta) + (m/2) log(alpha/beta) + log I_|m|(2 sqrt(alpha beta))`.
#' Evaluating `log I_nu` directly is the hard part, and two things ruled out every
#' arrangement built around it:
#'
#' * A fixed-length **ascending series** silently under-sums once `alpha * beta`
#'   grows, because its terms peak near the ARGUMENT `2 sqrt(alpha beta)` and no
#'   data-derived term count can bound a parameter-dependent quantity.  It was 5729
#'   nats wrong at `Skellam(20000, 500)`, and 7253 nats wrong there even with 1024
#'   terms.
#' * **`besselI()`** is exact where it is representable but returns 0 across the
#'   BULK of an asymmetric Skellam (`alpha >> beta`, the normal case here), and R
#'   abandons its DERIVATIVE even earlier -- at `nu = 101, z = 0.094` it returns a
#'   perfectly representable 6.6e-295 with a NaN derivative.
#'
#' Mixing the two by a smooth weight does not rescue it, and the reason is worth
#' recording because it is not obvious: **a weight of zero does not neutralise a
#' branch.**  A blend's derivative carries `d(weight) * value`, so an unused branch
#' whose value is far from the truth injects a spurious gradient.  Guarding the
#' Bessel call by feeding `1` into its `log()` left the branch at +673 against a
#' true log-pmf of -4.56, and the fit then converged to `p = 0.68` on data generated
#' at `p = 0.90`.
#'
#' So the Bessel function is not evaluated at all.  The **saddlepoint** of the
#' Skellam cumulant generating function `K(s) = alpha(e^s - 1) + beta(e^-s - 1)` has
#' a closed form -- the saddle solves `alpha e^s - beta e^-s = m`, a quadratic in
#' `e^s` -- so the whole density is one differentiable expression with no series, no
#' truncation and no special function.  It is accurate to 1e-7 near the mode of
#' `Skellam(1e5, 1e4)` and to ~5e-3 on ordinary cells, degrading only as
#' `alpha + beta -> 0` (2.3 nats at `alpha = 0.0016, beta = 0.00035`).
#'
#' That corner is exactly where the ascending series converges on its first few
#' terms, so the two are complementary, and -- unlike every arrangement above --
#' BOTH are bounded relative to the truth: the saddlepoint is never more than a few
#' nats out, and the series is an all-positive sum, hence a LOWER bound, which is
#' floored near the saddlepoint so a truncated one cannot run away.  A bounded blend
#' has a bounded weight-derivative leak.  Worst error over the (alpha, beta, m) grid
#' plus the cells a real fit visits: 4.4e-4 nats.
#'
#' **Measured accuracy against an independent exact reference.**  Checked against
#' `sum_w Pois(w + z; alpha) Pois(w; beta)` -- a representation sharing no machinery
#' with either branch here (the ascending series above IS the Bessel series, so a
#' Bessel reference would not be independent).  See `devel/spa_diagnostics/`.
#'
#' * Below `alpha + beta ~ 30` the blend is exact to machine precision (1e-14): the
#'   series branch carries it.
#' * Above that a small **normalisation deficit** appears -- the pmf is correctly
#'   shaped but sums to slightly under 1.  It is worst near `alpha + beta ~ 50`
#'   (1.2e-3 nats, i.e. 0.12% of the mass) and decays as ~1/rate thereafter
#'   (6.2e-4 at 100, 2.5e-4 at 250, 6.2e-5 at 1000).
#' * The far tail is fine: agreement is ~1e-6 nats even 49 standard deviations from
#'   the mean, which is where a real count-cumulative fit spends much of its
#'   likelihood.
#'
#' The deficit is parameter-dependent, so in principle it acts as an unintended
#' extra likelihood term.  It was measured on the FluSight count-cumulative fit and
#' is not material there: over 1470 approximated cells it totals 0.41 nats, and its
#' net effect on the comparison between the two competing optima is -0.38 nats
#' against a 14,275-nat difference (0.003%, and in the direction that DISfavours the
#' pathological solution).  Documented rather than fixed for that reason.
#' @keywords internal
#' @noRd
.log_skellam_increment <- function(increment, alpha, beta, bin_type) {
  if (bin_type == 0L) return(dpois(increment, alpha + 1e-8, log = TRUE))
  if (bin_type == 2L) return(dpois(-increment, beta  + 1e-8, log = TRUE))
  alpha <- alpha + 1e-8
  beta  <- beta  + 1e-8

  # -- saddlepoint --------------------------------------------------------------
  # K'(s) = alpha e^s - beta e^-s = m  =>  alpha u^2 - m u - beta = 0 for u = e^s.
  # The positive root is taken in whichever algebraic form avoids cancellation:
  # `(m + root) / (2 alpha)` loses all precision for m < 0, where the conjugate
  # form `2 beta / (root - m)` is exact.  `increment` is DATA, so branching on its
  # sign never touches an AD value.
  root  <- sqrt(increment^2 + 4 * alpha * beta)
  u_hat <- if (increment >= 0) (increment + root) / (2 * alpha) else 2 * beta / (root - increment)
  s_hat <- log(u_hat)
  cgf   <- alpha * (u_hat - 1) + beta * (1 / u_hat - 1)
  cgf2  <- alpha * u_hat + beta / u_hat
  log_saddle <- cgf - s_hat * increment - 0.5 * log(2 * pi * cgf2)

  # -- exact ascending series, for the small-(alpha + beta) corner ---------------
  order      <- abs(increment)
  argument   <- 2 * sqrt(alpha * beta)
  common     <- -(alpha + beta) + (increment / 2) * log(alpha / beta)
  term_index <- 0:.SKELLAM_SERIES_TERMS
  log_terms  <- (2 * term_index + order) * log(argument / 2) -
                lgamma(term_index + 1) - lgamma(term_index + order + 1)
  largest_term <- .smooth_vector_max(log_terms)
  log_series   <- common + largest_term + log(sum(exp(log_terms - largest_term)))

  # Every term is positive, so a truncated series is a LOWER bound and can only err
  # downward.  Flooring it near the saddlepoint therefore never touches a converged
  # series, and stops a badly truncated one from leaking through the weight
  # derivative below.
  series_floor <- log_saddle - .SKELLAM_SERIES_FLOOR
  log_series   <- (log_series + series_floor +
                     sqrt((log_series - series_floor)^2 + 1e-12)) / 2

  # `gap` -- how far the last retained term fell below the largest -- is large
  # exactly when the series has converged, which is the regime the saddlepoint is
  # weakest in.
  gap           <- largest_term - log_terms[length(log_terms)]
  series_usable <- 1 / (1 + exp(-(gap - .SKELLAM_SERIES_GAP) / .SKELLAM_SWITCH_SCALE))
  series_usable * log_series + (1 - series_usable) * log_saddle
}

# Terms retained in the ascending series, and how far the last one must have fallen
# below the largest before the series is preferred to the saddlepoint.  The switch
# is sharp because the two disagree by a lot outside their own regimes; the
# transition band sits where both are accurate.
.SKELLAM_SERIES_TERMS <- 60L
.SKELLAM_SERIES_GAP   <- 12
.SKELLAM_SWITCH_SCALE <- 0.25

# How far below the saddlepoint a truncated series is allowed to fall before it is
# floored.  Bounds the weight-derivative leak without ever binding on a converged
# series.
.SKELLAM_SERIES_FLOOR <- 20

#' Smooth maximum of a vector, AD-safe.
#'
#' `max()` kinks at ties and yields NaN gradients under RTMB, so the maximum is
#' folded pairwise with the same smooth `sqrt` construction `.logspace_add()` uses.
#' @keywords internal
#' @noRd
.smooth_vector_max <- function(values) {
  running <- values[1]
  for (value_index in seq_along(values)[-1]) {
    left <- running; right <- values[value_index]
    running <- (left + right + sqrt((left - right)^2 + 1e-12)) / 2
  }
  running
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
