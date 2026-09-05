# =============================================================================
# Count-cumulative observation models
# =============================================================================
# Shared, AD-safe mathematics for the cumulative-level composite likelihood and
# the signed hurdle--zero-truncated-negative-binomial update composite
# likelihood.  The finite-age retraction kernel h_R is primitive here; no
# biological truth probability is part of this observation model.
# =============================================================================

#' Validate a finite settlement horizon
#' @keywords internal
#' @noRd
.validate_settlement_horizon <- function(settlement) {
  if (length(settlement) != 1L || is.na(settlement) ||
      settlement != as.integer(settlement) || settlement < 1L) {
    cli::cli_abort("`settlement` must be one positive integer model step.")
  }
  as.integer(settlement)
}

#' Build the finite-horizon count-cumulative kernel
#'
#' `report_pmf` is indexed by report delay `0:H`; `retraction_pmf` is indexed by
#' retraction age `1:H` and is conditional only in the convenient finite-horizon
#' factorisation `h_R(l) = retraction_mass * retraction_pmf(l)`.  The likelihood
#' uses `h_R` and `S_R` directly and does not identify a separate truth
#' probability.
#'
#' @keywords internal
#' @noRd
.count_cumulative_components <- function(report_pmf, retraction_pmf,
                                         retraction_mass, settlement) {
  settlement <- .validate_settlement_horizon(settlement)
  if (length(report_pmf) != settlement + 1L) {
    cli::cli_abort("`report_pmf` must have length `settlement + 1` (delays 0:H).")
  }
  if (length(retraction_pmf) != settlement) {
    cli::cli_abort("`retraction_pmf` must have length `settlement` (ages 1:H).")
  }

  "[<-" <- RTMB::ADoverload("[<-")
  h_R <- retraction_mass * retraction_pmf

  # S_R[a + 1] = Pr(R > a), a = 0, ..., H.
  S_R <- rep(h_R[1L] * 0, settlement + 1L)
  S_R[1L] <- 1
  running_mass <- h_R[1L] * 0
  for (age in seq_len(settlement)) {
    running_mass <- running_mass + h_R[age]
    S_R[age + 1L] <- 1 - running_mass
  }

  # q_C[d + 1] = sum_{r=0}^d g_D(r) S_R(d-r).
  # omega_unit[d + 1] = sum_{r=0}^{d-1} g_D(r) h_R(d-r).
  q_C <- rep(report_pmf[1L] * 0, settlement + 1L)
  omega_unit <- rep(report_pmf[1L] * 0, settlement + 1L)
  for (delay in 0:settlement) {
    cumulative_probability <- report_pmf[1L] * 0
    withdrawal_probability <- report_pmf[1L] * 0
    for (report_delay in 0:delay) {
      cumulative_probability <- cumulative_probability +
        report_pmf[report_delay + 1L] * S_R[delay - report_delay + 1L]
      if (report_delay < delay) {
        withdrawal_probability <- withdrawal_probability +
          report_pmf[report_delay + 1L] * h_R[delay - report_delay]
      }
    }
    q_C[delay + 1L] <- cumulative_probability
    omega_unit[delay + 1L] <- withdrawal_probability
  }

  list(
    h_R = h_R,
    S_R = S_R,
    q_C = q_C,
    alpha_unit = report_pmf,
    omega_unit = omega_unit,
    terminal_retention = q_C[settlement + 1L]
  )
}

#' Cumulative-level composite log mass
#' @keywords internal
#' @noRd
.poisson_logpmf <- function(count, mean) {
  count * log(mean) - mean - lgamma(count + 1)
}

#' Negative-binomial log mass in the package's mean/size convention
#' @keywords internal
#' @noRd
.nb_mean_size_logpmf <- function(count, mean, size) {
  lgamma(count + size) - lgamma(size) - lgamma(count + 1) +
    size * (log(size) - log(size + mean)) +
    count * (log(mean) - log(size + mean))
}

#' Cumulative-level composite log mass
#' @keywords internal
#' @noRd
.count_cumulative_level_logpmf <- function(count, mean, likelihood_id,
                                           size = 1) {
  if (likelihood_id == 0L) {
    .poisson_logpmf(count, mean)
  } else if (likelihood_id == 1L) {
    .nb_mean_size_logpmf(count, mean, size)
  } else {
    cli::cli_abort("Count-cumulative levels support only Poisson or negative-binomial likelihoods.")
  }
}

#' Mean of a zero-truncated NB from its parent NB mean
#' @keywords internal
#' @noRd
.ztnb_own_mean <- function(parent_mean, size) {
  log_p_zero <- -size * log1p(parent_mean / size)
  parent_mean / (-expm1(log_p_zero))
}

#' Invert the zero-truncated NB mean on the RTMB tape
#'
#' Returns the parent NB mean `m` whose zero-truncated distribution has mean
#' `own_mean`.  The fixed iteration count and smooth damping keep the complete
#' inverse differentiable by RTMB.  The small-mean expansion supplies a stable
#' initial value as `own_mean` approaches the support boundary at one.
#'
#' @keywords internal
#' @noRd
.ztnb_parent_mean <- function(own_mean, size, iterations = 30L) {
  excess_mean <- own_mean - 1
  near_boundary <- 2 * size * excess_mean / (size + 1)
  initial_mean <- (near_boundary + excess_mean * own_mean) / (1 + excess_mean)
  log_parent_mean <- log(initial_mean + 1e-14)

  for (iteration in seq_len(iterations)) {
    parent_mean <- exp(log_parent_mean)
    log_p_zero <- -size * log1p(parent_mean / size)
    p_zero <- exp(log_p_zero)
    nonzero_probability <- -expm1(log_p_zero)
    implied_mean <- parent_mean / nonzero_probability
    nonzero_derivative <- size * p_zero / (size + parent_mean)
    implied_derivative <-
      (nonzero_probability - parent_mean * nonzero_derivative) /
      nonzero_probability^2
    log_scale_step <- (implied_mean - own_mean) /
      (implied_derivative * parent_mean + 1e-14)
    log_parent_mean <- log_parent_mean -
      log_scale_step / sqrt(1 + log_scale_step^2)
  }
  exp(log_parent_mean)
}

#' Zero-truncated NB log mass indexed by its own mean
#' @keywords internal
#' @noRd
.ztnb_logpmf <- function(magnitude, own_mean, size) {
  parent_mean <- .ztnb_parent_mean(own_mean, size)
  log_p_zero <- -size * log1p(parent_mean / size)
  .nb_mean_size_logpmf(magnitude, parent_mean, size) -
    log(-expm1(log_p_zero))
}

#' Mean of a zero-truncated Poisson from its parent Poisson mean
#' @keywords internal
#' @noRd
.ztpoisson_own_mean <- function(parent_mean) {
  parent_mean / (-expm1(-parent_mean))
}

#' Invert the zero-truncated Poisson mean on the RTMB tape
#' @keywords internal
#' @noRd
.ztpoisson_parent_mean <- function(own_mean, iterations = 30L) {
  excess_mean <- own_mean - 1
  near_boundary <- 2 * excess_mean
  initial_mean <- (near_boundary + excess_mean * own_mean) / (1 + excess_mean)
  log_parent_mean <- log(initial_mean + 1e-14)

  for (iteration in seq_len(iterations)) {
    parent_mean <- exp(log_parent_mean)
    nonzero_probability <- -expm1(-parent_mean)
    implied_mean <- parent_mean / nonzero_probability
    implied_derivative <-
      (nonzero_probability - parent_mean * exp(-parent_mean)) /
      nonzero_probability^2
    log_scale_step <- (implied_mean - own_mean) /
      (implied_derivative * parent_mean + 1e-14)
    log_parent_mean <- log_parent_mean -
      log_scale_step / sqrt(1 + log_scale_step^2)
  }
  exp(log_parent_mean)
}

#' Zero-truncated Poisson log mass indexed by its own mean
#' @keywords internal
#' @noRd
.ztpoisson_logpmf <- function(magnitude, own_mean) {
  parent_mean <- .ztpoisson_parent_mean(own_mean)
  .poisson_logpmf(magnitude, parent_mean) -
    log(-expm1(-parent_mean))
}

#' Bounded probability of a non-null signed update
#'
#' `1 - exp(-total)` is at most both one and `total`, so multiplying it by a
#' logistic regression probability enforces
#' `0 < movement_probability <= min(1, total)`.
#'
#' @keywords internal
#' @noRd
.count_cumulative_movement_probability <- function(total, linear_predictor) {
  -expm1(-total) * plogis(linear_predictor)
}

#' Signed hurdle--ZTNB update log mass
#' @keywords internal
#' @noRd
.hurdle_ztnb_update_logpmf <- function(update, alpha, omega,
                                      movement_probability,
                                      magnitude_size) {
  if (update == 0) {
    return(log1p(-movement_probability))
  }

  total <- alpha + omega
  direction_log_probability <- if (update > 0) {
    log(alpha / total)
  } else {
    log(omega / total)
  }
  own_magnitude_mean <- total / movement_probability

  log(movement_probability) + direction_log_probability +
    .ztnb_logpmf(abs(update), own_magnitude_mean, magnitude_size)
}

#' Signed hurdle--zero-truncated-Poisson update log mass
#' @keywords internal
#' @noRd
.hurdle_ztpoisson_update_logpmf <- function(update, alpha, omega,
                                           movement_probability) {
  if (update == 0) {
    return(log1p(-movement_probability))
  }

  total <- alpha + omega
  direction_log_probability <- if (update > 0) {
    log(alpha / total)
  } else {
    log(omega / total)
  }
  own_magnitude_mean <- total / movement_probability

  log(movement_probability) + direction_log_probability +
    .ztpoisson_logpmf(abs(update), own_magnitude_mean)
}
