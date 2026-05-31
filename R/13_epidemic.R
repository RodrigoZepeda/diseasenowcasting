# =============================================================================
# Epidemic-process building blocks (AD-friendly)
# =============================================================================

#' Non-centred AR(1) trend (port of functions.stan `ar1_trend`)
#'
#' `trend[1] = innovations[1]*sigma/sqrt(1-phi^2)`,
#' `trend[t] = phi*trend[t-1] + innovations[t]*sigma`.  Returns an advector;
#' the elementwise writes dispatch via RTMB's registered `[<-.advector`.
#'
#' @param innovations standard-normal innovations (length n_time).
#' @param phi autocorrelation in (-1, 1).
#' @param sigma marginal innovation SD.
#' @keywords internal
#' @noRd
ar1_trend <- function(innovations, phi, sigma) {
  n_time <- length(innovations)
  trend <- RTMB::advector(numeric(n_time))
  trend[1] <- innovations[1] * sigma / sqrt(1.0 - phi * phi)
  if (n_time >= 2) for (t in 2:n_time) trend[t] <- phi * trend[t - 1] + innovations[t] * sigma
  trend
}

#' Build the HSGP eigenbasis matrix (data; plain R, no AD)
#'
#' Port of functions.stan `hsgp_basis` for an asymmetric domain
#' `[-left_halfwidth, +right_halfwidth]` with Dirichlet (sine) or Neumann
#' (cosine) eigenbasis.  Returns a `max_time x num_basis` matrix.
#' @keywords internal
#' @noRd
hsgp_basis <- function(time_scaled, left_halfwidth, right_halfwidth, num_basis, basis_type) {
  domain_length <- left_halfwidth + right_halfwidth
  n_time <- length(time_scaled)
  basis_matrix <- matrix(0.0, n_time, num_basis)
  for (basis_index in seq_len(num_basis)) {
    eigen_argument <- basis_index * pi * (time_scaled + left_halfwidth) / domain_length
    basis_matrix[, basis_index] <- if (basis_type == 2L)
      sqrt(2 / domain_length) * cos(eigen_argument)
    else
      sqrt(2 / domain_length) * sin(eigen_argument)
  }
  basis_matrix
}

#' HSGP spectral weights sqrt(S(omega_j)) (port of `hsgp_spectral_weights`)
#'
#' AD-friendly in (amplitude, length_scale).  `gp_kernel`: 1 = SqExp,
#' 2 = Matern32, 3 = Matern52.  `frequencies[j] = j*pi/domain_length` (data).
#' @keywords internal
#' @noRd
hsgp_spectral_weights <- function(frequencies, amplitude, length_scale, gp_kernel) {
  frequency_sq <- frequencies * frequencies
  if (gp_kernel == 1L) {
    # Squared exponential: S(w) = amplitude^2 * sqrt(2pi) * ell * exp(-0.5 ell^2 w^2)
    spectral_density <- amplitude * amplitude * sqrt(2 * pi) * length_scale *
      exp(-0.5 * length_scale * length_scale * frequency_sq)
  } else if (gp_kernel == 2L) {
    # Matern 3/2
    decay <- sqrt(3) / length_scale
    spectral_density <- amplitude * amplitude * 4 * decay^3 / (decay * decay + frequency_sq)^2
  } else {
    # Matern 5/2
    decay <- sqrt(5) / length_scale
    spectral_density <- amplitude * amplitude * (16.0 / 3.0) * decay^5 / (decay * decay + frequency_sq)^3
  }
  sqrt(spectral_density)
}

#' Standardised time grid for the HSGP (data); newest point at the right edge.
#' @keywords internal
#' @noRd
hsgp_time_scaled <- function(max_time, tmax_model) {
  time_centre <- (tmax_model + 1) / 2
  time_halfrange <- (tmax_model - 1) / 2
  time_halfrange <- if (time_halfrange > 0) time_halfrange else 1
  (seq_len(max_time) - time_centre) / time_halfrange
}
