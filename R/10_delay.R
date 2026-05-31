# =============================================================================
# Delay distribution math (AD-friendly)
# =============================================================================
# Ports of src/stan/functions.stan: the native delay parameters, the
# discretised log-PMF log( F(d) - F(d-1) ), the log-CDF, and the scalar CDF
# used for the Gstar truncation correction.  Families:
#   1 = LogNormal   (closed form via pnorm)
#   2 = Gamma       (RTMBdist::pgamma2, mean/SD parametrisation)
#   3 = GenGamma    (heavy-tail-robust pgamma body + Wilson-Hilferty tail)
#   4 = Dirichlet   (non-parametric simplex + geometric tail)
#
# Parameterisation (matches the Stan reference):
#   delay_mu     : log-mean of the natural-scale delay (unconstrained)
#   delay_sigma  : natural-scale SD of the delay (LogNormal/Gamma) (> 0.01)
# LogNormal native params:
#   log_scale    = sqrt(log1p(sd^2 / mean^2));  log_location = log_mean - 0.5*log_scale^2
#
# NAMING: prefer explicit, verbose names over single letters throughout.
# =============================================================================

#' LogNormal native (log-location, log-scale) from (delay log-mean, delay SD)
#' @keywords internal
#' @noRd
.lognormal_native_params <- function(delay_log_mean, delay_sd) {
  natural_mean <- exp(delay_log_mean)
  natural_variance <- delay_sd * delay_sd
  log_scale    <- sqrt(log1p(natural_variance / (natural_mean * natural_mean)))
  log_location <- delay_log_mean - 0.5 * log_scale * log_scale
  list(log_location = log_location, log_scale = log_scale)
}

#' Vectorised LogNormal log-CDF: log F(delay | log_location, log_scale)  (AD-safe)
#' @keywords internal
#' @noRd
.lognormal_log_cdf <- function(delay_value, log_location, log_scale) {
  pnorm((log(delay_value) - log_location) / log_scale, log.p = TRUE)
}

#' Scalar LogNormal CDF F(delay) on the natural scale (NOT log), used for Gstar
#' @keywords internal
#' @noRd
.lognormal_cdf <- function(delay_value, log_location, log_scale) {
  pnorm((log(delay_value) - log_location) / log_scale)
}

#' Robust discretised-delay log-likelihood (AD-safe, family-generic)
#'
#' Sum over observed delays (data) with their case-count weights of
#' `weight * log(F(delay) - F(delay-1))`.  The lower-tail form
#' `log_diff_exp(logF(delay), logF(delay-1))` saturates to `-Inf` (NaN
#' gradient) once `F(delay) = 1` in double precision — common for heavy-tailed
#' reporting delays.  Delays above `split_delay` are therefore evaluated via the
#' SURVIVAL tail `log_diff_exp(logS(delay-1), logS(delay))` (with `S = 1 - F`),
#' which never saturates there.  `split_delay` is data, so the partition (and
#' thus which stable expression is taped on each subset) is AD-safe.
#'
#' @param delay_values Observed delay values (data, >= 1).
#' @param weights Case-count weights per delay (data).
#' @param split_delay Delay separating lower-tail from survival-tail evaluation.
#' @param log_cdf_fn Function `delay -> log F(delay)` (AD).
#' @param log_survival_fn Function `delay -> log S(delay) = log(1 - F(delay))` (AD).
#' @keywords internal
#' @noRd
.discretised_delay_loglik <- function(delay_values, weights, split_delay,
                                      log_cdf_fn, log_survival_fn) {
  is_first_bin <- delay_values <= 1
  lower_mask   <- !is_first_bin & delay_values <= split_delay
  upper_mask   <- !is_first_bin & delay_values >  split_delay
  loglik <- 0
  if (any(is_first_bin)) {                       # first bin: F(1) (since F(0) = 0)
    loglik <- loglik + sum(weights[is_first_bin] * log_cdf_fn(delay_values[is_first_bin]))
  }
  if (any(lower_mask)) {                          # lower-tail log_diff_exp(logF(d), logF(d-1))
    log_cdf_upper <- log_cdf_fn(delay_values[lower_mask])
    log_cdf_lower <- log_cdf_fn(delay_values[lower_mask] - 1)
    loglik <- loglik +
      sum(weights[lower_mask] * (log_cdf_upper + log1p(-exp(log_cdf_lower - log_cdf_upper))))
  }
  if (any(upper_mask)) {                          # survival-tail log_diff_exp(logS(d-1), logS(d))
    log_surv_lower <- log_survival_fn(delay_values[upper_mask] - 1)
    log_surv_upper <- log_survival_fn(delay_values[upper_mask])
    loglik <- loglik +
      sum(weights[upper_mask] * (log_surv_lower + log1p(-exp(log_surv_upper - log_surv_lower))))
  }
  loglik
}

#' Family-generic delay functions: log-CDF, log-survival, and natural CDF.
#'
#' Returns a list of closures `log_cdf(delay)`, `log_survival(delay)`,
#' `cdf(delay)` (AD in the supplied parameters), plus the natural-scale
#' `log_location`/`log_scale` (LogNormal/GenGamma, for diagnostics).  The three
#' positional parameters mean different things per family (mirrors the native
#' parameterisations in functions.stan):
#'   family 1 LogNormal : parameter_1 = delay log-mean, parameter_2 = delay SD
#'   family 2 Gamma     : parameter_1 = delay log-mean, parameter_2 = delay SD
#'   family 3 GenGamma  : parameter_1 = log-location,   parameter_2 = shape Q (> 0),
#'                        parameter_3 = log-scale sigma
#' @keywords internal
#' @noRd
.delay_distribution_functions <- function(family, parameter_1, parameter_2, parameter_3 = NULL) {
  if (family == 1L) {                                   # LogNormal
    natural_mean <- exp(parameter_1)
    log_scale    <- sqrt(log1p(parameter_2 * parameter_2 / (natural_mean * natural_mean)))
    log_location <- parameter_1 - 0.5 * log_scale * log_scale
    list(
      log_cdf      = function(delay) pnorm((log(delay) - log_location) / log_scale, log.p = TRUE),
      log_survival = function(delay) pnorm((log(delay) - log_location) / log_scale,
                                           lower.tail = FALSE, log.p = TRUE),
      cdf          = function(delay) pnorm((log(delay) - log_location) / log_scale),
      log_location = log_location, log_scale = log_scale
    )
  } else if (family == 2L) {                            # Gamma (mean/SD via pgamma2)
    natural_mean <- exp(parameter_1)                    # parameter_1 = log-mean; parameter_2 = SD
    delay_sd     <- parameter_2
    list(
      log_cdf      = function(delay) RTMBdist::pgamma2(delay, mean = natural_mean, sd = delay_sd, log.p = TRUE),
      log_survival = function(delay) RTMBdist::pgamma2(delay, mean = natural_mean, sd = delay_sd,
                                                       lower.tail = FALSE, log.p = TRUE),
      cdf          = function(delay) RTMBdist::pgamma2(delay, mean = natural_mean, sd = delay_sd),
      natural_mean = natural_mean, delay_sd = delay_sd
    )
  } else if (family == 3L) {                            # Generalized Gamma (flexsurv; Q > 0)
    # Heavy-tail-robust port of generalized_gamma.stan.  BODY: the exact gamma
    # CDF via the AD-stable basic `pgamma` (stable to shape ~1e8).  TAIL (the
    # survival side, used only for large delays in .discretised_delay_loglik):
    # the Wilson-Hilferty normal approximation, which is AD-stable and accurate
    # in the deep tail but is an APPROXIMATION there (the exact pgamma upper tail
    # is not AD-safe via RTMBdist).  This avoids the NaN gradients RTMBdist's
    # pgengamma produces near Q -> 0 / large |Q| / small sigma.  The caller
    # bounds Q > 0 (.gengamma_shape_transform), so only the Q > 0 branch is needed.
    log_location <- parameter_1; shape_Q <- parameter_2; log_scale <- parameter_3
    list(
      log_cdf      = function(delay) .gengamma_log_cdf(delay, shape_Q, log_location, log_scale),
      log_survival = function(delay) .gengamma_log_survival(delay, shape_Q, log_location, log_scale),
      cdf          = function(delay) .gengamma_cdf(delay, shape_Q, log_location, log_scale),
      log_location = log_location, log_scale = log_scale
    )
  } else {
    cli::cli_abort("Unsupported parametric delay family {family} in .delay_distribution_functions().")
  }
}

#' Generalized-Gamma gamma-argument helper (flexsurv parametrisation, Q > 0).
#' gamma_argument = exp(Q * (log delay - log_location) / log_scale) / Q^2 ;
#' gamma_shape    = 1 / Q^2 ;  F(delay) = gamma_cdf(gamma_argument | gamma_shape, 1).
#' @keywords internal
#' @noRd
.gengamma_gamma_argument <- function(delay, shape_Q, log_location, log_scale) {
  gamma_shape <- 1 / (shape_Q * shape_Q)
  list(gamma_shape = gamma_shape,
       gamma_argument = exp(shape_Q * (log(delay) - log_location) / log_scale) * gamma_shape)
}

#' GenGamma natural CDF F(delay) — exact gamma body (pgamma is AD-stable).
#' @keywords internal
#' @noRd
.gengamma_cdf <- function(delay, shape_Q, log_location, log_scale) {
  parts <- .gengamma_gamma_argument(delay, shape_Q, log_location, log_scale)
  pgamma(parts$gamma_argument, shape = parts$gamma_shape, rate = 1)
}

#' GenGamma log-CDF (body).
#' @keywords internal
#' @noRd
.gengamma_log_cdf <- function(delay, shape_Q, log_location, log_scale) {
  log(.gengamma_cdf(delay, shape_Q, log_location, log_scale) + 1e-300)
}

#' GenGamma log-survival (deep tail) via the Wilson-Hilferty normal approximation.
#' @keywords internal
#' @noRd
.gengamma_log_survival <- function(delay, shape_Q, log_location, log_scale) {
  parts <- .gengamma_gamma_argument(delay, shape_Q, log_location, log_scale)
  gamma_shape <- parts$gamma_shape
  cube_root      <- (parts$gamma_argument / gamma_shape)^(1 / 3)
  wilson_hilferty_mean <- 1 - 1 / (9 * gamma_shape)
  wilson_hilferty_sd   <- sqrt(1 / (9 * gamma_shape))
  pnorm((cube_root - wilson_hilferty_mean) / wilson_hilferty_sd,
        lower.tail = FALSE, log.p = TRUE)
}

#' Bounded GenGamma shape transform: shape_Q in (0.05, 3) from an unconstrained
#' raw value.  Keeps `1/Q^2` (the gamma shape) in the AD-stable range while still
#' spanning near-lognormal (Q -> 0.05) through Weibull (Q = 1) and beyond.
#' Returns list(shape_Q, log_jacobian).
#' @keywords internal
#' @noRd
.gengamma_shape_transform <- function(raw_value) {
  lower_bound <- 0.05; upper_bound <- 3
  logistic <- plogis(raw_value)
  list(shape_Q = lower_bound + (upper_bound - lower_bound) * logistic,
       log_jacobian = log(upper_bound - lower_bound) + log(logistic) + log(1 - logistic))
}

#' Non-parametric (Dirichlet simplex + geometric tail) delay functions
#'
#' Ports the family-4 logic of functions.stan exactly: explicit probability
#' masses for delays `1..n_bins` and a geometric tail beyond `n_bins`.
#'   log P(X = delay)  = log(probs[delay])                              delay <= n_bins
#'                     = log(tail) + log(1 - e^-1) - (delay - n_bins - 1)  delay >  n_bins
#'   F(delay)          = cumsum(probs)[delay]                           delay <  n_bins
#'                     = 1 - tail * exp(-(delay - n_bins))              delay >= n_bins
#' (the `1 - e^-1` geometric decay and the `tail * exp(...)` CDF match Stan's
#' `delay_lpmf_vec`/`np_cdf_at` for family 4 — not a fresh modelling choice).
#'
#' `simplex_probs` is the length-(n_bins + 1) simplex (AD); `n_bins =
#' np_model_length`.  Returns closures (all AD in `simplex_probs`).
#' @keywords internal
#' @noRd
.nonparametric_delay_functions <- function(simplex_probs, n_bins) {
  cumulative_probs   <- cumsum(simplex_probs)        # [delay] = F(delay) for delay <= n_bins
  tail_mass          <- simplex_probs[n_bins + 1]
  log_geometric_step <- log1p(-exp(-1.0))            # log(1 - e^-1)

  cdf <- function(delay) {                            # delay: numeric (data) vector
    result      <- 0 * tail_mass + numeric(length(delay))   # AD zero vector, right length
    in_grid     <- delay >= 1 & delay < n_bins
    in_tail     <- delay >= n_bins
    if (any(in_grid)) result[in_grid] <- cumulative_probs[delay[in_grid]]
    if (any(in_tail)) result[in_tail] <- 1 - tail_mass * exp(-(delay[in_tail] - n_bins))
    result
  }
  log_cdf <- function(delay) log(cdf(delay) + 1e-12)

  log_pmf_raw <- function(delay) {                    # delay: numeric (data) vector
    result      <- 0 * tail_mass + numeric(length(delay))
    in_bins     <- delay <= n_bins
    in_tail     <- delay >  n_bins
    if (any(in_bins)) result[in_bins] <- log(simplex_probs[delay[in_bins]] + 1e-12)
    if (any(in_tail)) result[in_tail] <- log(tail_mass + 1e-12) + log_geometric_step -
                                         (delay[in_tail] - n_bins - 1)
    result
  }

  list(cdf = cdf, log_cdf = log_cdf, log_pmf_raw = log_pmf_raw)
}
