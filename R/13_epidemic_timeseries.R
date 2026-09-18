# =============================================================================
# Classical time-series epidemic trends (AD-friendly, dual numeric / advector)
# =============================================================================
# ARIMA, the structural semi-local linear trend, and the single-source
# exponential-smoothing trend all reduce to the same shape: a fixed-length
# recursion driven by standard-normal innovations, returning a trend column that
# is added to `mu_intercept + X %*% gamma` exactly like `ar1_trend()`.
#
# Each builder here is written so that ONE implementation serves both callers:
# the RTMB tape in `build_joint_obj()` (innovations are advectors) and the
# plain-R mirror in `.joint_reconstruct()` (innovations are doubles).  That
# matters because the two used to be transcribed separately for AR(1)/SIR, and a
# transcription that drifts makes `predict()` disagree with the fit it came
# from.  The only thing the two paths do differently is how the accumulator is
# allocated, which is what `.trend_zeros()` decides.
#
# Every branch below is on DATA (a lag index, an order, a flag), never on a
# parameter value, so the tape has a single shape.
# =============================================================================

#' Allocate a zero accumulator of the same flavour as the innovations
#'
#' An advector accumulator is needed on the tape so that `[<-` dispatches to
#' RTMB's method; a plain numeric one is needed off it, because allocating an
#' advector outside a tape context errors.
#' @keywords internal
#' @noRd
.trend_zeros <- function(n, like) {
  if (inherits(like, "advector")) RTMB::advector(numeric(n)) else numeric(n)
}

#' Partial autocorrelations to AR coefficients (Levinson-Durbin)
#'
#' Parameterising an AR(p) by its partial autocorrelations `r_i` in (-1, 1) and
#' mapping them through the Levinson-Durbin recursion gives coefficients that are
#' stationary BY CONSTRUCTION (Barndorff-Nielsen & Schou 1973; Monahan 1984).
#' Optimising the coefficients directly would let `nlminb` wander outside the
#' stationary region, where the recursion explodes and the fit dies rather than
#' backing off -- which is the failure mode the user cares most about avoiding.
#'
#' The same map is used for the MA coefficients: invertibility is not needed for
#' the likelihood to be defined, but without it `theta` and its reciprocal give
#' the same autocovariances and the parameter is not identified.
#'
#' @param pacf Partial autocorrelations in (-1, 1) (numeric or advector).
#' @returns The corresponding AR (or MA) coefficient vector, same length.
#' @keywords internal
#' @noRd
.pacf_to_coefficients <- function(pacf) {
  order <- length(pacf)
  if (order <= 1L) return(pacf)
  coefficients <- pacf
  for (stage in 2:order) {
    previous <- coefficients
    for (lag in seq_len(stage - 1L))
      coefficients[lag] <- previous[lag] - pacf[stage] * previous[stage - lag]
  }
  coefficients
}

#' ARIMA(p, d, q) trend on log incidence
#'
#' The d-th difference of the trend follows a conditional ARMA(p, q):
#' `w_t = drift + sum_i ar_i w_{t-i} + eps_t + sum_j ma_j eps_{t-j}`, with
#' `eps_t = sigma * innovations_t` and zero pre-sample values, and the trend is
#' recovered by `d` cumulative sums.
#'
#' The zero pre-sample (a conditional, rather than exact, likelihood) means the
#' first `max(p, q)` points carry a transient.  With `d >= 1` -- the default, and
#' the case that matters for incidence -- the level is a random walk anyway and
#' `mu_intercept` absorbs the starting point, so the transient is not a bias.
#' With `d = 0` it is the usual conditional-sum-of-squares approximation.
#'
#' `drift` is only identified once `d >= 1`: at `d = 0` the ARMA mean and
#' `mu_intercept` are the same quantity, which is why the constructor refuses
#' that combination rather than letting the two trade off invisibly.
#'
#' @param innovations Standard-normal innovations, length `n_time`.
#' @param ar_coefficients Stationary AR coefficients (length `p`, may be empty).
#' @param ma_coefficients Invertible MA coefficients (length `q`, may be empty).
#' @param sigma Innovation SD (> 0).
#' @param drift Per-step drift on the differenced scale (0 when `d = 0`).
#' @param n_difference Order of differencing `d` (>= 0).
#' @keywords internal
#' @noRd
arima_trend <- function(innovations, ar_coefficients, ma_coefficients, sigma,
                        drift, n_difference) {
  n_time <- length(innovations)
  ar_order <- length(ar_coefficients)
  ma_order <- length(ma_coefficients)
  errors <- sigma * innovations
  differenced <- .trend_zeros(n_time, innovations)
  for (t in seq_len(n_time)) {
    value <- drift + errors[t]
    if (ar_order > 0L) for (lag in seq_len(ar_order)) {
      if (t - lag >= 1L) value <- value + ar_coefficients[lag] * differenced[t - lag]
    }
    if (ma_order > 0L) for (lag in seq_len(ma_order)) {
      if (t - lag >= 1L) value <- value + ma_coefficients[lag] * errors[t - lag]
    }
    differenced[t] <- value
  }
  trend <- differenced
  if (n_difference >= 1L) for (pass in seq_len(n_difference)) trend <- cumsum(trend)
  trend
}

#' Structural time-series trend (local level / local linear / semi-local linear)
#'
#' `mu_{t} = mu_{t-1} + delta_{t-1} + sigma_level * u_t`
#' `delta_{t} = slope_mean + phi * (delta_{t-1} - slope_mean) + sigma_slope * v_t`
#'
#' The semi-local form (`phi < 1`) is the one worth having for nowcasting: the
#' slope reverts to `slope_mean` instead of random-walking, so the trend cannot
#' run away over the unobserved tail the way a local linear trend does.  Setting
#' `phi = 1` and `slope_mean = 0` recovers the ordinary local linear trend, and
#' a zero-length `slope_innovations` recovers the local level (a random walk).
#'
#' `mu_1` is fixed at the first level innovation rather than given its own
#' parameter: the starting level is `mu_intercept`'s job, and giving the trend a
#' second one would make the pair unidentified.
#'
#' @param level_innovations Standard-normal level innovations, length `n_time`.
#' @param slope_innovations Standard-normal slope innovations (length `n_time`,
#'   or length 0 for a local-level trend).
#' @param sigma_level,sigma_slope Innovation SDs (> 0).
#' @param phi_slope Slope persistence in (-1, 1); 1 for a local linear trend.
#' @param slope_mean Long-run slope the trend reverts to.
#' @param slope_init Slope at `t = 1`.
#' @keywords internal
#' @noRd
sts_trend <- function(level_innovations, slope_innovations, sigma_level,
                      sigma_slope, phi_slope, slope_mean, slope_init) {
  n_time <- length(level_innovations)
  has_slope <- length(slope_innovations) > 0L
  level <- .trend_zeros(n_time, level_innovations)
  level[1] <- sigma_level * level_innovations[1]
  slope <- slope_init
  if (n_time >= 2L) for (t in 2:n_time) {
    level[t] <- level[t - 1] + slope + sigma_level * level_innovations[t]
    if (has_slope)
      slope <- slope_mean + phi_slope * (slope - slope_mean) +
        sigma_slope * slope_innovations[t]
  }
  level
}

#' Single-source exponential-smoothing trend, ETS(A, A_d, N) style
#'
#' `level_t = level_{t-1} + phi_damp * b_{t-1} + drift + sigma * u_t`
#' `b_t     = phi_damp * b_{t-1} + beta_ratio * sigma * u_t`
#' and the trend reported at `t` is the pre-innovation level
#' `level_{t-1} + phi_damp * b_{t-1} + drift`.
#'
#' The single innovation shared by level and slope is what distinguishes this
#' from [sts_trend()]: it is a rank-one restriction of the same state space
#' (Hyndman et al. 2008's single-source-of-error form), so the slope can only
#' move in step with the level.
#'
#' Note the parameterisation.  Writing the level innovation as `alpha * sigma`
#' and the slope innovation as `beta * sigma`, as the classical ETS notation
#' does, leaves `(alpha, beta, sigma)` identified only up to a common rescaling,
#' because the pair `(alpha * sigma, beta * sigma)` is all the recursion sees.
#' So `sigma` here IS the level innovation SD (classical `alpha * sigma`) and
#' `beta_ratio` is Hyndman's `beta*` = `beta / alpha` in (0, 1).  With
#' `beta_ratio = 0` and no slope this is a random walk -- which is the correct
#' answer, not a degeneracy: under a count observation model the exponential
#' smoothing of the classical method is what the Kalman filter for a local level
#' model already does, with the smoothing weight set by the signal-to-noise
#' ratio rather than estimated separately.
#'
#' @param innovations Standard-normal innovations, length `n_time`.
#' @param sigma Level innovation SD (> 0).
#' @param beta_ratio Slope-to-level innovation ratio in (0, 1); 0 disables the slope.
#' @param phi_damp Damping in (0, 1]; 1 is undamped.
#' @param drift Per-step drift (0 when the variant has none).
#' @param slope_init Slope at `t = 0`.
#' @param has_slope `TRUE` for an additive trend, `FALSE` for level-only.
#' @keywords internal
#' @noRd
ets_trend <- function(innovations, sigma, beta_ratio, phi_damp, drift,
                      slope_init, has_slope) {
  n_time <- length(innovations)
  trend <- .trend_zeros(n_time, innovations)
  level <- 0 * sigma                      # keeps the accumulator's AD flavour
  slope <- slope_init
  for (t in seq_len(n_time)) {
    expected <- if (has_slope) level + phi_damp * slope + drift else level + drift
    trend[t] <- expected
    error <- sigma * innovations[t]
    level <- expected + error
    if (has_slope) slope <- phi_damp * slope + beta_ratio * error
  }
  trend
}

# -----------------------------------------------------------------------------
# Constrained-parameter maps shared by the objective and the reconstruction
# -----------------------------------------------------------------------------
# Each returns the natural-scale value AND the log-Jacobian of the map, because
# the priors below are placed on the natural scale (as they are for AR(1)'s phi
# and sigma) and the tape needs the correction to stay a posterior.

#' Unconstrained -> (-1, 1), the map AR(1)'s `phi` already uses.
#' @keywords internal
#' @noRd
.unit_interval_signed <- function(unconstrained) {
  proportion <- plogis(unconstrained)
  list(value = -0.999 + 1.998 * proportion,
       log_jacobian = sum(log(1.998) + log(proportion) + log(1 - proportion)))
}

#' Unconstrained -> (0, upper), the map AR(1)'s `sigma` already uses.
#' @keywords internal
#' @noRd
.bounded_positive <- function(unconstrained, upper) {
  proportion <- plogis(unconstrained)
  list(value = upper * proportion,
       log_jacobian = sum(log(upper) + log(proportion) + log(1 - proportion)))
}

#' Unconstrained -> (lower, upper).
#' @keywords internal
#' @noRd
.bounded_interval <- function(unconstrained, lower, upper) {
  proportion <- plogis(unconstrained)
  list(value = lower + (upper - lower) * proportion,
       log_jacobian = sum(log(upper - lower) + log(proportion) + log(1 - proportion)))
}

# -----------------------------------------------------------------------------
# Per-stratum constrained parameters, shared by the tape and the reconstruction
# -----------------------------------------------------------------------------
# These take the UNCONSTRAINED parameters for one stratum and return the natural
# scale values the trend builders want, together with the log-Jacobian of the
# map.  Both callers go through them, so the objective and `.joint_reconstruct()`
# cannot disagree about what `log_ets_sigma_unc = -2` means.  The reconstruction
# ignores the Jacobian; only the tape adds it to the posterior.

#' @keywords internal
#' @noRd
.arima_stratum_parameters <- function(log_sigma_unc, ar_pacf_unc, ma_pacf_unc,
                                      drift, sigma_max, free = NULL) {
  free <- .free_flags(free, c("sigma", "ar", "ma"))
  sigma_map <- .bounded_positive(log_sigma_unc, sigma_max)
  log_jacobian <- if (free$sigma) sigma_map$log_jacobian else 0
  ar_pacf <- ar <- ma_pacf <- ma <- numeric(0)
  if (length(ar_pacf_unc)) {
    ar_map <- .unit_interval_signed(ar_pacf_unc)
    ar_pacf <- ar_map$value
    ar <- .pacf_to_coefficients(ar_pacf)
    if (free$ar) log_jacobian <- log_jacobian + ar_map$log_jacobian
  }
  if (length(ma_pacf_unc)) {
    ma_map <- .unit_interval_signed(ma_pacf_unc)
    ma_pacf <- ma_map$value
    ma <- .pacf_to_coefficients(ma_pacf)
    if (free$ma) log_jacobian <- log_jacobian + ma_map$log_jacobian
  }
  list(sigma = sigma_map$value, ar = ar, ma = ma, ar_pacf = ar_pacf,
       ma_pacf = ma_pacf, drift = drift, log_jacobian = log_jacobian)
}

#' @keywords internal
#' @noRd
.ets_stratum_parameters <- function(log_sigma_unc, beta_unc, damp_unc, drift,
                                    slope_init, has_slope, is_damped, sigma_max,
                                    free = NULL) {
  free <- .free_flags(free, c("sigma", "beta", "damping"))
  sigma_map <- .bounded_positive(log_sigma_unc, sigma_max)
  log_jacobian <- if (free$sigma) sigma_map$log_jacobian else 0
  beta <- 0
  damping <- 1
  slope_start <- 0
  if (has_slope) {
    beta_map <- .bounded_interval(beta_unc, 0, 1)
    beta <- beta_map$value
    if (free$beta) log_jacobian <- log_jacobian + beta_map$log_jacobian
    slope_start <- slope_init
    if (is_damped) {
      damp_map <- .bounded_interval(damp_unc, 0.8, 0.998)
      damping <- damp_map$value
      if (free$damping) log_jacobian <- log_jacobian + damp_map$log_jacobian
    }
  }
  list(sigma = sigma_map$value, beta = beta, damping = damping, drift = drift,
       slope_init = slope_start, log_jacobian = log_jacobian)
}

#' @keywords internal
#' @noRd
.sts_stratum_parameters <- function(log_level_sigma_unc, log_slope_sigma_unc,
                                    slope_phi_unc, slope_mean, slope_init,
                                    has_slope, reverting, sigma_max, free = NULL) {
  free <- .free_flags(free, c("level_sigma", "slope_sigma", "slope_phi"))
  level_map <- .bounded_positive(log_level_sigma_unc, sigma_max)
  log_jacobian <- if (free$level_sigma) level_map$log_jacobian else 0
  slope_sigma <- 0
  slope_phi <- 1
  slope_centre <- 0
  slope_start <- 0
  if (has_slope) {
    slope_map <- .bounded_positive(log_slope_sigma_unc, sigma_max)
    slope_sigma <- slope_map$value
    if (free$slope_sigma) log_jacobian <- log_jacobian + slope_map$log_jacobian
    slope_start <- slope_init
    if (reverting) {
      phi_map <- .unit_interval_signed(slope_phi_unc)
      slope_phi <- phi_map$value
      if (free$slope_phi) log_jacobian <- log_jacobian + phi_map$log_jacobian
      slope_centre <- slope_mean
    }
  }
  list(level_sigma = level_map$value, slope_sigma = slope_sigma,
       slope_phi = slope_phi, slope_mean = slope_centre,
       slope_init = slope_start, log_jacobian = log_jacobian)
}

# -----------------------------------------------------------------------------
# Plain-R reconstruction of a classical time-series trend
# -----------------------------------------------------------------------------
# `.joint_reconstruct()` and the prior-predictive simulator both need the trend
# from a flat parameter list rather than from a live tape.  These two functions
# are the only place that knows how a `parlist` maps onto the constrained
# parameters, and they call the same helpers the objective does.

#' Constrained per-stratum trend parameters from a parameter list
#'
#' @param data The prepared engine (carries the order / variant flags).
#' @param parlist A parameter list from `obj$env$parList()` or a posterior draw.
#' @param n_strata Number of strata.
#' @returns A list with `kind` and one constrained parameter list per stratum,
#'   plus the innovation matrices the trend builders consume.
#' @keywords internal
#' @noRd
.timeseries_reconstruct_parameters <- function(data, parlist, n_strata) {
  n_time <- as.integer(data$max_time)
  sigma_max <- data$ar_sigma_max
  reshape <- function(x, n_rows) matrix(as.numeric(x), n_rows, n_strata)
  scalar <- function(x, index) {
    values <- as.numeric(x %||% 0)
    if (length(values) >= index) values[index] else 0
  }
  epidemic_model <- as.integer(data$epidemic_model)
  if (epidemic_model == 5L) {
    ar_order <- as.integer(data$arima_p %||% 0L)
    ma_order <- as.integer(data$arima_q %||% 0L)
    has_drift <- isTRUE(data$arima_include_drift == 1L)
    ar_unc <- if (ar_order > 0L) reshape(parlist$arima_ar_pacf_unc, ar_order) else NULL
    ma_unc <- if (ma_order > 0L) reshape(parlist$arima_ma_pacf_unc, ma_order) else NULL
    by_stratum <- lapply(seq_len(n_strata), function(s)
      .arima_stratum_parameters(
        scalar(parlist$log_arima_sigma_unc, s),
        if (ar_order > 0L) ar_unc[, s] else numeric(0),
        if (ma_order > 0L) ma_unc[, s] else numeric(0),
        if (has_drift) scalar(parlist$arima_drift, s) else 0,
        sigma_max))
    return(list(kind = "arima", by_stratum = by_stratum,
                innovations = reshape(parlist$arima_innov, n_time),
                n_difference = as.integer(data$arima_d %||% 0L)))
  }
  if (epidemic_model == 6L) {
    has_slope <- isTRUE(data$ets_has_slope == 1L)
    is_damped <- has_slope && isTRUE(data$ets_damped == 1L)
    has_drift <- isTRUE(data$ets_include_drift == 1L)
    by_stratum <- lapply(seq_len(n_strata), function(s)
      .ets_stratum_parameters(
        scalar(parlist$log_ets_sigma_unc, s),
        if (has_slope) scalar(parlist$ets_beta_unc, s) else 0,
        if (is_damped) scalar(parlist$ets_damp_unc, s) else 0,
        if (has_drift) scalar(parlist$ets_drift, s) else 0,
        if (has_slope) scalar(parlist$ets_slope_init, s) else 0,
        has_slope, is_damped, sigma_max))
    return(list(kind = "ets", by_stratum = by_stratum,
                innovations = reshape(parlist$ets_innov, n_time),
                has_slope = has_slope))
  }
  has_slope <- isTRUE(data$sts_has_slope == 1L)
  reverting <- has_slope && isTRUE(data$sts_reverting_slope == 1L)
  by_stratum <- lapply(seq_len(n_strata), function(s)
    .sts_stratum_parameters(
      scalar(parlist$log_sts_level_sigma_unc, s),
      if (has_slope) scalar(parlist$log_sts_slope_sigma_unc, s) else 0,
      if (reverting) scalar(parlist$sts_slope_phi_unc, s) else 0,
      if (reverting) scalar(parlist$sts_slope_mean, s) else 0,
      if (has_slope) scalar(parlist$sts_slope_init, s) else 0,
      has_slope, reverting, sigma_max))
  list(kind = "sts", by_stratum = by_stratum,
       innovations = reshape(parlist$sts_level_innov, n_time),
       slope_innovations = if (has_slope) reshape(parlist$sts_slope_innov, n_time) else NULL,
       has_slope = has_slope)
}

#' One stratum's trend column from reconstructed parameters
#' @keywords internal
#' @noRd
.timeseries_trend_column <- function(data, trend_parameters, stratum, n_time) {
  stratum_parameters <- trend_parameters$by_stratum[[stratum]]
  innovations <- trend_parameters$innovations[, stratum]
  if (identical(trend_parameters$kind, "arima"))
    return(arima_trend(innovations, stratum_parameters$ar, stratum_parameters$ma,
                       stratum_parameters$sigma, stratum_parameters$drift,
                       trend_parameters$n_difference))
  if (identical(trend_parameters$kind, "ets"))
    return(ets_trend(innovations, stratum_parameters$sigma, stratum_parameters$beta,
                     stratum_parameters$damping, stratum_parameters$drift,
                     stratum_parameters$slope_init, trend_parameters$has_slope))
  sts_trend(innovations,
            if (trend_parameters$has_slope)
              trend_parameters$slope_innovations[, stratum] else numeric(0),
            stratum_parameters$level_sigma, stratum_parameters$slope_sigma,
            stratum_parameters$slope_phi, stratum_parameters$slope_mean,
            stratum_parameters$slope_init)
}

#' Default every constraint map to "still moving"
#'
#' The reconstruction never wants a Jacobian, and the tape only wants the terms
#' for parameters that are still being optimised.  Passing `free = NULL` (the
#' reconstruction's case) keeps the previous behaviour of computing them all.
#' @keywords internal
#' @noRd
.free_flags <- function(free, names) {
  defaults <- stats::setNames(rep(TRUE, length(names)), names)
  if (is.null(free)) return(as.list(defaults))
  for (name in names) if (!is.null(free[[name]])) defaults[[name]] <- isTRUE(free[[name]])
  as.list(defaults)
}
