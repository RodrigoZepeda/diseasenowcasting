# =============================================================================
# fit() -- optimise the RTMB objective (Laplace for latent epidemic coefs)
# =============================================================================

#' Fit a nowcast model with the RTMB engine
#'
#' Optimises the negative log-posterior built from `model` + `data`.  For
#' `delay_only` data this fits the reporting-delay process alone (no epidemic);
#' the joint epidemic fit is added in later phases.
#'
#' @param model A [model()] object.
#' @param data Prepared-data list from [prepare_data()].
#' @param priors Optional prior bundle; defaults to [default_priors()].
#' @param init Optional named init list.
#' @param control `nlminb` control list.  `NULL` (the default) sizes the
#'   iteration budget to the number of free parameters -- see
#'   [.scaled_nlminb_control()].  A supplied list is used verbatim.
#' @param warn If `TRUE`, warn when the returned joint fit does not pass the
#'   optimizer adequacy checks. Internal warm-start and imputation fits set this
#'   to `FALSE` and report only diagnostics for the fits that affect the result.
#' @returns A list with `par` (named estimates), `obj`, `opt`, `data`, `priors`,
#'   `model`, `convergence`, and (delay-only) `delay_mu` / `delay_sigma`.
#' @export
fit <- function(model, data, priors = NULL, init = NULL,
                control = NULL,
                warn = TRUE) {
  priors <- priors %||% default_priors(model, data)
  hier   <- S7::S7_inherits(model, model_class) && model@strata_pooling == "hierarchical"

  # Checked here, not inside build_joint_obj(): `.fit_joint()` runs an init ladder
  # that swallows build errors, so a configuration mistake would surface as an
  # unhelpful "failed to converge for all init attempts".
  if (isTRUE(data$is_linelist_retraction == 1L) && is.null(priors$confirm_p))
    cli::cli_abort(c("The engine carries linelist retractions but the priors have no confirmation block.",
                     "i" = "Build the model with {.code model(revision = revision_process())}, or go through {.fn nowcast}, which attaches one automatically."))
  if (isTRUE(data$is_linelist_retraction == 1L) && isTRUE(priors$confirm_p$is_constant == 1L) &&
      isTRUE(priors$confirm_p$fixed >= 1) && data$n_retracted > 0)
    cli::cli_abort(c("`p = 1` says no report is ever retracted, but {data$n_retracted} retraction{?s} {?is/are} observed.",
                     "i" = "Leave `p` free (the default) or give it a prior, so the observed retractions have positive probability."))

  if (isTRUE(data$delay_only)) {
    return(.fit_delay_only(model, data, priors, init = init,
                           control = control %||%
                             list(iter.max = 500, eval.max = 1000, rel.tol = 1e-9)))
  }
  .fit_joint(model, data, priors, init = init, control = control,
             hierarchical_strata = hier, warn = warn)
}

#' Joint epidemic + delay fit (Laplace over the latent epidemic coefficients)
#'
#' Runs a small init ladder so a single bad start (flat epidemic + long-tail
#' delay -> non-finite gradient at iter 0, the documented Stage-2 gotcha) does
#' not sink an otherwise-fittable series.  Each rung perturbs the epidemic-level
#' intercept and (HSGP) the GP amplitude / (AR1) the innovation SD.
#' @keywords internal
#' @noRd
.joint_parameter_bounds <- function(par, settlement_horizon = 26L) {
  parameter_names <- names(par)
  lower <- rep(-Inf, length(par))
  upper <- rep(Inf, length(par))
  set_bounds <- function(pattern, lower_value, upper_value) {
    selected <- grepl(pattern, parameter_names)
    lower[selected] <<- lower_value
    upper[selected] <<- upper_value
  }
  delay_upper <- log(max(as.integer(settlement_horizon), 2L)) + 2
  set_bounds("^mu_intercept$|^mu_global$", -10, 16)
  set_bounds("^delay_mu$|^cumulative_retraction_mu$", -6, delay_upper)
  set_bounds("sigma_excess$|sigma_exc$", -8, delay_upper)
  set_bounds("^delay_Q$|^cumulative_retraction_Q$", -10, 10)
  set_bounds("cumulative_retraction_mass_raw$", -12, 12)
  set_bounds("^ar_phi_unc$|^log_ar_sigma_unc$", -10, 10)
  # The classical time-series trends are all bounded reparameterisations, so the
  # box only has to keep nlminb out of the flat tails where plogis() saturates
  # and the gradient stops carrying information.
  set_bounds("^arima_ar_pacf_unc$|^arima_ma_pacf_unc$|^log_arima_sigma_unc$", -10, 10)
  set_bounds("^log_ets_sigma_unc$|^ets_beta_unc$|^ets_damp_unc$", -10, 10)
  set_bounds("^log_sts_level_sigma_unc$|^log_sts_slope_sigma_unc$|^sts_slope_phi_unc$", -10, 10)
  set_bounds("^arima_drift$|^ets_drift$|^ets_slope_init$|^sts_slope_mean$|^sts_slope_init$", -5, 5)
  set_bounds("^log_gp_alpha$|^log_gp_ell$", -8, 8)
  set_bounds("^log_R0$", -6, 6)
  set_bounds("^u_gamma$|^u_neff$", -10, 10)
  set_bounds("^log_phi_nb$", -12, 8)
  set_bounds("^log_magnitude_size$", -8, 12)
  set_bounds("^movement_", -12, 12)
  list(lower = lower, upper = upper)
}

#' An `nlminb` budget sized to the problem, not to a constant
#'
#' The joint fit optimises one latent innovation per event-time, so its parameter
#' count is set by the DATA: 28 for an HSGP on any series, but 1,623 for a
#' structural time series on a 1,095-week one.  A fixed `iter.max = 500` is
#' generous for the first and nowhere near enough for the second, and the symptom
#' is not an error -- `nlminb` returns code 1, the fit is kept, and the only
#' trace is a `fit_check()` warning about a non-positive-definite Hessian.  On
#' the package's dengue series that silently affected six of the nine epidemic
#' processes: at 500 iterations ETS stopped with a maximum gradient of 6.6 and
#' Theta with 107; given room they reach 0.32 and 0.036.
#'
#' Raising a cap is free when it does not bind, which is what makes this safe:
#' a short series converges in the same number of steps and returns a
#' bit-identical objective.  Only fits that were previously stopping early cost
#' more, and those were the ones being reported wrong.
#'
#' @param n_parameters Number of free parameters in the tape.
#' @returns An `nlminb` control list.
#' @keywords internal
#' @noRd
.scaled_nlminb_control <- function(n_parameters) {
  # 25 iterations per parameter, which covers the worst case observed (a
  # structural trend on 1,095 weeks needed roughly 12 per parameter) with room
  # to spare, floored so small problems keep the budget they always had and
  # capped so a pathological fit cannot run forever.
  iterations <- max(500L, min(50000L, 25L * as.integer(n_parameters)))
  list(iter.max = iterations, eval.max = 2L * iterations, rel.tol = 1e-9)
}

#' How close the latent log-incidence may come to its softplus ceiling
#'
#' `prepare_data()` caps `log_mean` with
#' `ub - log1p(exp(ub - log_mean))`, a softplus, so the ceiling bites long
#' before it saturates: the fraction of `lambda` that survives it is exactly
#' `plogis(ub - log_mean)` -- 95.3% three log units below the ceiling, 88.1% two
#' below, 50% at it.  Three units is therefore where a fit stops being within a
#' rounding error of the model that was written down.
#' @keywords internal
#' @noRd
.log_mean_headroom_tolerance <- function() 3

#' Is the fitted latent incidence pressed against its ceiling?
#'
#' The comparison has to use the UNCAPPED `log_mean` (`rc$mu`).  `rc$mu_safe`
#' approaches the bound asymptotically and never reaches it, so a check written
#' against it can never fire.
#'
#' @param log_mean Uncapped latent log-incidence, any shape.
#' @param upper_bound `data$mu_log_upper_bound`.
#' @param tolerance Headroom, in log units, below which the cap is reported.
#' @returns A list with the bound, the peak `log_mean`, their gap, a flag, and a
#'   human-readable `reason` (`character(0)` when the cap is not binding).
#' @keywords internal
#' @noRd
.log_mean_cap_diagnostic <- function(log_mean, upper_bound,
                                     tolerance = .log_mean_headroom_tolerance()) {
  empty <- list(
    log_mean_upper_bound = NA_real_, max_log_mean = NA_real_,
    log_mean_headroom = NA_real_, log_mean_cap_bound = FALSE,
    reason = character()
  )
  bound <- suppressWarnings(as.numeric(upper_bound %||% NA_real_))
  if (length(bound) != 1L || !is.finite(bound)) return(empty)
  values <- suppressWarnings(as.numeric(log_mean))
  values <- values[is.finite(values)]
  if (!length(values)) return(empty)

  peak <- max(values)
  headroom <- bound - peak
  bound_binding <- headroom < tolerance
  reason <- if (bound_binding) {
    paste0(
      "latent log_mean within ", signif(headroom, 3),
      " of its upper bound ", signif(bound, 4),
      " (softplus cap keeps ", signif(100 * stats::plogis(headroom), 3),
      "% of peak lambda)"
    )
  } else {
    character()
  }
  list(
    log_mean_upper_bound = bound, max_log_mean = peak,
    log_mean_headroom = headroom, log_mean_cap_bound = bound_binding,
    reason = reason
  )
}

#' Default threshold for the ARMA near-collinearity report
#'
#' Calibrated in `devel/calibrate_arima_ridge.R` over 65 fits spanning every
#' series length in the validation set, against the observable consequence --
#' the ratio of the ARIMA(p,d,q) 90% band to the ARIMA(p+q,d,0) band on the same
#' cell, with a blow-up defined as 5x.  The two populations separate cleanly:
#'
#' | band ratio | n  | min   | median | max   |
#' |------------|----|-------|--------|-------|
#' | <= 5x      | 35 | 0.044 | 0.198  | 0.423 |
#' | > 5x       | 30 | 0.157 | 0.834  | 0.921 |
#'
#' Every cut in `[0.5, 0.7]` flags the identical 24 fits with ZERO false alarms,
#' so the value is taken from the middle of the empty band between 0.423 and
#' 0.777 rather than from either edge.
#'
#' **What it catches, and what it does not.**  With `q = 1` it caught 20 of 20
#' blow-ups -- 10 of 10 on ARIMA(1,1,1) and 10 of 10 on ARIMA(2,1,1) -- and those
#' fits score within 1 nll unit of their pure-AR reference, which is the
#' signature of a flat ridge.  The 6 it missed are all `q = 2`, and they are a
#' DIFFERENT failure: their AR x MA correlation is 0.16-0.26, their within-MA
#' correlation is lower still (0.10-0.13), and they beat their reference by 8.6
#' to 37.0 nll units.  That is overfitting, not non-identifiability, and no
#' correlation statistic should be expected to flag it.  A quiet check is
#' therefore not a promise that the interval is sound.
#' @keywords internal
#' @noRd
.arma_ridge_tolerance <- function() 0.6

#' Are the AR and MA coefficients trading against each other along a flat ridge?
#'
#' An ARMA(p, q) with `p >= 1` and `q >= 1` can sit near a common factor, where
#' the AR and MA polynomials nearly cancel.  The likelihood is then almost flat
#' along the direction that increases one coefficient and decreases the other,
#' so the pair is only weakly identified -- but the fit still CONVERGES, and the
#' Hessian is still positive definite, because the flatness is a 2x2 sub-block
#' and not a global near-singularity (`min|eig|` around 0.9 on the worst cells
#' measured).  `hessian_positive_definite` therefore cannot see it.
#'
#' What it costs is the predictive interval: the two ends of the ridge imply very
#' different variance over the unobserved tail.  On a 1,000-week series an
#' ARIMA(1,1,1) whose band was 784x the equivalent ARIMA(2,1,0)'s scored an
#' objective within 0.2 of it.
#'
#' The statistic is the largest absolute correlation between any AR and any MA
#' coordinate in the Laplace covariance.  It is free of the parameter count,
#' unlike `cond(H)`, which on these fits is dominated by `max_time`.
#'
#' Computed from the Cholesky factor of the free Hessian block by solving for the
#' handful of columns needed, rather than inverting it.
#'
#' @param par_names Names of the full parameter vector.
#' @param free Logical over that vector: coordinates not fixed at a bound.
#' @param free_factor `Matrix::Cholesky` of the free Hessian block.
#' @param tolerance Absolute correlation at or above which the ridge is reported.
#' @returns A list with the correlation, a flag and a `reason`.
#' @keywords internal
#' @noRd
.arma_ridge_diagnostic <- function(par_names, free, free_factor,
                                   tolerance = .arma_ridge_tolerance()) {
  empty <- list(
    arma_ridge_correlation = NA_real_, arma_ridge = FALSE, reason = character()
  )
  if (is.null(free_factor) || is.null(par_names) || is.null(free)) return(empty)
  ar_full <- grep("^arima_ar_pacf_unc", par_names)
  ma_full <- grep("^arima_ma_pacf_unc", par_names)
  if (!length(ar_full) || !length(ma_full)) return(empty)

  position <- rep(NA_integer_, length(free))
  position[free] <- seq_len(sum(free))
  ar <- position[ar_full]; ar <- ar[!is.na(ar)]
  ma <- position[ma_full]; ma <- ma[!is.na(ma)]
  if (!length(ar) || !length(ma)) return(empty)

  wanted <- unique(c(ar, ma))
  rhs <- matrix(0, sum(free), length(wanted))
  rhs[cbind(wanted, seq_along(wanted))] <- 1
  columns <- tryCatch(
    as.matrix(Matrix::solve(free_factor, rhs, system = "A")),
    error = function(e) NULL
  )
  if (is.null(columns) || any(!is.finite(columns))) return(empty)
  at <- setNames(seq_along(wanted), as.character(wanted))
  covariance <- function(i, j) columns[i, at[[as.character(j)]]]

  strongest <- 0
  for (a in ar) for (m in ma) {
    var_a <- covariance(a, a); var_m <- covariance(m, m)
    if (!is.finite(var_a) || !is.finite(var_m) || var_a <= 0 || var_m <= 0) next
    correlation <- covariance(a, m) / sqrt(var_a * var_m)
    if (is.finite(correlation) && abs(correlation) > abs(strongest)) {
      strongest <- correlation
    }
  }
  if (!is.finite(strongest) || strongest == 0) return(empty)

  binding <- abs(strongest) >= tolerance
  list(
    arma_ridge_correlation = strongest,
    arma_ridge = binding,
    reason = if (binding) {
      paste0(
        "AR and MA coefficients near-collinear in the fitted curvature ",
        "(correlation ", signif(strongest, 3),
        "): weakly identified, so the predictive interval is poorly determined"
      )
    } else {
      character()
    }
  )
}

#' Which coordinates of a box-constrained mode are still moving
#'
#' Shared by the adequacy diagnostic and the Newton refinement, so the two
#' cannot disagree about the active set.  Under strict complementarity a
#' coordinate sitting on a bound with a non-zero, correctly signed multiplier
#' is locally fixed and the curvature in that direction is irrelevant.  Weakly
#' active coordinates (approximately zero multiplier) stay free, which is
#' deliberately conservative.
#'
#' @param par The mode, as a plain numeric vector.
#' @param gradient The gradient at `par`.
#' @param bounds A list with `lower` and `upper`.
#' @param apply_signs Whether to zero the KKT residual on correctly signed
#'   active coordinates.  `FALSE` when the gradient is not finite, where the
#'   sign test is meaningless.
#' @returns A list with `at_lower`, `at_upper`, `kkt_residual` and `free`.
#' @keywords internal
#' @noRd
.box_active_set <- function(par, gradient, bounds, apply_signs = TRUE) {
  lower <- as.numeric(bounds$lower)
  upper <- as.numeric(bounds$upper)
  bound_scale <- pmax(
    1, abs(par),
    ifelse(is.finite(lower), abs(lower), 0),
    ifelse(is.finite(upper), abs(upper), 0)
  )
  bound_tolerance <- sqrt(.Machine$double.eps) * bound_scale
  at_lower <- is.finite(lower) & par <= lower + bound_tolerance
  at_upper <- is.finite(upper) & par >= upper - bound_tolerance

  kkt_residual <- gradient
  free <- rep(TRUE, length(par))
  if (isTRUE(apply_signs)) {
    # At a lower bound, g >= 0 satisfies KKT; at an upper bound, g <= 0 does.
    kkt_residual[at_lower & gradient >= 0] <- 0
    kkt_residual[at_upper & gradient <= 0] <- 0
    multiplier_scale <- max(1, if (length(gradient)) max(abs(gradient)) else 0)
    multiplier_tolerance <- sqrt(.Machine$double.eps) * multiplier_scale
    free <- !(
      (at_lower & gradient > multiplier_tolerance) |
      (at_upper & gradient < -multiplier_tolerance)
    )
  }
  list(at_lower = at_lower, at_upper = at_upper,
       kkt_residual = kkt_residual, free = free)
}

#' Put an RTMB tape back on a chosen point, including `last.par.best`
#'
#' `obj$fn()` records a new `last.par.best` whenever it sees a lower objective,
#' and `.nowcast_draws()` samples the Laplace posterior at exactly that vector.
#' A line search that evaluates a better-but-rejected candidate therefore
#' leaves the draws pointed at a point the fit discarded -- which is how a
#' rejected step can still cost the precision matrix a ridge, long after the
#' optimizer state looks correct.  Re-evaluating the kept point is not enough,
#' because its objective is by construction the higher one.
#' @keywords internal
#' @noRd
.restore_tape_best <- function(obj, par, value = NULL) {
  tryCatch(obj$fn(par), error = function(e) NULL)
  env <- tryCatch(obj$env, error = function(e) NULL)
  if (is.null(env)) return(invisible(NULL))
  best <- tryCatch(env$last.par.best, error = function(e) NULL)
  # Under `use_random = TRUE` the best vector spans fixed AND random
  # coordinates and is longer than `par`; leave it to RTMB in that case.
  if (is.null(best) || length(best) != length(par)) return(invisible(NULL))
  try(assign("last.par.best", stats::setNames(as.numeric(par), names(best)),
             envir = env), silent = TRUE)
  if (!is.null(value) && is.finite(value))
    try(assign("value.best", as.numeric(value), envir = env), silent = TRUE)
  invisible(NULL)
}

#' Take the Newton step the adequacy check has already priced
#'
#' `.joint_fit_diagnostic()` reports `quadratic_gap = 0.5 r' H_FF^-1 r`, which
#' IS the objective decrease a Newton step on the free subspace would buy.
#' When that gap is the only thing standing between a fit and adequacy, and the
#' Hessian it was computed from is positive definite, nothing has gone wrong:
#' the optimizer has stopped just short of a step it can compute.  Reporting
#' that as a failed fit while holding the cure is not useful to a caller.
#'
#' It happens where one hyperparameter is coupled to thousands of latent
#' states.  On a 985-week dengue series `sts_epidemic("semilocal")` fits
#' `slope_phi` at about 0, which leaves `slope_mean` as a pure linear drift on
#' the level -- the same path a uniform shift of all 985 level innovations
#' traces.  Neither `nlminb`'s quasi-Newton nor a five-vector L-BFGS-B finds
#' that 986-coordinate direction, so the fit stopped with a gradient of 1.68 in
#' ONE coordinate (`sts_slope_mean`; every other coordinate was below 0.052)
#' and a gap of 0.019 against a tolerance of 0.01, on an objective of 63,405.
#' Eight Newton steps took 11 seconds on an 8-minute fit and moved it to a
#' gradient of 5.4e-08 and a gap of 2.8e-20.
#'
#' Only a fit that would otherwise be reported inadequate pays for this: the
#' diagnostic that gates it has already been computed by the caller, and the
#' loop stops as soon as the remaining gap is under tolerance.
#'
#' @param diagnostic The `.joint_fit_diagnostic()` result at `opt`.
#' @param quadratic_gap_tolerance The tolerance the diagnostic used.
#' @param max_steps Newton steps to attempt before giving up.
#' @returns A list with `opt` (refined or unchanged), `applied`, `attempted`,
#'   `steps`, `objective_change`, the gradient either side and `reason`.
#' @keywords internal
#' @noRd
.refine_on_quadratic_gap <- function(obj, opt, bounds, diagnostic,
                                     quadratic_gap_tolerance = 0.01,
                                     max_steps = 8L) {
  unchanged <- function(reason) list(
    opt = opt, applied = FALSE, attempted = FALSE, steps = 0L,
    objective_change = 0, max_gradient_before = NA_real_,
    max_gradient_after = NA_real_, reason = reason
  )
  # Snapshot before any evaluation, so a decline can undo the line search.
  best_at_entry <- tryCatch(obj$env$last.par.best, error = function(e) NULL)
  value_at_entry <- tryCatch(obj$env$value.best, error = function(e) NULL)
  if (isTRUE(diagnostic$adequate)) return(unchanged("already_adequate"))
  # A Hessian that is not positive definite is a different failure with a
  # different remedy, and offers no descent direction to solve for.
  if (!isTRUE(diagnostic$hessian_positive_definite))
    return(unchanged("hessian_not_positive_definite"))
  if (!isTRUE(diagnostic$finite_objective) ||
      !isTRUE(diagnostic$finite_gradient))
    return(unchanged("nonfinite_fit"))
  gap <- diagnostic$quadratic_gap
  if (!is.finite(gap) || gap <= quadratic_gap_tolerance)
    return(unchanged("gap_not_binding"))
  if (is.null(obj$he)) return(unchanged("no_analytic_hessian"))

  parameter_names <- names(opt$par)
  par <- as.numeric(opt$par)
  objective_at_start <- as.numeric(opt$objective)
  gradient_at_start <- tryCatch(max(abs(obj$gr(opt$par))),
                                error = function(e) NA_real_)
  lower <- as.numeric(bounds$lower)
  upper <- as.numeric(bounds$upper)
  named <- function(values) stats::setNames(values, parameter_names)
  steps_taken <- 0L
  # The point whose curvature was last certified positive definite.  A Newton
  # step is a LOCAL model: on a badly conditioned problem a full step can lower
  # the objective and still land where the Hessian is indefinite, which costs
  # the Laplace precision a ridge and so the posterior draws.  Never return a
  # point worse in that sense than the one stepped from.
  last_certified <- par
  certified_steps <- 0L
  for (step in seq_len(max_steps)) {
    gradient <- tryCatch(as.numeric(obj$gr(named(par))), error = function(e) NULL)
    if (is.null(gradient) || any(!is.finite(gradient))) break
    active <- .box_active_set(par, gradient, bounds)
    free <- active$free
    if (!any(free)) break
    hessian <- tryCatch(obj$he(named(par)), error = function(e) NULL)
    if (is.null(hessian) || any(!is.finite(hessian))) break
    hessian <- Matrix::forceSymmetric(Matrix::Matrix(hessian, sparse = TRUE))
    factor <- tryCatch(
      suppressWarnings(Matrix::Cholesky(
        hessian[free, free, drop = FALSE], LDL = FALSE, perm = TRUE, super = TRUE
      )),
      error = function(e) NULL, warning = function(w) NULL
    )
    if (is.null(factor)) break
    last_certified <- par
    certified_steps <- steps_taken
    direction <- tryCatch(
      as.numeric(Matrix::solve(
        factor, matrix(active$kkt_residual[free], ncol = 1L), system = "A"
      )),
      error = function(e) NULL
    )
    if (is.null(direction) || any(!is.finite(direction))) break
    # The same quantity the adequacy check reports, recomputed here: once it is
    # under tolerance there is nothing left to buy.
    remaining <- 0.5 * sum(active$kkt_residual[free] * direction)
    if (steps_taken > 0L && is.finite(remaining) &&
        remaining <= quadratic_gap_tolerance) break
    # The gap prices the FULL step.  A backtracking line search keeps the
    # refinement monotone where the quadratic model overshoots.
    current <- tryCatch(as.numeric(obj$fn(named(par))), error = function(e) NA_real_)
    if (!is.finite(current)) break
    accepted <- NULL
    for (scale in c(1, 0.5, 0.25, 0.1, 0.01)) {
      candidate <- par
      candidate[free] <- candidate[free] - scale * direction
      candidate <- pmin(pmax(candidate, lower), upper)
      value <- tryCatch(as.numeric(obj$fn(named(candidate))),
                        error = function(e) NA_real_)
      if (is.finite(value) && value <= current) { accepted <- candidate; break }
    }
    if (is.null(accepted)) break
    par <- accepted
    steps_taken <- step
  }
  # A step whose landing point never had its curvature certified is discarded,
  # even though it lowered the objective.
  par <- last_certified
  steps_taken <- certified_steps
  if (steps_taken == 0L) {
    # The line search may have evaluated a rejected candidate that RTMB
    # recorded as `last.par.best`; undo that, or the draws sample from it.
    .restore_tape_best(obj, best_at_entry %||% opt$par, value_at_entry)
    return(unchanged("no_certified_step"))
  }

  refined <- named(par)
  objective <- tryCatch(as.numeric(obj$fn(refined)), error = function(e) NA_real_)
  if (!is.finite(objective) || objective > objective_at_start) {
    .restore_tape_best(obj, best_at_entry %||% opt$par, value_at_entry)
    return(unchanged("refinement_did_not_improve"))
  }
  gradient_after <- tryCatch(max(abs(obj$gr(refined))), error = function(e) NA_real_)
  # A later rejected step may have been lower still; the KEPT point is the one
  # the draws must use.
  .restore_tape_best(obj, refined, objective)
  opt$par <- refined
  opt$objective <- objective
  opt$message <- paste0(
    "Newton refinement on the free subspace (", steps_taken, " step",
    if (steps_taken == 1L) "" else "s", "); ",
    opt$message %||% "no optimizer message"
  )
  list(
    opt = opt, applied = TRUE, attempted = TRUE, steps = steps_taken,
    objective_change = objective - objective_at_start,
    max_gradient_before = gradient_at_start,
    max_gradient_after = gradient_after,
    reason = "accepted"
  )
}

#' Mathematically coherent diagnostics for a box-constrained joint fit
#'
#' The raw maximum gradient is retained for debugging, but adequacy is based on
#' the KKT residual, positive curvature, and the curvature-scaled estimate
#' `0.5 * r' H^-1 r` of the objective decrease still available locally. The
#' latter is invariant under invertible linear reparameterisations and gives the
#' tolerance (`0.01` by default) a log-posterior interpretation.
#'
#' The softplus ceiling on the latent `log_mean` is reported alongside them.
#' It is not an optimizer property -- a cap-bound fit can converge perfectly --
#' so it does not enter `adequate`, but it does set `status` to `"warning"`,
#' because the quantity being reported is no longer the one the model wrote
#' down.  See `.log_mean_cap_diagnostic()`.
#'
#' @param log_mean The UNCAPPED latent log-incidence from `.joint_reconstruct()`
#'   (`rc$mu`, not `rc$mu_safe`).  `NULL` when no reconstruction is available.
#' @param log_mean_upper_bound `data$mu_log_upper_bound`, the softplus ceiling.
#' @param log_mean_headroom_tolerance Headroom below which the cap is reported
#'   as binding, in log units.
#' @keywords internal
#' @noRd
.joint_fit_diagnostic <- function(obj, opt, bounds,
                                  finite_reconstruction = TRUE,
                                  quadratic_gap_tolerance = 0.01,
                                  log_mean = NULL,
                                  log_mean_upper_bound = NA_real_,
                                  log_mean_upper_bound_legacy = NA_real_,
                                  log_mean_headroom_tolerance =
                                    .log_mean_headroom_tolerance(),
                                  count_cumulative = FALSE) {
  objective <- as.numeric(opt$objective %||% opt$value %||% NA_real_)
  par <- as.numeric(opt$par)
  names(par) <- names(opt$par)
  convergence <- as.integer(opt$convergence %||% NA_integer_)
  gradient <- tryCatch(as.numeric(obj$gr(opt$par)), error = function(e) NA_real_)
  if (length(gradient) == length(par)) names(gradient) <- names(par)

  finite_objective <- length(objective) == 1L && is.finite(objective)
  finite_gradient <- length(gradient) == length(par) && all(is.finite(gradient))
  max_gradient <- if (finite_gradient && length(gradient)) {
    max(abs(gradient))
  } else if (length(gradient) == 0L) {
    0
  } else {
    NA_real_
  }

  active <- .box_active_set(par, gradient, bounds, apply_signs = finite_gradient)
  at_lower <- active$at_lower
  at_upper <- active$at_upper
  kkt_residual <- active$kkt_residual
  projected_gradient <- if (finite_gradient && length(kkt_residual)) {
    max(abs(kkt_residual))
  } else if (length(kkt_residual) == 0L) {
    0
  } else {
    NA_real_
  }

  hessian_positive_definite <- FALSE
  retained_factor <- NULL
  hessian_status <- "unavailable"
  hessian_source <- "unavailable"
  quadratic_gap <- NA_real_
  curvature_dimension <- 0L
  free_coordinates <- rep(TRUE, length(par))
  names(free_coordinates) <- names(par)
  if (finite_gradient && length(par) == 0L) {
    hessian_positive_definite <- TRUE
    hessian_status <- "no_free_parameters"
    hessian_source <- "not_applicable"
    quadratic_gap <- 0
  } else if (finite_gradient) {
    hessian <- tryCatch(obj$he(opt$par), error = function(e) NULL)
    if (!is.null(hessian)) hessian_source <- "analytic"
    if (is.null(hessian)) {
      # A Laplace-marginal RTMB objective has an analytic gradient but `he()`
      # is unavailable with some RTMB/TMB builds.  optimHess differentiates
      # that gradient by centred differences, providing the same local
      # observed-curvature object needed by the second-order adequacy check.
      hessian <- tryCatch(
        stats::optimHess(opt$par, obj$fn, obj$gr),
        error = function(e) NULL
      )
      if (!is.null(hessian)) hessian_source <- "finite_difference"
    }
    if (!is.null(hessian)) {
      if (any(!is.finite(hessian))) {
        hessian_status <- "nonfinite"
      } else {
        hessian <- Matrix::forceSymmetric(Matrix::Matrix(hessian, sparse = TRUE))
        # Under strict complementarity, an active coordinate with a non-zero,
        # correctly signed KKT multiplier is locally fixed. Second-order
        # sufficiency therefore concerns H_FF, not curvature in an infeasible
        # direction. Weakly active coordinates (approximately zero multiplier)
        # remain in F, which is deliberately conservative.
        free <- active$free
        free_coordinates <- free
        curvature_dimension <- sum(free)
        if (!any(free)) {
          hessian_positive_definite <- TRUE
          hessian_status <- "no_free_parameters"
          quadratic_gap <- 0
        } else {
          free_hessian <- hessian[free, free, drop = FALSE]
          free_factor <- tryCatch(
            suppressWarnings(Matrix::Cholesky(
              free_hessian, LDL = FALSE, perm = TRUE, super = TRUE
            )),
            error = function(e) NULL,
            warning = function(w) NULL
          )
          if (is.null(free_factor)) {
            hessian_status <- "not_positive_definite_on_free_subspace"
          } else {
            retained_factor <- free_factor
            hessian_positive_definite <- TRUE
            hessian_status <- if (all(free)) {
              "positive_definite"
            } else {
              "positive_definite_on_free_subspace"
            }
            residual <- matrix(kkt_residual[free], ncol = 1L)
            newton_step <- tryCatch(
              Matrix::solve(free_factor, residual, system = "A"),
              error = function(e) NULL
            )
            if (!is.null(newton_step)) {
              quadratic_gap <- 0.5 * as.numeric(
                crossprod(kkt_residual[free], as.numeric(newton_step))
              )
              if (quadratic_gap < 0 &&
                  abs(quadratic_gap) <= sqrt(.Machine$double.eps)) {
                quadratic_gap <- 0
              }
            }
          }
        }
      }
    }
  }

  reasons <- character()
  if (!finite_objective) reasons <- c(reasons, "non-finite objective")
  if (is.na(convergence) || convergence != 0L) {
    reasons <- c(reasons, paste0("optimizer code ", convergence))
  }
  if (!finite_gradient) reasons <- c(reasons, "non-finite gradient")
  if (!hessian_positive_definite) {
    reasons <- c(reasons, paste0("Hessian ", hessian_status))
  }
  if (!is.finite(quadratic_gap)) {
    reasons <- c(reasons, "quadratic objective gap unavailable")
  } else if (quadratic_gap > quadratic_gap_tolerance) {
    reasons <- c(
      reasons,
      paste0(
        "quadratic objective gap ", signif(quadratic_gap, 4),
        " exceeds ", quadratic_gap_tolerance
      )
    )
  }
  if (!isTRUE(finite_reconstruction)) {
    reasons <- c(reasons, "non-finite reconstructed incidence")
  }

  # The cap is deliberately kept out of `adequate`: the optimizer can be at a
  # textbook mode and still be reporting a truncated epidemic, and the two
  # failures want different remedies.  It is carried in `reasons` and in
  # `status` so no caller has to know to look for it.
  cap <- .log_mean_cap_diagnostic(
    log_mean, log_mean_upper_bound, log_mean_headroom_tolerance
  )
  # On a COUNT-CUMULATIVE stream the horizon-0 nowcast is built by the cohort
  # kernels from the observed cumulative, not from `lambda`, so a saturated cap
  # has no predictive consequence there.  Measured on flusight: lifting the
  # bound from ~12 to 20 moved the median 0.5% and -0.2% and the objective by
  # 0.2-0.5 (noise), while `lambda` peaked at t=62/102 and t=165/408 -- the
  # interior of the series, nowhere near the event-time being scored.  Left
  # reportable it fires on 52-65% of flusight fits and teaches callers to ignore
  # a warning that matters a great deal on the count-incidence path.  The fact
  # is still recorded in `log_mean_cap_bound`; only the alarm is suppressed.
  cap_reportable <- isTRUE(cap$log_mean_cap_bound) && !isTRUE(count_cumulative)

  # Kept out of `adequate` for the same reason the cap is: the optimizer has
  # arrived, at a mode that happens to sit on a flat ridge.  Convergence and
  # identifiability are different failures and want different remedies.
  ridge <- .arma_ridge_diagnostic(names(par), free_coordinates, retained_factor)

  list(
    adequate = length(reasons) == 0L,
    status = if (length(reasons) == 0L && !cap_reportable && !ridge$arma_ridge) {
      "pass"
    } else {
      "warning"
    },
    reasons = unique(c(
      reasons,
      if (cap_reportable) cap$reason else character(),
      ridge$reason
    )),
    log_mean_cap_reportable = cap_reportable,
    arma_ridge_correlation = ridge$arma_ridge_correlation,
    arma_ridge = ridge$arma_ridge,
    log_mean_upper_bound = cap$log_mean_upper_bound,
    log_mean_upper_bound_legacy = as.numeric(log_mean_upper_bound_legacy %||% NA_real_),
    max_log_mean = cap$max_log_mean,
    log_mean_headroom = cap$log_mean_headroom,
    log_mean_cap_bound = cap$log_mean_cap_bound,
    log_mean_headroom_tolerance = log_mean_headroom_tolerance,
    finite_objective = finite_objective,
    optimizer_convergence = convergence,
    finite_gradient = finite_gradient,
    max_gradient = max_gradient,
    projected_gradient = projected_gradient,
    active_bounds = sum(at_lower | at_upper),
    curvature_dimension = curvature_dimension,
    free_coordinates = free_coordinates,
    hessian_positive_definite = hessian_positive_definite,
    hessian_status = hessian_status,
    hessian_source = hessian_source,
    quadratic_gap = quadratic_gap,
    quadratic_gap_tolerance = quadratic_gap_tolerance,
    finite_reconstruction = isTRUE(finite_reconstruction)
  )
}

#' @keywords internal
#' @noRd
.fit_is_adequate <- function(fit) {
  isTRUE(fit$diagnostic$adequate %||% identical(fit$fit_status, "pass"))
}

#' Select the MAP candidate after applying the adequacy predicate
#'
#' Adequate candidates are preferred categorically and are compared by their
#' objective. If none is adequate, the lowest-objective finite candidate is
#' returned as an explicitly degraded fit or internal initializer.
#' @keywords internal
#' @noRd
.select_joint_candidate <- function(candidates) {
  if (length(candidates) == 0L) return(NULL)
  adequate <- vapply(candidates, .fit_is_adequate, logical(1))
  eligible <- if (any(adequate)) which(adequate) else seq_along(candidates)
  objectives <- vapply(candidates[eligible], function(candidate) {
    value <- candidate$nll %||% candidate$opt$objective %||% Inf
    if (length(value) != 1L || !is.finite(value)) Inf else as.numeric(value)
  }, numeric(1))
  candidates[[eligible[[which.min(objectives)]]]]
}

#' @keywords internal
#' @noRd
.fit_diagnostic_summary <- function(fit) {
  diagnostic <- fit$diagnostic %||% list()
  list(
    status = diagnostic$status %||% fit$fit_status %||% "unknown",
    adequate = isTRUE(diagnostic$adequate),
    convergence = as.integer(
      diagnostic$optimizer_convergence %||% fit$convergence %||% NA_integer_
    ),
    objective = as.numeric(fit$nll %||% fit$opt$objective %||% NA_real_),
    max_gradient = as.numeric(
      diagnostic$max_gradient %||% fit$max_gradient %||% NA_real_
    ),
    projected_gradient = as.numeric(
      diagnostic$projected_gradient %||% fit$projected_gradient %||% NA_real_
    ),
    quadratic_gap = as.numeric(
      diagnostic$quadratic_gap %||% fit$quadratic_gap %||% NA_real_
    ),
    hessian_positive_definite = isTRUE(
      diagnostic$hessian_positive_definite %||%
        fit$hessian_positive_definite
    ),
    hessian_status = as.character(
      diagnostic$hessian_status %||% fit$hessian_status %||% "unknown"
    ),
    log_mean_upper_bound = as.numeric(
      diagnostic$log_mean_upper_bound %||% fit$log_mean_upper_bound %||% NA_real_
    ),
    max_log_mean = as.numeric(
      diagnostic$max_log_mean %||% fit$max_log_mean %||% NA_real_
    ),
    log_mean_headroom = as.numeric(
      diagnostic$log_mean_headroom %||% fit$log_mean_headroom %||% NA_real_
    ),
    log_mean_cap_bound = isTRUE(
      diagnostic$log_mean_cap_bound %||% fit$log_mean_cap_bound
    ),
    # Whether that binding is worth telling the caller about: FALSE on a
    # count-cumulative fit, where the horizon-0 nowcast comes from the cohort
    # kernels rather than `lambda` and a saturated cap moves it by well under a
    # percent.  Falls back to the engine flag for a fit serialized before this
    # field existed, so an old saved nowcast still reports sensibly.
    arma_ridge_correlation = as.numeric(
      diagnostic$arma_ridge_correlation %||% fit$arma_ridge_correlation %||% NA_real_
    ),
    arma_ridge = isTRUE(diagnostic$arma_ridge %||% fit$arma_ridge),
    log_mean_cap_reportable = {
      # `%||%` can still yield NULL when neither source carries the field, and
      # `NULL && x` is an error, not FALSE.  Resolve the field first, then fall
      # back to deriving it, coercing each operand with isTRUE().
      recorded <- diagnostic$log_mean_cap_reportable %||%
        fit$log_mean_cap_reportable
      if (!is.null(recorded)) {
        isTRUE(recorded)
      } else {
        isTRUE(diagnostic$log_mean_cap_bound %||% fit$log_mean_cap_bound) &&
          !isTRUE(fit$data$is_count_cumulative == 1L)
      }
    },
    log_mean_upper_bound_legacy = as.numeric(
      diagnostic$log_mean_upper_bound_legacy %||%
        fit$log_mean_upper_bound_legacy %||% NA_real_
    ),
    reasons = diagnostic$reasons %||% fit$diagnostic_reasons %||% character()
  )
}

#' Attach the common diagnostic contract to an existing objective fit
#'
#' Delay-only fits predate the joint-fit diagnostic fields. This adapter makes
#' their Stage-1 Laplace status auditable without changing their public return
#' shape or optimizer.
#' @keywords internal
#' @noRd
.attach_fit_diagnostic <- function(fit, bounds = NULL) {
  if (!is.null(fit$diagnostic)) return(fit)
  if (is.null(fit$obj)) return(fit)
  par <- fit$opt$par %||%
    tryCatch(fit$obj$env$last.par.best, error = function(e) NULL) %||%
    fit$obj$par
  if (is.null(par)) return(fit)
  opt <- list(
    par = par,
    objective = fit$nll %||% fit$opt$objective %||% fit$obj$fn(par),
    convergence = fit$convergence %||% fit$opt$convergence %||% NA_integer_
  )
  if (is.null(bounds)) {
    bounds <- list(
      lower = rep(-Inf, length(par)),
      upper = rep(Inf, length(par))
    )
  }
  diagnostic <- .joint_fit_diagnostic(fit$obj, opt, bounds)
  fit$max_gradient <- diagnostic$max_gradient
  fit$projected_gradient <- diagnostic$projected_gradient
  fit$quadratic_gap <- diagnostic$quadratic_gap
  fit$hessian_positive_definite <- diagnostic$hessian_positive_definite
  fit$hessian_status <- diagnostic$hessian_status
  fit$log_mean_headroom <- diagnostic$log_mean_headroom
  fit$log_mean_cap_bound <- diagnostic$log_mean_cap_bound
  fit$log_mean_upper_bound_legacy <- diagnostic$log_mean_upper_bound_legacy
  fit$fit_status <- diagnostic$status
  fit$diagnostic_reasons <- diagnostic$reasons
  fit$gradient_status <- if (!diagnostic$finite_gradient) "nonfinite" else
    diagnostic$status
  fit$diagnostic <- diagnostic
  fit
}

#' Warn that the latent incidence is pressed against its ceiling
#'
#' Shared by `fit()`, `.finish_nowcast_collection()` and `fit_check()` so the
#' three entry points say the same thing.  `headroom` is one value per fit;
#' non-binding fits are dropped by the caller or ignored here.
#'
#' The warning points BOTH ways, because a cap-bound fit is ambiguous on its
#' own.  Either the stream genuinely needs that much inflation -- early-2020
#' `covid_us` needs 37-83x and the pre-2.5.0 ceiling made it nowcast 8.7% of the
#' settled count -- or the process is running away and the ceiling was the only
#' thing holding it down, which is what `sir_epidemic()` does on
#' `covid_colombia`'s 2020 growth phase.  Nothing in the fit distinguishes them,
#' so the warning names the tighter legacy bound as something to TRY rather than
#' as a diagnosis, and sends the reader to `reporting_fraction()`, which shows
#' the multiplier being applied and is the number that actually settles it.
#'
#' @param headroom One value per cap-bound fit.
#' @param bound The ceiling(s) those fits were taped with.
#' @param legacy_bound The pre-2.5.0 ceiling, `min(max(6, log1p(casemax)), 16)`,
#'   quoted so the suggestion is a number the caller can paste. Suppressed when
#'   it is not actually tighter than the bound in force.
#' @keywords internal
#' @noRd
#' Warn that an ARMA pair is only weakly identified
#'
#' Deliberately phrased as something to CHECK rather than something that is
#' definitely wrong.  A high correlation is a reason to look at the interval, not
#' proof that the interval is bad: the statistic is measured on the curvature,
#' and whether it matters depends on how far the predictive has to extrapolate.
#' @keywords internal
#' @noRd
.warn_arma_ridge <- function(correlation, n_fits = length(correlation),
                             context = "fit") {
  correlation <- correlation[is.finite(correlation)]
  if (!length(correlation)) return(invisible(FALSE))
  strongest <- correlation[which.max(abs(correlation))]
  # The subject is built here rather than with cli's `{?s}`: the quantity that
  # governs it is `length(correlation)`, but `{context}` sits between the two and
  # is itself length 1, which resets cli's pluralization to the singular.
  subject <- paste0(context, if (length(correlation) == 1L) "" else "s")
  verb <- if (length(correlation) == 1L) "has" else "have"
  cli::cli_warn(c(
    "{length(correlation)} of {n_fits} {subject} {verb} near-collinear AR and MA coefficients.",
    "x" = "Their correlation in the fitted curvature is {format(strongest, digits = 3)}: the likelihood is nearly flat along the direction that trades one against the other, so the pair is only weakly identified.",
    "i" = "The fit still converged and the Hessian is still positive definite -- the flat direction is a 2x2 block, not a global one, so {.code hessian_positive_definite} cannot see it. What it costs is the INTERVAL, not the point estimate.",
    "i" = "Compare against the pure-AR model of the same total order, {.code arima_epidemic(p = p + q, d = d, q = 0)}: on a 1,000-week series an ARIMA(1,1,1) produced a 90% band 784 times wider than the matching ARIMA(2,1,0) while scoring an objective within 0.2 of it.",
    "i" = "Check the nowcast interval either way -- {.fn autoplot}, or the {.code q5}/{.code q95} columns of {.fn predict} -- and prefer the narrower model when the two agree on the median.",
    "i" = "This check finds near-collinearity only. An ARMA with {.code q >= 2} can widen its interval by overfitting instead, which leaves the correlation low, so a silent check is not a guarantee that the interval is sound."
  ))
  invisible(TRUE)
}

.warn_log_mean_cap <- function(headroom, bound, n_fits = length(headroom),
                               context = "fit", legacy_bound = NA_real_) {
  headroom <- headroom[is.finite(headroom)]
  if (!length(headroom)) return(invisible(FALSE))
  tightest <- min(headroom)
  ceiling_value <- suppressWarnings(max(as.numeric(bound), na.rm = TRUE))

  legacy <- suppressWarnings(min(as.numeric(legacy_bound), na.rm = TRUE))
  suggestion <- if (is.finite(legacy) && is.finite(ceiling_value) &&
                    legacy < ceiling_value) {
    c("i" = "If instead the trend is running away -- a band spanning the whole plot, a nowcast many times the reported count -- the pre-2.5.0 ceiling {.code min(max(6, log1p(casemax)), 16)} held it down. Try {.code nowcast(..., mu_log_upper_bound = {signif(legacy, 4)})} and compare. It is a blunt instrument: it truncates a genuine inflation just as readily.")
  } else {
    character()
  }

  subject <- paste0(context, if (length(headroom) == 1L) "" else "s")
  cli::cli_warn(c(
    "{length(headroom)} of {n_fits} {subject} reached the `log_mean` upper bound.",
    "x" = "The fitted `log_mean` comes within {format(tightest, digits = 3)} log units of a bound of {format(ceiling_value, digits = 4)}, where the softplus cap keeps {format(100 * stats::plogis(tightest), digits = 3)}% of the peak latent incidence.",
    "i" = "The cap is a numerical guard sized from the counts REPORTED so far, so a growing, mostly-unreported stream can want a latent incidence above it. The nowcast is then truncated, and flat, wherever it saturates.",
    "i" = "Read {.code reporting_fraction(nc)} first: its {.code inflation} column is the multiplier being applied, and is what separates a stream that genuinely needs one from a process that has run away.",
    suggestion
  ))
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.warn_joint_fit <- function(fit, context = "The fit") {
  if (.fit_is_adequate(fit)) return(invisible(FALSE))
  summary <- .fit_diagnostic_summary(fit)
  warning <- c(
    "{context} did not pass the optimizer adequacy check.",
    "x" = "{paste(summary$reasons, collapse = '; ')}.",
    "i" = "Maximum absolute gradient: {format(summary$max_gradient, digits = 4)}; projected gradient: {format(summary$projected_gradient, digits = 4)}; quadratic objective gap: {format(summary$quadratic_gap, digits = 4)}.",
    "i" = "Inspect the returned `fit$diagnostic` before using predictions."
  )
  if (as.integer(fit$data$P_delay %||% 0L) > 0L ||
      as.integer(fit$data$P_revision %||% 0L) > 0L) {
    warning <- c(
      warning,
      "i" = "The report/revision regression may be weakly identified. Remove the affected `delay_covariates` or `revision_covariates` tags (or use fewer covariates) before trusting these predictions."
    )
  }
  cli::cli_warn(warning)
  invisible(TRUE)
}

#' Polish a bounded candidate on an additive-constant-invariant objective
#'
#' `optim()` uses relative function reduction in its L-BFGS-B stopping rule.
#' Applying it directly to a large negative log-posterior therefore makes its
#' stopping behavior depend on an inferentially irrelevant additive constant.
#' This helper optimizes `Q(theta) - Q(theta_start)` while retaining the exact
#' gradient of `Q`. Acceptance is likewise based on the centered change, with a
#' floating-point tolerance that does not scale with the absolute level of `Q`.
#' @keywords internal
#' @noRd
.effective_optimizer_convergence <- function(base_code, polish_code,
                                             polish_accepted) {
  base_code <- as.integer(base_code %||% NA_integer_)
  polish_code <- as.integer(polish_code %||% NA_integer_)
  if (!isTRUE(polish_accepted)) return(base_code)
  # The two solvers form a sequential optimization path. A successful base
  # solve is not invalidated by a later line-search termination after the point
  # has improved; likewise, a successful polish can rehabilitate the path.
  if (isTRUE(base_code == 0L) || isTRUE(polish_code == 0L)) return(0L)
  if (!is.na(polish_code)) polish_code else base_code
}

#' @keywords internal
#' @noRd
.polish_joint_candidate <- function(
    obj, opt, bounds, gradient_trigger = 0.05,
    control = NULL) {
  # Same reasoning as .scaled_nlminb_control(): a constant iteration budget is
  # not a property of the problem being polished.
  control <- control %||% list(maxit = max(2000L, min(50000L, 25L * length(opt$par))),
                               factr = 1e4, pgtol = 1e-8)
  start_gradient <- tryCatch(
    max(abs(obj$gr(opt$par))), error = function(e) NA_real_
  )
  diagnostic <- list(
    attempted = FALSE,
    accepted = FALSE,
    reason = "gradient_below_trigger",
    objective_at_start = as.numeric(opt$objective),
    centered_objective = NA_real_,
    objective_change = NA_real_,
    acceptance_tolerance = NA_real_,
    max_gradient_before = start_gradient,
    max_gradient_after = start_gradient,
    base_optimizer_convergence = as.integer(
      opt$convergence %||% NA_integer_
    ),
    polish_optimizer_convergence = NA_integer_,
    effective_optimizer_convergence = as.integer(
      opt$convergence %||% NA_integer_
    )
  )
  if (is.finite(start_gradient) && start_gradient <= gradient_trigger) {
    return(list(
      opt = opt, polished = FALSE, max_gradient = start_gradient,
      diagnostic = diagnostic
    ))
  }

  objective_at_start <- tryCatch(
    as.numeric(obj$fn(opt$par)), error = function(e) NA_real_
  )
  diagnostic$attempted <- TRUE
  diagnostic$objective_at_start <- objective_at_start
  if (length(objective_at_start) != 1L || !is.finite(objective_at_start)) {
    diagnostic$reason <- "nonfinite_start_objective"
    return(list(
      opt = opt, polished = FALSE, max_gradient = start_gradient,
      diagnostic = diagnostic
    ))
  }

  centered_objective <- function(par) {
    as.numeric(obj$fn(par)) - objective_at_start
  }
  polish_error <- NULL
  polish <- tryCatch(
    optim(
      opt$par, centered_objective, obj$gr, method = "L-BFGS-B",
      lower = bounds$lower, upper = bounds$upper, control = control
    ),
    error = function(error) {
      polish_error <<- conditionMessage(error)
      NULL
    }
  )
  if (is.null(polish)) {
    diagnostic$reason <- paste0("optimizer_error: ", polish_error)
    return(list(
      opt = opt, polished = FALSE, max_gradient = start_gradient,
      diagnostic = diagnostic
    ))
  }

  polish_gradient <- tryCatch(
    max(abs(obj$gr(polish$par))), error = function(e) NA_real_
  )
  centered_value <- as.numeric(polish$value)
  candidate_objective <- tryCatch(
    as.numeric(obj$fn(polish$par)), error = function(e) NA_real_
  )
  # This tolerance is expressed in units of the centered objective. It permits
  # only round-off-scale increases and is unchanged if a constant is added to Q.
  acceptance_tolerance <- sqrt(.Machine$double.eps) *
    (1 + abs(centered_value))
  accepted <-
    length(centered_value) == 1L && is.finite(centered_value) &&
    length(candidate_objective) == 1L && is.finite(candidate_objective) &&
    is.finite(polish_gradient) &&
    centered_value <= acceptance_tolerance &&
    (!is.finite(start_gradient) || polish_gradient < start_gradient)

  diagnostic$accepted <- accepted
  diagnostic$reason <- if (accepted) "accepted" else "acceptance_check_failed"
  diagnostic$centered_objective <- centered_value
  diagnostic$objective_change <- candidate_objective - objective_at_start
  diagnostic$acceptance_tolerance <- acceptance_tolerance
  diagnostic$max_gradient_after <- polish_gradient
  diagnostic$polish_optimizer_convergence <- as.integer(polish$convergence)
  diagnostic$effective_optimizer_convergence <-
    .effective_optimizer_convergence(
      opt$convergence, polish$convergence, accepted
    )

  if (accepted) {
    opt$par <- polish$par
    opt$objective <- candidate_objective
    opt$convergence <- diagnostic$effective_optimizer_convergence
    opt$message <- paste0(
      "centered L-BFGS-B polish (base code ",
      diagnostic$base_optimizer_convergence,
      ", polish code ", diagnostic$polish_optimizer_convergence,
      "): ", polish$message %||% ""
    )
  }
  list(
    opt = opt,
    polished = accepted,
    max_gradient = if (accepted) polish_gradient else start_gradient,
    diagnostic = diagnostic
  )
}

#' @keywords internal
#' @noRd
.fit_joint <- function(model, data, priors, init = NULL, n_tries = 6L,
                       use_random = NULL,
                       control = NULL,
                       hierarchical_strata = FALSE, warn = TRUE) {
  if (is.null(use_random)) {
    use_random <- if (isTRUE(data$is_count_cumulative == 1L)) {
      identical(as.integer(data$count_cumulative_observation), 1L)
    } else {
      getOption("diseasenowcasting.use_random", FALSE)
    }
  }
  base_init <- init %||% list()
  mu_offsets <- c(0, 0.5, -0.5, 1.0, 1.5, -1.0)
  n_strata <- as.integer(data$num_strata %||% 1L)
  cc_mat <- if (is.matrix(data$case_counts)) data$case_counts else matrix(data$case_counts, ncol = 1L)
  intercept_base <- apply(cc_mat, 2, function(col) { positive <- col[col > 0]
    if (length(positive)) log(stats::median(positive)) else 0 })   # one per stratum
  candidates <- list()
  attempt_errors <- character()
  fit_started <- proc.time()[["elapsed"]]
  # A supplied warm start already identifies one basin. Cold fits evaluate the
  # complete initialization ladder so admissibility and MAP selection remain
  # separate; warm Stage-2 fits avoid multiplying K imputations by six rungs.
  attempt_count <- if (is.null(init)) as.integer(n_tries) else 1L
  for (j in seq_len(attempt_count)) {
    ini <- base_init
    off <- mu_offsets[((j - 1) %% length(mu_offsets)) + 1]
    # Intercept init: for hierarchical we set mu_global + delta; for independent, per-stratum vector
    if (isTRUE(hierarchical_strata) && n_strata > 1L) {
      ini$mu_global         <- (base_init$mu_global %||% mean(intercept_base)) + off
      ini$delta_intercept   <- base_init$delta_intercept %||% rep(0, n_strata)
      ini$log_tau_intercept <- base_init$log_tau_intercept %||% 0
    } else {
      base_intercept   <- base_init$mu_intercept %||% intercept_base
      ini$mu_intercept <- base_intercept + off
    }
    if (data$epidemic_model == 1L && is.null(base_init$log_gp_alpha))
      ini$log_gp_alpha <- log(1) + (j - 1) * 0.15          # shared GP amplitude (scalar)
    if (data$epidemic_model == 2L && is.null(base_init$log_ar_sigma_unc))
      ini$log_ar_sigma_unc <- rep(-2 + (j - 1) * 0.3, n_strata)   # per-stratum AR innovation SD
    # The time-series trends get the same innovation-SD ladder: their sigma is the
    # one parameter whose starting value decides whether the first fit sees a flat
    # trend or a noisy one, and a cold start at the wrong end can stall there.
    if (data$epidemic_model == 5L && is.null(base_init$log_arima_sigma_unc))
      ini$log_arima_sigma_unc <- rep(-2 + (j - 1) * 0.3, n_strata)
    if (data$epidemic_model == 6L && is.null(base_init$log_ets_sigma_unc))
      ini$log_ets_sigma_unc <- rep(-2 + (j - 1) * 0.3, n_strata)
    if (data$epidemic_model == 7L && is.null(base_init$log_sts_level_sigma_unc))
      ini$log_sts_level_sigma_unc <- rep(-2 + (j - 1) * 0.3, n_strata)

    res <- tryCatch({
      built <- build_joint_obj(data, priors, init = ini, use_random = use_random,
                               hierarchical_strata = hierarchical_strata)
      obj <- built$obj
      bounds <- .joint_parameter_bounds(
        obj$par, data$settlement_horizon %||% 26L
      )
      attempt_control <- control %||% .scaled_nlminb_control(length(obj$par))
      opt <- nlminb(
        obj$par, obj$fn, obj$gr,
        lower = bounds$lower, upper = bounds$upper, control = attempt_control
      )
      if (!is.finite(opt$objective)) stop("non-finite objective")
      # A nominal nlminb convergence code is not enough for either inference
      # strategy.  Hurdle fits are joint MAP fits (use_random = FALSE), and can
      # need the same bounded quasi-Newton polish as Laplace-marginal fits.
      polish_result <- .polish_joint_candidate(obj, opt, bounds)
      opt <- polish_result$opt
      max_gradient <- polish_result$max_gradient
      polished <- polish_result$polished
      obj$fn(opt$par)
      pl  <- obj$env$parList()
      rc  <- .joint_reconstruct(data, priors, pl, built$Bmat, built$freq)
      if (any(!is.finite(rc$lambda))) stop("non-finite lambda")
      diagnose <- function(optimum, reconstruction) .joint_fit_diagnostic(
        obj, optimum, bounds, finite_reconstruction = TRUE,
        # `$mu` is the UNCAPPED log_mean; `$mu_safe` never reaches the bound.
        log_mean = reconstruction$mu,
        log_mean_upper_bound = data$mu_log_upper_bound,
        log_mean_upper_bound_legacy = data$mu_log_upper_bound_legacy,
        count_cumulative = isTRUE(data$is_count_cumulative == 1L)
      )
      diagnostic <- diagnose(opt, rc)
      # The quadratic gap IS the objective decrease a Newton step would buy, and
      # the diagnostic has just factorised the Hessian that prices it.  Where
      # that gap is what fails the fit, take the step rather than report it.
      refinement <- .refine_on_quadratic_gap(
        obj, opt, bounds, diagnostic,
        quadratic_gap_tolerance = diagnostic$quadratic_gap_tolerance
      )
      if (isTRUE(refinement$applied)) {
        refined_opt <- refinement$opt
        obj$fn(refined_opt$par)
        refined_pl <- obj$env$parList()
        refined_rc <- .joint_reconstruct(
          data, priors, refined_pl, built$Bmat, built$freq
        )
        refined_diagnostic <- if (any(!is.finite(refined_rc$lambda))) NULL else
          diagnose(refined_opt, refined_rc)
        # The refinement is a CANDIDATE, not a result.  Keep it only where the
        # diagnostic actually improves: a lower objective is not enough, since
        # a step can buy objective and lose positive curvature, and the Laplace
        # precision pays for that in a ridge.
        keep <- !is.null(refined_diagnostic) &&
          isTRUE(refined_diagnostic$hessian_positive_definite) &&
          (isTRUE(refined_diagnostic$adequate) ||
             (is.finite(refined_diagnostic$quadratic_gap) &&
                refined_diagnostic$quadratic_gap < diagnostic$quadratic_gap))
        if (keep) {
          opt <- refined_opt
          pl <- refined_pl
          rc <- refined_rc
          diagnostic <- refined_diagnostic
        } else {
          # Put the tape back on the point being returned, `last.par.best`
          # included -- the draws are sampled at that vector.
          .restore_tape_best(obj, opt$par, opt$objective)
          refinement$applied <- FALSE
          refinement$reason <- "rejected_by_diagnostic"
        }
      }
      list(
        par = opt$par, parList = pl, nll = opt$objective, convergence = opt$convergence,
        obj = obj, opt = opt, random = built$random, use_random = use_random,
        max_gradient = diagnostic$max_gradient,
        projected_gradient = diagnostic$projected_gradient,
        quadratic_gap = diagnostic$quadratic_gap,
        hessian_positive_definite = diagnostic$hessian_positive_definite,
        log_mean_headroom = diagnostic$log_mean_headroom,
        log_mean_cap_bound = diagnostic$log_mean_cap_bound,
        log_mean_cap_reportable = diagnostic$log_mean_cap_reportable,
        arma_ridge_correlation = diagnostic$arma_ridge_correlation,
        arma_ridge = diagnostic$arma_ridge,
        log_mean_upper_bound_legacy = diagnostic$log_mean_upper_bound_legacy,
        fit_status = diagnostic$status,
        diagnostic_reasons = diagnostic$reasons,
        gradient_status = if (!diagnostic$finite_gradient) "nonfinite"
          else diagnostic$status,
        diagnostic = diagnostic,
        attempt = j,
        polished = polished,
        polish = polish_result$diagnostic,
        refinement = refinement,
        elapsed = proc.time()[["elapsed"]] - fit_started,
        epi_model = built$epi_model, is_nb = built$is_nb,
        lambda = rc$lambda, mu = rc$mu, mu_safe = rc$mu_safe, Gstar = rc$Gstar,
        log_loc = rc$log_loc, log_scale = rc$log_scale,
        delay_mu = rc$delay_mu, delay_sigma = rc$delay_sigma, phi_nb = rc$phi_nb,
        reconstruct = rc, Bmat = built$Bmat, freq = built$freq,
        data = data, priors = priors, model = model
      )
    }, error = function(e) {
      # A bad configuration is not something another init rung can rescue, and
      # burying it under "failed to converge for all init attempts" hides the one
      # message that says what to change.
      if (inherits(e, "diseasenowcasting_invalid_fixed_value")) stop(e)
      attempt_errors <<- unique(c(attempt_errors, conditionMessage(e)))
      NULL
    })

    if (!is.null(res)) candidates[[length(candidates) + 1L]] <- res
  }
  selected <- .select_joint_candidate(candidates)
  if (!is.null(selected)) {
    selected$attempt_diagnostics <- do.call(rbind, lapply(candidates, function(candidate) {
      summary <- .fit_diagnostic_summary(candidate)
      data.frame(
        attempt = candidate$attempt,
        adequate = summary$adequate,
        status = summary$status,
        convergence = summary$convergence,
        objective = summary$objective,
        max_gradient = summary$max_gradient,
        projected_gradient = summary$projected_gradient,
        quadratic_gap = summary$quadratic_gap,
        hessian_positive_definite = summary$hessian_positive_definite,
        hessian_status = summary$hessian_status,
        polish_attempted = isTRUE(candidate$polish$attempted),
        polished = isTRUE(candidate$polished),
        base_optimizer_convergence = as.integer(
          candidate$polish$base_optimizer_convergence %||% NA_integer_
        ),
        polish_optimizer_convergence = as.integer(
          candidate$polish$polish_optimizer_convergence %||% NA_integer_
        ),
        polish_objective_change = as.numeric(
          candidate$polish$objective_change %||% NA_real_
        ),
        newton_steps = as.integer(candidate$refinement$steps %||% NA_integer_),
        newton_objective_change = as.numeric(
          candidate$refinement$objective_change %||% NA_real_
        ),
        newton_reason = as.character(
          candidate$refinement$reason %||% NA_character_
        ),
        reasons = paste(summary$reasons, collapse = "; "),
        stringsAsFactors = FALSE
      )
    }))
    if (isTRUE(warn)) {
      if (isTRUE(data$is_count_cumulative == 1L) &&
          identical(as.integer(data$count_cumulative_observation), 3L) &&
          !.fit_is_adequate(selected)) {
        cli::cli_warn(c(
          "The `hurdle_ztpoisson` optimizer did not pass the adequacy check.",
          "x" = "{paste(selected$diagnostic_reasons, collapse = '; ')}.",
          "i" = "Try `cumulative_process(observation = \"hurdle_ztnb\")`; the ZTNB magnitude law was stable in the package-wide sweep.",
          "i" = "Inspect the returned `fit$diagnostic` before using predictions."
        ))
      } else {
        .warn_joint_fit(selected)
      }
      if (isTRUE(selected$diagnostic$log_mean_cap_bound)) {
        .warn_log_mean_cap(
          selected$diagnostic$log_mean_headroom,
          selected$diagnostic$log_mean_upper_bound, n_fits = 1L,
          legacy_bound = selected$diagnostic$log_mean_upper_bound_legacy
        )
      }
    }
    return(selected)
  }
  if (isTRUE(data$is_count_cumulative == 1L) &&
      identical(as.integer(data$count_cumulative_observation), 3L)) {
    optimizer_error <- if (length(attempt_errors)) {
      attempt_errors[length(attempt_errors)]
    } else {
      "no optimizer result was returned"
    }
    cli::cli_warn(c(
      "The `hurdle_ztpoisson` optimizer failed for every initialization.",
      "x" = "Last optimizer error: {optimizer_error}",
      "i" = "Try `cumulative_process(observation = \"hurdle_ztnb\")`; the ZTNB magnitude law was stable in the package-wide sweep."
    ))
  }
  cli::cli_abort("Joint fit failed to converge for all init attempts.")
}

#' Delay-only fit with a small init ladder (parametric families 1/2/3; the
#' non-parametric Dirichlet simplex is dispatched to `.fit_delay_only_np()`)
#' @keywords internal
#' @noRd
.fit_delay_only <- function(model, data, priors, init = NULL,
                            control = list(iter.max = 500, eval.max = 1000, rel.tol = 1e-9)) {
  if (data$delay_family == 4L) return(.fit_delay_only_np(model, data, priors, init, control))
  if (data$delay_family == 5L) return(.fit_delay_only_custom(model, data, priors, init, control))
  is_gengamma <- data$delay_family == 3L
  total_count <- sum(data$row_sums_exact)
  log_mean_seed <- if (total_count > 0 && length(data$obs_delays) > 0)
    sum(log(data$obs_delays) * data$row_sums_exact) / total_count else log(3)
  prior_mu_mean <- .pad3(priors$delay_mu$params)[1]
  delay_sd_seed <- { empirical_sd <- sqrt(.wtd_var(data$m[, 3], data$m[, 2]))
                     if (is.finite(empirical_sd) && empirical_sd > 0) max(2, min(empirical_sd, 60)) else 5 }
  sigma_seed <- if (is_gengamma) 0.6 else delay_sd_seed

  # delay_Q is the UNCONSTRAINED raw value (Q = 0.05 + 2.95*plogis(raw)); the
  # ladder spans near-lognormal (raw -2.5 -> Q 0.27) through Weibull and beyond.
  init_ladder <- if (!is.null(init)) list(init) else list(
    list(delay_mu = log_mean_seed, delay_sigma = sigma_seed,             delay_Q = -2),
    list(delay_mu = prior_mu_mean, delay_sigma = sigma_seed,             delay_Q = -1),
    list(delay_mu = log(3),        delay_sigma = max(1, sigma_seed / 2), delay_Q = 0),
    list(delay_mu = log_mean_seed, delay_sigma = sigma_seed * 1.5,       delay_Q = -2.5),
    list(delay_mu = prior_mu_mean, delay_sigma = max(0.5, sigma_seed / 3), delay_Q = 1)
  )

  delay_mu_is_fixed    <- isTRUE(priors$delay_mu$is_constant == 1L)
  delay_sigma_is_fixed <- isTRUE(priors$delay_sigma$is_constant == 1L)
  shape_Q_is_fixed     <- is_gengamma && isTRUE(priors$delay_Q$is_constant == 1L)

  finish <- function(obj, opt) {
    reported <- obj$report()
    parlist  <- obj$env$parList()
    fitted_delay_mu <- if (delay_mu_is_fixed) priors$delay_mu$fixed else unname(parlist$delay_mu)
    fitted_delay_sd <- as.numeric(reported$delay_sd)
    fitted_shape_Q  <- if (is_gengamma) (if (shape_Q_is_fixed) priors$delay_Q$fixed
                                         else .gengamma_shape_transform(parlist$delay_Q)$shape_Q) else NA_real_
    delay_mu_se <- delay_sigma_se <- NA_real_
    sd_report <- tryCatch(RTMB::sdreport(obj), error = function(e) NULL)
    if (!is.null(sd_report)) {
      cov_fixed <- sd_report$cov.fixed
      if (!is.null(cov_fixed) && "delay_mu" %in% rownames(cov_fixed))
        delay_mu_se <- sqrt(cov_fixed["delay_mu", "delay_mu"])
      value_names <- names(sd_report$value)
      if (length(value_names)) {
        if ("delay_sd" %in% value_names) delay_sigma_se <- sd_report$sd[which(value_names == "delay_sd")[1]]
        if (is.na(delay_mu_se) && "delay_mu" %in% value_names)
          delay_mu_se <- sd_report$sd[which(value_names == "delay_mu")[1]]
      }
    }
    list(par = c(delay_mu = fitted_delay_mu, delay_sigma = fitted_delay_sd),
         delay_mu = fitted_delay_mu, delay_sigma = fitted_delay_sd, delay_Q = fitted_shape_Q,
         delay_beta = as.numeric(parlist$delay_beta %||% numeric(0)),
         parList = parlist,
         delay_mu_sd = unname(delay_mu_se), delay_sigma_sd = unname(delay_sigma_se),
         nll = if (is.null(opt)) obj$fn(obj$par) else opt$objective,
         convergence = if (is.null(opt)) 0L else opt$convergence,
         obj = obj, opt = opt, data = data, priors = priors, model = model)
  }

  # Everything fixed -> nothing to optimise (degenerate, but handle gracefully).
  if (delay_mu_is_fixed && delay_sigma_is_fixed &&
      (!is_gengamma || shape_Q_is_fixed) &&
      as.integer(data$P_delay %||% 0L) == 0L) {
    obj <- build_delay_only_obj(data, priors, init = init_ladder[[1]])
    obj$fn(obj$par)
    return(finish(obj, NULL))
  }

  last_err <- NULL
  for (init_try in init_ladder) {
    obj <- build_delay_only_obj(data, priors, init = init_try)
    opt <- tryCatch(nlminb(obj$par, obj$fn, obj$gr, control = control),
                    error = function(e) { last_err <<- e; NULL })
    if (is.null(opt) || opt$convergence != 0) next
    reported <- obj$report()
    if (!is.finite(as.numeric(reported$delay_sd))) next
    return(finish(obj, opt))
  }
  cli::cli_abort(c("Delay-only fit failed for all init attempts.",
                   if (!is.null(last_err)) c("x" = conditionMessage(last_err)) else NULL))
}

#' Delay-only fit for the non-parametric Dirichlet simplex (Stage-1 of the
#' two-stage Dirichlet nowcast).  Optimises `delay_logits`; returns the fitted
#' simplex `delay_probs` plus the `obj` (whose Hessian over `delay_logits`
#' drives the simplex imputation).
#' @keywords internal
#' @noRd
.fit_delay_only_np <- function(model, data, priors, init = NULL,
                               control = list(iter.max = 500, eval.max = 1000, rel.tol = 1e-9)) {
  obj <- build_delay_only_obj(data, priors, init = init)
  opt <- tryCatch(nlminb(obj$par, obj$fn, obj$gr, control = control), error = function(e) NULL)
  if (is.null(opt))
    cli::cli_abort("Non-parametric delay-only fit failed.")
  fitted_simplex <- as.numeric(obj$report()$simplex_probs)
  parlist <- obj$env$parList()
  list(delay_probs = fitted_simplex,
       delay_logits = as.numeric(parlist$delay_logits),
       delay_beta = as.numeric(parlist$delay_beta %||% numeric(0)),
       parList = parlist,
       convergence = opt$convergence, nll = opt$objective,
       obj = obj, data = data, priors = priors, model = model)
}

#' Delay-only fit for a user-defined (custom) delay distribution (family 5,
#' Stage-1 of two-stage).  A custom delay is parametric -- it carries its own
#' `custom_delay_params` -- but those are not the `delay_mu`/`delay_sigma` the
#' parametric path assumes, so it gets its own handler that optimises the free
#' custom parameters and returns them (with `delay_mu`/`delay_sigma` left `NA`).
#' @keywords internal
#' @noRd
.fit_delay_only_custom <- function(model, data, priors, init = NULL,
                                   control = list(iter.max = 500, eval.max = 1000, rel.tol = 1e-9)) {
  obj <- build_delay_only_obj(data, priors, init = init)
  opt <- tryCatch(nlminb(obj$par, obj$fn, obj$gr, control = control), error = function(e) NULL)
  if (is.null(opt))
    cli::cli_abort("Custom delay-only fit failed.")
  # parList() reassembles the full parameter vector (free + fixed) via the map.
  fitted_params <- as.numeric(obj$env$parList()$custom_delay_params)
  param_names <- tryCatch(model@delay@param_names, error = function(e) NULL)
  if (length(param_names) != length(fitted_params))
    param_names <- paste0("param_", seq_along(fitted_params))
  parlist <- obj$env$parList()
  list(custom_delay_params = fitted_params,
       delay_beta = as.numeric(parlist$delay_beta %||% numeric(0)),
       parList = parlist,
       par = stats::setNames(fitted_params, param_names),
       delay_mu = NA_real_, delay_sigma = NA_real_, delay_Q = NA_real_,
       delay_mu_sd = NA_real_, delay_sigma_sd = NA_real_,
       nll = opt$objective, convergence = opt$convergence,
       obj = obj, opt = opt, data = data, priors = priors, model = model)
}
