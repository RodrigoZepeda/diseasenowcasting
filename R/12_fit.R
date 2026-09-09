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
#' @param control `nlminb` control list.
#' @param warn If `TRUE`, warn when the returned joint fit does not pass the
#'   optimizer adequacy checks. Internal warm-start and imputation fits set this
#'   to `FALSE` and report only diagnostics for the fits that affect the result.
#' @returns A list with `par` (named estimates), `obj`, `opt`, `data`, `priors`,
#'   `model`, `convergence`, and (delay-only) `delay_mu` / `delay_sigma`.
#' @export
fit <- function(model, data, priors = NULL, init = NULL,
                control = list(iter.max = 500, eval.max = 1000, rel.tol = 1e-9),
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
    return(.fit_delay_only(model, data, priors, init = init, control = control))
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
  set_bounds("^log_gp_alpha$|^log_gp_ell$", -8, 8)
  set_bounds("^log_R0$", -6, 6)
  set_bounds("^u_gamma$|^u_neff$", -10, 10)
  set_bounds("^log_phi_nb$", -12, 8)
  set_bounds("^log_magnitude_size$", -8, 12)
  set_bounds("^movement_", -12, 12)
  list(lower = lower, upper = upper)
}

#' Mathematically coherent diagnostics for a box-constrained joint fit
#'
#' The raw maximum gradient is retained for debugging, but adequacy is based on
#' the KKT residual, positive curvature, and the curvature-scaled estimate
#' `0.5 * r' H^-1 r` of the objective decrease still available locally. The
#' latter is invariant under invertible linear reparameterisations and gives the
#' tolerance (`0.01` by default) a log-posterior interpretation.
#' @keywords internal
#' @noRd
.joint_fit_diagnostic <- function(obj, opt, bounds,
                                  finite_reconstruction = TRUE,
                                  quadratic_gap_tolerance = 0.01) {
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
  if (finite_gradient) {
    # At a lower bound, g >= 0 satisfies KKT; at an upper bound, g <= 0 does.
    kkt_residual[at_lower & gradient >= 0] <- 0
    kkt_residual[at_upper & gradient <= 0] <- 0
  }
  projected_gradient <- if (finite_gradient && length(kkt_residual)) {
    max(abs(kkt_residual))
  } else if (length(kkt_residual) == 0L) {
    0
  } else {
    NA_real_
  }

  hessian_positive_definite <- FALSE
  hessian_status <- "unavailable"
  quadratic_gap <- NA_real_
  curvature_dimension <- 0L
  free_coordinates <- rep(TRUE, length(par))
  names(free_coordinates) <- names(par)
  if (finite_gradient && length(par) == 0L) {
    hessian_positive_definite <- TRUE
    hessian_status <- "no_free_parameters"
    quadratic_gap <- 0
  } else if (finite_gradient) {
    hessian <- tryCatch(obj$he(opt$par), error = function(e) NULL)
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
        multiplier_scale <- max(1, max(abs(gradient)))
        multiplier_tolerance <- sqrt(.Machine$double.eps) * multiplier_scale
        fixed_active <-
          (at_lower & gradient > multiplier_tolerance) |
          (at_upper & gradient < -multiplier_tolerance)
        free <- !fixed_active
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

  list(
    adequate = length(reasons) == 0L,
    status = if (length(reasons) == 0L) "pass" else "warning",
    reasons = unique(reasons),
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
  fit$fit_status <- diagnostic$status
  fit$diagnostic_reasons <- diagnostic$reasons
  fit$gradient_status <- if (!diagnostic$finite_gradient) "nonfinite" else
    diagnostic$status
  fit$diagnostic <- diagnostic
  fit
}

#' @keywords internal
#' @noRd
.warn_joint_fit <- function(fit, context = "The fit") {
  if (.fit_is_adequate(fit)) return(invisible(FALSE))
  summary <- .fit_diagnostic_summary(fit)
  cli::cli_warn(c(
    "{context} did not pass the optimizer adequacy check.",
    "x" = "{paste(summary$reasons, collapse = '; ')}.",
    "i" = "Maximum absolute gradient: {format(summary$max_gradient, digits = 4)}; projected gradient: {format(summary$projected_gradient, digits = 4)}; quadratic objective gap: {format(summary$quadratic_gap, digits = 4)}.",
    "i" = "Inspect the returned `fit$diagnostic` before using predictions."
  ))
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
    control = list(maxit = 2000L, factr = 1e4, pgtol = 1e-8)) {
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
                       control = list(iter.max = 1000, eval.max = 2000, rel.tol = 1e-9),
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

    res <- tryCatch({
      built <- build_joint_obj(data, priors, init = ini, use_random = use_random,
                               hierarchical_strata = hierarchical_strata)
      obj <- built$obj
      bounds <- .joint_parameter_bounds(
        obj$par, data$settlement_horizon %||% 26L
      )
      opt <- nlminb(
        obj$par, obj$fn, obj$gr,
        lower = bounds$lower, upper = bounds$upper, control = control
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
      diagnostic <- .joint_fit_diagnostic(
        obj, opt, bounds, finite_reconstruction = TRUE
      )
      list(
        par = opt$par, parList = pl, nll = opt$objective, convergence = opt$convergence,
        obj = obj, opt = opt, random = built$random, use_random = use_random,
        max_gradient = diagnostic$max_gradient,
        projected_gradient = diagnostic$projected_gradient,
        quadratic_gap = diagnostic$quadratic_gap,
        hessian_positive_definite = diagnostic$hessian_positive_definite,
        fit_status = diagnostic$status,
        diagnostic_reasons = diagnostic$reasons,
        gradient_status = if (!diagnostic$finite_gradient) "nonfinite"
          else diagnostic$status,
        diagnostic = diagnostic,
        attempt = j,
        polished = polished,
        polish = polish_result$diagnostic,
        elapsed = proc.time()[["elapsed"]] - fit_started,
        epi_model = built$epi_model, is_nb = built$is_nb,
        lambda = rc$lambda, mu = rc$mu, mu_safe = rc$mu_safe, Gstar = rc$Gstar,
        log_loc = rc$log_loc, log_scale = rc$log_scale,
        delay_mu = rc$delay_mu, delay_sigma = rc$delay_sigma, phi_nb = rc$phi_nb,
        reconstruct = rc, Bmat = built$Bmat, freq = built$freq,
        data = data, priors = priors, model = model
      )
    }, error = function(e) {
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
         delay_mu_sd = unname(delay_mu_se), delay_sigma_sd = unname(delay_sigma_se),
         nll = if (is.null(opt)) obj$fn(obj$par) else opt$objective,
         convergence = if (is.null(opt)) 0L else opt$convergence,
         obj = obj, opt = opt, data = data, priors = priors, model = model)
  }

  # Everything fixed -> nothing to optimise (degenerate, but handle gracefully).
  if (delay_mu_is_fixed && delay_sigma_is_fixed && (!is_gengamma || shape_Q_is_fixed)) {
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
  list(delay_probs = fitted_simplex, delay_logits = obj$env$last.par.best,
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
  list(custom_delay_params = fitted_params,
       par = stats::setNames(fitted_params, param_names),
       delay_mu = NA_real_, delay_sigma = NA_real_, delay_Q = NA_real_,
       delay_mu_sd = NA_real_, delay_sigma_sd = NA_real_,
       nll = opt$objective, convergence = opt$convergence,
       obj = obj, opt = opt, data = data, priors = priors, model = model)
}
