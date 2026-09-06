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
#' @returns A list with `par` (named estimates), `obj`, `opt`, `data`, `priors`,
#'   `model`, `convergence`, and (delay-only) `delay_mu` / `delay_sigma`.
#' @export
fit <- function(model, data, priors = NULL, init = NULL,
                control = list(iter.max = 500, eval.max = 1000, rel.tol = 1e-9)) {
  priors <- priors %||% default_priors(model, data)
  hier   <- S7::S7_inherits(model, model_class) && model@strata_pooling == "hierarchical"

  # Checked here, not inside build_joint_obj(): `.fit_joint()` runs an init ladder
  # that swallows build errors, so a configuration mistake would surface as an
  # unhelpful "failed to converge for all init attempts".
  if (isTRUE(data$is_linelist_retraction == 1L) && is.null(priors$confirm_p))
    cli::cli_abort(c("The engine carries linelist retractions but the priors have no confirmation block.",
                     "i" = "Build the model with {.code model(validation = validation_process())}, or go through {.fn nowcast}, which attaches one automatically."))
  if (isTRUE(data$is_linelist_retraction == 1L) && isTRUE(priors$confirm_p$is_constant == 1L) &&
      isTRUE(priors$confirm_p$fixed >= 1) && data$n_retracted > 0)
    cli::cli_abort(c("`p = 1` says no report is ever retracted, but {data$n_retracted} retraction{?s} {?is/are} observed.",
                     "i" = "Leave `p` free (the default) or give it a prior, so the observed retractions have positive probability."))

  if (isTRUE(data$delay_only)) {
    return(.fit_delay_only(model, data, priors, init = init, control = control))
  }
  .fit_joint(model, data, priors, init = init, control = control,
             hierarchical_strata = hier)
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

#' @keywords internal
#' @noRd
.fit_joint <- function(model, data, priors, init = NULL, n_tries = 6L,
                       use_random = NULL,
                       control = list(iter.max = 1000, eval.max = 2000, rel.tol = 1e-9),
                       hierarchical_strata = FALSE) {
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
  best <- NULL
  attempt_errors <- character()
  fit_started <- proc.time()[["elapsed"]]
  for (j in seq_len(n_tries)) {
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
      max_gradient <- max(abs(obj$gr(opt$par)))
      polished <- FALSE
      # A nominal nlminb convergence code is not enough for either inference
      # strategy.  Hurdle fits are joint MAP fits (use_random = FALSE), and can
      # need the same bounded quasi-Newton polish as Laplace-marginal fits.
      if (!is.finite(max_gradient) || max_gradient > 0.05) {
        polish <- optim(
          opt$par, obj$fn, obj$gr, method = "L-BFGS-B",
          lower = bounds$lower, upper = bounds$upper,
          control = list(maxit = 2000L, factr = 1e4, pgtol = 1e-8)
        )
        polish_gradient <- max(abs(obj$gr(polish$par)))
        objective_tolerance <- 1e-8 * (1 + abs(opt$objective))
        if (is.finite(polish$value) && is.finite(polish_gradient) &&
            polish$value <= opt$objective + objective_tolerance &&
            polish_gradient < max_gradient) {
          opt$par <- polish$par
          opt$objective <- polish$value
          opt$convergence <- polish$convergence
          opt$message <- paste("L-BFGS-B polish:", polish$message %||% "")
          max_gradient <- polish_gradient
          polished <- TRUE
        }
      }
      obj$fn(opt$par)
      pl  <- obj$env$parList()
      rc  <- .joint_reconstruct(data, priors, pl, built$Bmat, built$freq)
      if (any(!is.finite(rc$lambda))) stop("non-finite lambda")
      list(
        par = opt$par, parList = pl, nll = opt$objective, convergence = opt$convergence,
        obj = obj, opt = opt, random = built$random, use_random = use_random,
        max_gradient = max_gradient,
        gradient_status = if (!is.finite(max_gradient)) "nonfinite"
          else if (max_gradient > 0.1) "warning" else "pass",
        polished = polished,
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

    if (!is.null(res) && res$convergence == 0L &&
        is.finite(res$max_gradient) && res$max_gradient <= 0.1) return(res)
    res_gradient <- if (!is.null(res) && is.finite(res$max_gradient))
      res$max_gradient else Inf
    best_gradient <- if (!is.null(best) && is.finite(best$max_gradient))
      best$max_gradient else Inf
    if (!is.null(res) && (is.null(best) || res_gradient < best_gradient))
      best <- res
  }
  if (!is.null(best)) {
    if (!is.finite(best$max_gradient) || best$max_gradient > 0.1) {
      if (isTRUE(data$is_count_cumulative == 1L) &&
          identical(as.integer(data$count_cumulative_observation), 3L)) {
        optimizer_message <- best$opt$message %||% "no optimizer message"
        cli::cli_warn(c(
          "The `hurdle_ztpoisson` optimizer did not pass the stability gate.",
          "x" = "Optimizer code {best$convergence}: {optimizer_message}",
          "x" = "Maximum absolute gradient: {format(best$max_gradient, digits = 4)}.",
          "i" = "Try `count_cumulative_process(observation = \"hurdle_ztnb\")`; the ZTNB magnitude law was stable in the package-wide sweep.",
          "i" = "This finite fit is returned with `gradient_status = \"warning\"`; inspect `fit$opt` before using it."
        ))
      } else {
        cli::cli_warn(c(
          "The fit is finite but did not pass the gradient stability gate.",
          "x" = "Maximum absolute gradient: {format(best$max_gradient, digits = 4)}.",
          "i" = "Inspect `fit$gradient_status`, `fit$max_gradient`, and `fit$opt` before using predictions."
        ))
      }
    }
    return(best)
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
      "i" = "Try `count_cumulative_process(observation = \"hurdle_ztnb\")`; the ZTNB magnitude law was stable in the package-wide sweep."
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
