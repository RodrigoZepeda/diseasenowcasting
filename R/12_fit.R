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
.fit_joint <- function(model, data, priors, init = NULL, n_tries = 6L,
                       use_random = getOption("diseasenowcasting.use_random", FALSE),
                       control = list(iter.max = 1000, eval.max = 2000, rel.tol = 1e-9),
                       hierarchical_strata = FALSE) {
  base_init <- init %||% list()
  mu_offsets <- c(0, 0.5, -0.5, 1.0, 1.5, -1.0)
  n_strata <- as.integer(data$num_strata %||% 1L)
  cc_mat <- if (is.matrix(data$case_counts)) data$case_counts else matrix(data$case_counts, ncol = 1L)
  intercept_base <- apply(cc_mat, 2, function(col) { positive <- col[col > 0]
    if (length(positive)) log(stats::median(positive)) else 0 })   # one per stratum
  best <- NULL
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
      opt <- nlminb(obj$par, obj$fn, obj$gr, control = control)
      if (!is.finite(opt$objective)) stop("non-finite objective")
      pl  <- obj$env$parList()
      rc  <- .joint_reconstruct(data, priors, pl, built$Bmat, built$freq)
      if (any(!is.finite(rc$lambda))) stop("non-finite lambda")
      list(
        par = opt$par, parList = pl, nll = opt$objective, convergence = opt$convergence,
        obj = obj, opt = opt, random = built$random, use_random = use_random,
        epi_model = built$epi_model, is_nb = built$is_nb,
        lambda = rc$lambda, mu = rc$mu, mu_safe = rc$mu_safe, Gstar = rc$Gstar,
        log_loc = rc$log_loc, log_scale = rc$log_scale,
        delay_mu = rc$delay_mu, delay_sigma = rc$delay_sigma, phi_nb = rc$phi_nb,
        reconstruct = rc, Bmat = built$Bmat, freq = built$freq,
        data = data, priors = priors, model = model
      )
    }, error = function(e) NULL)

    if (!is.null(res) && res$convergence == 0L) return(res)
    if (!is.null(res) && is.null(best)) best <- res   # keep a non-converged fallback
  }
  if (!is.null(best)) return(best)
  cli::cli_abort("Joint fit failed to converge for all init attempts.")
}

#' Delay-only fit with a small init ladder (parametric families 1/2/3; the
#' non-parametric Dirichlet simplex is dispatched to `.fit_delay_only_np()`)
#' @keywords internal
#' @noRd
.fit_delay_only <- function(model, data, priors, init = NULL,
                            control = list(iter.max = 500, eval.max = 1000, rel.tol = 1e-9)) {
  if (data$delay_family == 4L) return(.fit_delay_only_np(model, data, priors, init, control))
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
