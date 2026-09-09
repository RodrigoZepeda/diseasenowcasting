# =============================================================================
# Collect the fitted objects for a nowcast (one- or two-stage), WITHOUT drawing
# =============================================================================
# This is the "fit only" core behind the lazy S7 nowcast(): it returns the
# underlying RTMB fit object(s) so prediction (Laplace sampling + count draws)
# can be deferred to predict()/mean()/median()/etc.  The two-stage path returns
# the K converged delay-imputed Stage-2 fits (pooling over them re-injects the
# right-skewed delay uncertainty); the one-stage path returns a single joint fit.
# =============================================================================

#' Collect nowcast fit object(s) from prepared engine data (no prediction)
#'
#' @param model A [model()] object.
#' @param engine Prepared-data list from [prepare_data()] (`delay_only = FALSE`).
#' @param priors Prior bundle from [default_priors()].
#' @param type `"two_stage"` (default), `"one_stage"`, or `"auto"` (resolves per
#'   delay: dirichlet one-stage, everything else two-stage).
#' @param K Delay imputations for the two-stage path.
#' @param floor_mu,floor_sig_frac Imputation-spread floors (parametric families).
#' @param np_spread Dirichlet simplex imputation covariance inflation.
#' @param delay_window Recent window length for the parametric Stage-1 delay fit.
#' @returns list(`fits` = list of fit objects, `rung`, `target`).
#' @keywords internal
#' @noRd
.fit_diagnostics_frame <- function(fits) {
  if (length(fits) == 0L) {
    return(data.frame(
      fit = integer(), status = character(), adequate = logical(),
      convergence = integer(), objective = numeric(), max_gradient = numeric(),
      projected_gradient = numeric(), quadratic_gap = numeric(),
      hessian_positive_definite = logical(), hessian_status = character(),
      reasons = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(seq_along(fits), function(index) {
    summary <- .fit_diagnostic_summary(fits[[index]])
    data.frame(
      fit = index,
      status = summary$status,
      adequate = summary$adequate,
      convergence = summary$convergence,
      objective = summary$objective,
      max_gradient = summary$max_gradient,
      projected_gradient = summary$projected_gradient,
      quadratic_gap = summary$quadratic_gap,
      hessian_positive_definite = summary$hessian_positive_definite,
      hessian_status = summary$hessian_status,
      reasons = paste(summary$reasons, collapse = "; "),
      stringsAsFactors = FALSE
    )
  }))
}

#' @keywords internal
#' @noRd
.finish_nowcast_collection <- function(fits, rung, target, diagnostics,
                                       warn = TRUE) {
  diagnostics$retained_fit_diagnostics <- .fit_diagnostics_frame(fits)
  diagnostics$retained_K <- if (identical(rung, "multi")) length(fits) else 0L
  diagnostics$excluded_K <- max(
    0L, diagnostics$attempted_K - diagnostics$retained_K
  )
  diagnostics$collection_warning_emitted <- FALSE

  if (isTRUE(warn)) {
    retained_bad <- diagnostics$retained_fit_diagnostics$status != "pass"
    problems <- character()
    if (diagnostics$excluded_K > 0L) {
      problems <- c(
        problems,
        paste0(
          diagnostics$excluded_K, " of ", diagnostics$attempted_K,
          " attempted Stage-2 imputation fits were excluded"
        )
      )
    }
    if (any(retained_bad)) {
      problems <- c(
        problems,
        paste0(
          sum(retained_bad), " retained ", rung,
          " fit", if (sum(retained_bad) == 1L) "" else "s",
          " did not pass the optimizer adequacy check"
        )
      )
    }
    if (length(problems) > 0L) {
      diagnostics$collection_warning_emitted <- TRUE
      cli::cli_warn(c(
        "The final nowcast fit collection has optimizer diagnostics to review.",
        "x" = "{paste(problems, collapse = '; ')}.",
        "i" = "No prediction draw is based on an excluded fit.",
        "i" = "Run {.code fit_check(result, warn = FALSE)} for retained-fit details."
      ))
    }
  }

  list(
    fits = fits,
    rung = rung,
    target = target,
    diagnostics = diagnostics
  )
}

#' Warn if predictive sampling had to alter a retained Laplace precision
#' @keywords internal
#' @noRd
.warn_laplace_sampling <- function(diagnostics, warn = TRUE) {
  if (!isTRUE(warn) ||
      !isTRUE(diagnostics$laplace_sampling$any_regularized) ||
      isTRUE(diagnostics$collection_warning_emitted)) {
    return(invisible(FALSE))
  }
  retained <- diagnostics$retained_fit_diagnostics
  # A degraded retained fit has already generated the collection warning. Its
  # sampling regularization remains visible in `fit_check()` without producing
  # a second warning for the same final fit.
  if (!is.null(retained) && any(retained$status != "pass")) {
    return(invisible(FALSE))
  }
  regularized_count <- sum(vapply(
    diagnostics$laplace_sampling$fits,
    function(item) isTRUE(item$applied), logical(1)
  ))
  cli::cli_warn(c(
    "The retained Laplace precision required regularization for prediction.",
    "x" = "{regularized_count} retained fit{?s} used an altered precision matrix.",
    "i" = "Run {.code fit_check(result, warn = FALSE)} for the method, ridge, or eigenvalue floor."
  ))
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.collect_nowcast_fits <- function(model, engine, priors, type = "two_stage",
                                  K = 25L, floor_mu = 0.08, floor_sig_frac = 0.08,
                                  np_spread = 1, delay_window = 120L,
                                  warm_inits = NULL, warn = TRUE) {
  target <- engine$max_time
  requested_type <- type
  warm_was_provided <- !is.null(warm_inits)
  diagnostics <- list(
    requested_type = requested_type,
    resolved_type = NA_character_,
    requested_K = as.integer(K),
    attempted_K = 0L,
    retained_K = 0L,
    excluded_K = 0L,
    exclusion_reasons = character(),
    warm_fit = list(
      used = warm_was_provided,
      source = if (warm_was_provided) "provided" else "not_run",
      status = if (warm_was_provided) "provided" else "not_run"
    ),
    stage1 = list(status = "not_run"),
    fallback = list()
  )

  # `type = "auto"` chooses the stage per delay family: the non-parametric
  # (Dirichlet) delay is fit ONE-stage, every parametric delay TWO-stage.  In
  # our experiments the Dirichlet delay fit *worse* under the two-stage simplex
  # imputation than fit directly one-stage, while parametric delays benefit from
  # the two-stage delay-uncertainty propagation -- so "auto" picks the better of
  # the two for each.
  if (type == "auto")
    type <- if (model@delay@num_id == 4L) "one_stage" else "two_stage"

  # Custom delays (family 5) are parametric but carry arbitrary `custom_delay_params`,
  # not the `delay_mu`/`delay_sigma` the two-stage parametric imputation pools.  Until
  # a dedicated custom imputation exists they are fit one-stage (the delay is still
  # estimated jointly), regardless of the requested `type`.
  if (model@delay@num_id == 5L) type <- "one_stage"

  # Count-cumulative models are fit one-stage: the ordinary delay-only Stage 1
  # does not apply to cumulative levels or signed updates.  Report delay,
  # defective retraction kernel, and epidemic intensity are estimated jointly.
  if (isTRUE(engine$is_count_cumulative == 1L) ||
      isTRUE(engine$is_confirmation == 1L)) type <- "one_stage"
  diagnostics$resolved_type <- type

  if (type == "one_stage") {
    fitted <- fit(
      model, engine, priors = priors, init = warm_inits, warn = FALSE
    )
    return(.finish_nowcast_collection(
      list(fitted), "onestage", target, diagnostics, warn = warn
    ))
  }

  m <- engine$m; max_time <- engine$max_time
  is_nonparametric <- model@delay@num_id == 4L

  # Stage A: warm one-stage fit (free delay) -> warm epidemic inits.  `update()`
  # supplies `warm_inits` from the previous fit to skip this cold fit.
  if (is.null(warm_inits)) {
    warm_error <- NULL
    warm_fit <- tryCatch(
      fit(model, engine, priors = priors, warn = FALSE),
      error = function(e) {
        warm_error <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(warm_fit)) {
      warm_summary <- .fit_diagnostic_summary(warm_fit)
      warm_values <- tryCatch(
        as.numeric(unlist(warm_fit$parList, recursive = TRUE, use.names = FALSE)),
        error = function(e) NA_real_
      )
      warm_usable <- length(warm_values) > 0L && all(is.finite(warm_values))
      if (warm_usable) warm_inits <- warm_fit$parList
      diagnostics$warm_fit <- c(
        list(used = warm_usable, source = "estimated"),
        warm_summary
      )
    } else {
      diagnostics$warm_fit <- list(
        used = FALSE, source = "estimated", status = "error",
        reasons = warm_error %||% "warm fit failed"
      )
    }
  }

  # -- Two-stage DIRICHLET (simplex imputation) --------------------------------
  if (is_nonparametric && !is.null(warm_inits)) {
    stage1_error <- NULL
    delay_engine <- prepare_data(
      model, m, X = engine$X,
      d_star = matrix(engine$d_star, ncol = 1),
      max_time = max_time, delay_only = TRUE
    )
    stage1 <- tryCatch(
      fit(
        model, delay_engine, priors = default_priors(model, delay_engine),
        warn = FALSE
      ),
      error = function(e) {
        stage1_error <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(stage1)) {
      stage1 <- .attach_fit_diagnostic(stage1)
      diagnostics$stage1 <- .fit_diagnostic_summary(stage1)
    } else {
      diagnostics$stage1 <- list(
        status = "error", adequate = FALSE,
        reasons = stage1_error %||% "Stage-1 delay fit failed"
      )
    }

    if (!is.null(stage1) && .fit_is_adequate(stage1)) {
      logits_mode <- as.numeric(stage1$delay_logits)
      precision <- methods::as(
        stage1$obj$he(logits_mode), "sparseMatrix"
      ) / np_spread
      logit_draws <- .sample_mvnorm_precision(logits_mode, precision, K)
      warm_epidemic_inits <- warm_inits[
        setdiff(names(warm_inits), "delay_logits")
      ]
      collected <- list()
      for (k in seq_len(K)) {
        diagnostics$attempted_K <- diagnostics$attempted_K + 1L
        exp_logits <- exp(logit_draws[, k])
        imputed_simplex <- c(exp_logits, 1) / (sum(exp_logits) + 1)
        imputation_priors <- fix_param(priors, "delay_probs", imputed_simplex)
        fit_error <- NULL
        imputation_fit <- tryCatch(
          fit(
            model, engine, priors = imputation_priors,
            init = warm_epidemic_inits, warn = FALSE
          ),
          error = function(e) {
            fit_error <<- conditionMessage(e)
            NULL
          }
        )
        if (!is.null(imputation_fit) && .fit_is_adequate(imputation_fit)) {
          collected[[length(collected) + 1L]] <- imputation_fit
        } else {
          reason <- if (is.null(imputation_fit)) {
            fit_error %||% "fit failed"
          } else {
            paste(imputation_fit$diagnostic_reasons, collapse = "; ")
          }
          diagnostics$exclusion_reasons <- c(
            diagnostics$exclusion_reasons,
            stats::setNames(reason, paste0("imputation_", k))
          )
        }
      }
      if (length(collected) > 0L) {
        return(.finish_nowcast_collection(
          collected, "multi", target, diagnostics, warn = warn
        ))
      }
    }
  }

  # -- Two-stage PARAMETRIC (windowed Stage-1, impute mu/sigma) -----------------
  # When a revision process is active, only the EVENT-TO-REPORT parameters are
  # fixed by these imputations. The revision delay and p remain free in every
  # Stage-2 objective, where their cure/marked-state likelihood is estimated
  # jointly with the epidemic. Its posterior is sampled within each fitted block;
  # stacking blocks adds the reporting-delay imputation uncertainty.
  delay_estimate <- NULL
  if (!is_nonparametric) {
    stage1_error <- NULL
    delay_fit <- tryCatch({
      window <- .window_delay_m(m, max_time, delay_window)
      delay_engine <- prepare_data(
        model, window$m, max_time = window$max_time, delay_only = TRUE
      )
      fit(
        model, delay_engine, priors = default_priors(model, delay_engine),
        warn = FALSE
      )
    }, error = function(e) {
      stage1_error <<- conditionMessage(e)
      NULL
    })
    if (!is.null(delay_fit)) {
      delay_fit <- .attach_fit_diagnostic(delay_fit)
      diagnostics$stage1 <- .fit_diagnostic_summary(delay_fit)
      if (.fit_is_adequate(delay_fit)) {
        delay_estimate <- list(
          mu = delay_fit$delay_mu,
          sigma = delay_fit$delay_sigma,
          mu_sd = delay_fit$delay_mu_sd,
          sigma_sd = delay_fit$delay_sigma_sd,
          shape_Q = delay_fit$delay_Q
        )
      }
    } else {
      diagnostics$stage1 <- list(
        status = "error", adequate = FALSE,
        reasons = stage1_error %||% "Stage-1 delay fit failed"
      )
    }
  }
  is_gengamma <- model@delay@num_id == 3L

  if (!is.null(delay_estimate) && !is.null(warm_inits)) {
    spread_mu    <- max(floor_mu, if (is.finite(delay_estimate$mu_sd)) delay_estimate$mu_sd else 0)
    spread_sigma <- max(floor_sig_frac * delay_estimate$sigma,
                        if (is.finite(delay_estimate$sigma_sd)) delay_estimate$sigma_sd else 0)
    imputed_mu    <- rnorm(K, delay_estimate$mu, spread_mu)
    imputed_sigma <- pmax(0.05, rnorm(K, delay_estimate$sigma, spread_sigma))
    warm_epidemic_inits <- warm_inits[setdiff(names(warm_inits),
                                              c("delay_mu", "log_delay_sigma_excess", "delay_Q"))]
    collected <- list()
    for (k in seq_len(K)) {
      imputation_priors <- fix_param(fix_param(priors, "delay_mu", imputed_mu[k]),
                                     "delay_sigma", imputed_sigma[k])
      if (is_gengamma && is.finite(delay_estimate$shape_Q %||% NA))
        imputation_priors <- fix_param(imputation_priors, "delay_Q", delay_estimate$shape_Q)
      diagnostics$attempted_K <- diagnostics$attempted_K + 1L
      fit_error <- NULL
      imputation_fit <- tryCatch(
        fit(
          model, engine, priors = imputation_priors,
          init = warm_epidemic_inits, warn = FALSE
        ),
        error = function(e) {
          fit_error <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(imputation_fit) && .fit_is_adequate(imputation_fit)) {
        collected[[length(collected) + 1L]] <- imputation_fit
      } else {
        reason <- if (is.null(imputation_fit)) {
          fit_error %||% "fit failed"
        } else {
          paste(imputation_fit$diagnostic_reasons, collapse = "; ")
        }
        diagnostics$exclusion_reasons <- c(
          diagnostics$exclusion_reasons,
          stats::setNames(reason, paste0("imputation_", k))
        )
      }
    }
    if (length(collected) > 0L) {
      return(.finish_nowcast_collection(
        collected, "multi", target, diagnostics, warn = warn
      ))
    }
  }

  # -- Fallback: anchored prior (parametric) then plain one-stage ---------------
  if (!is.null(delay_estimate)) {
    anchored_priors <- default_priors(model, engine, phi = priors$phi_nb_prior %||% lognormal_prior(log(20), 0.5),
      delay_mu    = normal_prior(delay_estimate$mu, max(0.10, delay_estimate$mu_sd %||% 0.10)),
      delay_sigma = gamma_prior(4, 4 / max(0.5, delay_estimate$sigma)))
    anchored_error <- NULL
    anchored_fit <- tryCatch(
      fit(
        model, engine, priors = anchored_priors,
        init = warm_inits, warn = FALSE
      ),
      error = function(e) {
        anchored_error <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(anchored_fit)) {
      diagnostics$fallback$anchored <- .fit_diagnostic_summary(anchored_fit)
    } else {
      diagnostics$fallback$anchored <- list(
        status = "error", reasons = anchored_error %||% "anchored fit failed"
      )
    }
    if (!is.null(anchored_fit) && .fit_is_adequate(anchored_fit)) {
      return(.finish_nowcast_collection(
        list(anchored_fit), "anchored", target, diagnostics, warn = warn
      ))
    }
  }
  onestage <- fit(model, engine, priors = priors, warn = FALSE)
  diagnostics$fallback$onestage <- .fit_diagnostic_summary(onestage)
  .finish_nowcast_collection(
    list(onestage), "onestage", target, diagnostics, warn = warn
  )
}

#' Pool the posterior-predictive nowcast draws across a list of fits.
#' @param fits list of fit objects.
#' @param target event-time index.
#' @param n_draws draws per fit.
#' @returns list(M = pooled total `[Sigma n_draws x max_time]` matrix,
#'   lambda = pooled latent total, M_strata = pooled `[Sigma n_draws x max_time
#'   x n_strata]` array (or NULL when unstratified)).
#' @keywords internal
#' @noRd
.pool_fit_draws <- function(fits, target, n_draws = 200L) {
  # Draw from each fit separately, then stack the draws.  For a one-stage fit
  # there is a single block; for two-stage there is one block per imputation, and
  # stacking them pools the delay uncertainty across imputations.
  nowcast_blocks <- vector("list", length(fits))
  lambda_blocks  <- vector("list", length(fits))
  strata_blocks  <- vector("list", length(fits))
  projection_count <- 0L
  estimand <- reconstruction <- NULL
  regularization_blocks <- vector("list", length(fits))
  n_strata <- 1L
  for (fit_index in seq_along(fits)) {
    fit_draws <- .nowcast_draws(fits[[fit_index]], target = target, n_draws = n_draws)
    nowcast_blocks[[fit_index]] <- fit_draws$M
    lambda_blocks[[fit_index]]  <- fit_draws$lambda_draws
    strata_blocks[[fit_index]]  <- fit_draws$M_strata
    projection_count <- projection_count +
      as.integer(fit_draws$negative_projection_count %||% 0L)
    estimand <- estimand %||% fit_draws$estimand
    reconstruction <- reconstruction %||%
      fit_draws$cumulative_reconstruction
    regularization_blocks[[fit_index]] <-
      fit_draws$laplace_regularization %||% list(
        applied = NA, method = "unknown", ridge = NA_real_,
        eigenvalue_floor = NA_real_, original_cholesky = NA
      )
    n_strata <- fit_draws$n_strata %||% 1L
  }

  # Per-stratum draws are a 3-D array [draws x time x strata]; only pool them when
  # the fit is actually stratified and every block produced one.
  pooled_strata <- NULL
  if (n_strata > 1L && all(!vapply(strata_blocks, is.null, logical(1)))) {
    n_time <- dim(strata_blocks[[1]])[2]
    pooled_strata <- array(NA_real_, c(0L, n_time, n_strata))   # empty; grown below
    for (stratum_block in strata_blocks)
      pooled_strata <- abind_draws(pooled_strata, stratum_block)
  }

  list(M = do.call(rbind, nowcast_blocks),
       lambda = do.call(rbind, lambda_blocks),
       M_strata = pooled_strata, n_strata = n_strata,
       estimand = estimand,
       cumulative_reconstruction = reconstruction,
       negative_projection_count = projection_count,
       laplace_sampling = list(
         any_regularized = any(vapply(
           regularization_blocks,
           function(item) isTRUE(item$applied), logical(1)
         )),
         fits = regularization_blocks
       ))
}

#' Bind two `[draws x time x strata]` arrays along the draws (first) dimension
#'
#' A small base-R stand-in for `abind::abind(..., along = 1)`, used to pool
#' per-stratum draw arrays without taking on an extra dependency.
#'
#' @param first,second Numeric 3-D arrays sharing the same time and strata
#'   dimensions (either may be `NULL` or have zero draws, in which case the other
#'   is returned unchanged).
#' @returns The two arrays stacked along the first (draws) dimension.
#' @keywords internal
#' @noRd
abind_draws <- function(first, second) {
  if (is.null(first)  || dim(first)[1]  == 0L) return(second)
  if (is.null(second) || dim(second)[1] == 0L) return(first)

  n_draws_first  <- dim(first)[1]
  n_draws_second <- dim(second)[1]
  n_time         <- dim(first)[2]
  n_strata       <- dim(first)[3]

  combined <- array(NA_real_, c(n_draws_first + n_draws_second, n_time, n_strata))
  combined[seq_len(n_draws_first), , ] <- first
  combined[n_draws_first + seq_len(n_draws_second), , ] <- second
  combined
}
