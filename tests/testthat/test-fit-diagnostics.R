static_objective <- function(gradient, hessian) {
  list(
    gr = function(par) gradient,
    he = function(par) hessian
  )
}

diagnose_static <- function(par, gradient, hessian,
                            lower = rep(-Inf, length(par)),
                            upper = rep(Inf, length(par)),
                            objective = 0, convergence = 0L,
                            log_mean = NULL,
                            log_mean_upper_bound = NA_real_) {
  diseasenowcasting:::.joint_fit_diagnostic(
    static_objective(gradient, hessian),
    list(par = par, objective = objective, convergence = convergence),
    list(lower = lower, upper = upper),
    log_mean = log_mean, log_mean_upper_bound = log_mean_upper_bound
  )
}

test_that("box-bound diagnostics use KKT signs rather than raw gradients", {
  lower_solution <- diagnose_static(
    par = c(x = 0), gradient = 5, hessian = matrix(1),
    lower = 0, upper = Inf
  )
  upper_solution <- diagnose_static(
    par = c(x = 1), gradient = -4, hessian = matrix(1),
    lower = -Inf, upper = 1
  )
  violating_lower <- diagnose_static(
    par = c(x = 0), gradient = -1, hessian = matrix(1),
    lower = 0, upper = Inf
  )

  expect_equal(lower_solution$max_gradient, 5)
  expect_equal(lower_solution$projected_gradient, 0)
  expect_equal(lower_solution$quadratic_gap, 0)
  expect_true(lower_solution$adequate)

  expect_equal(upper_solution$max_gradient, 4)
  expect_equal(upper_solution$projected_gradient, 0)
  expect_true(upper_solution$adequate)

  expect_equal(violating_lower$projected_gradient, 1)
  expect_equal(violating_lower$quadratic_gap, 0.5)
  expect_false(violating_lower$adequate)
  expect_match(violating_lower$reasons, "quadratic objective gap")
})

test_that("quadratic objective gap is invariant to linear rescaling", {
  gradient <- c(0.1, 0.2)
  hessian <- diag(c(2, 4))
  original <- diagnose_static(c(x = 0, y = 0), gradient, hessian)

  # theta = A phi implies g_phi = A' g and H_phi = A' H A.
  A <- diag(c(100, 0.01))
  transformed <- diagnose_static(
    c(x = 0, y = 0),
    as.numeric(t(A) %*% gradient),
    t(A) %*% hessian %*% A
  )

  expect_false(isTRUE(all.equal(
    original$max_gradient, transformed$max_gradient
  )))
  expect_equal(original$quadratic_gap, transformed$quadratic_gap,
               tolerance = 1e-12)
  expect_identical(original$adequate, transformed$adequate)
})

test_that("polishing and adequacy are invariant to objective shifts", {
  target <- c(x = 1, y = -2)
  start <- c(x = 7, y = 5)
  bounds <- list(lower = rep(-Inf, 2L), upper = rep(Inf, 2L))

  run_shifted <- function(shift) {
    objective <- list(
      fn = function(par) shift + 0.5 * sum((par - target)^2),
      gr = function(par) par - target,
      he = function(par) diag(2L)
    )
    initial <- list(
      par = start,
      objective = objective$fn(start),
      convergence = 0L
    )
    polished <- diseasenowcasting:::.polish_joint_candidate(
      objective, initial, bounds
    )
    list(
      polished = polished,
      diagnostic = diseasenowcasting:::.joint_fit_diagnostic(
        objective, polished$opt, bounds
      )
    )
  }

  unshifted <- run_shifted(0)
  shifted <- run_shifted(1e10)

  expect_true(unshifted$polished$polished)
  expect_true(shifted$polished$polished)
  expect_equal(
    unshifted$polished$opt$par,
    shifted$polished$opt$par,
    tolerance = 1e-8
  )
  expect_equal(
    unshifted$polished$diagnostic$centered_objective,
    shifted$polished$diagnostic$centered_objective,
    tolerance = 1e-8
  )
  expect_identical(
    unshifted$diagnostic$adequate,
    shifted$diagnostic$adequate
  )
  expect_true(shifted$diagnostic$adequate)
})

test_that("an accepted polish preserves success of the optimizer path", {
  effective <- diseasenowcasting:::.effective_optimizer_convergence

  # A line-search termination during an improving refinement does not erase a
  # successful base solve; a successful refinement can also rehabilitate one.
  expect_equal(effective(0L, 52L, TRUE), 0L)
  expect_equal(effective(1L, 0L, TRUE), 0L)
  expect_equal(effective(1L, 52L, TRUE), 52L)
  expect_equal(effective(0L, 52L, FALSE), 0L)
})

test_that("positive curvature is required for optimizer adequacy", {
  indefinite <- diagnose_static(
    c(x = 0, y = 0), c(0, 0), diag(c(1, -1))
  )

  expect_false(indefinite$hessian_positive_definite)
  expect_identical(
    indefinite$hessian_status,
    "not_positive_definite_on_free_subspace"
  )
  expect_false(indefinite$adequate)
  expect_match(paste(indefinite$reasons, collapse = "; "), "Hessian")
})

test_that("adequacy uses numerical curvature when analytic Hessian is unavailable", {
  objective <- list(
    fn = function(par) 0.5 * sum(par^2),
    gr = function(par) par,
    he = function(par) stop("analytic Hessian unavailable")
  )
  diagnostic <- diseasenowcasting:::.joint_fit_diagnostic(
    objective,
    list(par = c(x = 0, y = 0), objective = 0, convergence = 0L),
    list(lower = rep(-Inf, 2L), upper = rep(Inf, 2L))
  )

  expect_true(diagnostic$adequate)
  expect_true(diagnostic$hessian_positive_definite)
  expect_identical(diagnostic$hessian_source, "finite_difference")
  expect_equal(diagnostic$quadratic_gap, 0, tolerance = 1e-12)
})

test_that("curvature is checked on the locally free subspace", {
  constrained <- diagnose_static(
    par = c(x = 0, y = 0), gradient = c(5, 0),
    hessian = diag(c(-1, 2)), lower = c(0, -Inf), upper = c(Inf, Inf)
  )

  expect_true(constrained$adequate)
  expect_equal(constrained$active_bounds, 1L)
  expect_equal(constrained$curvature_dimension, 1L)
  expect_identical(unname(constrained$free_coordinates), c(FALSE, TRUE))
  expect_identical(
    constrained$hessian_status,
    "positive_definite_on_free_subspace"
  )
})

test_that("candidate selection separates adequacy from MAP objective", {
  candidate <- function(objective, adequate) {
    list(
      nll = objective,
      fit_status = if (adequate) "pass" else "warning",
      diagnostic = list(adequate = adequate)
    )
  }
  candidates <- list(
    candidate(8, TRUE),
    candidate(4, TRUE),
    candidate(1, FALSE)
  )

  selected <- diseasenowcasting:::.select_joint_candidate(candidates)
  expect_equal(selected$nll, 4)
  expect_true(diseasenowcasting:::.fit_is_adequate(selected))

  degraded <- diseasenowcasting:::.select_joint_candidate(list(
    candidate(8, FALSE), candidate(3, FALSE)
  ))
  expect_equal(degraded$nll, 3)
  expect_false(diseasenowcasting:::.fit_is_adequate(degraded))
})

test_that("collection warnings are final and aggregate", {
  adequate_fit <- list(
    nll = 1,
    convergence = 0L,
    max_gradient = 10,
    diagnostic = list(
      adequate = TRUE, status = "pass", reasons = character(),
      optimizer_convergence = 0L, max_gradient = 10,
      projected_gradient = 0, quadratic_gap = 0,
      hessian_positive_definite = TRUE
    )
  )
  metadata <- list(
    requested_type = "two_stage", resolved_type = "two_stage",
    requested_K = 3L, attempted_K = 3L, retained_K = 0L, excluded_K = 0L,
    exclusion_reasons = c(imputation_2 = "simulated failure",
                          imputation_3 = "simulated failure"),
    # A bad warm fit is informational and must not itself trigger the warning.
    warm_fit = list(used = TRUE, status = "warning"),
    stage1 = list(status = "pass"), fallback = list()
  )

  expect_warning(
    result <- diseasenowcasting:::.finish_nowcast_collection(
      list(adequate_fit), "multi", 1L, metadata
    ),
    "2 of 3 attempted Stage-2 imputation fits were excluded"
  )
  expect_equal(result$diagnostics$retained_K, 1L)
  expect_equal(result$diagnostics$excluded_K, 2L)

  metadata$attempted_K <- 1L
  metadata$exclusion_reasons <- character()
  expect_no_warning(
    diseasenowcasting:::.finish_nowcast_collection(
      list(adequate_fit), "multi", 1L, metadata
    )
  )

  metadata$retained_fit_diagnostics <- data.frame(status = "pass", adequate = TRUE)
  metadata$laplace_sampling <- list(
    any_regularized = TRUE,
    fits = list(list(
      applied = TRUE, method = "diagonal_ridge", ridge = 0.01,
      eigenvalue_floor = 0, original_cholesky = FALSE
    ))
  )
  expect_warning(
    diseasenowcasting:::.warn_laplace_sampling(metadata),
    "required regularization"
  )

  native <- diseasenowcasting:::nowcast_class(
    model = model(), data = NULL, now = Sys.Date(), type = "one_stage",
    fits = list(adequate_fit), rung = "onestage", target = 1,
    engine = list(), priors = list(), phi = NULL, n_draws = 1,
    fit_diagnostics = metadata
  )
  checked <- fit_check(native, warn = FALSE)
  expect_true(checked$laplace_regularized)
  expect_identical(checked$laplace_regularization, "diagonal_ridge")
  expect_equal(checked$laplace_ridge, 0.01)
  expect_identical(checked$fit_status, "warning")
  expect_match(checked$reasons, "Laplace precision regularized")
})

test_that("Colombia two-stage fit reports only retained-fit adequacy", {
  skip_on_cran()
  data("covid_colombia", package = "tbl.now", envir = environment())
  cutoff <- as.Date("2021-04-01")
  covid <- dplyr::filter(
    covid_colombia,
    notification_date < cutoff,
    diagnosis_date < cutoff
  )
  covid_now <- suppressWarnings(tbl.now::tbl_now(
    covid,
    event_date = notification_date,
    report_date = diagnosis_date,
    case_count = n,
    data_type = "count-incidence"
  ))

  fitted <- suppressWarnings(suppressMessages(
    nowcast(
      covid_now, type = "two_stage", K = 25L, n_draws = 25L,
      seed = 27894L
    )
  ))

  expect_identical(fitted@type, "two_stage")
  expect_identical(fitted@rung, "multi")
  expect_equal(fitted@fit_diagnostics$requested_K, 25L)
  expect_equal(fitted@fit_diagnostics$attempted_K, 25L)
  expect_gt(fitted@fit_diagnostics$retained_K, 0L)
  expect_equal(
    fitted@fit_diagnostics$retained_K + fitted@fit_diagnostics$excluded_K,
    fitted@fit_diagnostics$attempted_K
  )
  expect_true(fitted@fit_diagnostics$warm_fit$used)
  expect_identical(
    fitted@metadata$diseasenowcasting$fit_diagnostics,
    fitted@fit_diagnostics
  )

  checked <- fit_check(fitted, warn = FALSE)
  expect_true(all(checked$fit_status == "pass"))
  expect_true(all(checked$hessian_positive_definite))
  expect_true(all(checked$quadratic_gap <= 0.01))
  expect_true(all(!checked$laplace_regularized))
  expect_false(fitted@fit_diagnostics$laplace_sampling$any_regularized)
})

test_that("legacy matrix two-stage interface exposes the shared diagnostics", {
  skip_on_cran()
  synthetic <- .make_synth(Tn = 50L, seed = 27894)
  model <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())

  result <- suppressWarnings(nowcast_twostage(
    model, synthetic$m, max_time = synthetic$Tn,
    K = 2L, n_draws_per = 10L, seed = 27894
  ))

  expect_identical(result$fit_diagnostics$requested_type, "two_stage")
  expect_identical(result$fit_diagnostics$resolved_type, "two_stage")
  expect_equal(
    result$fit_diagnostics$retained_K,
    if (identical(result$rung, "multi")) result$n_samp else 0L
  )
  expect_true(is.data.frame(
    result$fit_diagnostics$retained_fit_diagnostics
  ))
})

# =============================================================================
# The softplus ceiling on the latent log_mean
# =============================================================================
# `prepare_data()` caps log_mean at `mu_log_upper_bound` and the objective
# applies it downstream of every epidemic process, so a bound below the latent
# scale truncates the nowcast identically for all of them -- silently, because
# the optimiser converges perfectly to the ceiling.  These pin the diagnostic
# that makes it audible.

test_that(".log_mean_cap_diagnostic() reports the gap, not just a boolean", {
  cap <- diseasenowcasting:::.log_mean_cap_diagnostic(
    log_mean = matrix(c(1, 2, 4.5), ncol = 1L), upper_bound = 6
  )
  expect_equal(cap$log_mean_upper_bound, 6)
  expect_equal(cap$max_log_mean, 4.5)
  expect_equal(cap$log_mean_headroom, 1.5)
  expect_true(cap$log_mean_cap_bound)
  expect_match(cap$reason, "within 1.5 of its upper bound 6")

  clear <- diseasenowcasting:::.log_mean_cap_diagnostic(
    log_mean = matrix(c(1, 2, 2.5), ncol = 1L), upper_bound = 6
  )
  expect_equal(clear$log_mean_headroom, 3.5)
  expect_false(clear$log_mean_cap_bound)
  expect_length(clear$reason, 0L)
})

test_that(".log_mean_cap_diagnostic() fires at the 5%-distortion boundary", {
  # capped = ub - log1p(exp(ub - log_mean)), so lambda keeps exactly
  # plogis(headroom) of its value.  Three log units is 95.3%.
  just_inside <- diseasenowcasting:::.log_mean_cap_diagnostic(0, 3 - 1e-8)
  just_outside <- diseasenowcasting:::.log_mean_cap_diagnostic(0, 3 + 1e-8)
  expect_true(just_inside$log_mean_cap_bound)
  expect_false(just_outside$log_mean_cap_bound)
  expect_equal(round(100 * stats::plogis(3), 1), 95.3)
})

test_that(".log_mean_cap_diagnostic() is quiet without a bound or a fit", {
  expect_false(
    diseasenowcasting:::.log_mean_cap_diagnostic(c(1, 2), NULL)$log_mean_cap_bound
  )
  expect_false(
    diseasenowcasting:::.log_mean_cap_diagnostic(c(1, 2), Inf)$log_mean_cap_bound
  )
  expect_false(
    diseasenowcasting:::.log_mean_cap_diagnostic(NULL, 6)$log_mean_cap_bound
  )
  expect_false(
    diseasenowcasting:::.log_mean_cap_diagnostic(c(NA, NaN), 6)$log_mean_cap_bound
  )
})

test_that("a cap-bound fit is a warning without being optimizer-inadequate", {
  bound <- diagnose_static(
    par = c(x = 0), gradient = 0, hessian = matrix(1),
    log_mean = 5.5, log_mean_upper_bound = 6
  )
  clear <- diagnose_static(
    par = c(x = 0), gradient = 0, hessian = matrix(1),
    log_mean = 1, log_mean_upper_bound = 6
  )

  # The optimiser is at a textbook mode in both: adequacy must not move.
  expect_true(bound$adequate)
  expect_true(clear$adequate)
  # But the reported quantity is truncated in one of them, and `status` says so.
  expect_identical(bound$status, "warning")
  expect_identical(clear$status, "pass")
  expect_true(bound$log_mean_cap_bound)
  expect_equal(bound$log_mean_headroom, 0.5)
  expect_match(paste(bound$reasons, collapse = "; "), "upper bound")
  expect_length(clear$reasons, 0L)
})

test_that("fit_check() carries the cap headroom and warns when the bound binds", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 60L)
  mdl <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay())

  clear <- suppressMessages(nowcast(
    tn, mdl, type = "one_stage", n_draws = 50L, seed = 4L
  ))
  checked <- fit_check(clear, warn = FALSE)
  expect_true(all(c("log_mean_upper_bound", "max_log_mean",
                    "log_mean_headroom", "log_mean_cap_bound") %in%
                    names(checked)))
  expect_false(any(checked$log_mean_cap_bound))
  expect_gt(min(checked$log_mean_headroom), 3)
  # Sanity: the default bound really is above the observed scale it is built
  # from, so the cap does nothing on ordinary data.
  expect_gt(clear@engine$mu_log_upper_bound, log1p(max(clear@engine$case_counts)))

  # Force a ceiling the fit cannot help but hit.
  bound <- suppressMessages(suppressWarnings(nowcast(
    tn, mdl, type = "one_stage", n_draws = 50L, seed = 4L,
    mu_log_upper_bound = 1
  )))
  bound_check <- fit_check(bound, warn = FALSE)
  expect_true(all(bound_check$log_mean_cap_bound))
  expect_true(all(bound_check$log_mean_headroom < 3))
  expect_true(all(bound_check$fit_status == "warning"))
  expect_match(paste(bound_check$reasons, collapse = "; "), "upper bound")
  # It is a reporting failure, not an optimizer one, and stays labelled as such.
  expect_true(all(bound_check$optimizer_adequate))
  expect_warning(fit_check(bound), "reached the `log_mean` upper bound")
})

test_that("the cap warning offers the legacy ceiling with a concrete value", {
  # A cap-bound fit is ambiguous: the stream may genuinely need the inflation
  # (early-2020 covid_us needs 37-83x) or the process may be running away.  The
  # warning therefore points at reporting_fraction() first and offers the
  # pre-2.5.0 ceiling as something to try, quoting the number so it can be
  # pasted.
  expect_warning(
    diseasenowcasting:::.warn_log_mean_cap(0.5, bound = 12, legacy_bound = 7.47),
    "mu_log_upper_bound = 7.47"
  )
  expect_warning(
    diseasenowcasting:::.warn_log_mean_cap(0.5, bound = 12, legacy_bound = 7.47),
    "reporting_fraction"
  )
  # Not offered when it would not actually be tighter, or is unknown.
  no_legacy <- capture_warnings(
    diseasenowcasting:::.warn_log_mean_cap(0.5, bound = 12, legacy_bound = NA_real_))
  expect_false(any(grepl("pre-2.5.0", no_legacy)))
  same_bound <- capture_warnings(
    diseasenowcasting:::.warn_log_mean_cap(0.5, bound = 12, legacy_bound = 12))
  expect_false(any(grepl("pre-2.5.0", same_bound)))
})

test_that("prepare_data() records the legacy ceiling alongside the one in force", {
  m   <- .make_synth()$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng <- prepare_data(mdl, m)
  casemax <- max(abs(eng$case_counts))
  expect_equal(eng$mu_log_upper_bound_legacy, min(max(6, log1p(casemax)), 16))
  # The legacy bound is strictly tighter wherever the new one has not hit 16.
  expect_lt(eng$mu_log_upper_bound_legacy, eng$mu_log_upper_bound)
  # An override changes the bound in force but never the recorded legacy value.
  eng2 <- prepare_data(mdl, m, mu_log_upper_bound = 9)
  expect_equal(eng2$mu_log_upper_bound, 9)
  expect_equal(eng2$mu_log_upper_bound_legacy, eng$mu_log_upper_bound_legacy)
})

# -----------------------------------------------------------------------------
# .refine_on_quadratic_gap(): take the step the gap has already priced
# -----------------------------------------------------------------------------
quadratic_objective <- function(hessian, mode) {
  list(
    fn = function(par) as.numeric(0.5 * t(par - mode) %*% hessian %*% (par - mode)),
    gr = function(par) as.numeric(hessian %*% (par - mode)),
    he = function(par) hessian
  )
}

refine_quadratic <- function(hessian, mode, start,
                             lower = rep(-Inf, length(start)),
                             upper = rep(Inf, length(start)),
                             overrides = list()) {
  objective <- quadratic_objective(hessian, mode)
  bounds <- list(lower = lower, upper = upper)
  optimum <- list(par = start, objective = objective$fn(start), convergence = 0L)
  diagnostic <- utils::modifyList(
    diseasenowcasting:::.joint_fit_diagnostic(objective, optimum, bounds),
    overrides
  )
  list(
    refinement = diseasenowcasting:::.refine_on_quadratic_gap(
      objective, optimum, bounds, diagnostic
    ),
    diagnostic = diagnostic,
    objective = objective
  )
}

test_that("a binding quadratic gap is closed by the Newton step it prices", {
  set.seed(20260919)
  hessian <- crossprod(matrix(stats::rnorm(25), 5, 5)) + diag(5)
  mode <- stats::rnorm(5)
  start <- stats::setNames(rep(0, 5), paste0("x", seq_len(5)))

  out <- refine_quadratic(hessian, mode, start)

  # The gap is what makes the starting point inadequate ...
  expect_false(out$diagnostic$adequate)
  expect_gt(out$diagnostic$quadratic_gap, 0.01)
  expect_match(out$diagnostic$reasons, "quadratic objective gap")

  # ... and one Newton step on an exact quadratic lands on the mode.
  expect_true(out$refinement$applied)
  expect_identical(out$refinement$steps, 1L)
  expect_equal(as.numeric(out$refinement$opt$par), mode, tolerance = 1e-8)
  expect_equal(out$refinement$objective_change, -out$diagnostic$quadratic_gap,
               tolerance = 1e-8)
  expect_match(out$refinement$opt$message, "Newton refinement")

  # The refined point passes the check that the starting point failed.
  refined <- diseasenowcasting:::.joint_fit_diagnostic(
    out$objective, out$refinement$opt,
    list(lower = rep(-Inf, 5), upper = rep(Inf, 5))
  )
  expect_true(refined$adequate)
  expect_lt(refined$quadratic_gap, 1e-12)
})

test_that("refinement declines whenever the gap is not the failure", {
  set.seed(20260919)
  hessian <- crossprod(matrix(stats::rnorm(25), 5, 5)) + diag(5)
  mode <- stats::rnorm(5)
  start <- stats::setNames(rep(0, 5), paste0("x", seq_len(5)))
  decline <- function(overrides) {
    refine_quadratic(hessian, mode, start, overrides = overrides)$refinement
  }

  # An adequate fit is never touched, so a passing fit pays nothing.
  expect_identical(decline(list(adequate = TRUE))$reason, "already_adequate")
  # Indefinite curvature is a different failure with a different remedy.
  expect_identical(decline(list(hessian_positive_definite = FALSE))$reason,
                   "hessian_not_positive_definite")
  # And a gap under tolerance leaves nothing to buy.
  expect_identical(decline(list(quadratic_gap = 1e-6))$reason, "gap_not_binding")
  for (reason in c("already_adequate", "hessian_not_positive_definite",
                   "gap_not_binding")) {
    expect_false(decline(list(
      adequate = reason == "already_adequate",
      hessian_positive_definite = reason != "hessian_not_positive_definite",
      quadratic_gap = if (reason == "gap_not_binding") 1e-6 else 5
    ))$applied)
  }
})

test_that("refinement leaves a strictly active bound alone", {
  hessian <- diag(c(2, 2))
  mode <- c(-3, 1)                       # x1's mode is outside the box
  start <- stats::setNames(c(0, 0), c("x1", "x2"))

  out <- refine_quadratic(hessian, mode, start, lower = c(0, -Inf))

  expect_true(out$refinement$applied)
  refined <- as.numeric(out$refinement$opt$par)
  expect_equal(refined[1], 0)            # held at the bound, not pulled to -3
  expect_equal(refined[2], 1, tolerance = 1e-8)
})

test_that(".box_active_set() agrees with the KKT signs the diagnostic reports", {
  par <- c(0, 1, 0.5)
  gradient <- c(5, -4, 0.2)
  bounds <- list(lower = c(0, -Inf, -Inf), upper = c(Inf, 1, Inf))

  active <- diseasenowcasting:::.box_active_set(par, gradient, bounds)

  expect_identical(active$at_lower, c(TRUE, FALSE, FALSE))
  expect_identical(active$at_upper, c(FALSE, TRUE, FALSE))
  # Correctly signed active coordinates satisfy KKT and are locally fixed.
  expect_equal(active$kkt_residual, c(0, 0, 0.2))
  expect_identical(active$free, c(FALSE, FALSE, TRUE))

  # Without the sign test every coordinate stays free and the raw gradient
  # is returned untouched, which is what a non-finite gradient needs.
  unsigned <- diseasenowcasting:::.box_active_set(
    par, gradient, bounds, apply_signs = FALSE
  )
  expect_equal(unsigned$kkt_residual, gradient)
  expect_true(all(unsigned$free))
})

test_that("refinement will not step onto indefinite curvature", {
  # 0.5 x^2 - 0.01 x^4 curves upward only for |x| < 2.887.  At x = 2.8 the
  # curvature is positive but nearly flat (0.059), so the Newton step is huge
  # and lands at about -29.7, where the objective is far LOWER and the
  # curvature is negative.  A line search that accepts on objective alone takes
  # that trade; the refinement must not, because the Laplace precision would
  # then need a ridge and the posterior draws would pay for it.
  objective <- list(
    fn = function(par) 0.5 * par[[1]]^2 - 0.01 * par[[1]]^4,
    gr = function(par) as.numeric(par[[1]] - 0.04 * par[[1]]^3),
    he = function(par) matrix(1 - 0.12 * par[[1]]^2, 1, 1)
  )
  bounds <- list(lower = -Inf, upper = Inf)
  start <- c(x = 2.8)
  optimum <- list(par = start, objective = objective$fn(start), convergence = 0L)
  diagnostic <- diseasenowcasting:::.joint_fit_diagnostic(
    objective, optimum, bounds
  )

  # The trap is real: positive curvature here, a binding gap, and the full
  # Newton step strictly lowers the objective.
  expect_true(diagnostic$hessian_positive_definite)
  expect_gt(diagnostic$quadratic_gap, 0.01)
  full_step <- start - objective$gr(start) / objective$he(start)[[1]]
  expect_lt(objective$fn(full_step), objective$fn(start))
  expect_lt(objective$he(full_step)[[1]], 0)

  refinement <- diseasenowcasting:::.refine_on_quadratic_gap(
    objective, optimum, bounds, diagnostic
  )

  # It declines rather than returning a point it never certified.
  expect_false(refinement$applied)
  expect_identical(refinement$reason, "no_certified_step")
  expect_identical(refinement$opt$par, start)
  expect_gt(objective$he(refinement$opt$par)[[1]], 0)
})

test_that("a declined refinement leaves `last.par.best` on the kept point", {
  # RTMB records `last.par.best` whenever `fn()` sees a lower objective, and
  # `.nowcast_draws()` samples the Laplace posterior at exactly that vector.
  # The line search evaluates better-but-rejected candidates, so a decline that
  # only re-evaluates the kept point leaves the DRAWS pointed at a discarded
  # one -- which is how a rejected step still costs the precision a ridge.
  environment_of <- new.env(parent = emptyenv())
  environment_of$last.par.best <- c(x = 2.8)
  environment_of$value.best <- 0.5 * 2.8^2 - 0.01 * 2.8^4
  objective <- list(
    fn = function(par) {
      value <- 0.5 * par[[1]]^2 - 0.01 * par[[1]]^4
      if (value < environment_of$value.best) {
        environment_of$last.par.best <- par
        environment_of$value.best <- value
      }
      value
    },
    gr = function(par) as.numeric(par[[1]] - 0.04 * par[[1]]^3),
    he = function(par) matrix(1 - 0.12 * par[[1]]^2, 1, 1),
    env = environment_of
  )
  bounds <- list(lower = -Inf, upper = Inf)
  start <- c(x = 2.8)
  optimum <- list(par = start, objective = objective$fn(start), convergence = 0L)
  diagnostic <- diseasenowcasting:::.joint_fit_diagnostic(
    objective, optimum, bounds
  )

  refinement <- diseasenowcasting:::.refine_on_quadratic_gap(
    objective, optimum, bounds, diagnostic
  )

  expect_false(refinement$applied)
  # The rejected candidate had a LOWER objective, so a naive restore leaves it
  # behind; the kept point must win anyway.
  expect_equal(as.numeric(environment_of$last.par.best), 2.8)
  expect_gt(objective$he(environment_of$last.par.best)[[1]], 0)
})
