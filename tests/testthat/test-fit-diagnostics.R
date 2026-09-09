static_objective <- function(gradient, hessian) {
  list(
    gr = function(par) gradient,
    he = function(par) hessian
  )
}

diagnose_static <- function(par, gradient, hessian,
                            lower = rep(-Inf, length(par)),
                            upper = rep(Inf, length(par)),
                            objective = 0, convergence = 0L) {
  diseasenowcasting:::.joint_fit_diagnostic(
    static_objective(gradient, hessian),
    list(par = par, objective = objective, convergence = convergence),
    list(lower = lower, upper = upper)
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

  metadata$retained_fit_diagnostics <- data.frame(status = "pass")
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
