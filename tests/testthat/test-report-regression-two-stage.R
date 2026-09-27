# The two-stage cascade with a reporting regression.  The bargain Stage 2 relies
# on is that Stage 1 has already read the delay observations, so Stage 2 must fix
# the WHOLE reporting process -- baseline and hazard coefficients -- or it reads
# them twice.

regression_series <- function(seed = 8, Tn = 50) {
  set.seed(seed)
  start <- as.Date("2023-01-02")
  now <- start + Tn - 1
  rows <- do.call(rbind, lapply(seq_len(Tn), function(t) {
    n <- rpois(1, 30)
    if (!n) return(NULL)
    data.frame(onset = start + t - 1,
               reported = start + t - 1 + pmin(rgeom(n, 0.35), 20), n = 1)
  }))
  rows <- rows[rows$reported <= now, ]
  rows <- stats::aggregate(list(n = rows$n), rows[c("onset", "reported")], sum)
  list(now = now, tn = tbl.now::tbl_now(
    rows, event_date = onset, report_date = reported, case_count = n,
    data_type = "count-incidence", now = now, verbose = FALSE) |>
    tbl.now::add_temporal_effects(tbl.now::temporal_effects(weekend = TRUE),
                                  date_type = "report_date") |>
    tbl.now::compute_temporal_effects())
}

test_that("Stage 2 fixes the coefficients out of the free parameter vector", {
  series <- regression_series()
  mdl <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
  engine <- prepare_from_tbl_now(series$tn, mdl, now = series$now)$data
  priors <- default_priors(mdl, engine)

  free <- build_joint_obj(engine, priors, use_random = FALSE)
  expect_true("delay_beta" %in% names(free$obj$par))

  stage2_priors <- fix_param(fix_param(priors, "delay_mu", 1.1),
                             "delay_sigma", 3.0)
  stage2_priors$delay_beta_fixed <- 0.42
  stage2 <- build_joint_obj(engine, stage2_priors, use_random = FALSE)
  expect_false("delay_beta" %in% names(stage2$obj$par))
  expect_equal(length(stage2$obj$par), length(free$obj$par) - 1L - 2L)
  expect_equal(as.numeric(stage2$obj$env$parList()$delay_beta), 0.42)
})

test_that("a Stage-2 fit still reports through the imputed tilt", {
  # Fixing the coefficients must not quietly fall back to a stationary CDF: the
  # whole point is that this imputation's reporting law reaches the counts.
  series <- regression_series()
  mdl <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
  engine <- prepare_from_tbl_now(series$tn, mdl, now = series$now)$data
  priors <- fix_param(fix_param(default_priors(mdl, engine), "delay_mu", 1.1),
                      "delay_sigma", 3.0)
  gstar_for <- function(beta) {
    these <- priors
    these$delay_beta_fixed <- beta
    built <- build_joint_obj(engine, these, use_random = FALSE)
    .joint_reconstruct(engine, these, built$obj$env$parList(),
                       built$Bmat, built$freq)$Gstar[, 1]
  }
  expect_gt(max(abs(gstar_for(0.42) - gstar_for(0))), 0.01)
})

test_that("the Stage-1 window slices the reporting designs consistently", {
  # Window cohort t maps to global event time since + t - 1, so the calendar has
  # to lose exactly its first since - 1 rows or destinations shift.
  engine <- list(
    max_time = 10L,
    report_calendar = matrix(seq_len(10), ncol = 1L,
                             dimnames = list(NULL, ".report_x")),
    report_cohort = array(rep(seq_len(10), 2), c(10L, 2L, 1L))
  )
  sliced <- .window_report_designs(engine, since = 4L)
  expect_equal(as.numeric(sliced$report_calendar), 4:10)
  expect_equal(dim(sliced$report_cohort), c(7L, 2L, 1L))
  expect_equal(as.numeric(sliced$report_cohort[, 1L, 1L]), 4:10)
})

test_that("marginal inflation widens a coordinate without breaking correlation", {
  set.seed(3)
  latent <- rnorm(400)
  draws <- rbind(delay_mu = latent + rnorm(400, 0, 0.05),
                 delay_beta = latent + rnorm(400, 0, 0.05))
  before <- stats::cor(draws["delay_mu", ], draws["delay_beta", ])
  widened <- .inflate_marginal_spread(draws, c(delay_mu = 5))
  expect_gt(stats::sd(widened["delay_mu", ]), 4.5)
  # The other coordinate is untouched and the dependence survives.
  expect_equal(widened["delay_beta", ], draws["delay_beta", ])
  expect_equal(stats::cor(widened["delay_mu", ], widened["delay_beta", ]),
               before, tolerance = 1e-12)
  # A floor below the observed spread must leave the draws alone.
  expect_equal(.inflate_marginal_spread(draws, c(delay_mu = 1e-6)), draws)
})

test_that("two-stage runs with a reporting regression and agrees with one-stage", {
  skip_on_cran()
  series <- regression_series()
  for (delay in list(lognormal_delay(), dirichlet_delay(bins = 10))) {
    mdl <- model(nb_likelihood(), ar1_epidemic(), delay)
    fits <- lapply(c("one_stage", "two_stage"), function(type)
      suppressWarnings(suppressMessages(nowcast(
        series$tn, mdl, type = type, K = 6, n_draws = 200,
        temporal_effects = "none", seed = 4))))
    natives <- lapply(fits, diseasenowcasting:::.unwrap_nowcast)
    expect_equal(natives[[1]]@fit_diagnostics$resolved_type, "one_stage")
    expect_equal(natives[[2]]@fit_diagnostics$resolved_type, "two_stage")
    expect_length(natives[[2]]@fits, 6L)
    betas <- vapply(natives, function(nc) {
      coefficients <- coef(nc)
      unname(coefficients[grep("^delay_beta", names(coefficients))][1])
    }, numeric(1))
    expect_true(all(is.finite(betas)))
    # Pooling over imputations moves the point estimate, but not to a different
    # story about the reporting process.
    expect_lt(abs(betas[1] - betas[2]), 0.5)
  }
})
