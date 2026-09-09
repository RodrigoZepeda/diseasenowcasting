# Native model specifications delegate to tbl.now's canonical backtest.

test_that("backtest returns the canonical nowcast_backtest grammar", {
  tn <- .make_synth_tblnow(Tn = 100L, seed = 3)
  start <- min(tn$onset)
  dates <- start + c(50, 75) - 1
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())

  bt <- backtest(
    tn, mdl, dates = dates, type = "one_stage", n_draws = 100,
    keep_draws = TRUE, seed = 1, verbose = FALSE
  )

  expect_s3_class(bt, "nowcast_backtest")
  expect_setequal(bt$now_dates, dates)
  expect_true(nrow(bt$scores) > 0L)
  expect_true(nrow(bt$predictions) > 0L)
  expect_true(nrow(bt$draws) > 0L)
  expect_true(all(c(".method", ".now", "wis", "ae_median",
                    "coverage_50", "coverage_90") %in% names(bt$scores)))
  expect_s3_class(generics::tidy(bt), "tbl_df")
  if (requireNamespace("scoringutils", quietly = TRUE)) {
    expect_s3_class(scoringutils::as_forecast_quantile(bt), "forecast_quantile")
    expect_s3_class(scoringutils::as_forecast_point(bt), "forecast_point")
    expect_s3_class(scoringutils::as_forecast_sample(bt), "forecast_sample")
  }
})

test_that("backtest labels distinct native models for canonical weighting", {
  tn <- .make_synth_tblnow(Tn = 100L, seed = 4)
  start <- min(tn$onset)
  dates <- start + c(55, 75) - 1
  models <- list(
    model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
    model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
  )

  bt <- backtest(
    tn, models, dates = dates, type = "one_stage", n_draws = 100,
    seed = 1, verbose = FALSE
  )
  weights <- tbl.now::nowcast_weights(bt)

  expect_s3_class(bt, "nowcast_backtest")
  expect_length(bt$methods, 2L)
  expect_setequal(names(weights), bt$methods)
  expect_equal(sum(weights), 1, tolerance = 1e-8)
})

test_that("backtest rejects duplicate native model labels", {
  tn <- .make_synth_tblnow(Tn = 30L, seed = 5)
  duplicate <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
  expect_error(
    backtest(tn, list(duplicate, duplicate), verbose = FALSE),
    "unique specification"
  )
})

test_that("fit_check reports only RTMB fit diagnostics", {
  tn <- .make_synth_tblnow(Tn = 60L, seed = 6)
  fit <- nowcast(
    tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
    type = "one_stage", n_draws = 50L, temporal_effects = "none", seed = 1
  )
  checked <- fit_check(fit, warn = FALSE)

  expect_s3_class(checked, "data.frame")
  expect_true(all(c("fit", "rung", "convergence", "objective",
                    "max_gradient", "projected_gradient", "quadratic_gap",
                    "hessian_positive_definite", "hessian_status",
                    "laplace_regularized", "laplace_regularization",
                    "laplace_ridge", "laplace_eigenvalue_floor", "fit_status",
                    "gradient_status", "reasons") %in% names(checked)))
  expect_equal(nrow(checked), length(fit@fit@fits))
  expect_true(all(checked$convergence == 0L))
  expect_true(all(checked$fit_status == "pass"))
})

test_that("backtest truth defaults follow the native revision estimand", {
  make_revision_data <- function(outcomes) {
    n <- length(outcomes)
    origin <- as.Date("2024-01-01")
    report <- origin + seq_len(n) - 1L
    resolved <- outcomes != "pending"
    frame <- data.frame(
      event = report,
      report = report,
      revision = as.Date(ifelse(resolved, as.numeric(report), NA_real_),
                         origin = "1970-01-01"),
      outcome = outcomes
    )
    tbl.now::tbl_now(
      frame, event_date = event, report_date = report,
      revision_date = revision, revision_type = outcome,
      revision_units = "days", data_type = "linelist", verbose = FALSE
    )
  }

  confirmation <- diseasenowcasting:::.backtest_truth_defaults(
    make_revision_data(c("confirmed", "pending", "confirmed"))
  )
  retraction <- diseasenowcasting:::.backtest_truth_defaults(
    make_revision_data(c("retracted", "pending", "pending"))
  )
  both <- diseasenowcasting:::.backtest_truth_defaults(
    make_revision_data(c("confirmed", "retracted", "pending"))
  )

  expect_identical(confirmation, list(axis = "revision", type = "confirmed"))
  expect_identical(retraction, list(axis = "report", type = "pending"))
  expect_identical(both, list(axis = "revision", type = "confirmed"))
})
