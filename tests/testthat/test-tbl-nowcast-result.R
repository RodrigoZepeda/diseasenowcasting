test_that("nowcast() returns the common tbl_nowcast grammar", {
  dates <- as.Date("2020-01-01") + 0:9
  x <- tbl.now::tbl_now(
    data.frame(event = dates, report = dates, n = seq_along(dates)),
    event_date = "event",
    report_date = "report",
    case_count = "n",
    data_type = "count-incidence",
    verbose = FALSE
  )

  result <- nowcast(
    x,
    prior_only = TRUE,
    n_draws = 12L,
    temporal_effects = "none",
    seed = 1L
  )

  expect_true(tbl.now::is_tbl_nowcast(result))
  expect_true(S7::S7_inherits(result, diseasenowcasting_result_class))
  expect_true(S7::S7_inherits(result@fit, nowcast_class))
  expect_identical(result@data, x)
  expect_identical(result@event_date, tbl.now::get_event_date(x))
  expect_identical(result@strata, character(0))
  expect_identical(result@now, tbl.now::get_now(x))
  semantic_keys <- names(result@metadata$diseasenowcasting)
  expect_true(all(semantic_keys %in% c(
    "estimand", "cumulative_reconstruction", "negative_projection_count",
    "fit_diagnostics"
  )))
  expect_true("negative_projection_count" %in% semantic_keys)
  expect_false(any(c(
    "type", "rung", "target", "revision_mode", "comparison",
    "observed_series", "observed_strata"
  ) %in% names(result@metadata$diseasenowcasting)))
  expect_gt(nrow(result@predictions), 0L)
  expect_gt(nrow(result@draws), 0L)
  expect_s3_class(tbl.now::tidy(result), "data.frame")
  expect_s3_class(ggplot2::autoplot(result), "ggplot")

  prediction <- stats::predict(result, n_draws = 5L, seed = 2L)
  expect_true(S7::S7_inherits(prediction, nowcast_prediction_class))
  expect_type(summary(result, n_draws = 5L, seed = 2L), "list")
})

test_that("run_nowcast() passes through an already-normalised result", {
  dates <- as.Date("2020-01-01") + 0:9
  x <- tbl.now::tbl_now(
    data.frame(event = dates, report = dates, n = seq_along(dates)),
    event_date = "event",
    report_date = "report",
    case_count = "n",
    data_type = "count-incidence",
    verbose = FALSE
  )
  engine <- tbl.now::engine_diseasenowcasting(
    prior_only = TRUE,
    n_draws = 10L,
    quantile_levels = c(0.2, 0.5, 0.8),
    temporal_effects = "none",
    seed = 3L
  )

  result <- tbl.now::run_nowcast(x, engine, verbose = FALSE)

  expect_true(tbl.now::is_tbl_nowcast(result))
  expect_true(S7::S7_inherits(result, diseasenowcasting_result_class))
  expect_true(S7::S7_inherits(result@fit, nowcast_class))
  expect_equal(
    sort(unique(result@predictions$.quantile_level)),
    c(0.2, 0.5, 0.8)
  )
})

test_that("canonical results work directly with tbl.now scoring and ensembling", {
  dates <- as.Date("2020-01-01") + 0:9
  x <- tbl.now::tbl_now(
    data.frame(event = dates, report = dates, n = seq_along(dates)),
    event_date = "event", report_date = "report", case_count = "n",
    data_type = "count-incidence", verbose = FALSE
  )
  first <- nowcast(
    x,
    model = model(epidemic = hsgp_epidemic()),
    prior_only = TRUE, n_draws = 20L,
    temporal_effects = "none", seed = 5L
  )
  second <- nowcast(
    x,
    model = model(epidemic = ar1_epidemic()),
    prior_only = TRUE, n_draws = 20L,
    temporal_effects = "none", seed = 6L
  )

  scores <- tbl.now::score_nowcast(first, truth = x)
  ensemble <- tbl.now::nowcast_ensemble(
    first = first,
    second = second,
    verbose = FALSE
  )

  expect_s3_class(scores, "data.frame")
  expect_gt(nrow(scores), 0L)
  expect_s3_class(tbl.now::as_tibble(first), "tbl_df")
  expect_true(tbl.now::is_tbl_nowcast(ensemble))
  expect_identical(ensemble@method, "ensemble")
  expect_identical(ensemble@metadata$members, c("first", "second"))

  pooled <- tbl.now::nowcast_ensemble(
    HSGP = first,
    AR1 = second,
    type = "linear_pool",
    n_draws = 30L,
    verbose = FALSE
  )
  expect_true(tbl.now::is_tbl_nowcast(pooled))
  expect_identical(pooled@metadata$members, c("HSGP", "AR1"))
  expect_equal(length(unique(pooled@draws$.draw)), 30L)

  if (requireNamespace("scoringutils", quietly = TRUE)) {
    quantile_forecast <- scoringutils::as_forecast_quantile(first, truth = x)
    point_forecast <- scoringutils::as_forecast_point(first, truth = x)
    sample_forecast <- scoringutils::as_forecast_sample(first, truth = x)
    expect_s3_class(quantile_forecast, "forecast_quantile")
    expect_s3_class(point_forecast, "forecast_point")
    expect_s3_class(sample_forecast, "forecast_sample")
  }
})

test_that("save/load preserves the common quantile contract", {
  dates <- as.Date("2020-01-01") + 0:5
  x <- tbl.now::tbl_now(
    data.frame(event = dates, report = dates, n = seq_along(dates)),
    event_date = "event", report_date = "report", case_count = "n",
    data_type = "count-incidence", verbose = FALSE
  )
  result <- nowcast(
    x,
    prior_only = TRUE,
    n_draws = 10L,
    quantile_levels = c(0.1, 0.5, 0.9),
    temporal_effects = "none",
    seed = 7L
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  save_nowcast(result, path)
  restored <- load_nowcast(path)

  expect_equal(
    sort(unique(restored@predictions$.quantile_level)),
    c(0.1, 0.5, 0.9)
  )
})
