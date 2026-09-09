.make_count_cumulative_workflow_fixture <- function(n_events = 18L) {
  start <- as.Date("2023-01-07")
  rows <- lapply(seq_len(n_events), function(event_index) {
    event <- start + (event_index - 1L) * 7L
    final <- 25L + event_index
    levels <- round(final * c(0.45, 0.75, 0.92, 1))
    if (event_index %% 5L == 0L) levels[4L] <- levels[3L] - 1L
    data.frame(
      event = event,
      report = event + 0:3 * 7L,
      count = as.integer(levels)
    )
  })
  observations <- do.call(rbind, rows)
  tbl.now::tbl_now(
    observations,
    event_date = event,
    report_date = report,
    case_count = count,
    data_type = "count-cumulative",
    event_units = "weeks",
    report_units = "weeks",
    now = max(observations$report),
    verbose = FALSE
  )
}

.ztp_workflow_model <- function() {
  model(
    poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
    cumulative = cumulative_process(
      observation = "hurdle_ztpoisson", settlement = 6L,
      movement_previous = 0
    )
  )
}

test_that("hurdle ZTPoisson supports fit, prediction, save/load, and update", {
  skip_on_cran()
  data <- .make_count_cumulative_workflow_fixture()
  first_now <- as.Date("2023-01-07") + 13L * 7L
  later_now <- as.Date("2023-01-07") + 16L * 7L

  fitted <- nowcast(
    data, .ztp_workflow_model(), now = first_now, type = "two_stage",
    temporal_effects = "none", n_draws = 40L, seed = 11L
  )
  expect_true(tbl.now::is_tbl_nowcast(fitted))
  expect_identical(fitted@rung, "onestage")
  expect_false(fitted@fits[[1L]]$use_random)
  expect_lte(fitted@fits[[1L]]$max_gradient, 0.1)
  expect_null(fitted@fits[[1L]]$parList$log_magnitude_size)

  prediction <- predict(fitted, n_draws = 40L, seed = 12L)
  expect_true(all(is.finite(prediction@draws)))
  expect_match(prediction@estimand, "C_t\\(6\\)")
  expect_match(prediction@cumulative_reconstruction, "anchored sequential")
  expect_gte(prediction@negative_projection_count, 0L)
  expect_identical(
    fitted@metadata$diseasenowcasting$estimand,
    prediction@estimand
  )
  expect_identical(
    fitted@metadata$diseasenowcasting$cumulative_reconstruction,
    prediction@cumulative_reconstruction
  )
  expect_gte(
    fitted@metadata$diseasenowcasting$negative_projection_count,
    0L
  )

  path <- tempfile(fileext = ".rds")
  expect_invisible(suppressMessages(save_nowcast(fitted, path)))
  restored <- load_nowcast(path)
  expect_identical(restored@model@cumulative@observation,
                   "hurdle_ztpoisson")
  expect_identical(as.integer(restored@model@cumulative@settlement), 6L)
  expect_equal(
    predict(fitted, n_draws = 40L, seed = 99L)@draws,
    predict(restored, n_draws = 40L, seed = 99L)@draws
  )

  updated <- update(
    fitted, data, now = later_now, compute_surprise = FALSE
  )
  expect_identical(updated@model@cumulative@observation,
                   "hurdle_ztpoisson")
  expect_identical(updated@engine$settlement_horizon, 6L)
  expect_lte(updated@fits[[1L]]$max_gradient, 0.1)
  expect_true(all(is.finite(
    predict(updated, n_draws = 20L, seed = 13L)@draws
  )))
})

test_that("hurdle ZTPoisson supports prior-only and finite-horizon backtesting", {
  skip_on_cran()
  data <- .make_count_cumulative_workflow_fixture()
  specification <- .ztp_workflow_model()

  prior <- nowcast(
    data, specification, prior_only = TRUE, n_draws = 20L,
    temporal_effects = "none", seed = 21L
  )
  prior_prediction <- predict(prior, n_draws = 20L, seed = 22L)
  expect_true(all(is.finite(prior_prediction@draws)))
  expect_match(prior_prediction@estimand, "C_t\\(6\\)")

  evaluation_date <- as.Date("2023-01-07") + 12L * 7L
  evaluated <- suppressWarnings(backtest(
    data, specification, dates = evaluation_date,
    type = "one_stage", n_draws = 20L, seed = 23L,
    temporal_effects = "none", verbose = FALSE
  ))
  expect_s3_class(evaluated, "nowcast_backtest")
  expect_true(nrow(evaluated$scores) > 0L)
  expect_true(all(is.finite(evaluated$scores$.observed)))
})

test_that("cumulative-level fits use Laplace while hurdle fits use MAP", {
  data <- .make_count_cumulative_workflow_fixture(12L)
  now <- as.Date("2023-01-07") + 10L * 7L
  level_model <- model(
    poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
    cumulative = cumulative_process(
      observation = "cumulative", settlement = 6L
    )
  )
  hurdle_model <- .ztp_workflow_model()

  level_engine <- diseasenowcasting:::prepare_from_tbl_now(
    data, level_model, now = now
  )$data
  hurdle_engine <- diseasenowcasting:::prepare_from_tbl_now(
    data, hurdle_model, now = now
  )$data
  level_priors <- default_priors(level_model, level_engine)
  hurdle_priors <- default_priors(hurdle_model, hurdle_engine)

  level_fit <- fit(level_model, level_engine, priors = level_priors)
  hurdle_fit <- fit(hurdle_model, hurdle_engine, priors = hurdle_priors)
  expect_true(level_fit$use_random)
  expect_false(hurdle_fit$use_random)
  expect_lte(level_fit$max_gradient, 0.1)
  expect_lte(hurdle_fit$max_gradient, 0.1)
})

test_that("post-origin terminal mutations cannot change priors, fits, or predictions", {
  skip_on_cran()
  original <- .make_count_cumulative_workflow_fixture(14L)
  origin <- as.Date("2023-01-07") + 10L * 7L
  changed_frame <- as.data.frame(original)[c("event", "report", "count")]
  changed_frame$count[changed_frame$report > origin] <-
    changed_frame$count[changed_frame$report > origin] + 100000L
  changed <- tbl.now::tbl_now(
    changed_frame, event_date = event, report_date = report,
    case_count = count, data_type = "count-cumulative",
    event_units = "weeks", report_units = "weeks",
    now = max(changed_frame$report), verbose = FALSE
  )
  specification <- .ztp_workflow_model()

  original_prepared <- diseasenowcasting:::prepare_from_tbl_now(
    original, specification, now = origin
  )
  changed_prepared <- diseasenowcasting:::prepare_from_tbl_now(
    changed, specification, now = origin
  )
  expect_equal(original_prepared$data, changed_prepared$data)
  expect_equal(
    default_priors(specification, original_prepared$data),
    default_priors(specification, changed_prepared$data)
  )

  original_fit <- nowcast(
    original, specification, now = origin, temporal_effects = "none",
    n_draws = 20L, seed = 31L
  )
  changed_fit <- nowcast(
    changed, specification, now = origin, temporal_effects = "none",
    n_draws = 20L, seed = 31L
  )
  expect_equal(original_fit@fits[[1L]]$parList,
               changed_fit@fits[[1L]]$parList)
  expect_equal(
    predict(original_fit, n_draws = 20L, seed = 32L)@draws,
    predict(changed_fit, n_draws = 20L, seed = 32L)@draws
  )
})
