test_that("covariate role tags preserve values and existing classes", {
  date <- as.Date("2024-01-01") + 0:2
  tagged <- as_delay_covariates(date)
  expect_s3_class(tagged, "delay_covariates")
  expect_true(inherits(tagged, "Date"))
  expect_equal(unclass(tagged), unclass(date))

  twice <- as_revision_covariates(tagged)
  expect_true(inherits(twice, "delay_covariates"))
  expect_true(inherits(twice, "revision_covariates"))
  expect_true(inherits(twice, "Date"))
})

test_that("data-frame role helpers support tidy selection", {
  data <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tagged <- as_revision_covariates(data, b:c)
  expect_false(inherits(tagged$a, "revision_covariates"))
  expect_true(inherits(tagged$b, "revision_covariates"))
  expect_true(inherits(tagged$c, "revision_covariates"))
})

test_that("tagged factor strata compose with tbl.now grid completion", {
  data <- data.frame(
    event = as.Date("2024-01-01") + rep(0:1, each = 2),
    report = as.Date("2024-01-01") + rep(0:1, each = 2),
    location = as_delay_covariates(factor(rep(c("A", "B"), 2))),
    count = c(1, 2, 3, 4)
  )
  now <- tbl.now::tbl_now(
    data, event_date = event, report_date = report, case_count = count,
    strata = location, data_type = "count-cumulative", verbose = FALSE
  )
  completed <- tbl.now::complete_zeroes(now, max_delay = 1L)
  expect_true(inherits(completed$location, "delay_covariates"))
  expect_true(inherits(completed$location, "factor"))
})

test_that("role tags compose across grouped factor transformations", {
  grouped <- dplyr::group_by(
    data.frame(group = c("x", "x", "y", "y"), value = c("a", "b", "a", "c")),
    group
  )
  tagged <- dplyr::mutate(
    grouped, value = as_delay_covariates(factor(value))
  )
  expect_true(inherits(tagged$value, "delay_covariates"))
  expect_true(inherits(tagged$value, "factor"))
  expect_setequal(levels(tagged$value), c("a", "b", "c"))
})

test_that("untagged tbl.now covariates remain event covariates", {
  data <- data.frame(
    event = as.Date("2024-01-01") + 0:3,
    report = as.Date("2024-01-01") + 0:3,
    old = 1:4,
    delayed = as_delay_covariates(c(0, 1, 0, 1))
  )
  now <- tbl.now::tbl_now(
    data, event_date = event, report_date = report,
    covariates = c(old, delayed), verbose = FALSE
  )
  roles <- .covariate_roles(now)
  expect_equal(roles$event, "old")
  expect_equal(roles$delay, "delayed")
  expect_length(roles$revision, 0L)
})

test_that("temporal effects are rebuilt into distinct process matrices", {
  data <- data.frame(
    event = as.Date("2024-01-01") + 0:9,
    report = as.Date("2024-01-01") + 0:9
  )
  now <- tbl.now::tbl_now(
    data, event_date = event, report_date = report, verbose = FALSE
  ) |>
    tbl.now::add_temporal_effects(
      tbl.now::temporal_effects(day_of_week = TRUE),
      date_type = "event_date"
    ) |>
    tbl.now::add_temporal_effects(
      tbl.now::temporal_effects(weekend = TRUE),
      date_type = "report_date"
    ) |>
    tbl.now::compute_temporal_effects()

  prepared <- prepare_from_tbl_now(now, model(), now = as.Date("2024-01-10"))$data
  expect_length(colnames(prepared$X), 6L)
  expect_true(all(startsWith(colnames(prepared$X), ".event_day_of_week[")))
  expect_equal(colnames(prepared$report_calendar), ".report_weekend")
  expect_equal(prepared$P, 6L)
  expect_equal(prepared$P_delay, 1L)
})

# The hazard core itself (zero-tilt equivalence, tail accuracy, underflow,
# hand-enumerated paths) is covered by test-process-hazard.R.
