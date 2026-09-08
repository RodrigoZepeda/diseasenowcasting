.make_count_cumulative_asof_fixture <- function(post_now_value = 99) {
  observations <- data.frame(
    event = as.Date(c(
      "2024-01-06", "2024-01-06", "2024-01-06",
      "2024-01-13", "2024-01-13", "2024-01-20"
    )),
    report = as.Date(c(
      "2024-01-06", "2024-01-20", "2024-02-10",
      "2024-01-13", "2024-01-27", "2024-01-20"
    )),
    location = "A",
    count = c(5, 7, post_now_value, 0, 4, 3)
  )
  tbl.now::tbl_now(
    observations,
    event_date = event,
    report_date = report,
    strata = location,
    case_count = count,
    data_type = "count-cumulative",
    event_units = "weeks",
    report_units = "weeks",
    now = max(observations$report),
    verbose = FALSE
  )
}

test_that("count-cumulative completion preserves tbl_now metadata and observed zero cells", {
  original <- .make_count_cumulative_asof_fixture()
  now <- as.Date("2024-01-27")
  prepared <- diseasenowcasting:::.prepare_count_cumulative_as_of(
    original, now = now, settlement = 6L
  )

  expect_true(tbl.now::is_tbl_now(prepared))
  expect_identical(tbl.now::get_data_type(prepared), "count-cumulative")
  expect_identical(tbl.now::get_event_date(prepared), "event")
  expect_identical(tbl.now::get_report_date(prepared), "report")
  expect_identical(tbl.now::get_case_count(prepared), "count")
  expect_identical(tbl.now::get_strata(prepared), "location")
  expect_equal(tbl.now::get_now(prepared), now)
  expect_true(all(prepared$event <= now))
  expect_true(all(prepared$report <= now))
  expect_true(all(prepared$.observed_cell))

  first_event <- prepared[prepared$event == as.Date("2024-01-06"), ]
  expect_equal(first_event$.delay, 0:3)
  expect_equal(first_event$.cumulative_level, c(5, 5, 7, 7))
  expect_equal(first_event$.signed_update, c(5, 0, 2, 0))
  expect_equal(first_event$.previous_nonzero, c(0, 1, 0, 1))

  second_event <- prepared[prepared$event == as.Date("2024-01-13"), ]
  expect_equal(second_event$.cumulative_level[1L], 0)
  expect_equal(second_event$.signed_update[1L], 0)
})

test_that("post-now terminal mutations cannot change prepared fitting cells", {
  now <- as.Date("2024-01-27")
  before <- diseasenowcasting:::.prepare_count_cumulative_as_of(
    .make_count_cumulative_asof_fixture(post_now_value = 10),
    now = now, settlement = 6L
  )
  after <- diseasenowcasting:::.prepare_count_cumulative_as_of(
    .make_count_cumulative_asof_fixture(post_now_value = 1000000),
    now = now, settlement = 6L
  )

  expect_equal(as.data.frame(before), as.data.frame(after))
  expect_equal(attributes(before), attributes(after))
})

test_that("future cells are absent rather than represented as observed zeroes", {
  now <- as.Date("2024-01-20")
  prepared <- diseasenowcasting:::.prepare_count_cumulative_as_of(
    .make_count_cumulative_asof_fixture(), now = now, settlement = 52L
  )
  expect_true(all(prepared$report <= now))
  expect_false(any(prepared$event == as.Date("2024-01-20") & prepared$.delay > 0))
  expect_lte(max(prepared$.delay), 2L)
})

test_that("engine arrays distinguish observed zeroes from masked future cells", {
  now <- as.Date("2024-01-20")
  model <- model(
    poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
    cumulative = cumulative_process(
      observation = "hurdle_ztpoisson", settlement = 6L
    )
  )
  engine <- diseasenowcasting:::prepare_from_tbl_now(
    .make_count_cumulative_asof_fixture(), model, now = now
  )$data

  expect_identical(engine$is_count_cumulative, 1L)
  expect_identical(engine$count_cumulative_observation, 3L)
  expect_identical(engine$settlement_horizon, 6L)
  expect_equal(dim(engine$observation_mask)[2L], 7L)

  # The newest event has only age zero observable. Its future storage cells are
  # zero-valued but explicitly masked out.
  newest <- max(which(apply(engine$observation_mask, 1L, any)))
  expect_true(engine$observation_mask[newest, 1L, 1L])
  expect_false(any(engine$observation_mask[newest, -1L, 1L]))
  expect_equal(engine$signed_update_array[newest, -1L, 1L], rep(0, 6L))

  # The observed leading zero for the second event remains data.
  expect_true(engine$observation_mask[2L, 1L, 1L])
  expect_equal(engine$cumulative_level_array[2L, 1L, 1L], 0)
})

test_that("count-cumulative preparation rejects other data types and bad horizons", {
  x <- data.frame(event = as.Date("2024-01-06"),
                  report = as.Date("2024-01-06"), count = 1)
  incidence <- tbl.now::tbl_now(
    x, event_date = event, report_date = report, case_count = count,
    data_type = "count-incidence", event_units = "weeks",
    report_units = "weeks", verbose = FALSE
  )
  expect_error(
    diseasenowcasting:::.prepare_count_cumulative_as_of(
      incidence, now = as.Date("2024-01-06"), settlement = 26L
    ),
    "count-cumulative"
  )
  expect_error(
    diseasenowcasting:::.prepare_count_cumulative_as_of(
      .make_count_cumulative_asof_fixture(),
      now = as.Date("2024-01-27"), settlement = 0L
    ),
    "positive integer"
  )
})

test_that("calendar and compressed publication clocks preserve their intended gaps", {
  publication_weeks <- as.Date(c(
    "2023-12-30", "2024-01-06", "2024-12-28", "2025-01-04"
  ))
  compressed_index <- seq_along(publication_weeks) - 1L

  # The compressed clock crosses both year and season boundaries in one step.
  expect_equal(diff(compressed_index), rep(1L, 3L))
  expect_equal(as.integer(diff(publication_weeks) / 7), c(1L, 51L, 1L))

  raw <- data.frame(
    event_actual = publication_weeks,
    report_actual = publication_weeks,
    count = c(4L, 5L, 6L, 7L)
  )
  calendar <- tbl.now::tbl_now(
    raw, event_date = event_actual, report_date = report_actual,
    case_count = count, data_type = "count-cumulative",
    event_units = "weeks", report_units = "weeks", verbose = FALSE
  )

  compressed <- transform(
    raw,
    event_model = as.Date("2000-01-01") + 7L * compressed_index,
    report_model = as.Date("2000-01-01") + 7L * compressed_index
  )
  compressed <- tbl.now::tbl_now(
    compressed, event_date = event_model, report_date = report_model,
    case_count = count, data_type = "count-cumulative",
    event_units = "weeks", report_units = "weeks", verbose = FALSE
  )

  calendar_prepared <- diseasenowcasting:::.prepare_count_cumulative_as_of(
    calendar, now = max(raw$report_actual), settlement = 52L
  )
  compressed_prepared <- diseasenowcasting:::.prepare_count_cumulative_as_of(
    compressed, now = as.Date("2000-01-22"), settlement = 52L
  )
  expect_gt(max(calendar_prepared$.event_num),
            max(compressed_prepared$.event_num))
  expect_equal(sort(unique(compressed_prepared$.event_num)), 0:3)
})
