# Design-matrix contract for the observation-process regressions: the columns a
# coefficient vector refers to must not move between as-of dates, and anything
# the likelihood cannot identify must be removed deterministically and by name.

test_that("declared factor levels pin the reference level and the column set", {
  declared <- factor(c("a", "b"), levels = c("a", "b", "c"))
  built <- suppressMessages(
    .process_design(data.frame(site = declared), "site", "delay")
  )
  expect_equal(built$schema$terms, c("site[b]", "site[c]"))
  expect_equal(built$schema$levels$site, c("a", "b", "c"))

  # The same schema replayed on data that has since seen "c" gives the same
  # columns in the same order.
  later <- data.frame(site = factor(c("a", "b", "c"), levels = c("a", "b", "c")))
  replayed <- .process_design(later, "site", "delay", schema = built$schema)
  expect_equal(colnames(replayed$design), built$schema$terms)
})

test_that("a level the schema has never seen is an error, not a new coefficient", {
  schema <- suppressMessages(.process_design(
    data.frame(site = factor(c("a", "b"))), "site", "delay"
  ))$schema
  expect_error(
    .process_design(data.frame(site = c("a", "b", "z")), "site", "delay",
                    schema = schema),
    "not in the fitted design"
  )
})

test_that("a character covariate warns that its reference level is not pinned", {
  expect_warning(
    .process_design(data.frame(site = c("a", "b", "a")), "site", "delay"),
    "reference level is whichever value sorts first"
  )
})

test_that("unidentifiable columns are dropped by name", {
  frame <- data.frame(
    constant = rep(3, 10),
    signal = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    copy = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    other = c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9)
  )
  frame$alias <- frame$signal + frame$other
  expect_warning(built <- .process_design(frame, names(frame), "delay"),
                 "Dropped 3 delay design columns")
  expect_equal(built$schema$terms, c("signal", "other"))
  expect_named(built$schema$dropped, c("constant", "copy", "alias"))
  expect_match(built$schema$dropped[["constant"]], "aliased with the baseline hazard")
  expect_match(built$schema$dropped[["copy"]], "duplicate of signal")
  expect_match(built$schema$dropped[["alias"]], "collinear")
})

test_that("an unexposed declared level keeps its slot instead of renumbering", {
  frame <- data.frame(site = factor(c("a", "b", "a"), levels = c("a", "b", "c")))
  expect_message(built <- .process_design(frame, "site", "delay"),
                 "no exposed rows at this as-of date")
  expect_equal(built$schema$terms, c("site[b]", "site[c]"))
  expect_true(all(built$design[, "site[c]"] == 0))
})

test_that("continuous columns are standardized and binary ones are left alone", {
  frame <- data.frame(load = c(10, 20, 30, 40), flag = c(0, 1, 0, 1))
  built <- .process_design(frame, names(frame), "delay")
  expect_equal(mean(built$design[, "load"]), 0, tolerance = 1e-12)
  expect_equal(stats::sd(built$design[, "load"]), 1, tolerance = 1e-12)
  expect_equal(built$design[, "flag"], c(0, 1, 0, 1))
  # Replaying the schema on new rows reuses the stored constants rather than
  # recentring on whatever the new window happens to contain.
  replayed <- .process_design(data.frame(load = c(10, 10), flag = c(0, 0)),
                              names(frame), "delay", schema = built$schema)
  expect_equal(replayed$design[, "load"],
               rep((10 - built$schema$center[["load"]]) /
                     built$schema$scale[["load"]], 2))
})

test_that("inert revision-date effects are reported and dropped", {
  # `$` partial-matches on lists, so removing the revision calendar used to
  # resolve `$revision` to a sibling `$revision_schema` and hand a schema list
  # to prepare_data() as if it were a design matrix.
  frame <- data.frame(
    onset = as.Date("2024-01-01") + 0:29,
    reported = as.Date("2024-01-01") + 0:29,
    revision = as.Date(NA),
    status = "pending",
    n = 1
  )
  now_data <- tbl.now::tbl_now(
    frame, event_date = onset, report_date = reported,
    revision_date = revision, revision_type = status, case_count = n,
    data_type = "count-incidence", now = as.Date("2024-01-30"), verbose = FALSE
  ) |>
    tbl.now::add_temporal_effects(tbl.now::temporal_effects(weekend = TRUE),
                                  date_type = "revision_date") |>
    tbl.now::compute_temporal_effects()

  expect_message(
    prepared <- prepare_from_tbl_now(now_data, model(), now = as.Date("2024-01-30")),
    "this model has no revision process"
  )
  expect_equal(prepared$data$P_revision, 0L)
  expect_equal(nrow(prepared$data$revision_calendar), prepared$data$max_time)
})

test_that("unordered factors become dummies and ordered factors keep their order", {
  frame <- data.frame(
    regime = factor(c("lockdown", "open", "surge", "open"),
                    levels = c("lockdown", "open", "surge")),
    alert = factor(c("green", "amber", "red", "green"),
                   levels = c("green", "amber", "red"), ordered = TRUE)
  )
  built <- .process_design(frame, names(frame), "event", standardize = FALSE)
  # "lockdown < open < surge" is not a real ordering, so it costs two columns.
  expect_equal(colnames(built$design),
               c("regime[open]", "regime[surge]", "alert"))
  expect_equal(built$design[, "regime[open]"], c(0, 1, 0, 1))
  # "green < amber < red" is a real ordering, so one monotone score is enough.
  expect_equal(built$design[, "alert"], c(1, 2, 3, 1))
})

test_that("dummy coding ignores the session's contrast option", {
  # model.matrix() would switch to Helmert contrasts here.
  withr::local_options(contrasts = c("contr.helmert", "contr.poly"))
  frame <- data.frame(site = factor(c("a", "b", "c")))
  built <- .process_design(frame, "site", "delay")
  expect_equal(built$design[, "site[b]"], c(0, 1, 0))
  expect_equal(built$design[, "site[c]"], c(0, 0, 1))
})

test_that("event covariates reach the epidemic mean as dummies, not codes", {
  frame <- data.frame(
    onset = as.Date("2024-01-01") + 0:8,
    reported = as.Date("2024-01-01") + 0:8,
    regime = factor(rep(c("lockdown", "open", "surge"), each = 3),
                    levels = c("lockdown", "open", "surge")),
    n = 1
  )
  now_data <- tbl.now::tbl_now(
    frame, event_date = onset, report_date = reported, case_count = n,
    covariates = regime, data_type = "count-incidence",
    now = as.Date("2024-01-09"), verbose = FALSE
  )
  prepared <- suppressWarnings(
    prepare_from_tbl_now(now_data, model(), now = as.Date("2024-01-09"))
  )
  expect_equal(prepared$data$event_coef_names,
               c("regime[open]", "regime[surge]"))
  expect_equal(prepared$data$X[, "regime[open]"],
               c(0, 0, 0, 1, 1, 1, 0, 0, 0))
})
