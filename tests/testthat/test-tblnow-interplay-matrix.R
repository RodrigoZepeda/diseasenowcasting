# Regression tests for the tbl.now <-> diseasenowcasting boundary. These stay at
# prepare/mocked-fit level so the interoperability contract is checked without
# running expensive model fits.

interplay_incidence <- function(data_type = "linelist",
                                revision_state = "none",
                                censored_report = FALSE,
                                censored_revision = FALSE,
                                strata = "none",
                                covariates = "none",
                                units = "days") {
  stopifnot(data_type %in% c("linelist", "count-incidence"))
  origin <- as.Date("2024-01-01")
  event_index <- rep(0:5, each = 3)
  delay <- rep(0:2, times = 6)
  n <- as.integer(2 + event_index + delay)

  frame <- data.frame(
    event = origin + event_index,
    report = origin + event_index + delay,
    n = n
  )
  if (identical(units, "weeks")) {
    frame$event <- origin + 7L * event_index
    frame$report <- origin + 7L * (event_index + delay)
  } else if (identical(units, "numeric")) {
    frame$event <- event_index
    frame$report <- event_index + delay
  }

  if (identical(strata, "one")) {
    frame$site <- rep(c("north", "south"), length.out = nrow(frame))
  } else if (identical(strata, "two")) {
    frame$site <- rep(c("north", "south"), length.out = nrow(frame))
    frame$age_group <- rep(c("adult", "child"), each = 2, length.out = nrow(frame))
  } else if (identical(strata, "missing")) {
    frame$site <- rep(c("north", NA), length.out = nrow(frame))
  }

  if (identical(covariates, "numeric")) {
    frame$temp <- 20 + event_index
  } else if (identical(covariates, "missing")) {
    frame$temp <- 20 + event_index
    frame$temp[c(2, 7)] <- NA_real_
  } else if (identical(covariates, "multiple")) {
    frame$temp <- 20 + event_index
    frame$humidity <- 60 - delay
  }

  if (censored_report) {
    frame$is_censored_report <- delay == 2
  }

  if (!identical(revision_state, "none")) {
    frame$revision <- as.Date(NA)
    if (identical(units, "numeric")) frame$revision <- NA_real_
    frame$revision_type <- "pending"
    resolved <- c(2L, 5L, 8L, 11L)
    if (revision_state %in% c("confirmation", "both")) {
      pos <- resolved[c(TRUE, FALSE, TRUE, FALSE)]
      frame$revision[pos] <- frame$report[pos] + if (identical(units, "weeks")) 7L else 1L
      frame$revision_type[pos] <- "confirmed"
    }
    if (revision_state %in% c("retraction", "both")) {
      neg <- resolved[c(FALSE, TRUE, FALSE, TRUE)]
      frame$revision[neg] <- frame$report[neg] + if (identical(units, "weeks")) 7L else 1L
      frame$revision_type[neg] <- "retracted"
    }
    if (censored_revision) {
      frame$is_censored_revision <- !is.na(frame$revision) &
        frame$revision_type == "retracted"
    }
  }

  if (identical(data_type, "linelist")) {
    frame <- tidyr::uncount(frame, weights = n)
  }

  tbl.now::tbl_now(
    frame,
    event_date = "event",
    report_date = "report",
    revision_date = if (!identical(revision_state, "none")) "revision" else NULL,
    revision_type = if (!identical(revision_state, "none")) "revision_type" else NULL,
    case_count = if (identical(data_type, "count-incidence")) "n" else NULL,
    strata = intersect(c("site", "age_group"), names(frame)),
    covariates = intersect(c("temp", "humidity"), names(frame)),
    is_censored_report = if (censored_report) "is_censored_report" else NULL,
    is_censored_revision = if (censored_revision) "is_censored_revision" else NULL,
    data_type = data_type,
    event_units = units,
    report_units = units,
    revision_units = if (!identical(revision_state, "none")) units else "auto",
    now = max(frame$report, na.rm = TRUE),
    verbose = FALSE
  )
}

interplay_cumulative <- function(revision_state = "none", units = "days") {
  origin <- as.Date("2024-01-01")
  frame <- data.frame(
    event_index = c(0, 0, 0, 1, 1, 1, 2, 2),
    delay = c(0, 1, 2, 0, 1, 2, 0, 1),
    n = c(8, 10, 9, 5, 8, 7, 4, 6),
    site = c("north", "north", "north", "south", "south", "south", NA, NA),
    temp = c(20, 20, 20, 21, 21, 21, 22, 22)
  )
  frame$event <- origin + frame$event_index
  frame$report <- origin + frame$event_index + frame$delay
  if (identical(units, "weeks")) {
    frame$event <- origin + 7L * frame$event_index
    frame$report <- origin + 7L * (frame$event_index + frame$delay)
  } else if (identical(units, "numeric")) {
    frame$event <- as.integer(frame$event_index)
    frame$report <- as.integer(frame$event_index + frame$delay)
  }

  if (!identical(revision_state, "none")) {
    frame$revision <- frame$report
    frame$revision_type <- if (identical(revision_state, "confirmation")) {
      "confirmed"
    } else {
      "retracted"
    }
  }

  tbl.now::tbl_now(
    frame,
    event_date = "event",
    report_date = "report",
    revision_date = if (!identical(revision_state, "none")) "revision" else NULL,
    revision_type = if (!identical(revision_state, "none")) "revision_type" else NULL,
    case_count = "n",
    strata = "site",
    covariates = "temp",
    data_type = "count-cumulative",
    event_units = units,
    report_units = units,
    revision_units = if (!identical(revision_state, "none")) units else "auto",
    now = max(frame$report, na.rm = TRUE),
    verbose = FALSE
  )
}

test_that("prepare_from_tbl_now covers the core tbl.now shape matrix", {
  ordinary_shapes <- expand.grid(
    data_type = c("linelist", "count-incidence"),
    revision_state = c("none", "confirmation", "retraction", "both"),
    units = c("days", "weeks", "numeric"),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(ordinary_shapes))) {
    shape <- ordinary_shapes[i, ]
    x <- interplay_incidence(
      data_type = shape$data_type,
      revision_state = shape$revision_state,
      censored_report = TRUE,
      censored_revision = !identical(shape$revision_state, "none"),
      strata = "two",
      covariates = "multiple",
      units = shape$units
    )
    revision_mode <- switch(shape$revision_state,
      none = "none",
      confirmation = "confirmation_only",
      retraction = "retraction_only",
      both = "both"
    )
    prep <- suppressMessages(diseasenowcasting:::prepare_from_tbl_now(
      x, model(), revision_mode = revision_mode
    ))

    expect_equal(prep$data$max_time, prep$max_time)
    expect_equal(prep$data$num_strata, 4L)
    expect_equal(ncol(prep$data$X), 2L)
    expect_true(length(prep$data$obs_delays_cens) > 0)
    expect_true(sum(prep$data$row_sums_cens) > 0)
    if (identical(shape$revision_state, "none")) {
      expect_equal(prep$data$is_linelist_retraction, 0L)
    } else {
      expect_equal(prep$data$is_linelist_retraction, 1L)
      expect_equal(prep$data$resolution_mode, switch(revision_mode,
        retraction_only = 0L, confirmation_only = 1L, both = 2L
      ))
    }
  }

  cumulative <- suppressMessages(diseasenowcasting:::prepare_from_tbl_now(
    interplay_cumulative(revision_state = "retraction", units = "weeks"),
    model(cumulative = cumulative_process(settlement = 4L)),
    revision_mode = "retraction_only"
  ))
  expect_equal(cumulative$data$is_count_cumulative, 1L)
  expect_true(any(cumulative$data$m[, 2] < 0))
  expect_equal(ncol(cumulative$data$X), 1L)
  expect_equal(cumulative$data$num_strata, 3L)
})

test_that("nowcast() model selection is stable under mocked fits", {
  captured <- list()
  fake_collect <- function(model, engine, priors, type, ...) {
    captured[[length(captured) + 1L]] <<- list(
      revision_active = isTRUE(model@revision@active),
      cumulative_active = isTRUE(model@cumulative@active),
      resolution_mode = engine$resolution_mode,
      count_cumulative = isTRUE(engine$is_count_cumulative == 1L),
      type = type
    )
    list(fits = list(list(mock = TRUE)), rung = "mock", target = engine$max_time)
  }

  testthat::with_mocked_bindings({
    ordinary <- nowcast(
      interplay_incidence("linelist", "none", units = "numeric"),
      model(), type = "auto", temporal_effects = "none"
    )
    retraction <- nowcast(
      interplay_incidence("count-incidence", "retraction", units = "days"),
      model(), type = "auto", temporal_effects = "none"
    )
    both <- nowcast(
      interplay_incidence("linelist", "both", units = "weeks"),
      model(), type = "auto", temporal_effects = "none"
    )
    cumulative <- nowcast(
      interplay_cumulative("retraction", units = "numeric"),
      model(), type = "auto", temporal_effects = "none"
    )
  }, .collect_nowcast_fits = fake_collect, .package = "diseasenowcasting")

  expect_equal(ordinary@type, "auto")
  expect_equal(retraction@revision_mode, "retraction_only")
  expect_equal(both@revision_mode, "both")
  expect_equal(cumulative@revision_mode, "retraction_only")
  expect_false(captured[[1]]$revision_active)
  expect_true(captured[[2]]$revision_active)
  expect_equal(captured[[2]]$resolution_mode, 0L)
  expect_true(captured[[3]]$revision_active)
  expect_equal(captured[[3]]$resolution_mode, 2L)
  expect_true(captured[[4]]$cumulative_active)
  expect_true(captured[[4]]$count_cumulative)
  expect_true(all(vapply(captured, `[[`, character(1), "type") == "auto"))
})

test_that("count-cumulative revision semantics are locked down", {
  cc <- interplay_cumulative("retraction")
  prep <- suppressMessages(diseasenowcasting:::prepare_from_tbl_now(
    cc, model(cumulative = cumulative_process(settlement = 4L)),
    revision_mode = "retraction_only"
  ))
  expect_equal(prep$data$is_count_cumulative, 1L)
  expect_true(any(prep$data$signed_update_array < 0, na.rm = TRUE))
  expect_equal(prep$data$is_linelist_retraction, 0L)
  expect_equal(prep$data$resolution_mode, 0L)

  expect_error(
    nowcast(cc, model(revision = revision_process()), temporal_effects = "none"),
    "revision_process.*count-cumulative"
  )
  expect_error(
    nowcast(interplay_cumulative("confirmation"), model(), temporal_effects = "none"),
    "cannot carry.*confirmed"
  )
})

test_that("FluSight count-cumulative down-revisions feed cumulative_process", {
  rows <- tbl.now::flusight |>
    dplyr::filter(
      .data$location_name == "Texas",
      .data$target_end_date >= as.Date("2023-09-23"),
      .data$as_of >= as.Date("2023-09-23"),
      !is.na(.data$observation)
    ) |>
    dplyr::arrange(.data$target_end_date, .data$as_of)

  tn <- suppressWarnings(tbl.now::tbl_now(
    rows,
    event_date = target_end_date,
    report_date = as_of,
    case_count = observation,
    data_type = "count-cumulative",
    event_units = "weeks",
    report_units = "weeks",
    verbose = FALSE
  ))
  tn <- tbl.now::align_weeks(tn, date_col = "report_date")

  prep <- suppressMessages(diseasenowcasting:::prepare_from_tbl_now(
    tn,
    model(cumulative = cumulative_process(settlement = 6L)),
    revision_mode = "none"
  ))

  expect_equal(prep$data$is_count_cumulative, 1L)
  expect_equal(prep$data$is_linelist_retraction, 0L)
  expect_gt(sum(prep$data$m[, 2] < 0), 0L)
  expect_gt(sum(pmax(-prep$data$m[, 2], 0)), 0)
  expect_equal(prep$data$count_cumulative_observation, 2L)
})

test_that("revision type canonicalization is enforced at the boundary", {
  raw <- data.frame(
    onset = as.Date("2024-01-01") + 0:3,
    reported = as.Date("2024-01-02") + 0:3,
    revised = as.Date("2024-01-03") + c(0, NA, 2, NA),
    estado = c("confirmado", "pendiente", "retirado", NA)
  )
  recoded <- tbl.now::tbl_now(
    raw,
    event_date = onset,
    report_date = reported,
    revision_date = revised,
    revision_type = estado,
    revision_levels = c(
      confirmado = "confirmed",
      retirado = "retracted",
      pendiente = "pending"
    ),
    data_type = "linelist",
    verbose = FALSE
  )
  expect_setequal(
    stats::na.omit(unique(recoded[[tbl.now::get_revision_type(recoded)]])),
    c("confirmed", "retracted", "pending")
  )
  expect_equal(
    diseasenowcasting:::.resolve_revision_mode(recoded, diseasenowcasting:::no_revision()),
    "both"
  )

  noncanonical <- recoded
  type_col <- tbl.now::get_revision_type(noncanonical)
  date_col <- tbl.now::get_revision_date(noncanonical)
  resolved <- which(!is.na(noncanonical[[date_col]]))
  noncanonical[[type_col]][resolved[1]] <- "confirmado"
  expect_error(
    diseasenowcasting:::.resolve_revision_mode(
      noncanonical, diseasenowcasting:::no_revision()
    ),
    "Unrecognised outcome"
  )

  pending <- recoded
  pending[[type_col]][is.na(pending[[date_col]])] <- "pending"
  expect_equal(
    diseasenowcasting:::.resolve_revision_mode(pending, diseasenowcasting:::no_revision()),
    "both"
  )
})

test_that("mixed event/report units fail before preparation", {
  x <- tbl.now::tbl_now(
    data.frame(
      event = as.Date("2024-01-01") + 0:4,
      report = as.Date("2024-01-01") + 0:4,
      n = 1:5
    ),
    event_date = event,
    report_date = report,
    case_count = n,
    data_type = "count-incidence",
    event_units = "days",
    report_units = "weeks",
    verbose = FALSE
  )
  expect_error(
    diseasenowcasting:::prepare_from_tbl_now(x, model()),
    "matching event and report units"
  )
})
