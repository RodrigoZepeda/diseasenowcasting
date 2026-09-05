# As-of FluSight preparation for the count-cumulative prototypes.
#
# This file deliberately keeps two distinct clocks:
#   * calendar: every calendar week is retained and missing cells are completed;
#   * compressed: only observed publication weeks are numbered consecutively.
#
# The full clock is constructed first.  An as-of panel is then obtained by
# filtering on the ORIGINAL event and report dates, never on future values.

`%||%` <- function(x, y) if (is.null(x)) y else x

week_ending_saturday <- function(x) {
  x <- as.Date(x)
  x - ((as.POSIXlt(x)$wday - 6L) %% 7L)
}

deduplicate_flusight <- function(states, start) {
  tbl.now::flusight |>
    dplyr::filter(
      .data$location_name %in% states,
      .data$target_end_date >= as.Date(start),
      !is.na(.data$observation)
    ) |>
    dplyr::mutate(
      event_date_actual = week_ending_saturday(.data$target_end_date),
      report_date_actual = as.Date(.data$as_of),
      report_week_actual = week_ending_saturday(.data$as_of)
    ) |>
    # Later releases in the same model week supersede earlier ones.
    dplyr::group_by(.data$location_name, .data$event_date_actual,
                    .data$report_week_actual) |>
    dplyr::slice_max(.data$report_date_actual, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()
}

make_clock_arm <- function(raw, clock = c("calendar", "compressed"),
                           max_delay = 52L) {
  clock <- match.arg(clock)
  start_week <- min(raw$event_date_actual)

  if (clock == "calendar") {
    last_week <- max(raw$report_week_actual)
    dates <- seq(start_week, last_week, by = "1 week")
    clock_table <- data.frame(
      model_num = seq_along(dates) - 1L,
      actual_week = dates
    )
  } else {
    # This is the requested pasted clock: each week in which a snapshot was
    # actually published advances model time by exactly one, irrespective of
    # the calendar gap.  Event weeks outside this clock are intentionally not
    # part of this arm.
    dates <- sort(unique(raw$report_week_actual))
    clock_table <- data.frame(
      model_num = seq_along(dates) - 1L,
      actual_week = dates
    )
  }

  lookup <- stats::setNames(clock_table$model_num,
                            as.character(clock_table$actual_week))
  mapped <- raw |>
    dplyr::filter(
      .data$event_date_actual %in% clock_table$actual_week,
      .data$report_week_actual %in% clock_table$actual_week
    ) |>
    dplyr::mutate(
      event_num = unname(lookup[as.character(.data$event_date_actual)]),
      report_num = unname(lookup[as.character(.data$report_week_actual)]),
      delay = .data$report_num - .data$event_num,
      event_model_date = as.Date("2000-01-01") + 7L * .data$event_num,
      report_model_date = as.Date("2000-01-01") + 7L * .data$report_num
    ) |>
    dplyr::filter(.data$delay >= 0L, .data$delay <= max_delay)

  if (!nrow(mapped)) stop("No usable FluSight rows for the ", clock, " clock.")

  # Synthetic consecutive dates make the compressed-clock construction
  # explicit while remaining compatible with complete_zeroes().  The real
  # dates are joined back immediately afterwards.
  tn <- tbl.now::tbl_now(
    mapped,
    event_date = event_model_date,
    report_date = report_model_date,
    strata = location_name,
    case_count = observation,
    data_type = "count-cumulative",
    event_units = "weeks",
    report_units = "weeks",
    now = max(mapped$report_model_date),
    verbose = FALSE
  )
  tn <- tbl.now::complete_zeroes(tn, max_delay = max_delay,
                                 until = max(mapped$event_model_date))

  source_keys <- mapped |>
    dplyr::select("location_name", "event_num", "report_num") |>
    dplyr::distinct() |>
    dplyr::mutate(was_completed = FALSE)

  completed <- tibble::as_tibble(tn) |>
    dplyr::transmute(
      location_name = .data$location_name,
      event_num = as.integer(.data$.event_num),
      report_num = as.integer(.data$.report_num),
      delay = as.integer(.data$.delay),
      cumulative = as.numeric(.data$observation)
    ) |>
    dplyr::left_join(source_keys,
                     by = c("location_name", "event_num", "report_num")) |>
    dplyr::left_join(
      clock_table |>
        dplyr::rename(event_num = "model_num",
                      event_date_actual = "actual_week"),
      by = "event_num"
    ) |>
    dplyr::left_join(
      clock_table |>
        dplyr::rename(report_num = "model_num",
                      report_week_actual = "actual_week"),
      by = "report_num"
    ) |>
    # The actual release may be a few days after its normalized report week.
    # Use the latest raw release date represented by that model week.  This is
    # the date used by the no-leakage filter.
    dplyr::left_join(
      raw |>
        dplyr::group_by(.data$report_week_actual) |>
        dplyr::summarise(report_date_actual = max(.data$report_date_actual),
                         .groups = "drop"),
      by = "report_week_actual"
    ) |>
    dplyr::mutate(
      report_date_actual = dplyr::coalesce(
        .data$report_date_actual, .data$report_week_actual
      ),
      was_completed = dplyr::coalesce(.data$was_completed, TRUE),
      clock = clock
    ) |>
    dplyr::arrange(.data$location_name, .data$event_num, .data$report_num)

  stopifnot(tbl.now::is_tbl_now(tn),
            identical(tbl.now::get_data_type(tn), "count-cumulative"))

  list(
    name = clock,
    tbl_now = tn,
    data = completed,
    clock = clock_table,
    max_delay = max_delay,
    dropped_source_rows = nrow(raw) - nrow(mapped),
    completed_cells = sum(completed$was_completed)
  )
}

prepare_flusight_asof <- function(states = "Texas",
                                  start = as.Date("2023-09-02"),
                                  settlement_horizon = 52L) {
  stopifnot(requireNamespace("tbl.now", quietly = TRUE),
            requireNamespace("dplyr", quietly = TRUE),
            requireNamespace("tidyr", quietly = TRUE))
  settlement_horizon <- as.integer(settlement_horizon)
  if (settlement_horizon < 1L) stop("settlement_horizon must be positive.")

  raw <- deduplicate_flusight(states, start)
  arms <- list(
    calendar = make_clock_arm(raw, "calendar", settlement_horizon),
    compressed = make_clock_arm(raw, "compressed", settlement_horizon)
  )

  # Per the requested retrospective convention, the last available value is
  # treated as settled truth for event years 2024 and 2025.
  truth <- raw |>
    dplyr::mutate(event_year = as.integer(format(.data$event_date_actual, "%Y"))) |>
    dplyr::filter(.data$event_year %in% c(2024L, 2025L)) |>
    dplyr::group_by(.data$location_name, .data$event_date_actual,
                    .data$event_year) |>
    dplyr::slice_max(.data$report_date_actual, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      location_name = .data$location_name,
      event_date_actual = .data$event_date_actual,
      event_year = .data$event_year,
      truth = as.numeric(.data$observation),
      truth_report_date = .data$report_date_actual
    )

  list(raw = raw, arms = arms, truth = truth, states = states,
       start = as.Date(start), settlement_horizon = settlement_horizon)
}

asof_panel <- function(prepared, state, now,
                       clock = c("calendar", "compressed")) {
  clock <- match.arg(clock)
  now <- as.Date(now)
  arm <- prepared$arms[[clock]]
  x <- arm$data |>
    dplyr::filter(
      .data$location_name == state,
      .data$event_date_actual <= now,
      .data$report_date_actual <= now,
      .data$delay <= prepared$settlement_horizon
    ) |>
    dplyr::arrange(.data$event_num, .data$report_num)
  if (!nrow(x)) stop("No rows for ", state, " at now=", now, ".")

  cells <- x |>
    dplyr::group_by(.data$event_num) |>
    dplyr::arrange(.data$report_num, .by_group = TRUE) |>
    dplyr::mutate(
      increment = .data$cumulative - dplyr::lag(.data$cumulative,
                                                default = 0),
      previous_nonzero = as.numeric(
        dplyr::lag(.data$increment != 0, default = FALSE)
      )
    ) |>
    dplyr::ungroup()

  list(
    state = state,
    now = now,
    clock = clock,
    cells = cells,
    event_levels = sort(unique(cells$event_num)),
    settlement_horizon = prepared$settlement_horizon,
    latest_report_num = max(cells$report_num),
    latest_report_date = max(cells$report_date_actual)
  )
}

# Empirical multiplier using only cohorts whose age-a and horizon-H values were
# both observable at `now`.  No final values from after `now` enter calibration.
empirical_multiplier_draws <- function(panel, event_num, age, current,
                                       n = 1000L, min_pairs = 8L) {
  H <- panel$settlement_horizon
  x <- panel$cells
  at_age <- x |>
    dplyr::filter(.data$delay == age, .data$event_num < event_num) |>
    dplyr::select("event_num", at_age = "cumulative")
  at_horizon <- x |>
    dplyr::filter(.data$delay == H, .data$event_num < event_num) |>
    dplyr::select("event_num", at_horizon = "cumulative")
  ratios <- dplyr::inner_join(at_age, at_horizon, by = "event_num") |>
    dplyr::filter(.data$at_age > 0, .data$at_horizon >= 0) |>
    dplyr::mutate(multiplier = .data$at_horizon / .data$at_age) |>
    dplyr::filter(is.finite(.data$multiplier), .data$multiplier >= 0)

  if (nrow(ratios) < min_pairs) {
    # Pool neighbouring observed ages, still requiring maturity by this now.
    at_any <- x |>
      dplyr::filter(abs(.data$delay - age) <= 2L,
                    .data$event_num < event_num,
                    .data$cumulative > 0) |>
      dplyr::select("event_num", observed_age = "delay",
                    at_age = "cumulative")
    ratios <- dplyr::inner_join(at_any, at_horizon, by = "event_num") |>
      dplyr::mutate(multiplier = .data$at_horizon / .data$at_age) |>
      dplyr::filter(is.finite(.data$multiplier), .data$multiplier >= 0)
  }

  if (!nrow(ratios)) return(rep(as.numeric(current), n))
  as.numeric(current) * sample(ratios$multiplier, n, replace = TRUE)
}
