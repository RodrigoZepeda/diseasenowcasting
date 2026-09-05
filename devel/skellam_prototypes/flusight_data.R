# FluSight preparation for the standalone count-cumulative prototypes.
#
# The source has a calendar gap between surveillance seasons.  Calendar time is
# not the model clock here: observed publication weeks are pasted together and
# numbered 0, 1, ... .  Event weeks for which no publication week exists are
# consequently outside the prototype.  This avoids turning the off-season into
# an enormous reporting delay.

week_ending_saturday <- function(x) {
  x <- as.Date(x)
  x - ((as.POSIXlt(x)$wday - 6L) %% 7L)
}

prepare_flusight <- function(states = "Texas",
                             start = as.Date("2023-09-02"),
                             max_delay = 15L) {
  stopifnot(requireNamespace("tbl.now", quietly = TRUE),
            requireNamespace("dplyr", quietly = TRUE),
            requireNamespace("tidyr", quietly = TRUE))

  raw <- tbl.now::flusight |>
    dplyr::filter(.data$location_name %in% states,
                  .data$target_end_date >= start,
                  .data$as_of >= start,
                  !is.na(.data$observation)) |>
    dplyr::mutate(
      event_week_actual = week_ending_saturday(.data$target_end_date),
      report_week_actual = week_ending_saturday(.data$as_of)
    )

  publication_weeks <- sort(unique(raw$report_week_actual))
  if (length(publication_weeks) < 3L)
    stop("The selected FluSight window has fewer than three publication weeks.")

  clock <- data.frame(
    actual_week = publication_weeks,
    compressed_index = seq_along(publication_weeks) - 1L
  )
  event_clock <- stats::setNames(clock$compressed_index,
                                 as.character(clock$actual_week))
  report_clock <- event_clock

  # Only weeks on the observed publication clock survive. This is the explicit
  # "paste seasons together" rule requested for 2023 -> 2024 -> 2025.
  raw_mapped <- raw |>
    dplyr::filter(.data$event_week_actual %in% publication_weeks) |>
    dplyr::mutate(
      event_index = unname(event_clock[as.character(.data$event_week_actual)]),
      report_index = unname(report_clock[as.character(.data$report_week_actual)]),
      delay = .data$report_index - .data$event_index
    ) |>
    dplyr::filter(.data$delay >= 0L)

  settled <- raw_mapped |>
    dplyr::group_by(.data$location_name, .data$event_index,
                    .data$event_week_actual) |>
    dplyr::slice_max(.data$report_index, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      location_name = .data$location_name,
      event_index = .data$event_index,
      event_week_actual = .data$event_week_actual,
      event_year = as.integer(format(.data$event_week_actual, "%Y")),
      final_report_index = .data$report_index,
      settled_count = .data$observation
    )

  raw <- raw_mapped |>
    dplyr::filter(.data$delay <= max_delay) |>
    dplyr::select("location_name", "event_week_actual", "report_week_actual",
                  "event_index", "report_index", "delay", "observation")

  # Build every observable state x event x report cell and make absent counts
  # explicit zeroes. This is intentionally done before tbl_now() so the exact
  # grid is easy to audit.
  full <- tidyr::expand_grid(
    location_name = states,
    event_index = sort(unique(raw$event_index)),
    report_index = sort(unique(raw$report_index))
  ) |>
    dplyr::mutate(delay = .data$report_index - .data$event_index) |>
    dplyr::filter(.data$delay >= 0L, .data$delay <= max_delay) |>
    dplyr::left_join(
      raw |>
        dplyr::select("location_name", "event_index", "report_index",
                      "observation"),
      by = c("location_name", "event_index", "report_index")
    ) |>
    dplyr::mutate(
      was_missing = is.na(.data$observation),
      observation = dplyr::coalesce(.data$observation, 0),
      event_week_actual = clock$actual_week[.data$event_index + 1L],
      report_week_actual = clock$actual_week[.data$report_index + 1L],
      # Synthetic dates are consecutive by construction. Actual dates remain in
      # `clock` and are used only for labels and audit output.
      event_date = as.Date("2023-01-07") + 7L * .data$event_index,
      report_date = as.Date("2023-01-07") + 7L * .data$report_index
    ) |>
    dplyr::arrange(.data$location_name, .data$event_index, .data$report_index)

  tn <- tbl.now::tbl_now(
    full,
    event_date = event_date,
    report_date = report_date,
    strata = location_name,
    case_count = observation,
    data_type = "count-cumulative",
    event_units = "weeks",
    report_units = "weeks",
    verbose = FALSE
  )
  tn <- tbl.now::complete_zeroes(tn)
  stopifnot(tbl.now::is_tbl_now(tn),
            identical(tbl.now::get_data_type(tn), "count-cumulative"))

  list(
    tbl_now = tn,
    data = tibble::as_tibble(tn),
    settled = settled,
    clock = clock,
    states = states,
    start = start,
    max_delay = max_delay,
    n_zeroes_added = sum(full$was_missing),
    calendar_gaps = data.frame(
      before = utils::head(publication_weeks, -1L),
      after = utils::tail(publication_weeks, -1L),
      calendar_days = as.integer(diff(publication_weeks)),
      compressed_steps = 1L
    )
  )
}

state_panel <- function(prepared, state, report_cut = Inf) {
  x <- prepared$data |>
    dplyr::filter(.data$location_name == state,
                  .data$report_index <= report_cut) |>
    dplyr::arrange(.data$event_index, .data$report_index)

  if (!nrow(x)) stop("No rows for state `", state, "` at this report cut.")
  split_x <- split(x, x$event_index)
  cells <- lapply(split_x, function(z) {
    z <- z[order(z$report_index), , drop = FALSE]
    cumulative <- z$observation
    increment <- cumulative - c(0, utils::head(cumulative, -1L))
    data.frame(
      event_index = z$event_index,
      event_week_actual = z$event_week_actual,
      report_index = z$report_index,
      age = z$delay,
      increment = increment,
      cumulative = cumulative,
      previous_moved = c(0, as.numeric(utils::head(increment, -1L) != 0))
    )
  })
  cells <- dplyr::bind_rows(cells)

  by_event <- split(cells, cells$event_index)
  first_total <- sum(vapply(by_event, function(z) z$cumulative[1L], numeric(1)))
  later <- cells[cells$age > ave(cells$age, cells$event_index, FUN = min), , drop = FALSE]
  up <- sum(pmax(later$increment, 0))
  down <- -sum(pmin(later$increment, 0))
  p_empirical <- min(max(1 - down / max(first_total + up, 1), 0.01), 0.995)

  list(
    state = state,
    cells = cells,
    event_levels = sort(unique(cells$event_index)),
    p_empirical = p_empirical,
    first_total = first_total,
    up = up,
    down = down,
    signed_zero_rate = mean(cells$increment[cells$age > 0L] == 0),
    report_cut = report_cut
  )
}
