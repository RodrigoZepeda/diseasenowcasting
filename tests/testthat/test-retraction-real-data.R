# =============================================================================
# Linelist retractions on the bundled real surveillance data
# =============================================================================
# `test-retraction.R` works on simulated linelists, where the truth is known
# exactly.  These tests run the same machinery over the real reporting patterns of
# `tbl.now::denguedat` (a genuine weekly linelist, with gender strata),
# `tbl.now::mpoxdat` and `tbl.now::covid_colombia` (daily counts, expanded to one
# row per case) -- irregular delays, day-of-week structure, zero weeks, epidemic
# peaks and tails, none of which a tidy simulation reproduces.
#
# The retraction marks themselves have to be injected: no public dataset records
# when a case was withdrawn from the register.  So the *reporting* process is real
# and the *retraction* process is known, which is what makes recovery checkable.
# =============================================================================

skip_if_no_data <- function(dataset) {
  skip_if_not_installed("tbl.now")
  skip_if(!exists(dataset, envir = asNamespace("tbl.now")),
          paste0("tbl.now::", dataset, " not available"))
}

#' One row per case, with a retraction date injected on a random `retract_share`.
#'
#' `lag_units` is in event units, so the retraction always lands in a later period
#' than the report and `g_C` keeps its `{1, 2, ...}` support.
inject_retractions <- function(linelist, step_days, retract_share = 0.15,
                               lag_mean = 2, seed = 42) {
  set.seed(seed)
  erroneous <- runif(nrow(linelist)) < retract_share
  lag_units <- 1L + rpois(nrow(linelist), lag_mean)
  linelist$retracted <- as.Date(NA)
  linelist$retracted[erroneous] <- linelist$reported[erroneous] +
    step_days * lag_units[erroneous]
  linelist
}

real_dengue_linelist <- function(from = as.Date("2007-01-01")) {
  linelist <- as.data.frame(tbl.now::denguedat)
  linelist <- data.frame(onset    = as.Date(linelist$onset_week),
                         reported = as.Date(linelist$report_week),
                         gender   = as.character(linelist$gender))
  linelist[linelist$onset >= from & linelist$reported >= linelist$onset, , drop = FALSE]
}

real_daily_linelist <- function(dataset) {
  frame <- as.data.frame(get(dataset, envir = asNamespace("tbl.now")))
  columns <- if (dataset == "mpoxdat") c("dx_date", "dx_report_date")
             else c("diagnosis_date", "notification_date")
  linelist <- data.frame(onset    = as.Date(frame[[columns[1]]]),
                         reported = as.Date(frame[[columns[2]]]),
                         n        = as.integer(frame$n))
  linelist <- linelist[linelist$reported >= linelist$onset & linelist$n > 0, , drop = FALSE]
  # Expand the aggregated counts into individual rows, which is what the retraction
  # model consumes.
  linelist[rep(seq_len(nrow(linelist)), linelist$n), c("onset", "reported")]
}

# Truth: cases that are reported at some point and NEVER retracted, per event time.
settled_truth <- function(linelist, event_dates) {
  settled <- linelist[is.na(linelist$retracted), , drop = FALSE]
  as.numeric(table(factor(as.character(settled$onset), levels = as.character(event_dates))))
}

fit_real <- function(linelist, now, ..., strata = FALSE, epidemic = hsgp_epidemic()) {
  tn <- if (strata) as_validation_tbl_now(linelist, now, strata = "gender")
        else        as_validation_tbl_now(linelist, now)
  suppressMessages(suppressWarnings(nowcast(
    tn, model(nb_likelihood(), epidemic, lognormal_delay(),
              validation = validation_process(validation_delay = dirichlet_validation(bins = 8),
                                                  ...)),
    now = now, type = "one_stage",
    temporal_effects = "none", n_draws = 300, seed = 8)))
}

# ── dengue: a real weekly linelist ───────────────────────────────────────────

test_that("dengue: p and the settled counts are recovered from real reporting delays", {
  skip_on_cran()
  skip_if_no_data("denguedat")
  linelist <- inject_retractions(real_dengue_linelist(), step_days = 7L, seed = 51)
  now <- max(linelist$onset) - 21L                       # three weeks before the end
  as_of <- linelist[linelist$onset <= now & linelist$reported <= now, , drop = FALSE]

  fitted <- fit_real(as_of, now)
  expect_equal(fitted@engine$is_linelist_retraction, 1L)
  expect_equal(fitted@fits[[1]]$reconstruct$retraction$p, 0.85, tolerance = 0.04)

  prediction <- predict(fitted)
  truth  <- settled_truth(linelist, prediction@event_dates)
  bounds <- apply(prediction@draws, 2, quantile, c(0.025, 0.975), na.rm = TRUE)
  expect_gt(mean(truth >= bounds[1, ] & truth <= bounds[2, ]), 0.85)

  # The point nowcast must land near the settled truth, not the gross row count --
  # which is what ignoring the retraction column would give.
  medians <- apply(prediction@draws, 2, median, na.rm = TRUE)
  settled_window <- seq_len(length(truth) - 4L)
  expect_lt(mean(abs(medians[settled_window] - truth[settled_window]) /
                 pmax(truth[settled_window], 1)), 0.15)
})

test_that("dengue: censored reports and censored retractions still recover p", {
  skip_on_cran()
  skip_if_no_data("denguedat")
  linelist <- inject_retractions(real_dengue_linelist(), step_days = 7L, seed = 52)
  now <- max(linelist$onset) - 21L
  as_of <- linelist[linelist$onset <= now & linelist$reported <= now, , drop = FALSE]

  # The three partial-observation regimes of the design, on real delays.
  set.seed(53)
  configurations <- list(
    "censored report"     = c(0.15, 0),
    "censored retraction" = c(0, 0.15),
    "both censored"       = c(0.15, 0.15))
  fitted_p <- vapply(configurations, function(fractions) {
    censored <- as_of
    censored$is_censored <- runif(nrow(censored)) < fractions[1]
    censored$q_bound <- !is.na(censored$retracted) & runif(nrow(censored)) < fractions[2]
    # A censored report date is recorded at its upper bound; it must stay before the
    # retraction (rule 1: the retraction bounds the report) and before `now`.
    bumped <- censored$is_censored
    censored$reported[bumped] <- pmin(
      censored$reported[bumped] + 7L, now,
      ifelse(is.na(censored$retracted[bumped]), now, censored$retracted[bumped] - 7L))
    censored$retracted[censored$q_bound] <- pmin(censored$retracted[censored$q_bound] + 7L, now)

    tn <- as_validation_tbl_now(censored, now, is_censored_report = is_censored)
    fitted <- suppressMessages(suppressWarnings(nowcast(tn,
      model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
            validation = validation_process(validation_delay = dirichlet_validation(bins = 8))),
      now = now, validation_censored = "q_bound",
      type = "one_stage", temporal_effects = "none", n_draws = 50, seed = 8)))
    expect_gt(fitted@engine$n_censored, 0)
    fitted@fits[[1]]$reconstruct$retraction$p
  }, numeric(1))

  expect_true(all(abs(fitted_p - 0.85) < 0.05))
  expect_lt(diff(range(fitted_p)), 0.03)
})

test_that("dengue: stratified_p recovers a real gender split with different p", {
  skip_on_cran()
  skip_if_no_data("denguedat")
  linelist <- real_dengue_linelist()
  # Different retraction rates by gender, on the real strata.
  set.seed(54)
  p_true <- c(Female = 0.90, Male = 0.72)
  linelist$retracted <- as.Date(NA)
  for (gender in names(p_true)) {
    in_stratum <- which(linelist$gender == gender)
    erroneous  <- in_stratum[runif(length(in_stratum)) > p_true[[gender]]]
    linelist$retracted[erroneous] <- linelist$reported[erroneous] +
      7L * (1L + rpois(length(erroneous), 2))
  }
  now <- max(linelist$onset) - 21L
  as_of <- linelist[linelist$onset <= now & linelist$reported <= now, , drop = FALSE]

  stratified <- fit_real(as_of, now, strata = TRUE, stratified_p = TRUE)
  shared     <- fit_real(as_of, now, strata = TRUE, stratified_p = FALSE)

  fitted_p <- stratified@fits[[1]]$reconstruct$retraction$p_by_stratum
  expect_length(fitted_p, 2L)
  expect_true(all(abs(sort(fitted_p) - sort(unname(p_true))) < 0.05))
  # The shared fit must be a strict compromise, and worse in likelihood.
  expect_lt(stratified@fits[[1]]$nll, shared@fits[[1]]$nll)
  shared_p <- shared@fits[[1]]$reconstruct$retraction$p_by_stratum[1]
  expect_gt(shared_p, min(p_true)); expect_lt(shared_p, max(p_true))
})

# ── mpox and covid: real daily reporting ─────────────────────────────────────

test_that("mpox and covid: p is recovered from real daily reporting delays", {
  skip_on_cran()
  for (dataset in c("mpoxdat", "covid_colombia")) {
    skip_if_no_data(dataset)
    linelist <- inject_retractions(real_daily_linelist(dataset), step_days = 1L, seed = 55)
    # Evaluate inside the outbreak, not in its tail, where counts are large enough
    # for the cure block to say anything.
    now <- stats::quantile(linelist$onset, 0.8, type = 1)
    as_of <- linelist[linelist$onset <= now & linelist$reported <= now, , drop = FALSE]

    fitted <- fit_real(as_of, now)
    expect_equal(fitted@fits[[1]]$reconstruct$retraction$p, 0.85, tolerance = 0.04,
                 label = paste0("p (", dataset, ")"))

    prediction <- predict(fitted)
    truth  <- settled_truth(linelist, prediction@event_dates)
    bounds <- apply(prediction@draws, 2, quantile, c(0.025, 0.975), na.rm = TRUE)
    expect_gt(mean(truth >= bounds[1, ] & truth <= bounds[2, ]), 0.8)
  }
})

test_that("covid: the retraction model beats ignoring or dropping the retracted rows", {
  # The comparison the feature exists for, on real reporting delays.  The two
  # naive alternatives fail in different places, so they are measured separately:
  #   keep_all   targets the GROSS report total, so it over-counts by ~1/p - 1
  #              everywhere, settled event times included;
  #   drop_known deletes the rows already retracted, which is right once an event
  #              time has settled but wrong at RECENT ones, where the retractions
  #              that will cancel today's reports have not landed yet.
  skip_on_cran()
  skip_if_no_data("covid_colombia")
  linelist <- inject_retractions(real_daily_linelist("covid_colombia"),
                                 step_days = 1L, seed = 56)
  now <- stats::quantile(linelist$onset, 0.8, type = 1)
  as_of <- linelist[linelist$onset <= now & linelist$reported <= now, , drop = FALSE]

  relative_bias <- function(rows, use_retraction) {
    # The validation process is detected from the object, so the two arms differ in
    # whether the tbl_now CARRIES one -- not in an argument to nowcast().
    tn <- if (use_retraction) as_validation_tbl_now(rows, now) else
      suppressWarnings(tbl.now::tbl_now(rows, event_date = onset,
            report_date = reported, now = now, data_type = "linelist", verbose = FALSE))
    fitted <- suppressMessages(suppressWarnings(nowcast(tn,
      model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
      now = now, type = "one_stage", temporal_effects = "none", n_draws = 200, seed = 8)))
    prediction  <- predict(fitted)
    truth       <- settled_truth(linelist, prediction@event_dates)
    medians     <- apply(prediction@draws, 2, median, na.rm = TRUE)
    n_events    <- length(truth)
    settled_window <- seq_len(n_events - 10L)
    # Recent but not the newest: the last event time is dominated by reporting
    # delay, which is not what this test is about.
    recent_window  <- (n_events - 6L):(n_events - 1L)
    scaled_error <- function(window)
      mean((medians[window] - truth[window]) / pmax(truth[window], 1))
    c(settled = scaled_error(settled_window), recent = scaled_error(recent_window))
  }

  retraction <- relative_bias(as_of, TRUE)
  keep_all   <- relative_bias(as_of, FALSE)
  drop_known <- relative_bias(
    as_of[is.na(as_of$retracted) | as_of$retracted > now, , drop = FALSE], FALSE)

  # Settled event times: keep_all over-counts by roughly the retraction rate;
  # dropping the known retractions is already correct there, as is the model.
  expect_gt(keep_all[["settled"]], 0.10)
  expect_lt(abs(retraction[["settled"]]), 0.05)

  # Recent event times: this is where drop_known breaks, and the model should not.
  expect_lt(abs(retraction[["recent"]]), abs(drop_known[["recent"]]))
  expect_lt(abs(retraction[["recent"]]), abs(keep_all[["recent"]]))
})
