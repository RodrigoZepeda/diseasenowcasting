# Tests for reporting_fraction() (R/30_reporting_fraction.R)
#
# Gstar is the fitted delay CDF at each cohort's age -- the fraction of that
# event-time the model believes has arrived.  `inflation = 1 / Gstar` is exactly
# the multiplier the nowcast applies to the observed count, and it is the
# quantity most nowcast level errors travel through.

test_that("reporting_fraction() returns one row per (event-time, stratum)", {
  skip_on_cran()
  tn  <- .make_synth_tblnow(Tn = 50L)
  nc  <- suppressMessages(nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                                  type = "one_stage", n_draws = 50L, seed = 3L))
  rf <- reporting_fraction(nc)

  expect_s3_class(rf, "data.frame")
  expect_equal(nrow(rf), nc@engine$max_time * nc@engine$num_strata)
  expect_true(all(c("stratum", "horizon", "observed",
                    "reporting_fraction", "inflation") %in% names(rf)))

  # A fraction is a probability and its reciprocal is at least one.  There is no
  # upper bound: a young cohort legitimately inflates by orders of magnitude.
  expect_true(all(rf$reporting_fraction > 0 & rf$reporting_fraction <= 1))
  expect_true(all(rf$inflation >= 1))
  expect_equal(rf$inflation, 1 / rf$reporting_fraction, tolerance = 1e-12)

  # Horizon 0 is the as-of cohort and is the least complete; the oldest cohort
  # has had the whole series to report and is essentially settled.
  expect_equal(min(rf$horizon), 0L)
  expect_lt(rf$reporting_fraction[rf$horizon == 0],
            rf$reporting_fraction[rf$horizon == max(rf$horizon)])
  expect_gt(rf$inflation[rf$horizon == 0], 1)
})

test_that("reporting_fraction() is monotone in cohort age", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 50L)
  nc <- suppressMessages(nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                                 type = "one_stage", n_draws = 50L, seed = 3L))
  rf <- reporting_fraction(nc)
  rf <- rf[order(rf$horizon), ]
  # G*(t) = F_D(d*_t + 1) with d* increasing in age, and a CDF is non-decreasing,
  # so an older cohort can never be less complete than a younger one.
  expect_false(is.unsorted(rf$reporting_fraction))
})

test_that("reporting_fraction(summary = FALSE) exposes the delay imputations", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 50L)
  nc <- suppressMessages(suppressWarnings(
    nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
            type = "two_stage", K = 4L, n_draws = 50L, seed = 3L)))

  pooled  <- reporting_fraction(nc)
  per_fit <- reporting_fraction(nc, summary = FALSE)
  expect_true("fit" %in% names(per_fit))
  expect_equal(length(unique(per_fit$fit)), length(nc@fits))
  expect_equal(nrow(per_fit), nrow(pooled) * length(nc@fits))

  # The pooled column is the median over fits, and the reported range brackets
  # it -- that spread IS the delay uncertainty the two-stage cascade propagates.
  expect_true(all(pooled$reporting_fraction_low <= pooled$reporting_fraction))
  expect_true(all(pooled$reporting_fraction >= pooled$reporting_fraction_low))
  expect_true(all(pooled$reporting_fraction <= pooled$reporting_fraction_high))
})

test_that("reporting_fraction() labels strata and carries the event calendar", {
  skip_on_cran()
  tn <- .make_strata_tblnow(Tn = 45L)
  nc <- suppressMessages(nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                                 type = "one_stage", n_draws = 50L, seed = 3L))
  rf <- reporting_fraction(nc)
  expect_equal(length(unique(rf$stratum)), nc@engine$num_strata)
  expect_true("event_date" %in% names(rf))
  expect_s3_class(rf$event_date, "Date")
  # Every stratum spans the same calendar: the delay law is shared.
  expect_equal(length(unique(table(rf$stratum))), 1L)
})

test_that("reporting_fraction() rejects objects it cannot read", {
  expect_error(reporting_fraction(list()), "must be a result from")
})
