# Tests for prepare_data() edge cases and coverage gaps (R/09_prepare_data.R)

test_that("prepare_data() returns correct dimensions for unstratified data", {
  m <- .make_synth()$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng <- prepare_data(mdl, m)
  expect_equal(eng$num_strata, 1L)
  expect_equal(nrow(eng$case_counts), eng$max_time)
  expect_equal(ncol(eng$case_counts), 1L)
})

test_that("prepare_data() with explicit num_strata=3 pads case_counts correctly", {
  m <- .make_synth()$m
  m3 <- cbind(m, c(rep(1L, floor(nrow(m)/2)), rep(2L, nrow(m) - floor(nrow(m)/2))))
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng <- prepare_data(mdl, m3, num_strata = 3L)
  expect_equal(eng$num_strata, 3L)
  expect_equal(ncol(eng$case_counts), 3L)
  # Third stratum should be all zeros
  expect_equal(sum(eng$case_counts[, 3]), 0)
})

test_that("prepare_data() d_star is [max_time x num_strata] matrix", {
  m   <- .make_synth()$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng <- prepare_data(mdl, m)
  expect_true(is.matrix(eng$d_star))
  expect_equal(dim(eng$d_star), c(eng$max_time, 1L))
  # d_star[1,1] = max_time - 1 (most recent event, max observable delay)
  expect_equal(eng$d_star[1, 1], eng$max_time - 1L)
  # d_star[max_time,1] = 0 (oldest event, already fully reported)
  expect_equal(eng$d_star[eng$max_time, 1], 0L)
})

test_that("prepare_data() auto-computes num_basis correctly", {
  m   <- .make_synth(Tn = 60L)$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng <- prepare_data(mdl, m)
  # num_basis auto = ceiling(1.5 * sqrt(max_time))
  expected <- min(150L, max(12L, as.integer(ceiling(1.5 * sqrt(eng$max_time)))))
  expect_equal(eng$num_basis, expected)
})

test_that("prepare_data() respects explicit num_basis in hsgp_epidemic()", {
  m   <- .make_synth()$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(num_basis = 15L), lognormal_delay())
  eng <- prepare_data(mdl, m)
  expect_equal(eng$num_basis, 15L)
})

test_that("prepare_data() for dirichlet_delay sets np_model_length from data", {
  m   <- .make_synth()$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay())
  eng <- prepare_data(mdl, m, delay_only = TRUE)
  expect_equal(eng$np_model_length, as.integer(max(m[, 3])))
})

test_that("prepare_data() for dirichlet_delay with explicit bins", {
  m   <- .make_synth()$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay(bins = 12L))
  eng <- prepare_data(mdl, m)
  expect_equal(eng$np_model_length, 12L)
})

test_that("prepare_data() mu_log_upper_bound is finite and reasonable", {
  m   <- .make_synth()$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng <- prepare_data(mdl, m)
  expect_true(is.finite(eng$mu_log_upper_bound))
  expect_gte(eng$mu_log_upper_bound, 6)
  expect_lte(eng$mu_log_upper_bound, 16)
})

test_that("mu_log_upper_bound clears the LATENT scale, not just the observed one", {
  # The bound is a numerical guard on `exp()`, but it is sized from the counts
  # REPORTED so far.  The latent incidence exceeds those by 1/Gstar -- the
  # reciprocal of the reporting fraction, i.e. the quantity being nowcast -- so
  # a bound at log1p(casemax) truncates any stream that is still mostly
  # unreported.  This pins the headroom that keeps it clear of that.
  m   <- .make_synth()$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng <- prepare_data(mdl, m)
  casemax <- max(abs(eng$case_counts))

  # A latent scale an order of magnitude above the largest reported count is
  # ordinary at a 10% reporting fraction, and has to sit under the ceiling.
  expect_gt(eng$mu_log_upper_bound, log(10 * casemax))
  # The softplus is within 5% of the identity only three log units down, so
  # "under the ceiling" has to mean under it with room to spare.
  expect_gt(eng$mu_log_upper_bound - log(10 * casemax), 3)

  # covid_us, 2020-03-20: 122 cases reported against a settled 2,922, i.e. the
  # fit has to reach a lambda ~24x the largest count it can see.  The old bound
  # min(max(6, log1p(casemax)), 16) = 6.0 put the ceiling at 403 -- a seventh of
  # the answer -- and the fit pinned to it.  Sized from the same casemax, the
  # bound must now clear the settled truth by an order of magnitude.
  covid_bound <- diseasenowcasting:::.mu_log_upper_bound(122)
  expect_gt(exp(covid_bound) / 2922, 10)
})

test_that(".mu_log_upper_bound() keeps the hard overflow stop and honours an override", {
  # exp(16) ~ 8.9M is the overflow guard, and it still binds.
  expect_equal(diseasenowcasting:::.mu_log_upper_bound(1e9), 16)
  # Small counts get the floor of 6 plus the reporting headroom, not 6.
  expect_equal(diseasenowcasting:::.mu_log_upper_bound(5), 6 + log(100))
  expect_equal(diseasenowcasting:::.mu_log_upper_bound(exp(8) - 1), 8 + log(100))

  expect_equal(diseasenowcasting:::.mu_log_upper_bound(5, override = 4), 4)
  expect_error(diseasenowcasting:::.mu_log_upper_bound(5, override = -1), "finite positive number")
  expect_error(diseasenowcasting:::.mu_log_upper_bound(5, override = c(1, 2)), "finite positive number")
  expect_error(diseasenowcasting:::.mu_log_upper_bound(5, override = NA_real_), "finite positive number")
})

test_that("prepare_data() passes an explicit mu_log_upper_bound through", {
  m   <- .make_synth()$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng <- prepare_data(mdl, m, mu_log_upper_bound = 9)
  expect_equal(eng$mu_log_upper_bound, 9)
})

test_that("prepare_data() SIR path sets N_pop from sir_epidemic()", {
  m   <- .make_synth()$m
  mdl <- model(nb_likelihood(), sir_epidemic(N_pop = 1e5), lognormal_delay())
  eng <- prepare_data(mdl, m)
  expect_equal(eng$N_pop, 1e5)
})

test_that("prepare_data() with m_censored adds counts correctly", {
  m     <- .make_synth()$m
  m_cen <- m[1:5, , drop = FALSE]
  mdl   <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng   <- prepare_data(mdl, m, m_censored = m_cen)
  eng0  <- prepare_data(mdl, m)
  # case_counts with censored should be >= without
  expect_true(sum(eng$case_counts) >= sum(eng0$case_counts))
})

# =============================================================================
# HSGP basis count against the series length
# =============================================================================
# The basis count sets the shortest wavelength the trend can resolve.  Once it
# approaches max_time / 2 the trend can chase the most recent, least-reported
# points instead of smoothing them.  This was invisible while the log_mean cap
# clipped the resulting runaway; see .warn_hsgp_basis_fraction().

test_that(".auto_hsgp_num_basis() reproduces prepare_data()'s own ladder", {
  auto <- diseasenowcasting:::.auto_hsgp_num_basis
  expect_equal(auto(5), 3L)
  expect_equal(auto(15), 8L)
  expect_equal(auto(33), 12L)     # the floor, not 1.5 * sqrt(33) = 9
  expect_equal(auto(100), 15L)
  expect_equal(auto(1e6), 150L)   # the cap

  # And the engine agrees with the helper on a real series.
  m   <- .make_synth(Tn = 60L)$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng <- prepare_data(mdl, m)
  expect_equal(eng$num_basis, auto(eng$max_time))
})

test_that(".warn_hsgp_basis_fraction() fires above half the event-times", {
  warn <- diseasenowcasting:::.warn_hsgp_basis_fraction
  # 20 basis functions on 33 event-times is the mpox 2022-08-09 configuration
  # that nowcast 25x the settled truth with a band of [153, 17787].
  expect_warning(warn(20L, 33L, explicit = TRUE), "more flexibility than the series supports")
  expect_warning(warn(20L, 33L, explicit = TRUE), "automatic count, 12 here")
  # The auto count on the same series is inert even if it were checked.
  expect_no_warning(warn(12L, 33L, explicit = TRUE))
  # Exactly at the threshold is not a warning; one past it is.
  expect_no_warning(warn(16L, 32L, explicit = TRUE))
  expect_warning(warn(17L, 32L, explicit = TRUE))
  # The automatic count never reaches the threshold once the series is long
  # enough for its 1.5 * sqrt(max_time) branch to clear the floor of 12.
  auto <- diseasenowcasting:::.auto_hsgp_num_basis
  for (tn in c(24L, 30L, 60L, 120L, 365L, 1095L)) {
    expect_no_warning(warn(auto(tn), tn, explicit = TRUE))
  }
  # Below that the floor does exceed half the series -- which is exactly why an
  # automatic count is never checked at all.
  expect_gt(auto(20L), 20L %/% 2L)
  expect_no_warning(warn(auto(20L), 20L, explicit = FALSE))
})

test_that(".warn_hsgp_basis_fraction() stays silent for an automatic count", {
  warn <- diseasenowcasting:::.warn_hsgp_basis_fraction
  auto <- diseasenowcasting:::.auto_hsgp_num_basis
  # The automatic ladder's floor of 12 is itself more than half of a 20-step
  # series, so warning on it would fire on the package's own default path.
  # Short series are handled by auto_nowcast()'s min_hsgp instead.
  expect_no_warning(warn(auto(20L), 20L, explicit = FALSE))
  expect_no_warning(warn(auto(10L), 10L, explicit = FALSE))
  expect_no_warning(warn(auto(4L), 4L, explicit = FALSE))
  # A number the caller supplied is actionable, and is told so.
  expect_warning(warn(20L, 33L, explicit = TRUE), "Drop `num_basis`")
  expect_no_warning(warn(NA_integer_, 33L, explicit = TRUE))
  expect_no_warning(warn(12L, 0L, explicit = TRUE))
})

test_that("prepare_data() warns on an over-flexible basis but not in delay_only mode", {
  m   <- .make_synth(Tn = 30L)$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(num_basis = 25L), lognormal_delay())
  expect_warning(prepare_data(mdl, m), "HSGP basis function")
  # Stage 1 never builds the epidemic process, so the basis count is not its
  # problem and the warning would be pure noise on every two-stage fit.
  expect_no_warning(prepare_data(mdl, m, delay_only = TRUE))
  # A non-HSGP process is never asked about a basis count.
  ar <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
  expect_no_warning(prepare_data(ar, m))
})
