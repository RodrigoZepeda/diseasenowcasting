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
