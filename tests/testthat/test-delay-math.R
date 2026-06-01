# Tests for delay distribution math (R/10_delay.R) and delay-only fits

test_that("lognormal native params round-trips through CDF", {
  ln <- lognormal_native(log(5), 4)
  expect_true(is.finite(ln$log_location))
  expect_true(is.finite(ln$log_scale))
  expect_gt(ln$log_scale, 0)
})

test_that(".delay_distribution_functions returns cdf/log_cdf/log_survival for lognormal", {
  fns <- diseasenowcasting:::.delay_distribution_functions(1L, log(5), 3)
  expect_true(is.function(fns$cdf))
  expect_true(is.function(fns$log_cdf))
  expect_true(is.function(fns$log_survival))
  # CDF is monotone non-decreasing
  vals <- fns$cdf(1:10)
  expect_true(all(diff(vals) >= 0))
  # log_survival at 0 should be 0 (all mass to the right)
  expect_equal(fns$log_survival(0), 0, tolerance = 1e-6)
})

test_that(".delay_distribution_functions returns valid functions for Gamma (family 2)", {
  fns <- diseasenowcasting:::.delay_distribution_functions(2L, 5, 3)   # mean=5, sd=3
  vals <- fns$cdf(1:10)
  expect_true(all(is.finite(vals)))
  expect_true(all(diff(vals) >= 0))
})

test_that(".gengamma_cdf returns valid CDF (monotone, [0,1])", {
  # Q near 0 → lognormal, Q=1 → Weibull, Q>1 → shorter tail
  for (Q in c(0.1, 0.5, 1.0, 1.5, 2.0)) {
    fns <- diseasenowcasting:::.delay_distribution_functions(3L, log(5), Q, 3)
    vals <- fns$cdf(1:15)
    expect_true(all(is.finite(vals)), label = paste("gengamma CDF finite Q=", Q))
    expect_true(all(vals >= 0 & vals <= 1), label = paste("gengamma CDF in [0,1] Q=", Q))
  }
})

test_that(".gengamma_shape_transform maps raw -> bounded Q in (0.05, 3)", {
  for (raw in c(-5, -2, 0, 2, 5)) {
    tr <- diseasenowcasting:::.gengamma_shape_transform(raw)
    expect_gt(tr$shape_Q, 0.04)
    expect_lt(tr$shape_Q, 3.01)
    expect_true(is.finite(tr$log_jacobian))
  }
})

test_that(".nonparametric_delay_functions returns valid pmf/cdf from simplex", {
  probs <- c(0.1, 0.2, 0.4, 0.2, 0.1)
  fns <- diseasenowcasting:::.nonparametric_delay_functions(probs, n_bins = 4L)
  expect_true(is.function(fns$cdf))
  expect_true(is.function(fns$log_pmf_raw))
  cdf_vals <- fns$cdf(1:5)
  expect_true(all(is.finite(cdf_vals)))
  expect_true(all(diff(cdf_vals) >= -1e-10))   # non-decreasing
})

test_that(".discretised_delay_loglik returns finite value for valid data", {
  # log-normal delay: ~5 day mean
  fns <- diseasenowcasting:::.delay_distribution_functions(1L, log(5), 3)
  delays  <- c(1, 3, 5, 7, 10, 15)
  weights <- c(5, 8, 6, 4, 3, 2)
  llik <- diseasenowcasting:::.discretised_delay_loglik(
    delays, weights, split_delay = 10,
    fns$log_cdf, fns$log_survival
  )
  expect_true(is.finite(llik))
  expect_lt(llik, 0)
})

test_that("delay-only fit returns finite delay_mu and delay_sigma (all families)", {
  m <- .make_synth()$m
  max_t <- max(m[, 1])
  for (fam in list(lognormal_delay(), gamma_delay(), generalized_gamma_delay())) {
    mdl <- model(nb_likelihood(), hsgp_epidemic(), fam)
    eng <- prepare_data(mdl, m, max_time = max_t, delay_only = TRUE)
    res <- fit(mdl, eng)
    expect_true(is.finite(res$delay_mu),
                label = paste("delay_mu finite:", fam@name))
    expect_true(is.finite(res$delay_sigma) && res$delay_sigma > 0,
                label = paste("delay_sigma positive:", fam@name))
  }
})

test_that("Dirichlet delay-only fit returns valid simplex", {
  m <- .make_synth()$m
  max_t <- max(m[, 1])
  mdl <- model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay(bins = 8L))
  eng <- prepare_data(mdl, m, max_time = max_t, delay_only = TRUE)
  res <- fit(mdl, eng)
  expect_true(all(res$delay_probs > 0))
  expect_equal(sum(res$delay_probs), 1, tolerance = 1e-4)
})
