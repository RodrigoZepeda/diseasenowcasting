# Self-contained Phase 1 test: recover a known LogNormal delay from a fully
# observed (uncensored) synthetic series. No Stan dependency.

test_that("model() menu constructs the expected components", {
  m1 <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  expect_equal(m1@likelihood@name, "nb")
  expect_equal(m1@epidemic@name, "HSGP")
  expect_equal(m1@delay@name, "LogNormal")
})

test_that("delay-only LogNormal fit recovers the generating delay", {
  set.seed(1)
  # Generate delays from a known lognormal (natural-scale mean ~ 5, sd ~ 4).
  true_mu_log <- log(5)          # log-mean of natural-scale delay
  true_sigma  <- 4               # natural-scale SD
  ln          <- lognormal_native(true_mu_log, true_sigma)
  n <- 50000
  draws <- rlnorm(n, ln$log_location, ln$log_scale)
  d <- pmax(1L, ceiling(draws))  # 1-indexed discrete delays

  # All events at a single early time so censoring is non-binding (long horizon).
  max_time <- 400L
  m <- cbind(event = 1L, count = 1L, delay = d, strata = 1L)
  m <- m[m[, "delay"] <= max_time - 1L, , drop = FALSE]

  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  dat <- prepare_data(mdl, m, max_time = max_time, delay_only = TRUE)
  pr  <- default_priors(mdl, dat)
  rf  <- fit(mdl, dat, priors = pr)

  # With a long horizon and weak priors the MAP should land near the truth.
  expect_equal(rf$delay_mu,    true_mu_log, tolerance = 0.05)
  expect_equal(rf$delay_sigma, true_sigma,  tolerance = 0.15)
  expect_equal(rf$convergence, 0L)
})

test_that("hard-fixing the delay removes it from estimation", {
  set.seed(2)
  m <- cbind(1L, 1L, sample(1:20, 2000, replace = TRUE), 1L)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  dat <- prepare_data(mdl, m, max_time = 60L, delay_only = TRUE)
  pr  <- default_priors(mdl, dat)
  pr  <- fix_param(pr, "delay_mu", log(7))
  pr  <- fix_param(pr, "delay_sigma", 3)
  rf  <- fit(mdl, dat, priors = pr)
  expect_equal(rf$delay_mu, log(7))
  expect_equal(rf$delay_sigma, 3)
})
