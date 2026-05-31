# Two-stage Dirichlet: Stage-1 NP delay-only fit + simplex imputation + hard-fix
# Stage-2 pooling.  Verifies the NP path actually engages the `multi` rung
# (does not fall through to the plain one-stage fit) and produces a sane nowcast.

test_that("Dirichlet delay-only (Stage-1) fits a simplex", {
  m <- .make_synth(Tn = 90L, seed = 4)$m
  mdl <- model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay())
  dat <- prepare_data(mdl, m, max_time = 90L, delay_only = TRUE)
  rf  <- fit(mdl, dat, priors = default_priors(mdl, dat))
  expect_equal(rf$convergence, 0L)
  expect_equal(sum(rf$delay_probs), 1, tolerance = 1e-6)        # valid simplex
  expect_length(rf$delay_probs, as.integer(dat$np_model_length) + 1L)
})

test_that("Dirichlet two-stage engages the multi rung and is well-behaved", {
  s <- .make_synth(Tn = 110L, peak = 100, ctr = 65, seed = 8)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay())
  nc  <- nowcast_twostage(mdl, s$m, max_time = 110L, K = 6L, n_draws_per = 120L, seed = 1)
  expect_equal(nc$rung, "multi")                                # true two-stage, not one-stage fallback
  expect_true(nc$n_samp >= 1L)
  expect_true(all(is.finite(nc$quantiles)))
  expect_true(nc$quantiles[1] <= nc$quantiles[9])               # monotone
  expect_true(nc$median > 0)
})
