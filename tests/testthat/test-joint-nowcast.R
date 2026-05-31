# Self-contained joint-fit + nowcast + two-stage tests on synthetic data
# (no Stan dependency). Validates the epidemic-process Laplace path end to end.

.make_synth <- function(Tn = 80L, peak = 80, ctr = 45, wid = 14,
                        base = 8, mu_log = log(5), sigma = 4, seed = 7) {
  set.seed(seed)
  lambda <- peak * exp(-0.5 * ((seq_len(Tn) - ctr) / wid)^2) + base
  ln <- lognormal_native(mu_log, sigma)
  rows <- list()
  for (t in seq_len(Tn)) {
    n <- rpois(1, lambda[t])
    if (n > 0) {
      d <- pmax(1L, ceiling(rlnorm(n, ln$log_location, ln$log_scale)))
      keep <- d <= (Tn - t + 1)
      if (any(keep)) {
        tb <- table(d[keep])
        for (k in seq_along(tb))
          rows[[length(rows) + 1]] <- c(t, as.integer(tb[k]), as.integer(names(tb)[k]), 1L)
      }
    }
  }
  m <- do.call(rbind, rows); colnames(m) <- c("event", "count", "delay", "strata")
  list(m = m, Tn = Tn, lambda = lambda)
}

test_that("HSGP joint fit recovers the epidemic curve and delay", {
  s <- .make_synth()
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  dat <- prepare_data(mdl, s$m, max_time = s$Tn, delay_only = FALSE)
  pr  <- default_priors(mdl, dat, phi = lognormal_prior(log(20), 0.5))
  rf  <- fit(mdl, dat, priors = pr)
  expect_equal(rf$convergence, 0L)
  # Peak incidence recovered within ~20%.
  expect_equal(max(rf$lambda), max(s$lambda), tolerance = 0.2)
  # Delay log-location recovered.
  expect_equal(rf$log_loc, lognormal_native(log(5), 4)$log_location, tolerance = 0.1)
})

test_that("AR1 joint fit converges and produces a finite nowcast", {
  s <- .make_synth(seed = 3)
  mdl <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
  dat <- prepare_data(mdl, s$m, max_time = s$Tn, delay_only = FALSE)
  pr  <- default_priors(mdl, dat, phi = lognormal_prior(log(20), 0.5),
                        ar_sigma = exponential_prior(1))
  rf  <- fit(mdl, dat, priors = pr)
  expect_equal(rf$convergence, 0L)
  nc <- nowcast(rf, n_draws = 500, seed = 1)
  expect_true(all(is.finite(nc$quantiles)))
  expect_true(nc$quantiles[1] <= nc$quantiles[9])   # monotone quantiles
})

test_that("two-stage multisample nowcast pools imputations", {
  s <- .make_synth(Tn = 120L, peak = 100, ctr = 70, seed = 9)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  nc  <- nowcast_twostage(mdl, s$m, max_time = s$Tn, K = 6L,
                          n_draws_per = 100L, seed = 1)
  expect_true(nc$rung %in% c("multi", "anchored", "onestage"))
  expect_true(all(is.finite(nc$quantiles)))
  expect_true(nc$median > 0)
})
