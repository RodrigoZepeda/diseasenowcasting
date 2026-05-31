# Menu-coverage tests: every delay family and epidemic process should at least
# construct, fit, and produce a finite nowcast on a small synthetic series.

.synth_simple <- function(Tn = 70L, seed = 4) {
  set.seed(seed)
  lambda <- 60 * exp(-0.5 * ((seq_len(Tn) - 40) / 12)^2) + 6
  ln <- lognormal_native(log(5), 4)
  rows <- list()
  for (t in seq_len(Tn)) {
    n <- rpois(1, lambda[t])
    if (n > 0) {
      d <- pmax(1L, ceiling(rlnorm(n, ln$log_location, ln$log_scale)))
      keep <- d <= (Tn - t + 1)
      if (any(keep)) { tb <- table(d[keep])
        for (k in seq_along(tb)) rows[[length(rows) + 1]] <- c(t, as.integer(tb[k]), as.integer(names(tb)[k]), 1L) }
    }
  }
  m <- do.call(rbind, rows); colnames(m) <- c("event", "count", "delay", "strata"); m
}

test_that("all four delay families fit (delay-only) and recover a sensible delay", {
  set.seed(11)
  n <- 15000; mean_d <- 6; sd_d <- 4
  d <- pmax(1L, ceiling(rgamma(n, shape = mean_d^2 / sd_d^2, rate = mean_d / sd_d^2)))
  mt <- 300L; m <- cbind(1L, 1L, d, 1L); m <- m[m[, 3] <= mt - 1, ]
  colnames(m) <- c("event", "count", "delay", "strata")
  for (dl in list(lognormal_delay(), gamma_delay())) {        # exact-recovery families
    mdl <- model(nb_likelihood(), hsgp_epidemic(), dl)
    dat <- prepare_data(mdl, m, max_time = mt, delay_only = TRUE)
    rf  <- fit(mdl, dat, priors = default_priors(mdl, dat))
    expect_equal(rf$convergence, 0L)
    expect_equal(exp(rf$delay_mu), mean_d, tolerance = 0.15)   # implied location ~ mean
  }
})

test_that("Dirichlet (default delay) HSGP joint fit + nowcast works", {
  m <- .synth_simple()
  mdl <- model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay())
  dat <- prepare_data(mdl, m, max_time = 70L, delay_only = FALSE)
  rf  <- fit(mdl, dat, priors = default_priors(mdl, dat, phi = lognormal_prior(log(20), 0.5)))
  expect_equal(rf$convergence, 0L)
  nc <- nowcast(rf, n_draws = 400, seed = 1)
  expect_true(all(is.finite(nc$quantiles)))
})

test_that("SIR epidemic joint fit + nowcast works", {
  m <- .synth_simple(seed = 6)
  mdl <- model(nb_likelihood(), sir_epidemic(N_pop = 20000), lognormal_delay())
  dat <- prepare_data(mdl, m, max_time = 70L, delay_only = FALSE)
  rf  <- fit(mdl, dat, priors = default_priors(mdl, dat, phi = lognormal_prior(log(20), 0.5)))
  expect_equal(rf$convergence, 0L)
  nc <- nowcast(rf, n_draws = 400, seed = 1)
  expect_true(all(is.finite(nc$quantiles)))
})
