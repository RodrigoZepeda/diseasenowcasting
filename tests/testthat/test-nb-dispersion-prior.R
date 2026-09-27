# The NB `phi` is the dispersion 1/size (Var = mu + phi * mu^2), in the prior,
# the objective and the predictive draw alike.  diseasenowcast2 (Stan) put its
# lognormal(log(20), 0.5) prior on the SIZE; the RTMB port kept the numbers but
# applied them to 1/size, centring the default on size 0.05.  These tests pin
# the dispersion scale of the default and its consequences.

# Complete (untruncated) NB line counts: event, count, delay, stratum.
.nb_synth <- function(Tn = 30L, mean_cases = 60, size = 20, seed = 11) {
  set.seed(seed)
  rows <- list()
  for (t in seq_len(Tn)) {
    n <- stats::rnbinom(1, size = size, mu = mean_cases)
    if (n > 0) {
      d  <- pmax(1L, stats::rpois(n, 2) + 1L)
      tb <- table(d)
      for (k in seq_along(tb))
        rows[[length(rows) + 1]] <- c(t, as.integer(tb[k]), as.integer(names(tb)[k]), 1L)
    }
  }
  m <- do.call(rbind, rows)
  colnames(m) <- c("event", "count", "delay", "strata")
  # Leave the last few event times' later delays unobserved (right truncation).
  m[m[, "event"] + m[, "delay"] - 1L <= Tn, , drop = FALSE]
}

test_that("the default phi prior is on the dispersion (1/size) scale", {
  ph <- nb_likelihood()@phi
  expect_equal(ph@name, "LogNormal")
  # Median dispersion exp(meanlog) must correspond to a moderate size, not the
  # size ~ 0.05 that lognormal(log(20), 0.5) on 1/size implied.
  median_size <- 1 / exp(ph@stan_params[1])
  expect_gte(median_size, 5)
  expect_lte(median_size, 50)
})

test_that("log_phi_nb starts at the median of a lognormal phi prior", {
  m   <- .nb_synth()
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  dat <- prepare_data(mdl, m, max_time = 30L, delay_only = FALSE)
  for (meanlog in c(log(0.05), log(3))) {
    pr  <- default_priors(mdl, dat, phi = lognormal_prior(meanlog, 0.5))
    obj <- diseasenowcasting:::build_joint_obj(dat, pr, use_random = FALSE)$obj
    expect_equal(unname(obj$par[names(obj$par) == "log_phi_nb"]), meanlog)
  }
  # A non-lognormal prior falls back to the default's median, phi = 0.1.
  pr  <- default_priors(mdl, dat, phi = exponential_prior(1))
  obj <- diseasenowcasting:::build_joint_obj(dat, pr, use_random = FALSE)$obj
  expect_equal(unname(obj$par[names(obj$par) == "log_phi_nb"]), log(0.1))
})

test_that("a short NB series is not forced into extreme overdispersion", {
  # 30 days of NB(size = 20) counts: the fitted size should stay in the right
  # order of magnitude.  Under the old default it was pulled to 1.7.
  m   <- .nb_synth(size = 20)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  dat <- prepare_data(mdl, m, max_time = 30L, delay_only = FALSE)
  rf  <- fit(mdl, dat, priors = default_priors(mdl, dat))
  expect_equal(rf$convergence, 0L)
  expect_gt(1 / rf$phi_nb, 5)
})

test_that("a larger phi prior means more overdispersion and wider intervals", {
  m   <- .nb_synth(size = 20)
  mdl_for <- function(phi) model(nb_likelihood(phi = phi), hsgp_epidemic(), lognormal_delay())
  width <- function(phi) {
    mdl <- mdl_for(phi)
    dat <- prepare_data(mdl, m, max_time = 30L, delay_only = FALSE)
    rf  <- fit(mdl, dat, priors = default_priors(mdl, dat))
    q   <- diseasenowcasting:::.nowcast_draws(rf, n_draws = 2000, seed = 1)$quantiles
    list(phi = rf$phi_nb, width = q[length(q)] - q[1])
  }
  tight <- width(lognormal_prior(log(0.01), 0.1))   # size ~ 100
  loose <- width(lognormal_prior(log(1), 0.1))      # size ~ 1
  expect_lt(tight$phi, loose$phi)
  expect_lt(tight$width, loose$width)
})
