# Classical time-series epidemic processes: ARIMA, STS, ETS, random walk /
# naive, Theta.  The recursions themselves are checked against hand-written
# references, because a trend builder that is merely "plausible" is exactly the
# kind of thing that fits fine and forecasts wrong.

test_that("the PACF map reproduces Levinson-Durbin and stays stationary", {
  # AR(1): the partial autocorrelation IS the coefficient.
  expect_equal(.pacf_to_coefficients(0.7), 0.7)

  # AR(2): phi_1 = r_1 (1 - r_2), phi_2 = r_2.
  expect_equal(.pacf_to_coefficients(c(0.6, -0.3)),
               c(0.6 * (1 - -0.3), -0.3))

  # Whatever partial autocorrelations we throw at it, the companion matrix of
  # the resulting AR polynomial must have all roots inside the unit circle.
  set.seed(11)
  for (order in 1:5) {
    pacf <- stats::runif(order, -0.98, 0.98)
    coefficients <- .pacf_to_coefficients(pacf)
    companion <- matrix(0, order, order)
    companion[1, ] <- coefficients
    if (order > 1L) companion[cbind(2:order, seq_len(order - 1L))] <- 1
    expect_lt(max(Mod(eigen(companion, only.values = TRUE)$values)), 1)
  }
})

test_that("arima_trend() matches a direct recursion, and its special cases", {
  innovations <- c(0.4, -1.2, 0.3, 0.9, -0.5, 0.1)
  sigma <- 0.25

  # d = 0, p = 1, q = 0 is an AR(1) with a conditional (zero pre-sample) start.
  reference <- numeric(length(innovations))
  reference[1] <- sigma * innovations[1]
  for (t in 2:length(innovations))
    reference[t] <- 0.6 * reference[t - 1] + sigma * innovations[t]
  expect_equal(arima_trend(innovations, 0.6, numeric(0), sigma, 0, 0L), reference)

  # d = 1, p = q = 0 with drift is a random walk with drift.
  expect_equal(arima_trend(innovations, numeric(0), numeric(0), sigma, 0.05, 1L),
               cumsum(0.05 + sigma * innovations))

  # d = 2 differences twice.
  expect_equal(arima_trend(innovations, numeric(0), numeric(0), sigma, 0, 2L),
               cumsum(cumsum(sigma * innovations)))

  # MA terms enter as lagged errors.
  errors <- sigma * innovations
  ma_reference <- errors + c(0, 0.4 * errors[-length(errors)])
  expect_equal(arima_trend(innovations, numeric(0), 0.4, sigma, 0, 0L), ma_reference)
})

test_that("sts_trend() collapses to a random walk without a slope", {
  innovations <- c(0.2, 0.7, -0.4, 1.1)
  expect_equal(
    sts_trend(innovations, numeric(0), 0.3, 0, 1, 0, 0),
    cumsum(0.3 * innovations))

  # A local linear trend with zero slope innovations is a walk plus a line.
  walk <- sts_trend(innovations, rep(0, 4), 0.3, 0, 1, 0, 0.1)
  expect_equal(walk, cumsum(0.3 * innovations) + c(0, 0.1, 0.2, 0.3))
})

test_that("sts_trend() reverts the slope towards its mean when phi < 1", {
  # With no innovations at all, the slope decays geometrically from slope_init
  # towards slope_mean, so the level is the cumulative sum of that decay.
  n_time <- 6L
  zeros <- rep(0, n_time)
  level <- sts_trend(zeros, zeros, 0, 0, 0.5, 0, 0.8)
  slopes <- 0.8 * 0.5^(seq_len(n_time - 1L) - 1L)
  expect_equal(level, c(0, cumsum(slopes)))
})

test_that("ets_trend() is a random walk without a slope and damps with one", {
  innovations <- c(0.5, -0.2, 0.9, 0.1)
  # Level only: trend_t is the PRE-innovation level, so it lags by one step.
  expect_equal(ets_trend(innovations, 0.2, 0, 1, 0, 0, FALSE),
               c(0, cumsum(0.2 * innovations)[-length(innovations)]))

  # Drift with no innovations is a straight line through the pre-innovation level.
  # trend_t is the level BEFORE innovation t, so one drift step has already
  # been taken by t = 1; the starting level itself belongs to `mu_intercept`.
  expect_equal(ets_trend(rep(0, 4), 0.2, 0, 1, 0.05, 0, FALSE),
               c(0.05, 0.10, 0.15, 0.20))

  # A damped slope with no innovations decays as phi^t.
  damped <- ets_trend(rep(0, 4), 0.2, 0.3, 0.9, 0, 1, TRUE)
  slope_contributions <- 0.9 * 0.9^(seq_len(4) - 1L)
  expect_equal(damped, cumsum(c(0, slope_contributions[-4])) + slope_contributions)
})

test_that("the constructors validate their arguments", {
  expect_s3_class(arima_epidemic(), "diseasenowcasting::arima_epidemic_class")
  expect_identical(arima_epidemic(d = 1)@include_drift, TRUE)
  expect_identical(arima_epidemic(d = 0, q = 1)@include_drift, FALSE)
  expect_error(arima_epidemic(d = 0, include_drift = TRUE), "not identified")
  expect_error(arima_epidemic(d = 3), "above 2")
  expect_error(arima_epidemic(p = 6), "capped at 5")
  # A number holds the parameter at that value (see test-fixed-parameters.R);
  # only a value outside the parameter's domain is refused.
  expect_no_error(arima_epidemic(sigma = 0.1))
  expect_no_error(ets_epidemic(sigma = 0.1))
  expect_no_error(sts_epidemic(level_sigma = 0.1))
  expect_error(arima_epidemic(sigma = -1), "positive line")
  expect_error(ets_epidemic(beta = 1.5), "open interval \\(0, 1\\)")
  expect_error(sts_epidemic(trend = "quadratic"), "semilocal")
  expect_error(ets_epidemic(trend = "multiplicative"), "none")

  # The baselines are the same engine with parts switched off.
  expect_identical(naive_epidemic()@name, "Naive")
  expect_identical(theta_epidemic()@include_drift, TRUE)
  expect_identical(random_walk_epidemic()@include_drift, FALSE)
  expect_identical(theta_epidemic()@num_id, 6L)
})

test_that("each new process fits, and predict() agrees with the fitted trend", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 70L, seed = 3)
  processes <- list(ARIMA = arima_epidemic(), STS = sts_epidemic(),
                    ETS = ets_epidemic(), RW = random_walk_epidemic(),
                    Naive = naive_epidemic(), Theta = theta_epidemic())
  for (label in names(processes)) {
    nc <- suppressMessages(nowcast(
      tn, model(nb_likelihood(), processes[[label]], lognormal_delay()),
      type = "one_stage", n_draws = 200, seed = 5, temporal_effects = "none"))
    fit <- nc@fits[[1]]
    expect_true(all(is.finite(fit$lambda)), label = label)
    expect_true(all(fit$lambda > 0), label = label)

    # The plain-R reconstruction is what predict() runs on; it must reproduce
    # the tape's own lambda, or every posterior draw is built from a different
    # model than the one that was fitted.
    reconstructed <- .joint_reconstruct(fit$data, fit$priors, fit$parList,
                                        fit$Bmat, fit$freq)
    expect_equal(reconstructed$lambda, fit$lambda, tolerance = 1e-8)

    summary_frame <- summary(predict(nc, n_draws = 200, seed = 5))
    expect_true(all(is.finite(summary_frame$median)), label = label)
    expect_true(all(summary_frame$q97.5 >= summary_frame$q2.5), label = label)
  }
})

test_that("the new processes work stratified, with covariates, and under two_stage", {
  skip_on_cran()
  tn <- .make_strata_tblnow(Tn = 60L, seed = 2)
  nc <- suppressMessages(nowcast(
    tn, model(nb_likelihood(), sts_epidemic(), lognormal_delay()),
    type = "one_stage", n_draws = 150, seed = 4, temporal_effects = "none"))
  # One level/slope block per stratum, not one shared block.
  expect_length(nc@fits[[1]]$parList$log_sts_level_sigma_unc, 2L)
  expect_equal(dim(nc@fits[[1]]$parList$sts_level_innov)[2], 2L)

  # Day-of-week dummies are reference-coded, so P is 6 and not 7, and the
  # trend processes pick them up through the same `gamma` block HSGP uses.
  tn_effects <- tn |>
    tbl.now::add_temporal_effects(tbl.now::temporal_effects(day_of_week = TRUE)) |>
    tbl.now::compute_temporal_effects()
  nc_effects <- suppressMessages(nowcast(
    tn_effects, model(nb_likelihood(), arima_epidemic(), lognormal_delay()),
    type = "one_stage", n_draws = 150, seed = 4))
  expect_equal(nc_effects@engine$P, 6L)
  expect_equal(dim(nc_effects@fits[[1]]$parList$gamma), c(6L, 2L))

  nc_two_stage <- suppressMessages(nowcast(
    .make_synth_tblnow(Tn = 60L, seed = 8),
    model(nb_likelihood(), ets_epidemic(), lognormal_delay()),
    type = "two_stage", K = 3, n_draws = 150, seed = 4, temporal_effects = "none"))
  expect_true(all(is.finite(as.numeric(median(nc_two_stage, seed = 4)))))
})

test_that("parameters() labels the new trend parameters by process", {
  skip_on_cran()
  nc <- suppressMessages(nowcast(
    .make_synth_tblnow(Tn = 55L, seed = 6),
    model(nb_likelihood(), arima_epidemic(p = 1, d = 1, q = 1), lognormal_delay()),
    type = "one_stage", n_draws = 100, seed = 2, temporal_effects = "none"))
  estimates <- parameters(nc)
  expect_true("epidemic_arima" %in% estimates$type)
  expect_true(any(grepl("^arima_drift", estimates$term)))
})

test_that("prior_only draws a finite prior-predictive band for each process", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 45L, seed = 9)
  processes <- list(arima_epidemic(), sts_epidemic(), ets_epidemic(),
                    random_walk_epidemic(), theta_epidemic())
  for (process in processes) {
    nc <- suppressMessages(nowcast(
      tn, model(nb_likelihood(), process, lognormal_delay()),
      prior_only = TRUE, n_draws = 100, seed = 3, temporal_effects = "none"))
    band <- quantile(nc, probs = c(0.05, 0.5, 0.95), seed = 3)
    expect_true(all(is.finite(band)), label = process@name)
  }
})

test_that("a saved time-series fit reloads and still predicts", {
  skip_on_cran()
  path <- withr::local_tempfile(fileext = ".rds")
  nc <- suppressMessages(nowcast(
    .make_synth_tblnow(Tn = 50L, seed = 4),
    model(nb_likelihood(), sts_epidemic(), lognormal_delay()),
    type = "one_stage", n_draws = 100, seed = 1, temporal_effects = "none"))
  save_nowcast(nc, path)
  reloaded <- load_nowcast(path)
  expect_equal(as.numeric(median(reloaded, seed = 1)),
               as.numeric(median(nc, seed = 1)), tolerance = 1e-6)
})
