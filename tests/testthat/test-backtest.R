# backtest() across dates + models, scoring (WIS/APE/MSE), and autoplot.

test_that("backtest runs across dates and produces a results table", {
  tn  <- .make_synth_tblnow(Tn = 100L, seed = 3)
  start <- min(tn$onset)
  dates <- start + c(50, 75) - 1
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  bt  <- backtest(tn, mdl, dates = dates, type = "one_stage", n_draws = 300, seed = 1)

  expect_true(S7::S7_inherits(bt, diseasenowcasting:::backtest_class))
  expect_true(nrow(bt@results) > 0)
  expect_true(all(c("median", "q2.5", "q97.5", "final", "model", "date_run", ".event_num") %in% names(bt@results)))
  expect_true(is.data.frame(predict(bt)))
})

test_that("score() reports WIS/APE/MSE and ranks several models", {
  tn  <- .make_synth_tblnow(Tn = 100L, seed = 4)
  start <- min(tn$onset)
  dates <- start + c(55, 75, 95) - 1
  models <- list(model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 model(nb_likelihood(), ar1_epidemic(),  lognormal_delay()))
  bt <- backtest(tn, models, dates = dates, type = "two_stage", K = 5, n_draws = 300, seed = 1)

  sc <- score(bt, metric = "wis", report = FALSE)
  expect_equal(nrow(sc), 2L)
  expect_true(all(c("model", "wis", "ape", "mse", "coverage_50", "coverage_90") %in% names(sc)))
  expect_true(all(is.finite(sc$wis)))
  expect_true(sc$wis[1] <= sc$wis[2])                 # sorted best-first by wis

  # ranking by a different metric reorders if appropriate
  sc_mse <- score(bt, metric = "mse", report = FALSE)
  expect_equal(sc_mse$mse, sort(sc_mse$mse))
})

test_that("autoplot(backtest) returns a ggplot", {
  tn  <- .make_synth_tblnow(Tn = 90L, seed = 6)
  start <- min(tn$onset)
  bt <- backtest(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 dates = start + c(55, 80) - 1, type = "one_stage", n_draws = 300, seed = 1)
  p <- autoplot(bt)
  expect_s3_class(p, "ggplot")
})
