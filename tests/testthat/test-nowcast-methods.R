# Tests for nowcast S7 methods (R/21_nowcast_methods.R) and collect fits

test_that("coef() returns all expected parameter names", {
  tn  <- .make_synth_tblnow(Tn = 70L, seed = 10)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  nc  <- nowcast(tn, mdl, type = "one_stage", n_draws = 200, seed = 1)
  cf  <- coef(nc)
  expect_true(all(c("delay_mu", "delay_sigma") %in% names(cf)))
  expect_true(all(is.finite(cf)))
})

test_that("coef() averages delay across two-stage imputations", {
  tn  <- .make_synth_tblnow(Tn = 70L, seed = 11)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  nc  <- nowcast(tn, mdl, type = "two_stage", K = 3, n_draws = 200, seed = 1)
  cf  <- coef(nc)
  expect_true(is.finite(cf["delay_mu"]))
  expect_true(is.finite(cf["delay_sigma"]))
})

test_that("mean() and median() return vectors of length max_time", {
  tn  <- .make_synth_tblnow(Tn = 60L, seed = 12)
  mdl <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
  nc  <- nowcast(tn, mdl, type = "one_stage", n_draws = 200, seed = 1)
  m   <- mean(nc, seed = 2)
  med <- median(nc, seed = 2)
  expect_length(m,   nc@target)
  expect_length(med, nc@target)
  expect_true(all(is.finite(m)))
  expect_true(all(m > 0))
})

test_that("quantile() returns matrix [max_time × probs]", {
  tn  <- .make_synth_tblnow(Tn = 60L, seed = 13)
  nc  <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 type = "one_stage", n_draws = 200, seed = 1)
  q   <- quantile(nc, probs = c(0.1, 0.5, 0.9), seed = 2)
  expect_equal(nrow(q), nc@target)
  expect_equal(ncol(q), 3L)
  # q50 >= q10, q90 >= q50 (row-wise)
  expect_true(all(q[, 2] >= q[, 1] - 1e-6))
  expect_true(all(q[, 3] >= q[, 2] - 1e-6))
})

test_that("summary(nowcast) prints without error and returns invisibly", {
  tn  <- .make_synth_tblnow(Tn = 50L, seed = 14)
  nc  <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 type = "one_stage", n_draws = 200, seed = 1)
  out <- expect_no_error(summary(nc, seed = 2))
  expect_true(is.list(out))
  expect_named(out, c("coef", "latent", "type", "rung"))
})

test_that("predict(summary=TRUE) returns the summarise_nowcast_matrix table", {
  tn  <- .make_synth_tblnow(Tn = 60L, seed = 15)
  nc  <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 type = "one_stage", n_draws = 200, seed = 1)
  s   <- predict(nc, summary = TRUE, seed = 2)
  expect_true(is.data.frame(s))
  expect_true("q50" %in% names(s))
  expect_true(all(is.finite(s$median)))
})

test_that("print(nowcast) and print(nowcast_prediction) run without error", {
  tn  <- .make_synth_tblnow(Tn = 50L, seed = 16)
  nc  <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 type = "one_stage", n_draws = 200, seed = 1)
  expect_no_error(print(nc))
  pr  <- predict(nc, seed = 2)
  expect_no_error(print(pr))
})

test_that("poisson_likelihood() nowcast converges and returns finite results", {
  tn  <- .make_synth_tblnow(Tn = 60L, seed = 17)
  nc  <- nowcast(tn, model(poisson_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 type = "one_stage", n_draws = 200, seed = 1)
  s   <- summary(predict(nc, seed = 2))
  expect_true(all(is.finite(s$median)))
})

test_that("one_stage with AR1+Gamma delay converges and is finite", {
  tn  <- .make_synth_tblnow(Tn = 60L, seed = 18)
  nc  <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), gamma_delay()),
                 type = "one_stage", n_draws = 200, seed = 1)
  expect_true(all(is.finite(summary(predict(nc, seed = 2))$median)))
})

test_that("summarise_nowcast_matrix handles all-NA column gracefully", {
  M <- matrix(c(1:10, rep(NA, 10)), nrow = 10, ncol = 2)
  s <- summarise_nowcast_matrix(M)
  expect_equal(nrow(s), 2L)
  expect_true(all(is.na(s[2, c("mean","median")])))
})

test_that("collect_fits one_stage returns single fit in list", {
  tn   <- .make_synth_tblnow(Tn = 50L, seed = 19)
  mdl  <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  prep <- diseasenowcasting:::prepare_from_tbl_now(tn, mdl)
  eng  <- prep$data
  pri  <- default_priors(mdl, eng)
  col  <- diseasenowcasting:::.collect_nowcast_fits(mdl, eng, pri, type = "one_stage")
  expect_length(col$fits, 1L)
  expect_equal(col$rung, "onestage")
})
