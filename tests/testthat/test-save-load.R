# Tests for save_nowcast() / load_nowcast(): a fitted nowcast survives a
# saveRDS round-trip (the RTMB tape is dropped; the Laplace mode + precision are
# stored), so the loaded object predicts identically and can be re-fit.

.roundtrip <- function(nc) {
  f <- tempfile(fileext = ".rds")
  expect_invisible(save_nowcast(nc, f))
  expect_true(file.exists(f))
  load_nowcast(f)
}

test_that("a one-stage nowcast round-trips: predict/coef/tidy match", {
  tn <- .make_synth_tblnow(Tn = 50L, seed = 100)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 200, seed = 1)
  nc2 <- suppressMessages(.roundtrip(nc))

  expect_true(S7::S7_inherits(nc2, diseasenowcasting:::nowcast_class))
  # same seed -> identical draws (same stored mode + precision + reconstruct)
  expect_equal(predict(nc,  summary = TRUE, seed = 7)$median,
               predict(nc2, summary = TRUE, seed = 7)$median)
  expect_equal(unname(coef(nc)), unname(coef(nc2)))
  td <- tidy(nc2)
  expect_s3_class(td, "data.frame")
  ok <- is.finite(td$std.error)
  expect_true(all(is.finite(td$estimate[ok])))
  expect_s3_class(autoplot(nc2, seed = 2), "ggplot")
})

test_that("loaded nowcast supports a different n_draws (draws are not frozen)", {
  tn <- .make_synth_tblnow(Tn = 45L, seed = 103)
  nc <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 150, seed = 1)
  nc2 <- suppressMessages(.roundtrip(nc))
  p <- predict(nc2, n_draws = 40, summary = TRUE, seed = 3)
  expect_equal(nrow(p), nc2@target)
  expect_true(all(is.finite(p$median)))
})

test_that("two-stage (multiple fits) and Dirichlet round-trip", {
  tn <- .make_synth_tblnow(Tn = 55L, seed = 101)
  nc_ts <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                   type = "two_stage", K = 3, n_draws = 150, seed = 1)
  nc_ts2 <- suppressMessages(.roundtrip(nc_ts))
  expect_length(nc_ts2@fits, length(nc_ts@fits))
  expect_equal(predict(nc_ts, summary = TRUE, seed = 7)$median,
               predict(nc_ts2, summary = TRUE, seed = 7)$median)

  nc_d <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay()),
                  type = "one_stage", n_draws = 120, seed = 1)
  nc_d2 <- suppressMessages(.roundtrip(nc_d))
  expect_equal(predict(nc_d, summary = TRUE, seed = 7)$median,
               predict(nc_d2, summary = TRUE, seed = 7)$median)
})

test_that("prior_only nowcast round-trips", {
  tn <- .make_synth_tblnow(Tn = 45L, seed = 104)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                prior_only = TRUE, n_draws = 120, seed = 1)
  nc2 <- suppressMessages(.roundtrip(nc))
  expect_equal(nc2@type, "prior_only")
  expect_equal(predict(nc, summary = TRUE, seed = 7)$median,
               predict(nc2, summary = TRUE, seed = 7)$median)
})

test_that("auto_nowcast round-trips with its comparison scoreboard", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 48L, seed = 105)
  ac <- auto_nowcast(tn, n_dates = 2L, n_draws_select = 80L, n_draws = 120L,
                     temporal_effects = "none", verbose = FALSE)
  ac2 <- suppressMessages(.roundtrip(ac))
  expect_false(is.null(ac2@comparison))
  expect_identical(best_model_name(ac2), best_model_name(ac))
  expect_equal(nrow(comparison_scores(ac2)), nrow(comparison_scores(ac)))
  expect_true(all(is.finite(predict(ac2, summary = TRUE, seed = 2)$median)))
})

test_that("a loaded model can be re-fit, and rebuild = TRUE restores a live tape", {
  tn <- .make_synth_tblnow(Tn = 50L, seed = 106)
  nc <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                type = "two_stage", K = 2, n_draws = 120, seed = 1)
  f <- tempfile(fileext = ".rds")
  suppressMessages(save_nowcast(nc, f))

  # re-fit straight from the loaded model + bundled tbl_now
  nc2 <- load_nowcast(f)
  refit <- nowcast(nc2@data, nc2@model, type = "one_stage", n_draws = 80, seed = 1)
  expect_true(S7::S7_inherits(refit, diseasenowcasting:::nowcast_class))

  # rebuild = TRUE re-tapes the objective (no re-optimization)
  ncr <- load_nowcast(f, rebuild = TRUE)
  expect_false(is.null(ncr@fits[[1]]$obj))
  expect_true(all(is.finite(predict(ncr, summary = TRUE, seed = 2)$median)))
})

test_that("save_nowcast / load_nowcast validate their inputs", {
  expect_error(save_nowcast(42, tempfile()), "nowcast")
  expect_error(load_nowcast(tempfile(fileext = ".rds")), "existing")
  bad <- tempfile(fileext = ".rds"); saveRDS(list(a = 1), bad)
  expect_error(load_nowcast(bad), "save_nowcast")
})
