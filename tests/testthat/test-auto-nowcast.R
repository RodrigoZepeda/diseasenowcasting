# Tests for type = "auto" (per-delay stage choice) and auto_nowcast() (model
# selection by backtesting).  The auto_nowcast tests fit a whole grid of models
# over several dates, so they are slow -- skipped on CRAN.

test_that("type = 'auto' resolves per delay family", {
  tn <- .make_synth_tblnow(Tn = 40L, seed = 3)

  # Dirichlet under "auto" -> one-stage
  nc_dir <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), dirichlet_delay()),
                    type = "auto", temporal_effects = "none", n_draws = 150)
  expect_equal(nc_dir@rung, "onestage")
  expect_equal(nc_dir@type, "auto")

  # Parametric delay under "auto" -> two-stage (multi rung; may fall back if a
  # stage fails to converge, but never the dirichlet-only one-stage path)
  nc_par <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                    type = "auto", temporal_effects = "none", n_draws = 150)
  expect_true(nc_par@rung %in% c("multi", "anchored", "onestage"))
})

test_that("auto_nowcast selects a model and returns a nowcast with a scoreboard", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 45L, seed = 5)

  nc <- auto_nowcast(tn, n_dates = 2L, n_draws_select = 120L, n_draws = 200L,
                     temporal_effects = "none", verbose = FALSE)

  expect_true(S7::S7_inherits(nc, diseasenowcasting:::nowcast_class))
  expect_type(nc@comparison, "list")
  expect_true(all(c("scores", "chosen", "metric", "max_time") %in% names(nc@comparison)))
  expect_gt(nrow(nc@comparison$scores), 1L)        # a real grid was compared
  expect_true(nc@comparison$chosen %in% nc@comparison$scores$model)
  # predict() works on the returned object (it is a normal nowcast)
  expect_s3_class(tryCatch(predict(nc, summary = TRUE), error = function(e) e), "data.frame")
})

test_that("auto_nowcast force-includes an explicitly supplied epidemic process", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 60L, seed = 7)   # long -> default {SIR, AR1, HSGP}

  # Pass a SIR carrying a custom R0 prior; that prior must flow into the compared
  # grid (the SIR candidate is fit with it, not the plain constructor).
  nc <- auto_nowcast(tn, n_dates = 2L, n_draws_select = 120L, n_draws = 200L,
                     sir = sir_epidemic(R0 = lognormal_prior(log(2), 0.3)),
                     delays = list(lognormal_delay()), temporal_effects = "none",
                     verbose = FALSE)
  expect_true(any(grepl("^SIR/", nc@comparison$scores$model)))
})

test_that("auto_nowcast compares likelihoods, custom models, and the ape metric", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 45L, seed = 9)
  custom_mod <- model(nb_likelihood(), sir_epidemic(), lognormal_delay())

  nc <- auto_nowcast(tn, metric = "ape",
                     likelihood = list(nb_likelihood(), poisson_likelihood()),
                     delays = list(lognormal_delay()), models = custom_mod,
                     n_dates = 2L, n_draws_select = 120L, n_draws = 200L,
                     temporal_effects = "none", verbose = FALSE)

  expect_equal(nc@comparison$metric, "ape")
  expect_true(any(grepl("/poisson/", nc@comparison$scores$model)))   # poisson compared
  expect_true(any(grepl("/nb/",      nc@comparison$scores$model)))   # nb compared
  expect_true(nc@comparison$chosen %in% nc@comparison$scores$model)
})

test_that("auto_nowcast accessors expose the comparison and the winning model", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 45L, seed = 11)

  nc <- auto_nowcast(tn, metric = "wis", n_dates = 2L, n_draws_select = 120L,
                     n_draws = 200L, temporal_effects = "none", verbose = FALSE)

  # best_model_name() / selection_metric() mirror the comparison slot
  expect_identical(best_model_name(nc), nc@comparison$chosen)
  expect_identical(selection_metric(nc), "wis")

  # comparison_scores() is the full ranked board; best_score() the winner's row
  expect_identical(comparison_scores(nc), nc@comparison$scores)
  bs <- best_score(nc)
  expect_s3_class(bs, "data.frame")
  expect_equal(nrow(bs), 1L)
  expect_identical(bs$model, best_model_name(nc))

  # best_model() returns the winning model() object, reusable in nowcast()
  m <- best_model(nc)
  expect_true(S7::S7_inherits(m, diseasenowcasting:::model_class))
  expect_identical(diseasenowcasting:::.model_label(m), best_model_name(nc))

  # the auto accessors refuse a plain nowcast (no comparison slot)
  plain <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                   type = "one_stage", temporal_effects = "none", n_draws = 80)
  expect_null(plain@comparison)
  expect_error(best_model_name(plain), "auto_nowcast")
  expect_error(comparison_scores(plain), "auto_nowcast")
  expect_error(best_score(plain), "auto_nowcast")
  # best_model() still works on a plain nowcast (returns its model)
  expect_true(S7::S7_inherits(best_model(plain), diseasenowcasting:::model_class))
})

test_that("auto_nowcast coverage metrics pick the best-calibrated model", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 50L, seed = 13)

  # The miss-from-nominal each coverage metric minimises (see auto_nowcast()).
  miss <- list(
    coverage_50 = function(s) abs(s$coverage_50 - 0.50),
    coverage_90 = function(s) abs(s$coverage_90 - 0.90),
    coverage    = function(s) abs(s$coverage_50 - 0.50) + abs(s$coverage_90 - 0.90))

  for (m in names(miss)) {
    nc <- auto_nowcast(tn, metric = m, delays = list(lognormal_delay(), dirichlet_delay()),
                       n_dates = 2L, n_draws_select = 100L, n_draws = 150L,
                       temporal_effects = "none", verbose = FALSE)

    expect_identical(selection_metric(nc), m)              # metric recorded verbatim
    sb <- comparison_scores(nc)
    chosen_miss <- miss[[m]](sb)[sb$model == best_model_name(nc)]
    # the chosen model is the argmin of that metric's miss (ties allowed)
    expect_equal(chosen_miss, min(miss[[m]](sb), na.rm = TRUE))
  }
})

test_that("auto_nowcast rejects an unknown metric", {
  tn <- .make_synth_tblnow(Tn = 40L, seed = 15)
  expect_error(auto_nowcast(tn, metric = "coverage_95", verbose = FALSE),
               "should be one of")
})

test_that("auto_nowcast falls back to the next-best model when the winner fails to refit", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 50L, seed = 23)

  # Mock nowcast() so the FIRST full-data refit (the top-ranked candidate) throws.
  # backtest() always passes a concrete `now`; only the final refit leaves it
  # NULL, so we can target the refit and let every backtest fit run normally.
  real_nowcast <- get("nowcast", asNamespace("diseasenowcasting"))
  refit_calls  <- 0L
  fail_first_refit <- function(data, model, ..., now = NULL) {
    if (is.null(now)) {
      refit_calls <<- refit_calls + 1L
      if (refit_calls == 1L) stop("simulated refit failure")
    }
    real_nowcast(data, model, ..., now = now)
  }

  nc <- testthat::with_mocked_bindings(
    auto_nowcast(tn, delays = list(lognormal_delay()),
                 n_dates = 2L, n_draws_select = 100L, n_draws = 150L,
                 temporal_effects = "none", verbose = FALSE),
    nowcast = fail_first_refit,
    .package = "diseasenowcasting"
  )

  # The top pick failed but auto_nowcast still returned a usable nowcast from the
  # next-best candidate (the refit was retried at least once).
  expect_gte(refit_calls, 2L)
  expect_true(S7::S7_inherits(nc, diseasenowcasting:::nowcast_class))
  expect_true(nc@comparison$chosen %in% nc@comparison$scores$model)
  expect_s3_class(tryCatch(predict(nc, summary = TRUE), error = function(e) e),
                  "data.frame")
})

test_that("printing an auto_nowcast result shows the scoreboard", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 45L, seed = 17)
  nc <- auto_nowcast(tn, n_dates = 2L, n_draws_select = 100L, n_draws = 150L,
                     temporal_effects = "none", verbose = FALSE)

  out <- cli::cli_fmt(print(nc))
  expect_true(any(grepl("auto_nowcast", out)))
  expect_true(any(grepl("Selected", out)))
  expect_true(any(grepl(best_model_name(nc), out, fixed = TRUE)))

  # a plain nowcast prints without the auto_nowcast block
  plain <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                   type = "one_stage", temporal_effects = "none", n_draws = 80)
  out_plain <- cli::cli_fmt(print(plain))
  expect_false(any(grepl("auto_nowcast", out_plain)))
})
