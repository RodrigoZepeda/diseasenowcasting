# Tests for nowcast(prior_only = TRUE) and ess().

suppressMessages(library(tbl.now))

.grid_tn <- function(Tn = 60L, start = as.Date("2020-01-01")) {
  d <- data.frame(onset = start + 0:(Tn - 1L), reported = start + 0:(Tn - 1L))
  tbl_now(d, event_date = onset, report_date = reported,
          data_type = "linelist", verbose = FALSE)
}

# ── prior_only: basic mechanics ──────────────────────────────────────────────

test_that("nowcast(prior_only = TRUE) returns a usable prior-predictive nowcast", {
  tn  <- .grid_tn()
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  nc  <- nowcast(tn, mdl, prior_only = TRUE, n_draws = 100, seed = 1)
  expect_equal(nc@type, "prior_only")
  # predict / summary / quantile / autoplot all work
  s <- summary(predict(nc, seed = 2))
  expect_true(all(is.finite(s$median)))
  q <- quantile(nc, probs = c(0.05, 0.5, 0.95), seed = 2)
  expect_equal(nrow(q), nc@target)
  expect_s3_class(autoplot(nc, seed = 2), "ggplot")
})

test_that("prior_only works for AR1 and SIR epidemics", {
  tn <- .grid_tn()
  for (ep in list(ar1_epidemic(), sir_epidemic(N_pop = 5000))) {
    nc <- nowcast(tn, model(nb_likelihood(), ep, lognormal_delay()),
                  prior_only = TRUE, n_draws = 60, seed = 1)
    expect_true(all(is.finite(quantile(nc, probs = 0.5, seed = 2))))
  }
})

test_that("prior_only is sensitive to the prior: higher SIR R0 -> larger epidemic", {
  tn <- .grid_tn()
  sir <- function(r0) model(nb_likelihood(),
    sir_epidemic(R0 = lognormal_prior(log(r0), 0.05), gamma = lognormal_prior(log(0.1), 0.1),
                 N_pop = 5000), lognormal_delay())
  peak <- function(r0) max(quantile(nowcast(tn, sir(r0), prior_only = TRUE, n_draws = 150, seed = 1),
                                    probs = 0.5, seed = 2), na.rm = TRUE)
  expect_gt(peak(4.0), peak(1.5))
})

test_that("ess() errors on a prior_only nowcast", {
  tn <- .grid_tn()
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                prior_only = TRUE, n_draws = 50, seed = 1)
  expect_error(ess(nc), regexp = "prior_only")
})

# ── ess() on a real fit ──────────────────────────────────────────────────────

test_that("ess() returns a finite ESS with the expected attributes", {
  set.seed(1)
  df <- data.frame(onset = as.Date("2020-01-01") + sample(0:40, 160, replace = TRUE))
  df$reported <- df$onset + rpois(nrow(df), 2)
  tn <- tbl_now(df, event_date = onset, report_date = reported,
                data_type = "linelist", verbose = FALSE)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 200, temporal_effects = "none", seed = 1)
  e <- suppressWarnings(ess(nc, n_draws = 400, seed = 1))
  expect_true(is.finite(as.numeric(e)))
  expect_true(as.numeric(e) > 1)                 # not degenerate (the high-D failure mode)
  expect_true(!is.null(attr(e, "ratio")))
  expect_true(!is.null(attr(e, "n_hyper")))
  expect_lt(attr(e, "n_hyper"), 50L)             # reweights only the hyperparameters
})

test_that("ess() warns when the effective sample size is low", {
  # 4 data points: barely-identified, the Laplace approximation should be poor.
  df <- data.frame(onset = as.Date("2020-01-01") + c(0, 1, 2, 3),
                   reported = as.Date("2020-01-01") + c(1, 3, 4, 8))
  tn <- tbl_now(df, event_date = onset, report_date = reported,
                data_type = "linelist", verbose = FALSE)
  nc <- suppressWarnings(nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                                 type = "one_stage", n_draws = 80, temporal_effects = "none", seed = 1))
  # With threshold = 1 (i.e. require ESS == n_draws) it must warn.
  expect_warning(ess(nc, n_draws = 300, threshold = 1, seed = 1),
                 regexp = "Low effective sample size")
})
