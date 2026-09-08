# Tests for nowcast(prior_only = TRUE).

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

test_that("prior_only SIR respects use_beta_rw_trend = FALSE", {
  tn <- .grid_tn()
  specification <- model(
    nb_likelihood(), sir_epidemic(N_pop = 5000, use_beta_rw_trend = FALSE),
    lognormal_delay()
  )
  nc <- nowcast(tn, specification, prior_only = TRUE, n_draws = 40, seed = 1)
  sampled <- diseasenowcasting:::.sample_prior_parlist(
    nc@engine, nc@priors, num_basis = 0L, n_strata = 1L
  )
  expect_false(any(grepl("^ar_", names(sampled))))
  expect_true(all(is.finite(quantile(nc, probs = 0.5, seed = 2))))
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

# ── prior_only: delay families and the Poisson likelihood ────────────────────

test_that("prior_only samples Dirichlet and GeneralizedGamma delays", {
  tn <- .grid_tn()
  # Dirichlet (non-parametric) -> delay_family == 4 simplex-sampling path
  nc_dir <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), dirichlet_delay()),
                    prior_only = TRUE, n_draws = 60, seed = 1)
  expect_true(all(is.finite(quantile(nc_dir, probs = 0.5, seed = 2))))
  # Generalized Gamma -> delay_family == 3, exercising the delay_Q logit branch
  nc_gg <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), generalized_gamma_delay()),
                   prior_only = TRUE, n_draws = 60, seed = 1)
  expect_true(all(is.finite(quantile(nc_gg, probs = 0.5, seed = 2))))
})

test_that("prior_only works with a Poisson likelihood (no overdispersion draw)", {
  tn <- .grid_tn()
  # Poisson -> is_negative_binomial == 0, so log_phi_nb is NOT drawn and the
  # Poisson RNG path is used.
  nc_pois <- nowcast(tn, model(poisson_likelihood(), ar1_epidemic(), lognormal_delay()),
                     prior_only = TRUE, n_draws = 60, seed = 1)
  expect_equal(nc_pois@type, "prior_only")
  expect_true(all(is.finite(quantile(nc_pois, probs = 0.5, seed = 2))))
})

test_that("prior_only predict() resamples to a different draw count", {
  tn <- .grid_tn()
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                prior_only = TRUE, n_draws = 80, seed = 1)
  # fewer than simulated -> subsample; more -> sample with replacement
  expect_true(all(is.finite(summary(predict(nc, n_draws = 40,  seed = 2))$median)))
  expect_true(all(is.finite(summary(predict(nc, n_draws = 200, seed = 2))$median)))
})

# ── count-cumulative: issues #128 and #129 ───────────────────────────────────

# A cumulative stream whose delay-0 report is only part of the eventual total, so
# the engine takes its signed-increment (Skellam) path.
make_cumulative_grid <- function() {
  grid <- expand.grid(period = 0:19, delay = 0:3)
  grid$event  <- as.Date("2021-01-04") + grid$period
  grid$report <- as.Date("2021-01-04") + grid$period + grid$delay
  grid$n      <- as.integer(round((10 + 2 * grid$period) * 0.5^grid$delay)) + 1L
  grid        <- grid[grid$report <= max(grid$event), ]
  grid        <- grid[order(grid$report), ]
  grid$n      <- ave(grid$n, grid$event, FUN = cumsum)
  suppressMessages(tbl.now::tbl_now(
    grid[c("event", "report", "n")], event_date = "event", report_date = "report",
    case_count = "n", data_type = "count-cumulative", now = max(grid$event),
    verbose = FALSE))
}

test_that("count-cumulative data promotes the inert default to the dedicated process (#128)", {
  x <- make_cumulative_grid()
  expect_message(
    fitted <- nowcast(x, model(), n_draws = 50L, temporal_effects = "none", seed = 1L),
    "signed hurdle--ZTNB")
  expect_identical(fitted@model@revision@active, FALSE)
  expect_identical(fitted@model@cumulative@active, TRUE)
  expect_identical(fitted@model@cumulative@observation, "hurdle_ztnb")
  expect_identical(as.integer(fitted@model@cumulative@settlement), 26L)
  expect_identical(fitted@fits[[1]]$convergence, 0L)
})

test_that("prior_only on count-cumulative data returns real numbers, not NA (#129)", {
  x <- make_cumulative_grid()
  # `.sample_prior_parlist()` never sampled logit_confirm_p / retract_mu /
  # log_retract_sd_exc, so every draw died inside `.joint_reconstruct()` and the
  # pre-allocated NA matrix came back full-size and entirely missing.
  fitted <- suppressMessages(
    nowcast(x, prior_only = TRUE, n_draws = 50L, temporal_effects = "none", seed = 1L))
  sims <- fitted@fits[[1]]$prior_sims
  expect_false(anyNA(sims$M))
  expect_false(anyNA(sims$lambda_draws))
  expect_true(all(sims$M >= 0))
})

test_that("the prior sampler supplies every name .joint_reconstruct() reads", {
  # A guard against a new component reintroducing #129: whatever the parlist is
  # missing, the reconstruction is what notices, so ask it directly.  A parameter
  # the priors hold FIXED must be absent (it is mapped out, and `.joint_reconstruct()`
  # reads the fixed value instead); a free one must be present.
  x     <- make_cumulative_grid()
  basis_for <- function(mdl) {
    eng <- prepare_from_tbl_now(x, mdl, now = tbl.now::get_now(x), delay_only = FALSE)$data
    list(eng = eng, pri = default_priors(mdl, eng), b = .hsgp_basis_for_engine(eng))
  }

  ztnb <- basis_for(model(
    nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
    cumulative = cumulative_process(
      observation = "hurdle_ztnb", settlement = 6L
    )
  ))
  parlist <- .sample_prior_parlist(ztnb$eng, ztnb$pri,
                                   ncol(ztnb$b$Bmat), 1L)
  expect_true(all(c(
    "cumulative_retraction_mass_raw", "cumulative_retraction_mu",
    "log_cumulative_retraction_sigma_excess", "movement_intercept",
    "movement_age", "movement_previous", "log_magnitude_size"
  ) %in% names(parlist)))
  expect_false(any(c("logit_confirm_p", "retract_mu",
                     "log_retract_sd_exc") %in% names(parlist)))
  expect_no_error(.joint_reconstruct(ztnb$eng, ztnb$pri, parlist,
                                     ztnb$b$Bmat, ztnb$b$freq))

  ztp <- basis_for(model(
    nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
    cumulative = cumulative_process(
      observation = "hurdle_ztpoisson", settlement = 6L
    )
  ))
  parlist_ztp <- .sample_prior_parlist(ztp$eng, ztp$pri,
                                       ncol(ztp$b$Bmat), 1L)
  expect_false("log_magnitude_size" %in% names(parlist_ztp))
  expect_no_error(.joint_reconstruct(ztp$eng, ztp$pri, parlist_ztp,
                                     ztp$b$Bmat, ztp$b$freq))
})

test_that(".simulate_prior_draws() aborts instead of returning an all-NA result", {
  # The failure that hid #129: the per-draw tryCatch discarded its condition, so a
  # sampler that could not produce a single draw returned a correctly shaped,
  # entirely missing answer with no warning at all.
  x     <- make_cumulative_grid()
  mdl <- model(
    nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
    cumulative = cumulative_process(
      observation = "hurdle_ztpoisson", settlement = 6L
    )
  )
  eng   <- prepare_from_tbl_now(x, mdl, now = tbl.now::get_now(x), delay_only = FALSE)$data
  pri   <- default_priors(mdl, eng)
  basis <- .hsgp_basis_for_engine(eng)

  # Break the reconstruction for every draw by removing a prior it must read.
  broken <- pri; broken$count_cumulative_retraction_family <- NULL
  expect_error(
    .simulate_prior_draws(eng, broken, n_draws = 5L, Bmat = basis$Bmat,
                          freq = basis$freq, seed = 1L),
    "failed to reconstruct")
})
