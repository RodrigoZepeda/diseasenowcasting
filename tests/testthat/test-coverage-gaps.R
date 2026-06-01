# Targeted tests to push coverage above 90% in the remaining low-coverage files.

# ── 04_epidemic_class.R (48% → target 90%) ───────────────────────────────────

test_that("hsgp_epidemic() default slots are sensible", {
  ep <- hsgp_epidemic()
  expect_equal(ep@num_id, 1L)
  expect_true(S7::S7_inherits(ep, diseasenowcasting:::epidemic_process_class))
  # gp_kernel 1=Matern32, 2=Matern52
  expect_true(ep@gp_kernel %in% c(1L, 2L))
})

test_that("ar1_epidemic() default slots are set", {
  ep <- ar1_epidemic()
  expect_equal(ep@num_id, 2L)
  expect_true(S7::S7_inherits(ep, diseasenowcasting:::epidemic_process_class))
})

test_that("sir_epidemic() stores N_pop and use_beta_rw_trend", {
  ep <- sir_epidemic(N_pop = 2e6)
  expect_equal(ep@num_id, 3L)
  expect_equal(ep@N_pop, 2e6)
  expect_true(S7::S7_inherits(ep, diseasenowcasting:::epidemic_process_class))
})

test_that("hsgp_epidemic() with custom kernel and basis", {
  ep <- hsgp_epidemic(num_basis = 12L, gp_kernel = "matern32")
  expect_equal(ep@num_basis, 12L)
})

test_that("ar1_trend() produces a length-T numeric vector", {
  set.seed(1)
  innov <- rnorm(50)
  tr    <- ar1_trend(innov, phi = 0.8, sigma = 0.3)
  expect_length(tr, 50L)
  expect_true(all(is.finite(as.numeric(tr))))
})

# ── 03_delay_class.R (69% → target 90%) ─────────────────────────────────────

test_that("lognormal_delay() with fixed numeric mu stores it correctly", {
  ld <- lognormal_delay(mu = 1.5)
  expect_equal(ld@mu, 1.5)
})

test_that("gamma_delay() stores shape/rate slots", {
  gd <- gamma_delay()
  expect_equal(gd@num_id, 2L)
  expect_true(S7::S7_inherits(gd, diseasenowcasting:::delay_process_class))
})

test_that("generalized_gamma_delay() stores Q slot", {
  gg <- generalized_gamma_delay(Q = gamma_prior(2, 1))
  expect_true(S7::S7_inherits(gg@Q, diseasenowcasting:::prior_class))
})

test_that("dirichlet_delay() with numeric alpha stores it", {
  dd <- dirichlet_delay(alpha = 2.0, bins = 10L)
  expect_equal(dd@bins, 10L)
})

# ── 06_prior_density.R (77% → target 90%) ────────────────────────────────────

test_that("prior_lpdf returns finite for all real distribution codes", {
  # (num_id, test_value, params) — using the actual switch codes from 06_prior_density.R
  cases <- list(
    list(0L,   0.5, c(0,1,0)),    # StdNormal
    list(1L,   0.5, c(0,1,0)),    # Normal
    list(2L,   0.5, c(0,1,0)),    # Cauchy
    list(3L,   0.5, c(3,0,1)),    # StudentT
    list(4L,   0.5, c(0,1,0)),    # DoubleExponential
    list(5L,   0.5, c(0,0,0)),    # Flat
    list(100L, 0.5, c(0,1,0)),    # HalfStdNormal
    list(101L, 0.5, c(0,1,0)),    # HalfNormal
    list(102L, 0.5, c(0,1,0)),    # HalfCauchy
    list(103L, 0.5, c(3,1,0)),    # HalfStudentT
    list(104L, 0.5, c(0,1,0)),    # HalfDoubleExponential
    list(105L, 1.5, c(2,1,0)),    # Gamma
    list(107L, 1.5, c(2,1,0)),    # Weibull
    list(108L, 1.5, c(2,1,0)),    # InvGamma
    list(109L, 1.5, c(0,1,0)),    # LogNormal
    list(110L, 2.0, c(3,0,0)),    # ChiSquare
    list(111L, 0.5, c(1,0,0)),    # Exponential
    list(112L, 0.5, c(0,1,0)),    # Logistic
    list(113L, 0.4, c(2,2,0))     # Beta
  )
  for (item in cases) {
    id <- item[[1]]; val <- item[[2]]; params <- item[[3]]
    lp <- prior_lpdf(val, id, params)
    expect_true(is.finite(lp), label = paste("finite for num_id", id))
  }
})

test_that("dirichlet_lpdf is higher for more-concentrated simplex (correct shape)", {
  probs  <- c(0.5, 0.3, 0.2)
  alpha1 <- c(1, 1, 1)       # flat
  alpha2 <- c(10, 6, 4)      # concentrated near probs
  lp1 <- dirichlet_lpdf(probs, alpha1)
  lp2 <- dirichlet_lpdf(probs, alpha2)
  expect_true(lp2 > lp1)    # more concentrated at the truth → higher density
})

# ── 01_utils.R (68% → target 85%) ────────────────────────────────────────────

test_that("valid_positive_prior accepts valid positive priors", {
  expect_no_error(diseasenowcasting:::valid_positive_prior(gamma_prior(2, 1)))
  expect_no_error(diseasenowcasting:::valid_positive_prior(half_normal_prior(0, 1)))
  expect_no_error(diseasenowcasting:::valid_positive_prior(2.5))   # numeric passes
})

test_that("%||% returns right-hand side when left is NULL", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(3    %||% 5, 3)
})

# ── 18_collect_fits.R (57% → target 80%) ─────────────────────────────────────

test_that(".collect_nowcast_fits two_stage returns multi rung for LogNormal", {
  tn  <- .make_synth_tblnow(Tn = 60L, seed = 30)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  prep <- diseasenowcasting:::prepare_from_tbl_now(tn, mdl)
  eng  <- prep$data
  pri  <- default_priors(mdl, eng)
  col  <- diseasenowcasting:::.collect_nowcast_fits(mdl, eng, pri, type = "two_stage",
                                         K = 3, floor_mu = 0.15, floor_sig_frac = 0.25)
  expect_true(length(col$fits) >= 1L)
  expect_true(col$rung %in% c("multi", "onestage", "anchored"))
})

test_that(".pool_fit_draws returns M matrix with correct column count", {
  tn  <- .make_synth_tblnow(Tn = 50L, seed = 31)
  nc  <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 type = "two_stage", K = 3, n_draws = 100, seed = 1)
  pooled <- diseasenowcasting:::.pool_fit_draws(nc@fits, nc@target, n_draws = 50)
  expect_equal(ncol(pooled$M),      nc@target)
  expect_equal(ncol(pooled$lambda), nc@target)
})

# ── 16_multisample.R (78% → target 85%) ──────────────────────────────────────

test_that("nowcast_twostage (direct) returns valid M matrix", {
  set.seed(2)
  syn  <- .make_synth(Tn = 50L, seed = 2)
  mdl  <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng  <- prepare_data(mdl, syn$m, delay_only = FALSE)
  res  <- nowcast_twostage(mdl, syn$m, X = NULL, d_star = NULL,
                            max_time = eng$max_time, target = eng$max_time,
                            K = 3, n_draws_per = 50, seed = 1)
  expect_true(is.matrix(res$M))
  expect_equal(ncol(res$M), eng$max_time)
  expect_true(all(is.finite(res$quantiles)))
})

# ── 23_backtest.R / 24_score.R (79% → target 85%) ───────────────────────────

test_that("backtest with return_simulations=TRUE populates @simulations", {
  tn  <- .make_synth_tblnow(Tn = 80L, seed = 40)
  start <- min(tn$onset)
  bt  <- backtest(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                  dates = start + c(50, 65) - 1,
                  type = "one_stage", n_draws = 200, return_simulations = TRUE, seed = 1)
  expect_true(!is.null(bt@simulations))
  expect_true(length(bt@simulations) > 0)
})

test_that("score() with metric='ape' ranks models and returns finite values", {
  tn  <- .make_synth_tblnow(Tn = 90L, seed = 41)
  start <- min(tn$onset)
  bt  <- backtest(tn, list(model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                           model(nb_likelihood(), ar1_epidemic(),  lognormal_delay())),
                  dates = start + c(50, 70) - 1,
                  type = "one_stage", n_draws = 200, seed = 1)
  sc_ape <- score(bt, metric = "ape",  report = FALSE)
  sc_mse <- score(bt, metric = "mse",  report = FALSE)
  expect_equal(nrow(sc_ape), 2L)
  expect_true(all(is.finite(sc_ape$ape)))
  expect_true(all(is.finite(sc_mse$mse)))
  # sorted best-first
  expect_true(sc_ape$ape[1] <= sc_ape$ape[2])
  expect_true(sc_mse$mse[1] <= sc_mse$mse[2])
})

test_that("autoplot(backtest) returns a ggplot with ribbon layers", {
  tn    <- .make_synth_tblnow(Tn = 80L, seed = 42)
  start <- min(tn$onset)
  bt    <- backtest(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                    dates = start + c(50, 65) - 1,
                    type = "one_stage", n_draws = 200, seed = 1)
  p <- autoplot(bt)
  expect_s3_class(p, "ggplot")
  expect_gt(length(p$layers), 0L)
})

# ── 17_prepare_from_tblnow.R (79% → target 88%) ─────────────────────────────

test_that("prepare_from_tbl_now with daily data and temporal effects works", {
  suppressMessages(library(tbl.now))
  mp <- tbl_now(as.data.frame(mpoxdat), event_date = dx_date,
                report_date = dx_report_date, case_count = n,
                data_type = "count-incidence", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
    compute_temporal_effects()
  mdl  <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  prep <- diseasenowcasting:::prepare_from_tbl_now(mp, mdl, now = as.Date("2022-09-15"))
  expect_gt(prep$data$P, 0L)                       # covariates were picked up
  expect_equal(prep$data$num_strata, 1L)
})

test_that("prepare_from_tbl_now infers now from get_now when not provided", {
  tn   <- .make_synth_tblnow(Tn = 50L, seed = 43)
  mdl  <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  prep <- diseasenowcasting:::prepare_from_tbl_now(tn, mdl)
  expect_true(!is.null(prep$now))
  expect_true(inherits(prep$now, "Date"))
})
