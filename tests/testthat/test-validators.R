# Tests that trigger validators in class constructors (coverage for error paths)
# and cover the remaining warm-init path in collect_fits.

# ── 04_epidemic_class.R validators ───────────────────────────────────────────

test_that("hsgp_epidemic() rejects invalid alpha prior", {
  expect_error(hsgp_epidemic(alpha = "bad"), regexp = "alpha|prior")
})

test_that("hsgp_epidemic() rejects negative num_basis", {
  expect_error(hsgp_epidemic(num_basis = -1L), regexp = "num_basis")
})

test_that("hsgp_epidemic() rejects negative tmax_model", {
  expect_error(hsgp_epidemic(tmax_model = -5L), regexp = "tmax_model")
})

test_that("ar1_epidemic() rejects invalid sigma prior", {
  expect_error(ar1_epidemic(sigma = "bad"), regexp = "sigma|prior")
})

test_that("sir_epidemic() rejects invalid R0 prior", {
  expect_error(sir_epidemic(R0 = "bad"), regexp = "R0|prior")
})

test_that("sir_epidemic() rejects negative N_pop", {
  expect_error(sir_epidemic(N_pop = -1), regexp = "N_pop")
})

# ── 03_delay_class.R validators ──────────────────────────────────────────────

test_that("delay_process_class rejects unknown name", {
  expect_error(lognormal_delay(mu = "not_a_prior"), regexp = "prior")
})

test_that("lognormal_delay() num_delay_seasons parameter works", {
  ld <- lognormal_delay(num_delay_seasons = 2L)
  expect_equal(ld@num_delay_seasons, 2L)
})

test_that("gamma_delay() rejects invalid shape", {
  expect_error(gamma_delay(shape = "bad"), regexp = "prior")
})

test_that("generalized_gamma_delay() rejects invalid Q", {
  expect_error(generalized_gamma_delay(Q = "bad"), regexp = "prior|Q")
})

# ── 00_prior_class.R: prior constructors exercise all slots ──────────────────

test_that("all named prior constructors produce prior_class with correct num_id", {
  expect_equal(std_normal_prior()@num_id,          0L)
  expect_equal(normal_prior(0, 1)@num_id,          1L)
  expect_equal(cauchy_prior(0, 1)@num_id,          2L)
  expect_equal(student_t_prior(3, 0, 1)@num_id,    3L)
  expect_equal(double_exponential_prior(0,1)@num_id, 4L)
  expect_equal(flat_prior()@num_id,                5L)
  expect_equal(half_std_normal_prior()@num_id,     100L)
  expect_equal(half_normal_prior(0, 1)@num_id,     101L)
  expect_equal(half_cauchy_prior(0, 1)@num_id,     102L)
  expect_equal(half_student_t_prior(3, 1)@num_id,  103L)
  expect_equal(gamma_prior(2, 1)@num_id,           105L)
  expect_equal(inv_gamma_prior(2, 1)@num_id,       108L)
  expect_equal(lognormal_prior(0, 1)@num_id,       109L)
  expect_equal(exponential_prior(1)@num_id,        111L)
  expect_equal(beta_prior(2, 2)@num_id,            113L)
})

# ── 18_collect_fits.R Dirichlet warm-init path ────────────────────────────────

test_that("two-stage with Dirichlet delay uses NP warm-init path", {
  tn   <- .make_synth_tblnow(Tn = 60L, seed = 50)
  mdl  <- model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay(bins = 8L))
  nc   <- nowcast(tn, mdl, type = "two_stage", K = 3, n_draws = 150, seed = 1)
  expect_true(length(nc@fits) >= 1L)
  s <- summary(predict(nc, seed = 2))
  expect_true(all(is.finite(s$median)))
})

# ── Additional backtest/score lines ──────────────────────────────────────────

test_that("predict(backtest) returns a data.frame with key columns", {
  tn    <- .make_synth_tblnow(Tn = 80L, seed = 51)
  start <- min(tn$onset)
  bt    <- backtest(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                    dates = start + c(50, 65) - 1,
                    type = "one_stage", n_draws = 200, seed = 1)
  pr <- predict(bt)
  expect_true(is.data.frame(pr))
  expect_true("median" %in% names(pr))
})

test_that("score(backtest) with report=TRUE prints without error", {
  tn    <- .make_synth_tblnow(Tn = 80L, seed = 52)
  start <- min(tn$onset)
  bt    <- backtest(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                    dates = start + c(50) - 1,
                    type = "one_stage", n_draws = 150, seed = 1)
  expect_no_error(score(bt, report = TRUE))
})
