# Tests for prior constructors, sampling, and density evaluation

test_that("all prior constructors return prior_class with correct name", {
  expect_true(S7::S7_inherits(std_normal_prior(),         diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(normal_prior(0, 1),         diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(cauchy_prior(0, 1),         diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(student_t_prior(3, 0, 1),   diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(double_exponential_prior(0, 1), diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(flat_prior(),               diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(half_std_normal_prior(),    diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(half_normal_prior(0, 1),    diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(half_cauchy_prior(0, 1),    diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(half_student_t_prior(3, 1), diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(gamma_prior(2, 0.5),        diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(inv_gamma_prior(2, 1),      diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(lognormal_prior(0, 1),      diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(exponential_prior(1),       diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(weibull_prior(2, 1),        diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(beta_prior(2, 2),           diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(chi_square_prior(3),        diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(logistic_prior(0, 1),       diseasenowcasting:::prior_class))
})

test_that("normal_prior and lognormal_prior store params correctly", {
  p <- normal_prior(2, 0.5)
  expect_equal(p@stan_params[1], 2)
  expect_equal(p@stan_params[2], 0.5)

  lp <- lognormal_prior(log(10), 0.3)
  expect_equal(lp@stan_params[1], log(10))
  expect_equal(lp@stan_params[2], 0.3)
})

test_that("fix_prior() creates a fixed (is_constant) prior list via default_priors", {
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  m   <- .make_synth()$m
  eng <- prepare_data(mdl, m, delay_only = TRUE)
  pri <- default_priors(mdl, eng)
  fixed_pri <- fix_param(pri, "delay_mu", 1.5)
  expect_equal(fixed_pri$delay_mu$is_constant, 1L)
  expect_equal(fixed_pri$delay_mu$fixed, 1.5)
})

test_that("sample() draws the correct number of values from each prior type", {
  set.seed(1)
  for (pr in list(std_normal_prior(), normal_prior(0,1), cauchy_prior(0,1),
                  student_t_prior(3,0,1), gamma_prior(2,1), lognormal_prior(0,1),
                  exponential_prior(1), beta_prior(2,2), half_normal_prior(0,1),
                  half_cauchy_prior(0,1))) {
    draws <- sample(pr, 20L)
    expect_length(draws, 20L)
    expect_true(all(is.finite(draws)))
  }
})

test_that("sample() works for flat, double-exponential, and half-double-exponential", {
  set.seed(2)
  expect_length(sample(flat_prior(), 5L), 5L)
  expect_length(sample(double_exponential_prior(0, 1), 5L), 5L)
  expect_length(sample(half_std_normal_prior(), 5L), 5L)
  expect_true(all(sample(half_std_normal_prior(), 100L) >= 0))
})

test_that("sample() dispatches to base::sample for vectors/characters", {
  set.seed(3)
  v <- sample(1:10, size = 5L)
  expect_length(v, 5L)
  expect_true(all(v %in% 1:10))
  ch <- sample(letters, size = 3L)
  expect_length(ch, 3L)
})

test_that("prior_lpdf returns finite values for all distribution types", {
  vals <- list(
    list(normal_prior(0, 1),            0.5),
    list(cauchy_prior(0, 1),            1.0),
    list(student_t_prior(3, 0, 1),      0.5),
    list(gamma_prior(2, 1),             1.5),
    list(lognormal_prior(0, 1),         2.0),
    list(exponential_prior(1),          0.5),
    list(beta_prior(2, 2),              0.4),
    list(half_normal_prior(0, 1),       0.5),
    list(double_exponential_prior(0,1), 0.3)
  )
  for (item in vals) {
    pr <- item[[1]]; val <- item[[2]]
    lp <- prior_lpdf(val, pr@num_id, .pad3(pr@stan_params))
    expect_true(is.finite(lp),
                label = paste("prior_lpdf finite for", pr@name))
  }
})

test_that("dirichlet_lpdf returns finite value for valid simplex", {
  probs <- c(0.2, 0.3, 0.5)
  alpha <- c(1, 1, 1)
  lp <- dirichlet_lpdf(probs, alpha)
  expect_true(is.finite(lp))
})
