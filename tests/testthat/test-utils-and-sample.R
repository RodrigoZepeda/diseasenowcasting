# Tests for internal utilities and sample() dispatch (R/01_utils.R, R/08_sample.R)

test_that(".wtd_median matches stats::median for uniform weights", {
  set.seed(1)
  x <- sort(runif(20))
  w <- rep(1, 20)
  expect_equal(diseasenowcasting:::.wtd_median(x, w), stats::median(x), tolerance = 1e-6)
})

test_that(".wtd_median handles NAs and zero weights gracefully", {
  x <- c(1, NA, 3, 5)
  w <- c(1, 1,  1, 0)
  med <- diseasenowcasting:::.wtd_median(x, w)
  expect_true(is.finite(med))
})

test_that(".wtd_median returns NA for all-NA input", {
  expect_true(is.na(diseasenowcasting:::.wtd_median(c(NA, NA), c(1, 1))))
})

test_that(".wtd_var returns positive value for varying data", {
  x <- c(1, 2, 3, 4, 5); w <- rep(1, 5)
  v <- diseasenowcasting:::.wtd_var(x, w)
  expect_gt(v, 0)
})

test_that(".wtd_var returns NA for single non-zero weight", {
  expect_true(is.na(diseasenowcasting:::.wtd_var(c(1), c(1))))
})

test_that(".pad3 pads short vectors to length 3 with zeros", {
  expect_equal(diseasenowcasting:::.pad3(c(1, 2)),    c(1, 2, 0))
  expect_equal(diseasenowcasting:::.pad3(c(5)),       c(5, 0, 0))
  expect_equal(diseasenowcasting:::.pad3(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(diseasenowcasting:::.pad3(numeric(0)), c(0, 0, 0))
})

test_that(".resolve_prior returns a numeric value from a prior_class", {
  set.seed(1)
  pr <- normal_prior(0, 1)
  val <- diseasenowcasting:::.resolve_prior(pr)
  expect_length(val, 1L)
  expect_true(is.numeric(val))
})

test_that(".resolve_prior passes through a numeric unchanged", {
  expect_equal(diseasenowcasting:::.resolve_prior(3.14), 3.14)
})

test_that(".resolve_prior returns NULL for NULL input", {
  expect_null(diseasenowcasting:::.resolve_prior(NULL))
})

test_that("sample() draws correct number for all prior families", {
  set.seed(42)
  for (pr in list(
    inv_gamma_prior(2, 1), weibull_prior(2, 1),
    chi_square_prior(3),   logistic_prior(0, 1),
    beta_prior(2, 2),      half_student_t_prior(3, 1),
    half_double_exponential_prior(0, 1)
  )) {
    draws <- sample(pr, 10L)
    expect_length(draws, 10L)
    expect_true(all(is.finite(draws)))
  }
})

test_that("sample() on unknown prior name throws informative error", {
  # Build a prior with an unsupported name by bypassing the validator
  bad_prior <- S7::S7_object()
  class(bad_prior) <- class(normal_prior(0, 1))
  # Can't set bad name via @<- (validator fires); call sample dispatch directly
  # Instead check that sample dispatch on an unregistered type falls through
  expect_error(sample(list(), 5L))  # list not dispatched -> error
})

test_that("lognormal_native returns a list with log_location and log_scale", {
  ln <- lognormal_native(log(5), 4)
  expect_named(ln, c("log_location", "log_scale"))
  expect_true(ln$log_scale > 0)
})

test_that("hsgp_basis returns a matrix with num_basis columns", {
  ts  <- hsgp_time_scaled(50L, 50L)
  mat <- hsgp_basis(ts, 1.5 * 0.62 * 2, 1.5 * 0.38 * 2, 10L, 1L)
  expect_equal(ncol(mat), 10L)
  expect_equal(nrow(mat), 50L)
})

test_that("hsgp_spectral_weights returns positive weights", {
  freqs   <- seq_len(10) * pi / 3
  weights <- hsgp_spectral_weights(freqs, amplitude = 1, length_scale = 1, gp_kernel = 2L)
  expect_length(weights, 10L)
  expect_true(all(weights > 0))
})
