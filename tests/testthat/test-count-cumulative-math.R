test_that("finite-horizon retraction and cumulative kernels satisfy their contract", {
  for (H in c(6L, 26L, 52L)) {
    report_pmf <- dpois(0:H, lambda = 2)
    report_pmf <- report_pmf / sum(report_pmf)
    retraction_pmf <- dgamma(seq_len(H), shape = 2, rate = 0.7)
    retraction_pmf <- retraction_pmf / sum(retraction_pmf)
    components <- diseasenowcasting:::.count_cumulative_components(
      report_pmf, retraction_pmf, retraction_mass = 0.2, settlement = H
    )

    expect_true(all(components$h_R >= 0))
    expect_lte(sum(components$h_R), 1)
    expect_equal(components$S_R[1L], 1)
    expect_true(all(diff(components$S_R) <= 1e-12))
    expect_equal(components$omega_unit[1L], 0)

    q_reference <- vapply(0:H, function(delay) {
      sum(report_pmf[seq_len(delay + 1L)] *
            rev(components$S_R[seq_len(delay + 1L)]))
    }, numeric(1))
    expect_equal(components$q_C, q_reference, tolerance = 1e-12)
  }
})

test_that("numeric finite-horizon delay discretisation survives tail-only draws", {
  # This parameter draw reproduced the production prediction failures: the
  # natural CDF is exactly zero throughout 1:26 even though relative conditional
  # mass within that horizon remains well defined on the log scale.
  functions <- diseasenowcasting:::.delay_distribution_functions(
    1L, parameter_1 = 5.05107710, parameter_2 = 1.942549
  )
  expect_true(all(functions$cdf(seq_len(26L)) == 0))
  probability <- diseasenowcasting:::.finite_horizon_delay_pmf_numeric(
    functions, 26L
  )
  expect_length(probability, 26L)
  expect_true(all(is.finite(probability)))
  expect_true(all(probability >= 0))
  expect_equal(sum(probability), 1, tolerance = 1e-12)
  expect_gt(probability[26L], 0)
})

test_that("signed updates telescope back to cumulative levels", {
  cumulative <- c(4, 9, 9, 7, 12, 11)
  updates <- c(cumulative[1L], diff(cumulative))
  expect_equal(cumsum(updates), cumulative)
  expect_gte(updates[1L], 0)
})

test_that("cumulative Poisson and NB log masses use the package mean-size convention", {
  for (count in c(0, 1, 8, 100)) {
    mean <- 7.25
    size <- 3.5
    expect_equal(
      diseasenowcasting:::.count_cumulative_level_logpmf(count, mean, 0L),
      stats::dpois(count, mean, log = TRUE)
    )
    expect_equal(
      diseasenowcasting:::.count_cumulative_level_logpmf(count, mean, 1L, size),
      stats::dnbinom(count, size = size, mu = mean, log = TRUE)
    )
  }
})

test_that("Psi inverse returns a parent NB mean with the requested truncated mean", {
  grid <- expand.grid(
    own_mean = c(1 + 1e-8, 1.0001, 1.01, 1.1, 2, 10, 1e3, 1e6),
    size = c(0.01, 0.1, 1, 10, 1e3)
  )
  for (row in seq_len(nrow(grid))) {
    own_mean <- grid$own_mean[row]
    size <- grid$size[row]
    parent_mean <- diseasenowcasting:::.ztnb_parent_mean(own_mean, size)
    recovered <- diseasenowcasting:::.ztnb_own_mean(parent_mean, size)
    expect_gt(parent_mean, 0)
    expect_lte(parent_mean, own_mean)
    expect_lt(abs(recovered - own_mean), 2e-9 * (1 + abs(own_mean)))
  }
})

test_that("zero-truncated NB log masses normalize", {
  cases <- list(c(1.01, 0.05), c(2, 0.5), c(10, 2), c(100, 20))
  for (case in cases) {
    own_mean <- case[1L]
    size <- case[2L]
    parent_mean <- diseasenowcasting:::.ztnb_parent_mean(own_mean, size)
    support_max <- stats::qnbinom(1 - 1e-11, size = size, mu = parent_mean)
    support <- seq_len(max(1L, support_max))
    total_mass <- sum(exp(vapply(
      support,
      diseasenowcasting:::.ztnb_logpmf,
      numeric(1), own_mean = own_mean, size = size
    )))
    expect_equal(total_mass, 1, tolerance = 2e-9)
  }
})

test_that("zero-truncated Poisson is indexed by its own mean and normalizes", {
  for (own_mean in c(1 + 1e-8, 1.0001, 1.01, 1.1, 2, 10, 1e3, 1e6)) {
    parent_mean <- diseasenowcasting:::.ztpoisson_parent_mean(own_mean)
    recovered <- diseasenowcasting:::.ztpoisson_own_mean(parent_mean)
    expect_gt(parent_mean, 0)
    expect_lte(parent_mean, own_mean)
    expect_lt(abs(recovered - own_mean), 2e-9 * (1 + abs(own_mean)))

    if (own_mean <= 100) {
      support_max <- stats::qpois(1 - 1e-12, lambda = parent_mean)
      support <- seq_len(max(50L, support_max))
      total_mass <- sum(exp(vapply(
        support,
        diseasenowcasting:::.ztpoisson_logpmf,
        numeric(1), own_mean = own_mean
      )))
      expect_equal(total_mass, 1, tolerance = 2e-10)
    }
  }
})

test_that("hurdle movement is admissible and all update branches have the expected law", {
  alpha <- 1.7
  omega <- 0.3
  total <- alpha + omega
  movement <- diseasenowcasting:::.count_cumulative_movement_probability(
    total, linear_predictor = -0.4
  )
  expect_gt(movement, 0)
  expect_lte(movement, min(1, total))

  expect_equal(
    diseasenowcasting:::.hurdle_ztnb_update_logpmf(0, alpha, omega, movement, 2),
    log1p(-movement)
  )
  positive <- diseasenowcasting:::.hurdle_ztnb_update_logpmf(
    3, alpha, omega, movement, 2
  )
  negative <- diseasenowcasting:::.hurdle_ztnb_update_logpmf(
    -3, alpha, omega, movement, 2
  )
  expect_true(is.finite(positive))
  expect_true(is.finite(negative))
  expect_equal(positive - negative, log(alpha / omega), tolerance = 1e-12)

  positive_poisson <- diseasenowcasting:::.hurdle_ztpoisson_update_logpmf(
    3, alpha, omega, movement
  )
  negative_poisson <- diseasenowcasting:::.hurdle_ztpoisson_update_logpmf(
    -3, alpha, omega, movement
  )
  expect_true(is.finite(positive_poisson))
  expect_true(is.finite(negative_poisson))
  expect_equal(positive_poisson - negative_poisson,
               log(alpha / omega), tolerance = 1e-12)
})

test_that("count-cumulative kernels and hurdle ZTNB branches tape and differentiate", {
  report_pmf <- dpois(0:6, lambda = 1.5)
  report_pmf <- report_pmf / sum(report_pmf)
  retraction_pmf <- dgamma(1:6, shape = 2, rate = 1)
  retraction_pmf <- retraction_pmf / sum(retraction_pmf)

  for (update in c(0, 25, -8)) {
    objective <- RTMB::MakeADFun(function(parameters) {
      RTMB::getAll(parameters)
      components <- .count_cumulative_components(
        report_pmf, retraction_pmf, plogis(mass_raw), 6L
      )
      alpha <- exp(log_mu) * components$alpha_unit[4L] + 1e-12
      omega <- exp(log_mu) * components$omega_unit[4L] + 1e-12
      movement <- .count_cumulative_movement_probability(
        alpha + omega, movement_intercept
      )
      -.hurdle_ztnb_update_logpmf(
        update, alpha, omega, movement, exp(log_size)
      )
    }, list(
      log_mu = log(100), mass_raw = qlogis(0.1),
      movement_intercept = -1, log_size = log(0.5)
    ), silent = TRUE)

    expect_true(is.finite(objective$fn(objective$par)))
    expect_true(all(is.finite(objective$gr(objective$par))))
  }
})

test_that("the taped ZTNB inverse derivative agrees with a finite difference", {
  objective <- RTMB::MakeADFun(function(parameters) {
    RTMB::getAll(parameters)
    .ztnb_parent_mean(1 + exp(log_excess_mean), exp(log_size))
  }, list(log_excess_mean = log(0.3), log_size = log(0.2)), silent = TRUE)

  point <- objective$par
  automatic <- objective$gr(point)
  step <- 1e-5
  numeric <- vapply(seq_along(point), function(index) {
    upper <- lower <- point
    upper[index] <- upper[index] + step
    lower[index] <- lower[index] - step
    (objective$fn(upper) - objective$fn(lower)) / (2 * step)
  }, numeric(1))

  expect_true(all(is.finite(automatic)))
  expect_equal(as.numeric(automatic), numeric, tolerance = 2e-6)
})

test_that("hurdle ZTPoisson branches and inverse tape and differentiate", {
  for (update in c(0, 25, -8)) {
    objective <- RTMB::MakeADFun(function(parameters) {
      RTMB::getAll(parameters)
      alpha <- exp(log_alpha)
      omega <- exp(log_omega)
      movement <- .count_cumulative_movement_probability(
        alpha + omega, movement_intercept
      )
      -.hurdle_ztpoisson_update_logpmf(update, alpha, omega, movement)
    }, list(log_alpha = log(15), log_omega = log(3),
            movement_intercept = -1), silent = TRUE)

    expect_true(is.finite(objective$fn(objective$par)))
    expect_true(all(is.finite(objective$gr(objective$par))))
  }

  inverse <- RTMB::MakeADFun(function(parameters) {
    RTMB::getAll(parameters)
    .ztpoisson_parent_mean(1 + exp(log_excess_mean))
  }, list(log_excess_mean = log(0.3)), silent = TRUE)
  point <- inverse$par
  step <- 1e-5
  numeric <- (inverse$fn(point + step) - inverse$fn(point - step)) / (2 * step)
  expect_equal(as.numeric(inverse$gr(point)), numeric, tolerance = 2e-6)
})

test_that("both hurdle magnitude laws preserve the signed-update mean", {
  alpha <- 1.7
  omega <- 0.3
  movement <- diseasenowcasting:::.count_cumulative_movement_probability(
    alpha + omega, -0.4
  )
  own_mean <- (alpha + omega) / movement
  expected <- movement *
    (alpha / (alpha + omega) - omega / (alpha + omega)) * own_mean
  expect_equal(expected, alpha - omega, tolerance = 1e-14)

  nb_parent <- diseasenowcasting:::.ztnb_parent_mean(own_mean, size = 0.5)
  poisson_parent <- diseasenowcasting:::.ztpoisson_parent_mean(own_mean)
  expect_equal(diseasenowcasting:::.ztnb_own_mean(nb_parent, 0.5), own_mean,
               tolerance = 1e-10)
  expect_equal(diseasenowcasting:::.ztpoisson_own_mean(poisson_parent), own_mean,
               tolerance = 1e-10)
})
