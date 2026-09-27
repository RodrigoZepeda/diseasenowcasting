# The hurdle likelihoods model signed updates whose means telescope to
# E[C_t(H)] = lambda_t q_C(H).  These tests pin the pieces that keep the fitted
# `lambda` on that level scale: log-mean headroom, separate initial/revision
# ZTNB dispersion, and the trajectory rung of the cold-start ladder.

.simulate_hurdle_cumulative <- function(seed, n_events = 40L, settlement = 8L,
                                        magnitude_size = 50,
                                        movement = c(3, -0.5, 0.5)) {
  set.seed(seed)
  lambda <- round(300 * exp(1.2 * sin(seq_len(n_events) / 6)))
  report_pmf <- diseasenowcasting:::.finite_horizon_delay_pmf_numeric(
    diseasenowcasting:::.delay_distribution_functions(1L, log(0.8), 0.9),
    settlement + 1L
  )
  retraction_pmf <- diseasenowcasting:::.finite_horizon_delay_pmf_numeric(
    diseasenowcasting:::.delay_distribution_functions(1L, log(1.5), 0.6),
    settlement
  )
  kernel <- diseasenowcasting:::.count_cumulative_components(
    report_pmf, retraction_pmf, 0.05, settlement
  )
  start <- as.Date("2023-01-07")
  now <- start + (n_events - 1L) * 7L
  rows <- lapply(seq_len(n_events), function(event_index) {
    level <- 0
    previous_nonzero <- FALSE
    levels <- numeric(settlement + 1L)
    for (delay in 0:settlement) {
      alpha <- lambda[event_index] * kernel$alpha_unit[delay + 1L]
      omega <- lambda[event_index] * kernel$omega_unit[delay + 1L]
      total <- alpha + omega
      movement_probability <-
        diseasenowcasting:::.count_cumulative_movement_probability(
          total,
          movement[1L] + movement[2L] * log1p(delay) +
            movement[3L] * previous_nonzero
        )
      update <- 0
      if (total > 0 && stats::runif(1L) < movement_probability) {
        direction <- if (stats::runif(1L) < alpha / total) 1 else -1
        update <- direction * diseasenowcasting:::.draw_ztnb_own_mean(
          total / movement_probability, magnitude_size
        )
      }
      level <- level + update
      previous_nonzero <- update != 0
      levels[delay + 1L] <- level
    }
    event <- start + (event_index - 1L) * 7L
    report <- event + (0:settlement) * 7L
    keep <- report <= now
    data.frame(event = event, report = report[keep], count = levels[keep])
  })
  observations <- do.call(rbind, rows)
  list(
    lambda = lambda,
    terminal_retention = kernel$q_C[settlement + 1L],
    data = tbl.now::tbl_now(
      observations, event_date = event, report_date = report,
      case_count = count, data_type = "count-cumulative",
      event_units = "weeks", report_units = "weeks", now = now,
      verbose = FALSE
    )
  )
}

test_that("count-cumulative engines leave two orders of magnitude of log-mean headroom", {
  data <- .simulate_hurdle_cumulative(1L, n_events = 12L)$data
  hurdle <- model(
    nb_likelihood(), ar1_epidemic(), lognormal_delay(),
    cumulative = cumulative_process(settlement = 8L)
  )
  engine <- diseasenowcasting:::prepare_from_tbl_now(data, hurdle)$data
  expect_equal(
    engine$mu_log_upper_bound,
    min(max(6, log1p(engine$casemax)) + log(100), 16)
  )
  # The softplus keeps plogis(headroom) of lambda; at the largest published
  # level that is now above 99%.
  expect_gt(stats::plogis(engine$mu_log_upper_bound - log(engine$casemax)),
            0.99)
})

test_that("hurdle ZTNB uses the revision magnitude size only after delay 0", {
  data <- .simulate_hurdle_cumulative(2L, n_events = 10L, settlement = 4L)$data
  build <- function(revision_size) {
    mod <- model(
      nb_likelihood(), ar1_epidemic(), lognormal_delay(),
      cumulative = cumulative_process(
        settlement = 4L, magnitude_size = 3,
        revision_magnitude_size = revision_size
      )
    )
    engine <- diseasenowcasting:::prepare_from_tbl_now(data, mod)$data
    priors <- default_priors(mod, engine)
    built <- diseasenowcasting:::build_joint_obj(
      engine, priors, use_random = FALSE
    )
    list(engine = engine, priors = priors, built = built)
  }
  shared <- build(3)
  split <- build(0.5)
  expect_false(any(grepl("magnitude_size", names(shared$built$obj$par))))
  expect_identical(names(shared$built$obj$par), names(split$built$obj$par))

  par <- shared$built$obj$par
  shared$built$obj$fn(par)
  reconstructed <- diseasenowcasting:::.joint_reconstruct(
    shared$engine, shared$priors, shared$built$obj$env$parList(par),
    shared$built$Bmat, shared$built$freq
  )
  component <- reconstructed$count_cumulative
  expect_equal(component$magnitude_size, 3)
  expect_equal(component$revision_magnitude_size, 3)

  # Both sizes are fixed, so the objectives differ only through the ZTNB
  # magnitude of non-zero updates at delays 1:H.
  engine <- shared$engine
  expected_difference <- 0
  for (event_index in seq_len(engine$max_time)) {
    for (delay in 1:4) {
      if (!engine$observation_mask[event_index, delay + 1L, 1L]) next
      update <- engine$signed_update_array[event_index, delay + 1L, 1L]
      if (update == 0) next
      alpha <- reconstructed$lambda[event_index, 1L] *
        component$alpha_unit[delay + 1L] + 1e-12
      omega <- reconstructed$lambda[event_index, 1L] *
        component$omega_unit[delay + 1L] + 1e-12
      movement_probability <-
        diseasenowcasting:::.count_cumulative_movement_probability(
          alpha + omega,
          component$movement[["intercept"]] +
            component$movement[["age"]] * log1p(delay) +
            component$movement[["previous"]] *
              engine$previous_nonzero_array[event_index, delay + 1L, 1L]
        )
      expected_difference <- expected_difference +
        diseasenowcasting:::.hurdle_ztnb_update_logpmf(
          update, alpha, omega, movement_probability, 3
        ) -
        diseasenowcasting:::.hurdle_ztnb_update_logpmf(
          update, alpha, omega, movement_probability, 0.5
        )
    }
  }
  expect_gt(abs(expected_difference), 1)
  expect_equal(
    split$built$obj$fn(par) - shared$built$obj$fn(par),
    expected_difference,
    tolerance = 1e-8
  )
})

test_that("the trajectory rung starts the AR(1) path on the published levels", {
  data <- .simulate_hurdle_cumulative(3L, n_events = 15L)$data
  make_engine <- function(observation, epidemic = ar1_epidemic()) {
    mod <- model(
      nb_likelihood(), epidemic, lognormal_delay(),
      cumulative = cumulative_process(
        observation = observation, settlement = 8L
      )
    )
    diseasenowcasting:::prepare_from_tbl_now(data, mod)$data
  }
  expect_null(diseasenowcasting:::.count_cumulative_trajectory_init(
    make_engine("cumulative")
  ))
  expect_null(diseasenowcasting:::.count_cumulative_trajectory_init(
    make_engine("hurdle_ztnb", hsgp_epidemic())
  ))

  engine <- make_engine("hurdle_ztnb")
  init <- diseasenowcasting:::.count_cumulative_trajectory_init(engine)
  latest <- vapply(seq_len(engine$max_time), function(event_index) {
    observed <- which(engine$observation_mask[event_index, , 1L])
    engine$cumulative_level_array[event_index, max(observed), 1L]
  }, numeric(1))
  phi <- -0.999 + 1.998 * stats::plogis(init$ar_phi_unc)
  sigma <- engine$ar_sigma_max * stats::plogis(init$log_ar_sigma_unc)
  innovations <- init$ar_innov[, 1L]
  trend <- numeric(length(innovations))
  trend[1L] <- innovations[1L] * sigma / sqrt(1 - phi^2)
  for (event_index in seq_along(innovations)[-1L]) {
    trend[event_index] <- phi * trend[event_index - 1L] +
      innovations[event_index] * sigma
  }
  expect_equal(init$mu_intercept + trend, log(pmax(latest, 1)),
               tolerance = 1e-10)
})

test_that("hurdle ZTNB fitted levels match simulated settled levels", {
  skip_on_cran()
  # A delay-0 movement probability of plogis(1) = 0.73 is the regime where the
  # old fit collapsed to one flat `lambda` (AR(1) SD at its floor).
  simulation <- .simulate_hurdle_cumulative(1L, movement = c(1, -0.5, 0.5))
  settlement <- 8L
  fitted <- nowcast(
    simulation$data,
    model(
      nb_likelihood(), ar1_epidemic(), lognormal_delay(),
      cumulative = cumulative_process(settlement = settlement)
    ),
    temporal_effects = "none", n_draws = 20L, seed = 1L
  )
  fit <- fitted@fits[[1L]]
  expect_true(diseasenowcasting:::.fit_is_adequate(fit))

  engine <- fit$data
  component <- fit$reconstruct$count_cumulative
  lambda_hat <- as.numeric(fit$lambda)
  settled <- which(apply(engine$observation_mask[, , 1L], 1L, function(mask)
    max(which(mask)) - 1L) == settlement)
  expect_gt(length(settled), 25L)
  settled_level <- engine$cumulative_level_array[settled, settlement + 1L, 1L]
  fitted_level <- lambda_hat[settled] * component$q_C[settlement + 1L]
  true_level <- simulation$lambda[settled] * simulation$terminal_retention

  # E[C_t(H)] = lambda_t q_C(H) at the fitted mode.  The average alone cannot
  # detect the failure: a flat `lambda` at the season average also matches it.
  # Event by event, the flat fit is off by a median factor of about e^0.9 and
  # its log-SD ratio is 0.
  expect_lt(abs(log(mean(settled_level) / mean(fitted_level))), 0.15)
  expect_lt(median(abs(log(fitted_level / true_level))), 0.3)
  expect_gt(stats::sd(log(lambda_hat)),
            0.75 * stats::sd(log(simulation$lambda)))
  expect_lt(abs(mean(log(lambda_hat / simulation$lambda))), 0.25)
})
