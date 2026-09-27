# `cumulative_process(initial_report = "offset")`: each event has a
# Gamma(kappa, kappa) effect Xi_t shared by all its updates.  C_t(0) is
# Poisson(Xi_t mu_t q_C(0)), hence NB(mu_t q_C(0), kappa), and the later hurdle
# updates use mu_t E[Xi_t | C_t(0)].

.simulate_offset_cumulative <- function(seed, n_events = 40L, settlement = 8L,
                                        initial_size = 20,
                                        movement = c(1, -0.5, 0.5)) {
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
    intensity <- lambda[event_index] *
      stats::rgamma(1L, shape = initial_size, rate = initial_size)
    level <- stats::rpois(1L, intensity * kernel$alpha_unit[1L])
    previous_nonzero <- level != 0
    levels <- numeric(settlement + 1L)
    levels[1L] <- level
    for (delay in seq_len(settlement)) {
      alpha <- intensity * kernel$alpha_unit[delay + 1L]
      omega <- intensity * kernel$omega_unit[delay + 1L]
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
          total / movement_probability, 5
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
  list(
    lambda = lambda,
    terminal_retention = kernel$q_C[settlement + 1L],
    data = tbl.now::tbl_now(
      do.call(rbind, rows), event_date = event, report_date = report,
      case_count = count, data_type = "count-cumulative",
      event_units = "weeks", report_units = "weeks", now = now,
      verbose = FALSE
    )
  )
}

.offset_bundle <- function(data, initial_size, settlement = 4L,
                           observation = "hurdle_ztnb") {
  process <- if (identical(observation, "hurdle_ztnb")) {
    cumulative_process(
      observation = observation, settlement = settlement,
      initial_report = "offset", initial_size = initial_size,
      revision_magnitude_size = 3
    )
  } else {
    cumulative_process(
      observation = observation, settlement = settlement,
      initial_report = "offset", initial_size = initial_size
    )
  }
  mod <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
               cumulative = process)
  engine <- diseasenowcasting:::prepare_from_tbl_now(data, mod)$data
  priors <- default_priors(mod, engine)
  built <- diseasenowcasting:::build_joint_obj(engine, priors, use_random = FALSE)
  list(engine = engine, priors = priors, built = built)
}

test_that("cumulative_process() validates the initial-report offset", {
  offset <- cumulative_process(initial_report = "offset")
  expect_identical(offset@initial_report, "offset")
  expect_identical(offset@initial_size, 100)
  expect_true(S7::S7_inherits(
    cumulative_process(initial_report = "offset",
                       initial_size = lognormal_prior(log(50), 1))@initial_size,
    diseasenowcasting:::prior_class
  ))
  expect_null(offset@magnitude_size)
  expect_identical(cumulative_process()@initial_report, "hurdle")
  expect_null(cumulative_process()@initial_size)
  expect_error(
    cumulative_process(initial_report = "offset", magnitude_size = 2),
    "initial_size"
  )
  expect_error(cumulative_process(initial_size = 2), "offset")
  expect_error(cumulative_process(initial_report = "offset", initial_size = 0),
               "positive")
  expect_no_error(cumulative_process(
    observation = "hurdle_ztpoisson", initial_report = "offset",
    initial_size = 10
  ))

  priors <- default_priors(model(
    nb_likelihood(), ar1_epidemic(), lognormal_delay(),
    cumulative = cumulative_process(initial_report = "offset")
  ))
  expect_identical(priors$count_cumulative_initial_frailty, 1L)
  expect_identical(priors$initial_size$is_constant, 1L)
  expect_identical(priors$initial_size$fixed, 100)
})

test_that("the offset objective is NB at delay 0 and rescales later updates", {
  data <- .simulate_offset_cumulative(2L, n_events = 10L, settlement = 4L)$data
  small <- .offset_bundle(data, initial_size = 2)
  large <- .offset_bundle(data, initial_size = 50)
  # Fixed kappa and fixed revision size; the unused delay-0 size is mapped out.
  expect_false(any(c("log_initial_size", "log_magnitude_size",
                     "log_revision_magnitude_size") %in%
                     names(small$built$obj$par)))
  par <- small$built$obj$par
  expect_identical(names(par), names(large$built$obj$par))

  small$built$obj$fn(par)
  reconstructed <- diseasenowcasting:::.joint_reconstruct(
    small$engine, small$priors, small$built$obj$env$parList(par),
    small$built$Bmat, small$built$freq
  )
  component <- reconstructed$count_cumulative
  expect_true(component$initial_frailty)
  expect_equal(component$initial_size, 2)
  expect_null(component$magnitude_size)

  # Both kappa values are fixed, so the objectives differ only through the
  # delay-0 NB and the frailty mean that scales every later update.
  engine <- small$engine
  event_loglik <- function(event_index, kappa) {
    lambda <- reconstructed$lambda[event_index, 1L]
    initial_level <- engine$cumulative_level_array[event_index, 1L, 1L]
    initial_mean <- lambda * component$alpha_unit[1L] + 1e-12
    out <- diseasenowcasting:::.nb_mean_size_logpmf(
      initial_level, initial_mean, kappa
    )
    frailty_mean <- (kappa + initial_level) / (kappa + initial_mean)
    for (delay in 1:4) {
      if (!engine$observation_mask[event_index, delay + 1L, 1L]) next
      alpha <- frailty_mean * lambda * component$alpha_unit[delay + 1L] + 1e-12
      omega <- frailty_mean * lambda * component$omega_unit[delay + 1L] + 1e-12
      movement_probability <-
        diseasenowcasting:::.count_cumulative_movement_probability(
          alpha + omega,
          component$movement[["intercept"]] +
            component$movement[["age"]] * log1p(delay) +
            component$movement[["previous"]] *
              engine$previous_nonzero_array[event_index, delay + 1L, 1L]
        )
      out <- out + diseasenowcasting:::.hurdle_ztnb_update_logpmf(
        engine$signed_update_array[event_index, delay + 1L, 1L],
        alpha, omega, movement_probability, 3
      )
    }
    out
  }
  expected_difference <- sum(vapply(seq_len(engine$max_time), function(event)
    event_loglik(event, 2) - event_loglik(event, 50), numeric(1)))
  expect_gt(abs(expected_difference), 1)
  expect_equal(
    large$built$obj$fn(par) - small$built$obj$fn(par),
    expected_difference,
    tolerance = 1e-8
  )
})

test_that("offset draws preserve the settled mean for new and anchored events", {
  data <- .simulate_offset_cumulative(3L, n_events = 10L, settlement = 4L)$data
  bundle <- .offset_bundle(data, initial_size = 4,
                           observation = "hurdle_ztpoisson")
  bundle$built$obj$fn(bundle$built$obj$par)
  reconstructed <- diseasenowcasting:::.joint_reconstruct(
    bundle$engine, bundle$priors, bundle$built$obj$env$parList(),
    bundle$built$Bmat, bundle$built$freq
  )
  component <- reconstructed$count_cumulative
  settlement <- component$settlement_horizon

  # A new event: E[C_t(H)] = lambda q_C(H), with no point mass at zero from a
  # delay-0 hurdle.
  set.seed(1)
  future <- replicate(4000L, diseasenowcasting:::.draw_count_cumulative_future(
    matrix(200, 1L, 1L), component
  )[1L, 1L])
  expect_equal(mean(future), 200 * component$q_C[settlement + 1L],
               tolerance = 0.05)
  expect_lt(mean(future == 0), 0.01)

  # An event published only at delay 0: the future updates follow
  # lambda E[Xi | C_t(0)] (q_C(H) - q_C(0)).
  engine <- bundle$engine
  event <- engine$max_time
  expect_identical(max(which(engine$observation_mask[event, , 1L])), 1L)
  initial_level <- engine$cumulative_level_array[event, 1L, 1L]
  lambda <- reconstructed$lambda[event, 1L]
  frailty_mean <- (component$initial_size + initial_level) /
    (component$initial_size + lambda * component$alpha_unit[1L])
  expected <- initial_level + frailty_mean * lambda *
    (component$q_C[settlement + 1L] - component$q_C[1L])
  set.seed(2)
  anchored <- replicate(4000L, diseasenowcasting:::.draw_count_cumulative_terminal(
    engine, reconstructed
  )$terminal[event, 1L])
  expect_equal(mean(anchored), expected, tolerance = 0.05)
})

test_that("offset fits keep E[C_t(H)] = lambda q_C(H) on simulated data", {
  skip_on_cran()
  simulation <- .simulate_offset_cumulative(1L)
  settlement <- 8L
  fitted <- nowcast(
    simulation$data,
    model(
      nb_likelihood(), ar1_epidemic(), lognormal_delay(),
      cumulative = cumulative_process(
        settlement = settlement, initial_report = "offset"
      )
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
  settled_level <- engine$cumulative_level_array[settled, settlement + 1L, 1L]
  fitted_level <- lambda_hat[settled] * component$q_C[settlement + 1L]
  true_level <- simulation$lambda[settled] * simulation$terminal_retention

  expect_lt(abs(log(mean(settled_level) / mean(fitted_level))), 0.1)
  expect_lt(median(abs(log(fitted_level / true_level))), 0.3)
  expect_gt(stats::sd(log(lambda_hat)),
            0.75 * stats::sd(log(simulation$lambda)))
  expect_lt(abs(mean(log(lambda_hat / simulation$lambda))), 0.15)
})
