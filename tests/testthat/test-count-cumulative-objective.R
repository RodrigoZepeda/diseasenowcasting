.make_count_cumulative_objective_fixture <- function() {
  observations <- data.frame(
    event = as.Date(c(
      "2024-01-06", "2024-01-06", "2024-01-06",
      "2024-01-13", "2024-01-13", "2024-01-20"
    )),
    report = as.Date(c(
      "2024-01-06", "2024-01-13", "2024-01-20",
      "2024-01-13", "2024-01-20", "2024-01-20"
    )),
    count = c(4, 6, 5, 2, 3, 1)
  )
  tbl.now::tbl_now(
    observations,
    event_date = event,
    report_date = report,
    case_count = count,
    data_type = "count-cumulative",
    event_units = "weeks",
    report_units = "weeks",
    now = max(observations$report),
    verbose = FALSE
  )
}

.build_count_cumulative_test_bundle <- function(observation, likelihood) {
  mod <- model(
    likelihood,
    ar1_epidemic(),
    lognormal_delay(),
    count_cumulative = count_cumulative_process(
      observation = observation,
      settlement = 6L
    )
  )
  engine <- diseasenowcasting:::prepare_from_tbl_now(
    .make_count_cumulative_objective_fixture(), mod,
    now = as.Date("2024-01-20")
  )$data
  priors <- default_priors(mod, engine)
  built <- diseasenowcasting:::build_joint_obj(
    engine, priors, use_random = FALSE
  )
  list(model = mod, engine = engine, priors = priors, built = built)
}

.build_count_cumulative_test_objective <- function(observation, likelihood) {
  .build_count_cumulative_test_bundle(observation, likelihood)$built$obj
}

test_that("all count-cumulative objectives have finite values and gradients", {
  specifications <- list(
    list("cumulative", poisson_likelihood()),
    list("cumulative", nb_likelihood()),
    list("hurdle_ztnb", nb_likelihood()),
    list("hurdle_ztpoisson", nb_likelihood())
  )

  for (specification in specifications) {
    objective <- .build_count_cumulative_test_objective(
      specification[[1L]], specification[[2L]]
    )
    expect_true(is.finite(objective$fn(objective$par)),
                info = specification[[1L]])
    expect_true(all(is.finite(objective$gr(objective$par))),
                info = specification[[1L]])
  }
})

test_that("hurdle ZTPoisson does not carry a magnitude dispersion parameter", {
  objective <- .build_count_cumulative_test_objective(
    "hurdle_ztpoisson", nb_likelihood()
  )
  expect_false(any(grepl("magnitude_size", names(objective$par))))
  expect_false(any(grepl("phi_nb", names(objective$par))))
})

test_that("count-cumulative reconstruction reports the finite-horizon objects", {
  for (observation in c("cumulative", "hurdle_ztnb", "hurdle_ztpoisson")) {
    bundle <- .build_count_cumulative_test_bundle(
      observation, nb_likelihood()
    )
    bundle$built$obj$fn(bundle$built$obj$par)
    reconstructed <- diseasenowcasting:::.joint_reconstruct(
      bundle$engine, bundle$priors,
      bundle$built$obj$env$parList(),
      bundle$built$Bmat, bundle$built$freq
    )
    component <- reconstructed$count_cumulative
    expect_null(reconstructed$confirmation)
    expect_equal(component$settlement_horizon, 6L)
    expect_length(component$h_R, 6L)
    expect_length(component$S_R, 7L)
    expect_length(component$q_C, 7L)
    expect_equal(sum(component$h_R), component$retraction_mass,
                 tolerance = 1e-10)
    expect_equal(component$terminal_retention, component$q_C[7L])
    if (observation == "hurdle_ztpoisson")
      expect_null(component$magnitude_size)
  }
})

test_that("anchored hurdle predictions preserve the future-update mean", {
  bundle <- .build_count_cumulative_test_bundle(
    "hurdle_ztpoisson", poisson_likelihood()
  )
  bundle$built$obj$fn(bundle$built$obj$par)
  reconstructed <- diseasenowcasting:::.joint_reconstruct(
    bundle$engine, bundle$priors,
    bundle$built$obj$env$parList(),
    bundle$built$Bmat, bundle$built$freq
  )
  data <- bundle$engine
  event <- data$max_time
  horizon <- max(which(data$observation_mask[event, , 1L])) - 1L
  anchor <- data$cumulative_level_array[event, horizon + 1L, 1L]
  future <- if (horizon < data$settlement_horizon)
    seq.int(horizon + 1L, data$settlement_horizon) else integer(0)
  component <- reconstructed$count_cumulative
  expected <- anchor + sum(
    reconstructed$lambda[event, 1L] *
      (component$alpha_unit[future + 1L] -
         component$omega_unit[future + 1L])
  )

  set.seed(20240904)
  simulations <- replicate(
    4000L,
    diseasenowcasting:::.draw_count_cumulative_terminal(
      data, reconstructed
    )$terminal[event, 1L]
  )
  # Projection is part of the public count-valued result, so compare with a
  # regime where it is rare and allow Monte Carlo error.
  expect_equal(mean(simulations), expected, tolerance = 0.2)
})
