test_that("count_cumulative_process exposes all three finite-horizon models", {
  expected <- c("cumulative", "hurdle_ztnb", "hurdle_ztpoisson")
  for (observation in expected) {
    process <- count_cumulative_process(
      observation = observation,
      settlement = 26L
    )
    expect_s7_class(
      process,
      diseasenowcasting:::count_cumulative_process_class
    )
    expect_identical(process@observation, observation)
    expect_equal(process@settlement, 26)
    expect_true(process@active)
  }
})

test_that("count_cumulative_process validates horizons, delays, and parameter support", {
  expect_error(count_cumulative_process(settlement = 0), "positive integer")
  expect_error(count_cumulative_process(settlement = 2.5), "positive integer")
  expect_error(
    count_cumulative_process(retraction_delay = dirichlet_delay()),
    "Unsupported count-cumulative retraction-delay family"
  )
  expect_error(count_cumulative_process(retraction_mass = 1.1), "in \\[0, 1\\]")
  expect_error(count_cumulative_process(retraction_mass = normal_prior(0, 1)),
               "Beta prior")
  expect_error(count_cumulative_process(magnitude_size = 0), "positive")
  expect_error(
    count_cumulative_process(
      "hurdle_ztpoisson", magnitude_size = lognormal_prior(0, 1)
    ),
    "no magnitude-dispersion"
  )
})

test_that("model carries an inert or explicit dedicated count-cumulative component", {
  ordinary <- model()
  expect_false(ordinary@count_cumulative@active)

  process <- count_cumulative_process(
    observation = "hurdle_ztpoisson", settlement = 52L,
    movement_previous = 0
  )
  cumulative_model <- model(count_cumulative = process)
  expect_true(cumulative_model@count_cumulative@active)
  expect_identical(cumulative_model@count_cumulative@observation,
                   "hurdle_ztpoisson")
  expect_equal(cumulative_model@count_cumulative@settlement, 52)
  expect_no_error(print(cumulative_model))
})

test_that("count-cumulative priors are distinct from validation p and g_C", {
  ztnb_model <- model(count_cumulative = count_cumulative_process(
    observation = "hurdle_ztnb",
    retraction_delay = generalized_gamma_delay(),
    settlement = 26L
  ))
  priors <- default_priors(ztnb_model)
  expect_identical(priors$count_cumulative_observation, 2L)
  expect_identical(priors$count_cumulative_settlement, 26L)
  expect_false(is.null(priors$retraction_mass))
  expect_false(is.null(priors$count_cumulative_retraction_mu))
  expect_false(is.null(priors$count_cumulative_retraction_sigma))
  expect_false(is.null(priors$count_cumulative_retraction_Q))
  expect_false(is.null(priors$movement_intercept))
  expect_false(is.null(priors$movement_age))
  expect_false(is.null(priors$movement_previous))
  expect_false(is.null(priors$magnitude_size))
  expect_null(priors$confirm_p)

  ztpoisson <- default_priors(model(
    count_cumulative = count_cumulative_process("hurdle_ztpoisson")
  ))
  expect_identical(ztpoisson$count_cumulative_observation, 3L)
  expect_null(ztpoisson$magnitude_size)
})
