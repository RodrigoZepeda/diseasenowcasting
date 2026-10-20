test_that("cumulative_process exposes the hurdle and plain models", {
  for (observation in c("hurdle", "plain")) {
    process <- cumulative_process(
      observation = observation,
      settlement = 26L
    )
    expect_s7_class(
      process,
      diseasenowcasting:::cumulative_process_class
    )
    expect_identical(process@observation, observation)
    expect_equal(process@settlement, 26)
    expect_true(process@active)
  }
  expect_error(cumulative_process("hurdle_ztnb"))
  expect_error(cumulative_process("cumulative"))
})

test_that("cumulative_process validates horizons, delays, and parameter support", {
  expect_error(cumulative_process(settlement = 0), "positive integer")
  expect_error(cumulative_process(settlement = 2.5), "positive integer")
  expect_error(
    cumulative_process(retraction_delay = dirichlet_delay()),
    "Unsupported count-cumulative retraction-delay family"
  )
  expect_error(cumulative_process(retraction_mass = 1.1), "in \\[0, 1\\]")
  expect_error(cumulative_process(retraction_mass = normal_prior(0, 1)),
               "Beta prior")
  expect_error(cumulative_process(magnitude_size = 0), "positive")
  expect_error(
    cumulative_process("plain", magnitude_size = lognormal_prior(0, 1)),
    "only by the hurdle"
  )
})

test_that("the likelihood sets the hurdle magnitude law", {
  expect_error(
    model(poisson_likelihood(), cumulative = cumulative_process(
      "hurdle", magnitude_size = lognormal_prior(0, 1)
    )),
    "no magnitude-dispersion"
  )
  expect_error(
    model(poisson_likelihood(), cumulative = cumulative_process(
      "hurdle", revision_magnitude_size = 2
    )),
    "no magnitude-dispersion"
  )
  expect_no_error(model(nb_likelihood(), cumulative = cumulative_process(
    "hurdle", magnitude_size = 2, revision_magnitude_size = 2
  )))

  codes <- vapply(list(
    list(nb_likelihood(), "hurdle"),
    list(poisson_likelihood(), "hurdle"),
    list(nb_likelihood(), "plain"),
    list(poisson_likelihood(), "plain")
  ), function(specification) {
    default_priors(model(
      specification[[1L]],
      cumulative = cumulative_process(specification[[2L]])
    ))$count_cumulative_observation
  }, integer(1))
  expect_identical(codes, c(2L, 3L, 1L, 1L))
})

test_that("model carries an inert or explicit dedicated count-cumulative component", {
  ordinary <- model()
  expect_false(ordinary@cumulative@active)

  process <- cumulative_process(
    observation = "hurdle", settlement = 52L,
    movement_previous = 0
  )
  cumulative_model <- model(poisson_likelihood(), cumulative = process)
  expect_true(cumulative_model@cumulative@active)
  expect_identical(cumulative_model@cumulative@observation, "hurdle")
  expect_equal(cumulative_model@cumulative@settlement, 52)
  expect_no_error(cli::cli_fmt(print(cumulative_model)))
  expect_match(cli::cli_fmt(print(cumulative_model)), "ZTPoisson", all = FALSE)
  expect_match(cli::cli_fmt(print(model(cumulative = process))), "ZTNB",
               all = FALSE)
  expect_no_error(cli::cli_fmt(print(process)))
})

test_that("count-cumulative priors are distinct from revision p and g_C", {
  ztnb_model <- model(nb_likelihood(), cumulative = cumulative_process(
    observation = "hurdle",
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
    poisson_likelihood(), cumulative = cumulative_process("hurdle")
  ))
  expect_identical(ztpoisson$count_cumulative_observation, 3L)
  expect_null(ztpoisson$magnitude_size)
})
