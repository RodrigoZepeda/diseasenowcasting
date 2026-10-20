# A joint mode of standardized innovations omits their integration volume and
# can inflate the process SD. Simulate each process at a known SD, then check
# both observation types using the same complete counts.

test_that("marginal fitting recovers innovation SD across epidemic processes", {
  sigma <- 0.25
  n_time <- 48L
  dates <- as.Date("2023-01-07") + 7L * (seq_len(n_time) - 1L)
  processes <- list(
    arima = list(
      epidemic = arima_epidemic(p = 0, d = 1, q = 0),
      sigma_name = "log_arima_sigma_unc",
      trend = function(z, w) arima_trend(z, numeric(0), numeric(0),
                                         sigma, 0, 1L)
    ),
    ar1 = list(
      epidemic = ar1_epidemic(phi = 0.7),
      sigma_name = "log_ar_sigma_unc",
      trend = function(z, w) {
        out <- numeric(length(z))
        out[1L] <- sigma * z[1L] / sqrt(1 - 0.7^2)
        for (t in 2:length(z)) out[t] <- 0.7 * out[t - 1L] + sigma * z[t]
        out
      }
    ),
    ets = list(
      epidemic = ets_epidemic(beta = 0.2, damping = 0.9, slope_init = 0.02),
      sigma_name = "log_ets_sigma_unc",
      trend = function(z, w) ets_trend(z, sigma, 0.2, 0.9, 0, 0.02, TRUE)
    ),
    sts = list(
      epidemic = sts_epidemic(slope_sigma = 0.02, slope_phi = 0.7,
                             slope_mean = 0, slope_init = 0.02),
      sigma_name = "log_sts_level_sigma_unc",
      trend = function(z, w) sts_trend(z, w, sigma, 0.02, 0.7, 0, 0.02)
    )
  )
  cumulative <- cumulative_process(
    observation = "hurdle", settlement = 4L,
    initial_report = "offset", initial_size = 1000,
    retraction_delay = lognormal_delay(mu = -5, sigma = 0.1),
    retraction_mass = 0.001, movement_intercept = -3,
    movement_age = 0, movement_previous = 0
  )

  for (name in names(processes)) {
    process <- processes[[name]]
    set.seed(144)
    innovations <- stats::rnorm(n_time)
    slope_innovations <- stats::rnorm(n_time)
    lambda <- exp(log(500) + process$trend(innovations, slope_innovations))
    counts <- stats::rpois(n_time, lambda)
    incidence <- tbl.now::tbl_now(
      data.frame(event = dates, report = dates, count = counts),
      event_date = event, report_date = report, case_count = count,
      data_type = "count-incidence", units = "weeks", verbose = FALSE
    )
    snapshots <- do.call(rbind, lapply(seq_len(n_time), function(t) {
      age <- 0:min(4L, n_time - t)
      data.frame(event = dates[t], report = dates[t] + 7L * age,
                 count = rep(counts[t], length(age)))
    }))
    count_cumulative <- tbl.now::tbl_now(
      snapshots, event_date = event, report_date = report,
      case_count = count, data_type = "count-cumulative",
      units = "weeks", verbose = FALSE
    )

    for (type in c("incidence", "cumulative")) {
      is_cumulative <- identical(type, "cumulative")
      mod <- if (is_cumulative) {
        model(poisson_likelihood(), process$epidemic,
              lognormal_delay(mu = -5, sigma = 0.1), cumulative = cumulative)
      } else {
        model(poisson_likelihood(), process$epidemic,
              lognormal_delay(mu = -5, sigma = 0.1))
      }
      data <- if (is_cumulative) count_cumulative else incidence
      engine <- prepare_from_tbl_now(data, mod)$data
      priors <- default_priors(mod, engine)
      marginal <- suppressWarnings(.fit_joint(mod, engine, priors,
                                               n_tries = 2L, warn = FALSE))
      joint <- suppressWarnings(.fit_joint(mod, engine, priors,
                                            n_tries = 2L, use_random = FALSE,
                                            warn = FALSE))
      marginal_sigma <- stats::plogis(marginal$parList[[process$sigma_name]])
      joint_sigma <- stats::plogis(joint$parList[[process$sigma_name]])
      context <- paste(name, type)
      expect_true(marginal$use_random, info = context)
      expect_true(marginal$hessian_positive_definite, info = context)
      expect_lt(abs(marginal_sigma - sigma), 0.1)
      expect_gt(joint_sigma / marginal_sigma, 1.25)

      if (identical(name, "arima")) {
        result <- suppressWarnings(suppressMessages(nowcast(
          data, mod, type = "one_stage", temporal_effects = "none",
          n_draws = 30L, seed = 144L
        )))
        sigma_row <- as.data.frame(parameters(result))
        sigma_row <- sigma_row[
          sigma_row$term == process$sigma_name, , drop = FALSE
        ]
        expect_equal(nrow(sigma_row), 1L)
        expect_true(is.finite(sigma_row$std.error))
        future <- forecast(result, h = 2L, n_draws = 30L, seed = 144L)
        expect_true(all(is.finite(future@predictions$.value)))
      }
    }
  }
})
