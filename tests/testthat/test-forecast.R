# =============================================================================
# forecast(): carrying a fitted nowcast past `now`
# =============================================================================
# The forecast is the nowcast's own posterior draws with every latent recursion
# run `h` more steps.  The tests pin that down from both ends: the extended
# reconstruction must leave the fitted event times untouched and continue each
# recursion by exactly its own transition, and the predictive layer must split
# reports into the revision categories consistently.
# =============================================================================

quiet_nowcast <- function(...) suppressMessages(suppressWarnings(nowcast(...)))

.forecast_fit <- function(epidemic, data = .make_synth_tblnow(Tn = 60L, seed = 3),
                          likelihood = nb_likelihood(), n_draws = 200,
                          type = "one_stage", ...) {
  quiet_nowcast(data, model(likelihood, epidemic, lognormal_delay()),
                type = type, temporal_effects = "none", n_draws = n_draws,
                seed = 11, ...)
}

.mode_parlist <- function(result) {
  native <- diseasenowcasting:::.unwrap_nowcast(result)
  fit <- native@fits[[1]]
  list(fit = fit, native = native,
       parlist = .fill_fixed_parameters(fit$parList, fit$data, fit$priors))
}

test_that("an extended reconstruction leaves the fitted event times untouched", {
  processes <- list(ar1_epidemic(), arima_epidemic(), ets_epidemic(),
                    sts_epidemic(), random_walk_epidemic(), hsgp_epidemic(),
                    sir_epidemic(N_pop = 1e5))
  for (epidemic in processes) {
    pieces <- .mode_parlist(.forecast_fit(epidemic, n_draws = 20))
    fit <- pieces$fit
    in_sample <- .reconstruct_log_mean(fit$data, pieces$parlist, fit$Bmat,
                                       fit$freq, priors = fit$priors)
    set.seed(1)
    extended <- .reconstruct_log_mean(fit$data, pieces$parlist, fit$Bmat,
                                      fit$freq, horizon = 2L, priors = fit$priors)
    n_time <- fit$data$max_time
    expect_equal(dim(extended), c(n_time + 2L, 1L), info = epidemic@name)
    expect_equal(extended[seq_len(n_time), , drop = FALSE], in_sample,
                 info = epidemic@name)
    expect_true(all(is.finite(extended)), info = epidemic@name)
    # The reconstruction the nowcast draws from is the in-sample block.
    reconstructed <- .joint_reconstruct(fit$data, fit$priors, fit$parList,
                                        fit$Bmat, fit$freq)
    expect_equal(reconstructed$mu, in_sample, info = epidemic@name)
  }
})

test_that("the AR(1) forecast is exactly the AR(1) transition with fresh innovations", {
  pieces <- .mode_parlist(.forecast_fit(ar1_epidemic(), n_draws = 20))
  fit <- pieces$fit; parlist <- pieces$parlist
  n_time <- fit$data$max_time
  set.seed(42)
  extended <- .reconstruct_log_mean(fit$data, parlist, fit$Bmat, fit$freq,
                                    horizon = 3L, priors = fit$priors)
  set.seed(42)
  future_innovations <- stats::rnorm(3)

  phi <- -0.999 + 1.998 * stats::plogis(parlist$ar_phi_unc)
  sigma <- fit$data$ar_sigma_max * stats::plogis(parlist$log_ar_sigma_unc)
  trend <- extended[, 1] - parlist$mu_intercept
  for (k in 1:3) {
    expect_equal(trend[n_time + k],
                 phi * trend[n_time + k - 1] + sigma * future_innovations[k])
  }
})

test_that("classical trends forecast by continuing their own recursion", {
  # With the innovations zeroed after `now`, a damped ETS trend's forecast is
  # its deterministic point forecast: each step adds the damped slope.
  innovations <- c(0.3, -0.2, 0.5, 0.1)
  sigma <- 0.2; beta <- 0.4; damping <- 0.9; slope <- 0.05
  fitted <- ets_trend(innovations, sigma, beta, damping, 0, slope, TRUE)
  extended <- ets_trend(c(innovations, 0, 0), sigma, beta, damping, 0, slope, TRUE)
  expect_equal(extended[1:4], fitted)
  level <- 0; b <- slope
  for (t in 1:4) {
    expected <- level + damping * b
    level <- expected + sigma * innovations[t]
    b <- damping * b + beta * sigma * innovations[t]
  }
  expect_equal(extended[5], level + damping * b)
  expect_equal(extended[6], level + damping * b + damping^2 * b)

  # ARIMA(0, 1, 0) with drift: the zero-innovation forecast adds the drift.
  walk <- arima_trend(c(innovations, 0, 0), numeric(0), numeric(0), 0.3, 0.1, 1L)
  expect_equal(diff(walk[4:6]), c(0.1, 0.1))
})

test_that("forecast() returns a tbl_nowcast that runs from the nowcast into the future", {
  data <- .make_synth_tblnow(Tn = 60L, seed = 3)
  fitted <- .forecast_fit(ar1_epidemic(), data = data, n_draws = 150)
  result <- forecast(fitted, h = 3, seed = 1)

  expect_true(tbl.now::is_tbl_nowcast(result))
  event_col <- result@event_date
  dates <- sort(unique(result@predictions[[event_col]]))
  expect_equal(length(dates), 60L + 3L)
  expect_equal(as.numeric(diff(utils::tail(dates, 4))), c(1, 1, 1))
  expect_equal(max(dates), as.Date(fitted@now) + 3)
  expect_setequal(unique(result@predictions$.horizon), -59:3)
  expect_equal(nrow(result@draws), 150L * 63L)
  expect_true(all(result@draws$.value >= 0))

  forecast_meta <- result@metadata$diseasenowcasting$forecast
  expect_equal(forecast_meta$horizon, 3L)
  expect_equal(forecast_meta$category, "overall")
  expect_equal(as.Date(forecast_meta$forecast_dates), as.Date(fitted@now) + 1:3)

  only_future <- forecast(fitted, h = 2, include_nowcast = FALSE,
                          n_draws = 50, seed = 1)
  expect_setequal(unique(only_future@draws$.horizon), 1:2)
  expect_equal(nrow(only_future@draws), 100L)

  # The nowcast half of a forecast is the ordinary nowcast: same estimand, same
  # draws law.  Compare medians over the last weeks, where they differ most.
  nowcast_median <- stats::median(
    fitted@draws$.value[fitted@draws[[event_col]] == max(fitted@draws[[event_col]])])
  forecast_median <- stats::median(
    result@draws$.value[result@draws$.horizon == 0])
  expect_equal(forecast_median, nowcast_median, tolerance = 0.25)
})

test_that("forecast counts are the likelihood's draws around the extended latent mean", {
  fitted <- .forecast_fit(ar1_epidemic(), likelihood = poisson_likelihood(),
                          n_draws = 20)
  native <- diseasenowcasting:::.unwrap_nowcast(fitted)
  spec <- .forecast_spec(native, h = 1L)
  set.seed(5)
  pooled <- .pool_fit_draws(native@fits, native@target, n_draws = 3000L,
                            forecast = spec)
  counts <- pooled$forecast$strata[, 1, 1]
  latent <- pooled$forecast$lambda[, 1, 1]
  # Poisson: E[count] = E[lambda], Var[count] = E[lambda] + Var[lambda].
  expect_equal(mean(counts), mean(latent), tolerance = 0.03)
  expect_equal(stats::var(counts), mean(latent) + stats::var(latent),
               tolerance = 0.1)
  # One step past `now`, the latent mean continues from the nowcast's.
  expect_equal(stats::median(latent),
               stats::median(pooled$lambda[, native@engine$max_time]),
               tolerance = 0.3)
})

test_that("stratified and two-stage fits forecast every stratum", {
  data <- .make_strata_tblnow(Tn = 50L, seed = 2)
  fitted <- .forecast_fit(ar1_epidemic(), data = data, type = "two_stage",
                          n_draws = 100, K = 3L)
  result <- forecast(fitted, h = 2, include_nowcast = FALSE, seed = 1)
  expect_true("grp" %in% names(result@draws))
  expect_setequal(unique(result@draws$grp), c("A", "B"))
  expect_setequal(unique(result@draws$.horizon), 1:2)
  expect_true(all(is.finite(result@draws$.value)))
})

test_that("forecast() works on a saved and reloaded nowcast", {
  fitted <- .forecast_fit(ets_epidemic(), n_draws = 50)
  path <- withr::local_tempfile(fileext = ".rds")
  suppressMessages(save_nowcast(fitted, path))
  restored <- load_nowcast(path)
  result <- forecast(restored, h = 1, include_nowcast = FALSE, seed = 1)
  expect_equal(nrow(result@draws), 50L)
})

test_that("forecast() refuses what it cannot extend", {
  fitted <- .forecast_fit(ar1_epidemic(), n_draws = 20)
  expect_error(forecast(fitted, h = 0), "positive whole number")
  expect_error(forecast(fitted, h = 1.5), "positive whole number")
  expect_error(forecast(fitted, category = "sometimes"), "must be")
  expect_error(forecast(fitted, category = "confirmed"),
               class = "diseasenowcasting_forecast_category")

  # HSGP: the basis ends at the edge of its domain.
  hsgp <- .forecast_fit(hsgp_epidemic(), n_draws = 20)
  engine <- diseasenowcasting:::.unwrap_nowcast(hsgp)@engine
  expect_error(forecast(hsgp, h = 1000),
               class = "diseasenowcasting_forecast_unsupported")
  reachable <- sum(hsgp_time_scaled(engine$max_time + 1000L, engine$tmax_model) <
                     engine$gp_L_right) - engine$max_time
  expect_gt(reachable, 1)
  expect_no_error(forecast(hsgp, h = 1, n_draws = 10, include_nowcast = FALSE))

  # A custom epidemic returns a fixed-length matrix.
  data <- list(max_time = 5L, num_strata = 1L, epidemic_model = 4L)
  expect_error(.reconstruct_log_mean(data, list(custom_epidemic_params = 0),
                                     NULL, NULL, horizon = 1L),
               class = "diseasenowcasting_forecast_unsupported")

  prior <- quiet_nowcast(.make_synth_tblnow(Tn = 40L),
                         model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                         prior_only = TRUE, n_draws = 20, seed = 1)
  expect_error(forecast(prior), class = "diseasenowcasting_forecast_unsupported")
})

test_that("non-genuine reports drawn given the genuine ones make one NB report cloud", {
  # Genuine g ~ NB(m, r); the rest given g ~ Poisson(m (1-p)/p * Lambda) with
  # Lambda | g ~ Gamma(r + g, r + m).  Then g + rest ~ NB(m / p, r).
  set.seed(8)
  n <- 40000; m <- 30; p <- 0.7; phi <- 0.4; r <- 1 / phi
  genuine <- stats::rnbinom(n, size = r, mu = m)
  other <- as.numeric(.draw_complement_reports(
    matrix(genuine), matrix(m, n), matrix(m * (1 - p) / p, n), phi, TRUE))
  total <- genuine + other
  expect_equal(mean(total), m / p, tolerance = 0.02)
  expect_equal(stats::var(total), m / p + (m / p)^2 / r, tolerance = 0.05)
  expect_equal(sum(genuine) / sum(total), p, tolerance = 0.01)

  # Poisson: independent thinning.
  poisson_other <- as.numeric(.draw_complement_reports(
    matrix(genuine), matrix(m, n), matrix(5, n), NA_real_, FALSE))
  expect_equal(mean(poisson_other), 5, tolerance = 0.03)
  expect_equal(stats::cor(poisson_other, genuine), 0, tolerance = 0.02)
})

test_that("revision categories follow the mode and split the reports additively", {
  sim <- simulate_retraction_linelist(n_days = 60, p_true = 0.8, seed = 3)
  fitted <- fit_resolution(sim$linelist, sim$now, n_draws = 100)
  native <- diseasenowcasting:::.unwrap_nowcast(fitted)

  retraction_roles <- lapply(c("pending", "retracted", "overall"),
                             function(category) .forecast_category(native, category))
  expect_equal(vapply(retraction_roles, `[[`, "", "part"),
               c("target", "complement", "overall"))
  expect_equal(.forecast_category(native)$category, "pending")
  expect_error(forecast(fitted, category = "confirmed"),
               class = "diseasenowcasting_forecast_category")

  # One parameter draw, one seed: overall = target + complement, cell by cell,
  # in the nowcast and in the forecast alike.
  fit <- native@fits[[1]]
  data <- fit$data
  reconstructed <- .joint_reconstruct(data, fit$priors, fit$parList, fit$Bmat, fit$freq)
  n_time <- data$max_time
  genuine_mean <- matrix(reconstructed$lambda * (1 - reconstructed$Gstar), n_time, 1)
  future_genuine <- matrix(stats::rpois(n_time, genuine_mean), n_time, 1)
  pred_cells <- data$resolved_counts * 0 + future_genuine
  draw_part <- function(part) {
    spec <- list(horizon = 2L, X_future = NULL, part = part)
    set.seed(99)
    .forecast_draw_cells(data, fit$priors, fit$parList, fit, reconstructed, spec,
                         phi_nb = reconstructed$phi_nb, is_negbin = TRUE,
                         pred_cells = pred_cells, observed_all = data$case_counts,
                         future_genuine = future_genuine,
                         future_genuine_mean = genuine_mean)
  }
  target <- draw_part("target"); other <- draw_part("complement")
  overall <- draw_part("overall")
  expect_equal(overall$future, target$future + other$future)
  expect_equal(overall$past, pred_cells + other$past)
  expect_equal(overall$future_lambda, target$future_lambda / reconstructed$retraction$p)

  # And through the public interface, the overall forecast exceeds the
  # never-retracted one by roughly 1/p.
  overall_result <- forecast(fitted, h = 1, category = "overall",
                             include_nowcast = FALSE, n_draws = 2000, seed = 1)
  target_result <- forecast(fitted, h = 1, include_nowcast = FALSE,
                            n_draws = 2000, seed = 1)
  p_hat <- reconstructed$retraction$p
  expect_equal(mean(target_result@draws$.value) / mean(overall_result@draws$.value),
               p_hat, tolerance = 0.1)
  expect_match(overall_result@metadata$diseasenowcasting$forecast$estimand,
               "all reports")
})

test_that("confirmation-only and both-sign fits expose their own categories", {
  sim <- simulate_both_signs_linelist(n_days = 50, seed = 4)
  both <- fit_resolution(sim$linelist, sim$now, n_draws = 50)
  both_native <- diseasenowcasting:::.unwrap_nowcast(both)
  expect_equal(.forecast_category(both_native)$category, "confirmed")
  expect_equal(.forecast_category(both_native, "retracted")$part, "complement")
  expect_error(.forecast_category(both_native, "pending"),
               class = "diseasenowcasting_forecast_category")

  confirmation_only <- sim$linelist
  confirmation_only$retracted <- as.Date(NA)
  confirmed <- fit_resolution(confirmation_only, sim$now, n_draws = 50)
  confirmed_native <- diseasenowcasting:::.unwrap_nowcast(confirmed)
  expect_equal(.forecast_category(confirmed_native)$category, "confirmed")
  expect_equal(.forecast_category(confirmed_native, "pending")$part, "complement")
  expect_error(.forecast_category(confirmed_native, "retracted"),
               class = "diseasenowcasting_forecast_category")

  pending <- forecast(confirmed, h = 1, category = "pending", n_draws = 100, seed = 2)
  expect_true(all(pending@draws$.value >= 0))
  expect_match(pending@metadata$diseasenowcasting$forecast$estimand, "never confirmed")
})

test_that("temporal effects and event covariates are extended to the forecast dates", {
  data <- .make_synth_tblnow(Tn = 60L, seed = 3)
  seasonal <- quiet_nowcast(
    data, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
    type = "one_stage", temporal_effects = "auto", n_draws = 20, seed = 1)
  native <- diseasenowcasting:::.unwrap_nowcast(seasonal)
  X_future <- .forecast_design(native, 3L)
  expect_equal(dim(X_future), c(3L, native@engine$P))
  # Day-of-week dummies repeat with period seven.
  X_long <- .forecast_design(native, 7L + 3L)
  expect_equal(X_long[8:10, ], X_long[1:3, ], ignore_attr = TRUE)
  expect_no_error(forecast(seasonal, h = 2, n_draws = 10, include_nowcast = FALSE))

  frame <- as.data.frame(data)[c("onset", "reported")]
  frame$mobility <- as.numeric(frame$onset - min(frame$onset)) / 10
  with_covariate <- tbl.now::tbl_now(frame, event_date = onset, report_date = reported,
                                     covariates = mobility, data_type = "linelist",
                                     verbose = FALSE)
  covariate_fit <- quiet_nowcast(
    with_covariate, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
    type = "one_stage", temporal_effects = "none", n_draws = 20, seed = 1)
  expect_error(forecast(covariate_fit, h = 2),
               class = "diseasenowcasting_forecast_covariates")
  future_dates <- as.Date(covariate_fit@now) + 1:2
  expect_error(forecast(covariate_fit, h = 2,
                        new_data = data.frame(onset = future_dates[1], mobility = 6)),
               class = "diseasenowcasting_forecast_covariates")
  new_data <- data.frame(onset = future_dates, mobility = c(6, 6.1))
  covariate_native <- diseasenowcasting:::.unwrap_nowcast(covariate_fit)
  expect_equal(unname(.forecast_design(covariate_native, 2L, new_data)[, 1]),
               c(6, 6.1))
  expect_no_error(forecast(covariate_fit, h = 2, new_data = new_data,
                           n_draws = 10, include_nowcast = FALSE))
  # Dates given as text are read on the same calendar.
  text_dates <- transform(new_data, onset = as.character(onset))
  expect_equal(.forecast_design(covariate_native, 2L, text_dates),
               .forecast_design(covariate_native, 2L, new_data))
})

test_that("count-cumulative cohorts are forecast from an empty level", {
  # The level-composite update law telescopes: from zero, the settled level has
  # mean lambda * q_C(H).
  report_pmf <- c(0.6, 0.25, 0.1, 0.05)
  components <- .count_cumulative_components(report_pmf, c(0.5, 0.3, 0.2), 0.1, 3L)
  cc <- c(list(observation = 1L, settlement_horizon = 3L), components)
  set.seed(3)
  draws <- replicate(4000, .draw_count_cumulative_future(matrix(50), cc))
  expect_equal(mean(draws), 50 * components$terminal_retention, tolerance = 0.02)
  expect_true(all(draws >= 0))

  rows <- tbl.now::flusight |>
    dplyr::filter(location_name == "California",
                  target_end_date >= as.Date("2023-10-01"),
                  target_end_date <= as.Date("2024-01-06"),
                  as_of <= as.Date("2024-01-06"))
  x <- tbl.now::tbl_now(rows, event_date = target_end_date, report_date = as_of,
                        case_count = observation, data_type = "count-cumulative",
                        event_units = "weeks", report_units = "weeks",
                        align_weeks = TRUE, verbose = FALSE)
  fitted <- quiet_nowcast(
    x, model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
             cumulative = cumulative_process(observation = "cumulative",
                                             settlement = 8L)),
    temporal_effects = "none", n_draws = 50, seed = 1)
  result <- forecast(fitted, h = 1, n_draws = 50, seed = 1)
  expect_match(result@metadata$diseasenowcasting$forecast$estimand, "C_t\\(8\\)")
  future <- result@draws$.value[result@draws$.horizon == 1]
  expect_length(future, 50L)
  expect_true(all(future >= 0))
  expect_error(forecast(fitted, category = "retracted"),
               class = "diseasenowcasting_forecast_category")
})
