# =============================================================================
# nowcast(marginal_latent =): joint mode versus Laplace-marginal innovations
# =============================================================================
# The latent epidemic innovations are non-centred: the path is sigma * z with
# z ~ N(0, 1).  A joint mode over (sigma, z) can shrink z and inflate sigma,
# which a forecast then applies to fresh N(0, 1) innovations.  Integrating z
# out (RTMB `random=`) estimates sigma from the marginal likelihood instead.
# Forecast spreads shrink sharply.  Nowcasts anchored on a published level
# (count-cumulative offset fits) barely move; line-list nowcasts of the newest
# days, which lean on the latent path, improve.
# =============================================================================

quiet_nowcast <- function(...) suppressMessages(suppressWarnings(nowcast(...)))

.rw_nowcast <- function(x, marginal_latent = NULL) {
  quiet_nowcast(x, model(nb_likelihood(), random_walk_epidemic(), lognormal_delay()),
                type = "one_stage", temporal_effects = "none", n_draws = 300,
                seed = 1, marginal_latent = marginal_latent)
}

.rw_sigma <- function(result) {
  fit <- .unwrap_nowcast(result)@fits[[1]]
  fit$data$ar_sigma_max * stats::plogis(fit$obj$env$parList()$log_ets_sigma_unc[1])
}

test_that("marginal_latent is validated", {
  x <- .make_synth_tblnow(Tn = 40L)
  for (bad in list("yes", NA, c(TRUE, FALSE), 1))
    expect_error(.rw_nowcast(x, marginal_latent = bad), "must be")
})

test_that("marginal_latent selects the fit and NULL keeps the automatic choice", {
  x <- .make_synth_tblnow(Tn = 60L)
  use_random <- function(result) .unwrap_nowcast(result)@fits[[1]]$use_random
  expect_false(use_random(.rw_nowcast(x)))
  expect_false(use_random(.rw_nowcast(x, marginal_latent = FALSE)))
  expect_true(use_random(.rw_nowcast(x, marginal_latent = TRUE)))
})

test_that("integrating the innovations out stops a random walk's sigma inflating", {
  x <- .make_synth_tblnow(Tn = 60L)
  joint <- .rw_nowcast(x)
  marginal <- .rw_nowcast(x, marginal_latent = TRUE)
  # The synthetic epidemic is a smooth curve: its weekly log-changes are small.
  expect_lt(.rw_sigma(marginal), 0.5 * .rw_sigma(joint))

  spread <- function(result) {
    draws <- forecast(result, h = 10, n_draws = 300, seed = 2,
                      include_nowcast = FALSE)@draws
    last <- draws$.value[draws$.horizon == 10]
    unname(stats::quantile(last, 0.95) / max(stats::quantile(last, 0.05), 1))
  }
  expect_lt(spread(marginal), spread(joint) / 10)

  # A line-list nowcast of the newest days leans on the latent path, so the
  # inflated sigma costs it accuracy too: the marginal fit's is closer to the
  # simulated mean of the last five days.
  truth <- 40 * exp(-0.5 * ((56:60 - 55) / 18)^2) + 5
  nowcast_error <- function(result) {
    draws <- forecast(result, h = 1, n_draws = 300, seed = 3)@draws
    draws <- draws[draws$.horizon %in% -4:0, ]
    mean(abs(tapply(draws$.value, draws$.horizon, stats::median) - truth))
  }
  expect_lt(nowcast_error(marginal), nowcast_error(joint))
})

test_that("update() keeps the requested fit", {
  x <- .make_synth_tblnow(Tn = 60L)
  rows <- as.data.frame(x)
  cutoff <- max(rows$reported) - 5
  old <- tbl.now::tbl_now(rows[rows$reported <= cutoff, c("onset", "reported")],
                          event_date = onset, report_date = reported,
                          data_type = "linelist", verbose = FALSE)
  fitted <- .rw_nowcast(old, marginal_latent = TRUE)
  updated <- suppressMessages(suppressWarnings(
    update(fitted, rows[rows$reported > cutoff, c("onset", "reported")],
           compute_surprise = FALSE)))
  expect_true(.unwrap_nowcast(updated)@fits[[1]]$use_random)
})
