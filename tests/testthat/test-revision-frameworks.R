# Revision is an observation layer, so it must compose with every epidemic
# process. The mode-specific algebra is tested in test-confirmation-mode.R and
# test-resolution-math.R; this file checks the orthogonal epidemic-process axis.

.revision_framework_fixture <- function() {
  n_time <- 35L
  simulated <- simulate_both_signs_linelist(
    n_days = n_time, p_true = 0.7, lag_mean = 1.5, seed = 121
  )
  list(
    n_time = n_time,
    simulated = simulated,
    data = as_revision_tbl_now(simulated$linelist, simulated$now)
  )
}

.fit_revision_framework <- function(epidemic, fixture) {
  specification <- model(
    poisson_likelihood(), epidemic,
    lognormal_delay(mu = log(3), sigma = 0.6),
    revision = revision_process(
      lognormal_revision(mu = log(1.5), sigma = 0.7),
      p = 0.7,
      mode = "both"
    )
  )
  suppressMessages(suppressWarnings(nowcast(
    fixture$data, specification, now = fixture$simulated$now,
    type = "one_stage", temporal_effects = "none", n_draws = 20, seed = 122
  )))
}

.expect_revision_framework_fit <- function(fitted, framework) {
  expect_equal(fitted@revision_mode, "both", info = framework)
  expect_equal(fitted@fits[[1]]$opt$convergence, 0L, info = framework)
  expect_true(all(is.finite(median(fitted))), info = framework)
}

test_that("revision nowcasting composes with built-in epidemic processes", {
  skip_on_cran()
  fixture <- .revision_framework_fixture()
  processes <- list(
    hsgp = hsgp_epidemic(alpha = 0.5, ell = 5, num_basis = 6),
    ar = ar1_epidemic(phi = 0.7, sigma = 0.1),
    sir = sir_epidemic(R0 = 2, gamma = 0.2, N_eff = 0.8,
                       N_pop = 100000, use_beta_rw_trend = FALSE)
  )

  for (framework in names(processes)) {
    fitted <- .fit_revision_framework(processes[[framework]], fixture)
    .expect_revision_framework_fit(fitted, framework)
  }
})

test_that("revision nowcasting composes with a custom epidemic process", {
  skip_on_cran()
  library(RTMB)

  # Under load_all(), RTMB's vector-operation dispatch for user functions can be
  # inactive; the package's custom-component tests use the same probe. The
  # installed-package R CMD check exercises this path when dispatch is active.
  process_taping_ok <- isTRUE(tryCatch({
    probe_fn <- function(theta) {
      matrix(theta[1] + cumsum(exp(theta[2]) * theta[3:4]), 2L, 1L)
    }
    probe <- custom_epidemic(
      probe_fn, priors = rep(list(normal_prior(0, 1)), 4L),
      inits = rep(0.1, 4L)
    )
    validate_custom_epidemic(probe)
    TRUE
  }, error = function(e) FALSE))
  skip_if_not(
    process_taping_ok,
    "RTMB user-function dispatch inactive (load_all); runs under R CMD check"
  )

  fixture <- .revision_framework_fixture()
  constant_intensity <- function(theta) {
    matrix(theta[1] + rep(0, fixture$n_time), fixture$n_time, 1L)
  }
  epidemic <- custom_epidemic(
    constant_intensity,
    priors = list(normal_prior(log(40), 1)),
    name = "constant", param_names = "log_mean", inits = log(40)
  )
  fitted <- .fit_revision_framework(epidemic, fixture)
  .expect_revision_framework_fit(fitted, "custom")
})
