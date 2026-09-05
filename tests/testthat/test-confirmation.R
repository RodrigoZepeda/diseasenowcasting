# Tests for the count-cumulative confirmation model: the Skellam / SkNB
# signed-increment densities (R/28_confirmation_likelihood.R), the
# validation_process() class + priors, the count-cumulative de-accumulation,
# and one small end-to-end nowcast -> predict.
#
# The unit tests below use only plain-numeric arithmetic, but the densities are
# imported from RTMB (dpois etc.); attach RTMB explicitly (it is in Imports, not
# Depends, so loading the package does not attach it) as the other RTMB-touching
# test files do.
library(RTMB)

# ── validation_process() constructor and validator ─────────────────────────

test_that("validation_process() default is a valid, active component with unset p", {
  conf <- validation_process()
  expect_s3_class(conf, "diseasenowcasting::validation_process_class")
  expect_true(conf@active)
  # p unset -> length-0 numeric, resolved to a data-informed prior in default_priors()
  expect_true(is.numeric(conf@p) && length(conf@p) == 0L)
})

test_that("validation_process() accepts a fixed p in (0, 1] and a prior on p", {
  fixed <- validation_process(p = 0.97)
  expect_equal(fixed@p, 0.97)

  estimated <- validation_process(p = beta_prior(50, 1))
  expect_true(S7::S7_inherits(estimated@p, diseasenowcasting:::prior_class))
})

test_that("validation_process() rejects p outside (0, 1]", {
  expect_error(validation_process(p = 0), "in \\(0, 1\\]")
  expect_error(validation_process(p = 1.5), "in \\(0, 1\\]")
  expect_error(validation_process(p = "0.9"), "prior_class|numeric")
})

test_that("no_validation() is the inert p = 1 default", {
  inert <- diseasenowcasting:::no_validation()
  expect_false(inert@active)
  expect_equal(inert@p, 1)
})

test_that("model() carries a validation component (inert by default, active when supplied)", {
  default_model <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay())
  expect_false(default_model@validation@active)

  confirm_model <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
                         validation = validation_process())
  expect_true(confirm_model@validation@active)
})

# ── Skellam log-pmf: accuracy across the whole (alpha, beta, m) space ───────

# High-precision reference: the ascending series with far more terms than any peak
# needs.  It runs off the AD tape, so its cost does not matter, and it is in log
# space, so it never underflows -- unlike `besselI()`, which cannot serve as a
# reference here because it returns 0 across the bulk of an asymmetric Skellam.
reference_log_skellam <- function(m, alpha, beta, n_terms = 20000L) {
  alpha <- alpha + 1e-8
  beta  <- beta + 1e-8
  order <- abs(m)
  argument <- 2 * sqrt(alpha * beta)
  k <- 0:n_terms
  log_terms <- (2 * k + order) * log(argument / 2) -
    lgamma(k + 1) - lgamma(k + order + 1)
  largest <- max(log_terms)
  -(alpha + beta) + (m / 2) * log(alpha / beta) +
    largest + log(sum(exp(log_terms - largest)))
}

# The cells a real count-cumulative fit actually visits, taken from the event time
# that used to abort the fit.  Small rates, small increments -- the corner the
# saddlepoint is weakest in and the series carries.
real_fit_cells <- list(
  c(81, 78.31, 1e-9), c(101, 85.49, 2.577e-5), c(26, 26.87, 0.0684),
  c(8, 8.174, 0.5655), c(3, 2.725, 1.03), c(0, 0.9986, 0.8686),
  c(-1, 0.1701, 0.1878), c(0, 0.07725, 0.06962), c(0, 0.001641, 0.000346))

test_that(".log_skellam_increment is accurate across the bulk of the distribution", {
  # Both historical failure modes are covered by these rate pairs.  A fixed-length
  # ascending series was 5729 nats low at Skellam(20000, 500) (its terms peak at the
  # ARGUMENT, not the order); besselI returns 0 across the bulk of the asymmetric
  # pairs.  The saddlepoint has neither weakness.
  rate_pairs <- list(c(20, 3), c(100, 5), c(150, 140), c(1000, 20),
                     c(5000, 100), c(20000, 500), c(1e5, 1e4))
  for (rates in rate_pairs) {
    alpha <- rates[1]; beta <- rates[2]
    for (sd_offset in c(-6, -4, -2, -1, 0, 1, 2, 4, 6)) {
      increment <- round((alpha - beta) + sd_offset * sqrt(alpha + beta))
      expect_equal(
        as.numeric(diseasenowcasting:::.log_skellam_increment(increment, alpha, beta, 1L)),
        reference_log_skellam(increment, alpha, beta),
        tolerance = 1e-3,
        label = sprintf("Skellam(%g, %g) at %+g sd", alpha, beta, sd_offset))
    }
  }
})

test_that(".log_skellam_increment is accurate on the cells a real fit visits", {
  # The saddlepoint degrades as alpha + beta -> 0 (2.3 nats at alpha = 0.0016);
  # these assert the series branch is picking those up.
  for (cell in real_fit_cells) {
    expect_equal(
      as.numeric(diseasenowcasting:::.log_skellam_increment(cell[1], cell[2], cell[3], 1L)),
      reference_log_skellam(cell[1], cell[2], cell[3]),
      tolerance = 1e-3,
      label = sprintf("Skellam(%g, %g) at m = %g", cell[2], cell[3], cell[1]))
  }
})

test_that(".log_skellam_increment has a finite gradient everywhere, including sparse retractions", {
  # Regression for the abort this used to cause.  `log(besselI + 1e-323)` has a
  # finite VALUE but a `1/1e-323 = Inf` derivative, and besselI's own derivative
  # underflows to 0 there, so `0 * Inf = NaN` -- and because the increment densities
  # are summed, one such cell poisoned every gradient entry.  Fits then aborted with
  # "NA/NaN gradient evaluation" whenever retractions were sparse, which is the
  # realistic regime (p ~ 0.98).
  skip_if_not_installed("RTMB")
  cells <- c(real_fit_cells,
             list(c(2424, 1e-8, 0.02), c(980, 1000, 20), c(10, 20000, 500),
                  c(0, 1000, 20), c(-30, 5000, 100)))
  for (cell in cells) {
    objective <- function(par) {
      -diseasenowcasting:::.log_skellam_increment(cell[1], exp(par[1]), exp(par[2]), 1L)
    }
    taped <- suppressWarnings(
      RTMB::MakeADFun(objective, c(log(cell[2]), log(cell[3])), silent = TRUE))
    expect_true(is.finite(taped$fn()),
                label = sprintf("value at m = %g, alpha = %g", cell[1], cell[2]))
    expect_true(all(is.finite(suppressWarnings(taped$gr()))),
                label = sprintf("gradient at m = %g, alpha = %g", cell[1], cell[2]))
  }
})

test_that("the Skellam log-pmf integrates to one over its support", {
  # The check no single method passes: a truncated series makes this fall short and
  # a floored besselI makes it diverge.
  for (rates in list(c(20, 3), c(1000, 20), c(5000, 100))) {
    alpha <- rates[1]; beta <- rates[2]
    spread <- ceiling(8 * sqrt(alpha + beta))
    support <- seq(round(alpha - beta) - spread, round(alpha - beta) + spread)
    total <- sum(exp(vapply(support, function(m)
      as.numeric(diseasenowcasting:::.log_skellam_increment(m, alpha, beta, 1L)),
      numeric(1))))
    expect_equal(total, 1, tolerance = 1e-3,
                 label = sprintf("total mass of Skellam(%g, %g)", alpha, beta))
  }
})

test_that("the saddlepoint root is stable for negative increments", {
  # `(m + sqrt(m^2 + 4 alpha beta)) / (2 alpha)` loses all precision for m < 0; the
  # conjugate form is used there instead.  A symmetric Skellam must therefore give
  # the same answer at -m as the mirrored parameters give at +m.
  for (m in c(-1, -10, -200)) {
    expect_equal(
      as.numeric(diseasenowcasting:::.log_skellam_increment(m, 40, 900, 1L)),
      as.numeric(diseasenowcasting:::.log_skellam_increment(-m, 900, 40, 1L)),
      tolerance = 1e-10,
      label = sprintf("Skellam symmetry at m = %g", m))
  }
})

# ── Skellam increment log-density ────────────────────────────────────────────

test_that(".log_skellam_increment pure-addition / pure-retraction reduce to Poisson", {
  # bin_type 0: pure addition -> Poisson(alpha); bin_type 2: pure retraction ->
  # Poisson(beta) on the negated increment.  A tiny mean floor is applied.
  expect_equal(diseasenowcasting:::.log_skellam_increment(5, 10, 0, 0L),
               dpois(5, 10 + 1e-8, log = TRUE), tolerance = 1e-9)
  expect_equal(diseasenowcasting:::.log_skellam_increment(-4, 0, 7, 2L),
               dpois(4, 7 + 1e-8, log = TRUE), tolerance = 1e-9)
})

test_that(".log_skellam_increment mixed case equals the exact Poisson-difference convolution", {
  # P(N+ - N- = m) = sum_k Pois(k+m; alpha) Pois(k; beta) for m >= 0 (swap for m<0).
  skellam_reference <- function(m, alpha, beta) {
    k <- 0:200
    if (m >= 0) {
      terms <- dpois(k + m, alpha, log = TRUE) + dpois(k, beta, log = TRUE)
    } else {
      terms <- dpois(k - m, beta, log = TRUE) + dpois(k, alpha, log = TRUE)
    }
    max_term <- max(terms)
    max_term + log(sum(exp(terms - max_term)))
  }
  for (m in c(-3L, -1L, 0L, 2L, 6L)) for (params in list(c(2, 1), c(5, 3), c(8, 0.5))) {
    alpha <- params[1]; beta <- params[2]
    expect_equal(diseasenowcasting:::.log_skellam_increment(m, alpha, beta, 1L),
                 skellam_reference(m, alpha, beta), tolerance = 1e-5,
                 label = sprintf("Skellam m=%d alpha=%g beta=%g", m, alpha, beta))
  }
})

test_that(".log_skellam_increment mixed case is FINITE for the covid underflow regime", {
  # Regression: a long/broad appearance delay drives some delay cells' appearance
  # intensity toward 0 while a positive increment is observed there.  R's besselI
  # underflows to 0 (log -> -Inf, NaN gradient); the log-series must stay finite.
  hard_cases <- list(c(100, 1e-8, 0.06), c(120, 1e-8, 0.03),
                     c(90, 40, 0.005), c(2424, 1e-8, 0.02))
  for (case in hard_cases) {
    value <- diseasenowcasting:::.log_skellam_increment(case[1], case[2], case[3], 1L)
    expect_true(is.finite(value),
                label = sprintf("finite Skellam m=%g alpha=%g beta=%g", case[1], case[2], case[3]))
  }
})

test_that(".loglik_skellam_path and .loglik_sknb_path sum a full delay path finitely", {
  increments <- c(20, 8, 3, -1)
  alpha      <- c(20, 6, 2, 0)
  beta       <- c(0, 0.2, 0.3, 0.4)
  bin_type   <- c(0L, 1L, 1L, 1L)
  skellam <- diseasenowcasting:::.loglik_skellam_path(increments, alpha, beta, bin_type)
  expect_true(is.finite(skellam))
  expect_equal(skellam,
               sum(mapply(diseasenowcasting:::.log_skellam_increment,
                          increments, alpha, beta, bin_type)),
               tolerance = 1e-9)
  sknb <- diseasenowcasting:::.loglik_sknb_path(increments, alpha, beta, bin_type, nb_size = 5)
  expect_true(is.finite(sknb))
})

# ── count-cumulative de-accumulation (dedicated revised component) ───────────

make_cumulative_tblnow <- function(n_events = 12L, seed = 3L, down_revision = FALSE) {
  set.seed(seed)
  start <- as.Date("2023-01-07")
  appearance_cdf <- cumsum(c(0.4, 0.3, 0.2, 0.1))   # cumulative fraction by delay 0..3
  rows <- list()
  for (t in seq_len(n_events)) {
    event_date <- start + (t - 1L) * 7L
    final <- rpois(1, 45)
    for (d in 0:3) {
      cumulative <- round(final * appearance_cdf[d + 1L])
      # Inject a retraction: drive the last delay's cumulative BELOW the previous
      # delay's value, so the signed increment there is unambiguously negative.
      if (down_revision && t == 2L && d == 3L)
        cumulative <- round(final * appearance_cdf[3L]) - 5L
      rows[[length(rows) + 1L]] <- data.frame(event = event_date,
                                              report = event_date + d * 7L,
                                              n = cumulative)
    }
  }
  observations <- do.call(rbind, rows)
  tbl.now::tbl_now(observations, event_date = event, report_date = report,
                   case_count = n, data_type = "count-cumulative", verbose = FALSE)
}

test_that("count-cumulative data is detected and signed updates rebuild each level", {
  cumulative_tn <- make_cumulative_tblnow()
  cumulative_model <- model(
    poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
    count_cumulative = count_cumulative_process(
      observation = "hurdle_ztpoisson", settlement = 6L
    )
  )
  prepared <- diseasenowcasting:::prepare_from_tbl_now(
    cumulative_tn, cumulative_model, now = as.Date("2023-01-07") + 11L * 7L)
  engine <- prepared$data

  expect_equal(engine$is_count_cumulative, 1L)
  expect_equal(engine$is_confirmation, 0L)
  expect_true(!is.null(engine$signed_update_array))
  expect_true(!is.null(engine$cumulative_level_array))

  observed <- engine$observation_mask[1, , 1]
  increments <- engine$signed_update_array[1, observed, 1]
  levels <- engine$cumulative_level_array[1, observed, 1]
  expect_true(all(is.finite(increments)))
  expect_equal(cumsum(increments), levels)
})

test_that("down-revisions produce negative signed increments", {
  cumulative_tn <- make_cumulative_tblnow(down_revision = TRUE)
  cumulative_model <- model(
    poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
    count_cumulative = count_cumulative_process(
      observation = "hurdle_ztpoisson", settlement = 6L
    )
  )
  prepared <- diseasenowcasting:::prepare_from_tbl_now(
    cumulative_tn, cumulative_model, now = as.Date("2023-01-07") + 11L * 7L)
  observed_updates <- prepared$data$signed_update_array[
    prepared$data$observation_mask
  ]
  expect_true(any(observed_updates < 0))
})

# ── collapsed retraction-kernel priors ───────────────────────────────────────

test_that("count-cumulative priors contain h_R parameters and no separate p", {
  cumulative_tn <- make_cumulative_tblnow()
  cumulative_model <- model(
    poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
    count_cumulative = count_cumulative_process(
      observation = "hurdle_ztpoisson", settlement = 6L
    )
  )
  engine <- diseasenowcasting:::prepare_from_tbl_now(
    cumulative_tn, cumulative_model,
    now = as.Date("2023-01-07") + 11L * 7L
  )$data
  priors <- default_priors(cumulative_model, engine)

  expect_false(is.null(priors$retraction_mass))
  expect_false(is.null(priors$count_cumulative_retraction_mu))
  expect_false(is.null(priors$count_cumulative_retraction_sigma))
  expect_null(priors$confirm_p)
  expect_null(priors$retract_mu)
  expect_null(priors$retract_sigma)
})

test_that("linelist data keeps the WEAK data-informed Beta on p", {
  # The cure block identifies `p` directly there -- a report standing unresolved is
  # evidence about the cure fraction -- so the prior only has to keep it on (0, 1).
  sim <- simulate_retraction_linelist(n_days = 40, p_true = 0.85, seed = 4)
  tn  <- as_validation_tbl_now(sim$linelist, sim$now)
  mdl <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
               validation = validation_process())
  # The simulated reports run past `now` by construction; tbl.now says so, and the
  # as-of filter below is exactly what handles it.
  engine <- suppressWarnings(diseasenowcasting:::prepare_from_tbl_now(
    tn, mdl, now = sim$now, validation_mode = "retraction_only"))$data
  priors <- default_priors(mdl, engine)

  expect_identical(priors$confirm_p$is_constant, 0L)
  concentration <- sum(priors$confirm_p$params[1:2])
  expect_lt(concentration, 30)
  expect_gt(concentration, 3)
})

# ── one small end-to-end nowcast -> predict ──────────────────────────────────

test_that("count-cumulative nowcast() -> predict() runs end-to-end", {
  skip_on_cran()
  cumulative_tn <- make_cumulative_tblnow(n_events = 14L)
  cumulative_model <- model(
    poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
    count_cumulative = count_cumulative_process(
      observation = "hurdle_ztpoisson", settlement = 6L
    )
  )
  now_date <- as.Date("2023-01-07") + 13L * 7L

  fit <- nowcast(cumulative_tn, cumulative_model, now = now_date,
                 temporal_effects = "none", seed = 1L)
  expect_true(is.numeric(fit@target) && length(fit@target) == 1L)

  prediction <- predict(fit, n_draws = 200L)
  target_draws <- prediction@draws[, fit@target]
  expect_length(target_draws, 200L)
  expect_true(all(is.finite(target_draws)))
  expect_match(prediction@estimand, "C_t\\(6\\)")
})

test_that("the settled count is never negative, even when the stream revises down", {
  skip_on_cran()
  # Regression for the predictive that returned q5 = -4 on FluSight.  The mass
  # still standing but destined for retraction used to be an INDEPENDENT Poisson
  # subtracted from the observed cumulative, so at a newest event-time -- where
  # the cumulative is still near zero but the retraction intensity is not -- it
  # could exceed what it was subtracted from.  It is a sub-population of the
  # cumulative, so it is now a binomial thinning of the observed rows and is
  # bounded by them.
  cumulative_tn <- make_cumulative_tblnow(n_events = 14L, down_revision = TRUE)
  cumulative_model <- model(
    poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
    count_cumulative = count_cumulative_process(
      observation = "hurdle_ztpoisson", settlement = 6L
    )
  )
  now_date <- as.Date("2023-01-07") + 13L * 7L

  fit <- nowcast(cumulative_tn, cumulative_model, now = now_date,
                 temporal_effects = "none", seed = 1L)
  prediction <- predict(fit, n_draws = 400L, seed = 1L)

  expect_true(all(is.finite(prediction@draws)))
  expect_gte(min(prediction@draws), 0)
})
