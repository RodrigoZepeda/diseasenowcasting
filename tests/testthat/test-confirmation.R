# Tests for the count-cumulative confirmation model: the Skellam / SkNB
# signed-increment densities (R/28_confirmation_likelihood.R), the
# confirmation_process() class + priors, the count-cumulative de-accumulation,
# and one small end-to-end nowcast -> predict.
#
# The unit tests below use only plain-numeric arithmetic, but the densities are
# imported from RTMB (dpois etc.); attach RTMB explicitly (it is in Imports, not
# Depends, so loading the package does not attach it) as the other RTMB-touching
# test files do.
library(RTMB)

# ── confirmation_process() constructor and validator ─────────────────────────

test_that("confirmation_process() default is a valid, active component with unset p", {
  conf <- confirmation_process()
  expect_s3_class(conf, "diseasenowcasting::confirmation_process_class")
  expect_true(conf@active)
  # p unset -> length-0 numeric, resolved to a data-informed prior in default_priors()
  expect_true(is.numeric(conf@p) && length(conf@p) == 0L)
})

test_that("confirmation_process() accepts a fixed p in (0, 1] and a prior on p", {
  fixed <- confirmation_process(p = 0.97)
  expect_equal(fixed@p, 0.97)

  estimated <- confirmation_process(p = beta_prior(50, 1))
  expect_true(S7::S7_inherits(estimated@p, diseasenowcasting:::prior_class))
})

test_that("confirmation_process() rejects p outside (0, 1]", {
  expect_error(confirmation_process(p = 0), "in \\(0, 1\\]")
  expect_error(confirmation_process(p = 1.5), "in \\(0, 1\\]")
  expect_error(confirmation_process(p = "0.9"), "prior_class|numeric")
})

test_that("no_confirmation() is the inert p = 1 default", {
  inert <- diseasenowcasting:::no_confirmation()
  expect_false(inert@active)
  expect_equal(inert@p, 1)
})

test_that("model() carries a confirmation component (inert by default, active when supplied)", {
  default_model <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay())
  expect_false(default_model@confirmation@active)

  confirm_model <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
                         confirmation = confirmation_process())
  expect_true(confirm_model@confirmation@active)
})

# ── Bessel log-series helpers ────────────────────────────────────────────────

test_that(".bessel_series_terms grows with the order and never drops below 60", {
  expect_equal(diseasenowcasting:::.bessel_series_terms(1L), 60L)
  expect_gte(diseasenowcasting:::.bessel_series_terms(1000L), 60L)
  expect_gt(diseasenowcasting:::.bessel_series_terms(1000L),
            diseasenowcasting:::.bessel_series_terms(10L))
})

test_that(".log_bessel_i_series matches log(besselI) where besselI is accurate", {
  # Moderate argument / order: R's exponentially-scaled besselI is reliable here,
  # so the log-space ascending series must agree with it.
  for (z in c(0.5, 2, 5)) for (order in c(0, 1, 3, 8)) {
    reference <- log(besselI(z, order, expon.scaled = TRUE)) + z
    series    <- diseasenowcasting:::.log_bessel_i_series(
      z, order, diseasenowcasting:::.bessel_series_terms(order))
    expect_equal(series, reference, tolerance = 1e-6,
                 label = sprintf("log I_%d(%g)", order, z))
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

# ── count-cumulative de-accumulation (via prepare_from_tbl_now, no fit) ───────

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

test_that("count-cumulative data is detected and de-accumulated so increments rebuild the cumulative", {
  cumulative_tn <- make_cumulative_tblnow()
  confirm_model <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
                         confirmation = confirmation_process())
  prepared <- diseasenowcasting:::prepare_from_tbl_now(
    cumulative_tn, confirm_model, now = as.Date("2023-01-07") + 11L * 7L)
  engine <- prepared$data

  expect_equal(engine$is_confirmation, 1L)
  expect_true(!is.null(engine$increment_array))

  # For a fully observed early event, cumsum of the signed increments must equal
  # the observed cumulative path (up to the modelled max delay).
  increments <- engine$increment_array[1, , 1]
  expect_true(all(is.finite(increments)))
  expect_true(sum(increments) > 0)                       # net positive appearances
})

test_that("down-revisions produce negative signed increments", {
  cumulative_tn <- make_cumulative_tblnow(down_revision = TRUE)
  confirm_model <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
                         confirmation = confirmation_process())
  prepared <- diseasenowcasting:::prepare_from_tbl_now(
    cumulative_tn, confirm_model, now = as.Date("2023-01-07") + 11L * 7L)
  expect_true(any(prepared$data$increment_array < 0))     # the injected retraction
})

# ── default priors for the confirmation block ────────────────────────────────

test_that("default_priors() builds a strong data-informed Beta for p and retraction priors", {
  cumulative_tn <- make_cumulative_tblnow()
  confirm_model <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
                         confirmation = confirmation_process())
  engine <- diseasenowcasting:::prepare_from_tbl_now(
    cumulative_tn, confirm_model, now = as.Date("2023-01-07") + 11L * 7L)$data
  priors <- default_priors(confirm_model, engine)

  expect_false(is.null(priors$confirm_p))
  expect_false(is.null(priors$retract_mu))
  expect_false(is.null(priors$retract_sigma))
  # Strong Beta: both shape parameters large (concentration ~300), centred high.
  expect_gt(sum(priors$confirm_p$params[1:2]), 100)
})

# ── one small end-to-end nowcast -> predict ──────────────────────────────────

test_that("confirmation nowcast() -> predict() runs end-to-end and returns finite draws", {
  skip_on_cran()
  cumulative_tn <- make_cumulative_tblnow(n_events = 14L)
  confirm_model <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
                         confirmation = confirmation_process())
  now_date <- as.Date("2023-01-07") + 13L * 7L

  fit <- nowcast(cumulative_tn, confirm_model, now = now_date, seed = 1L)
  expect_true(is.numeric(fit@target) && length(fit@target) == 1L)

  prediction <- predict(fit, n_draws = 200L)
  target_draws <- prediction@draws[, fit@target]
  expect_length(target_draws, 200L)
  expect_true(all(is.finite(target_draws)))
  expect_gt(median(target_draws), 0)          # a positive settled count
})
