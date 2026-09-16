# Discrete-hazard regression core: the baseline must survive the tilt exactly,
# and the tail must stay as accurate as the un-regressed delay likelihood.

delay_bundles <- function() {
  list(
    "LogNormal" = list(fns = .delay_distribution_functions(1L, log(3), 0.6), split = 2),
    "Gamma"     = list(fns = .delay_distribution_functions(2L, log(3), 2), split = 2),
    "GenGamma"  = list(fns = .delay_distribution_functions(3L, log(3), 0.5, 2), split = 6),
    "Dirichlet" = list(fns = .nonparametric_delay_functions(c(.4, .3, .2, .08, .02), 4L),
                       split = 2)
  )
}

test_that("a zero tilt reproduces the baseline law", {
  for (label in names(delay_bundles())) {
    bundle <- delay_bundles()[[label]]
    n <- 40
    baseline <- .stable_log_survival(bundle$fns, seq_len(n), bundle$split)
    path <- .process_hazard_path(baseline, rep(0, n))
    expect_equal(as.numeric(path$log_survival), as.numeric(baseline),
                 tolerance = 1e-10, info = label)
    expect_true(all(diff(as.numeric(path$log_survival)) <= 1e-12), info = label)
    expect_true(all(as.numeric(path$cdf) >= 0 & as.numeric(path$cdf) <= 1), info = label)
  }
})

test_that("the tail log-PMF matches the stable delay likelihood", {
  # The regression path used to difference the CDF, which silently floored every
  # bin past the point where F saturates to 1 in double precision -- delay 16 for
  # this LogNormal.  At delay 40 that was 42 nats too cheap.
  fns <- .delay_distribution_functions(1L, log(3), 0.6)
  n <- 60
  path <- .process_hazard_path(.stable_log_survival(fns, seq_len(n), 2), rep(0, n))
  for (delay in c(2, 5, 10, 16, 25, 40, 60)) {
    expect_equal(
      as.numeric(path$log_pmf[delay]),
      as.numeric(.discretised_delay_loglik(delay, 1, 2, fns$log_cdf, fns$log_survival)),
      tolerance = 1e-8, info = paste("delay", delay)
    )
  }
})

test_that("an underflowing family tail stays finite instead of turning into NaN", {
  # RTMBdist::pgamma2()'s upper tail returns -Inf from about S = 1e-17, and
  # differencing two -Inf survivals would put NaN on the tape.
  fns <- .delay_distribution_functions(2L, log(3), 2)
  baseline <- .stable_log_survival(fns, seq_len(120), 2)
  expect_true(all(is.finite(as.numeric(baseline))))
  path <- .process_hazard_path(baseline, rep(0, 120))
  expect_true(all(is.finite(as.numeric(path$log_pmf))))
  expect_true(all(is.finite(as.numeric(path$log_cdf))))
  expect_equal(as.numeric(path$cdf[120]), 1)
})

test_that("the floor does not clip a tail the family can still represent", {
  # A very tight LogNormal puts delay 31 at log S = -2495; that is representable
  # and must not be flattened to the floor.
  fns <- .delay_distribution_functions(1L, 0.3, 0.06)
  baseline <- as.numeric(.stable_log_survival(fns, seq_len(40), 2))
  expect_lt(baseline[31], -2000)
  expect_gt(baseline[31], -3000)
  expect_true(all(is.finite(baseline)))
})

test_that("a positive tilt moves mass onto the flagged bins and conserves it", {
  fns <- .delay_distribution_functions(1L, log(4), 1.5)
  n <- 50
  baseline <- .stable_log_survival(fns, seq_len(n), 2)
  flagged <- rep(c(0, 0, 1), length.out = n)
  tilted <- .process_hazard_path(baseline, 1.5 * flagged)
  flat <- .process_hazard_path(baseline, rep(0, n))
  # Total mass is unchanged: timing moves, the eventual number reported does not.
  expect_equal(as.numeric(tilted$cdf[n]), as.numeric(flat$cdf[n]), tolerance = 1e-8)
  # The CONDITIONAL hazard rises exactly on the flagged bins and is untouched
  # elsewhere.  (Mass is a weaker claim: a late flagged bin can still lose,
  # because the earlier flagged bins have already drained its risk set.)
  hazard_of <- function(path) {
    survival <- as.numeric(path$log_survival)
    1 - exp(survival - c(0, utils::head(survival, -1)))
  }
  tilted_hazard <- hazard_of(tilted)
  flat_hazard <- hazard_of(flat)
  expect_true(all(tilted_hazard[flagged == 1] > flat_hazard[flagged == 1]))
  expect_equal(tilted_hazard[flagged == 0], flat_hazard[flagged == 0], tolerance = 1e-12)
  # The first flagged bin has its full risk set, so it must gain mass, and
  # reporting is uniformly faster.
  first_flagged <- which(flagged == 1)[1]
  expect_gt(exp(as.numeric(tilted$log_pmf[first_flagged])),
            exp(as.numeric(flat$log_pmf[first_flagged])))
  expect_true(all(as.numeric(tilted$log_survival) <= as.numeric(flat$log_survival) + 1e-12))
})

test_that("hand-enumerated paths agree with the recursion", {
  baseline <- log(c(0.7, 0.4, 0.15, 0.02))          # log S(1..4)
  eta <- c(0.3, -0.8, 0, 1.1)
  hazard <- 1 - exp(diff(c(0, baseline)))
  tilted_hazard <- stats::plogis(stats::qlogis(hazard) + eta)
  expected_survival <- cumsum(log1p(-tilted_hazard))
  expected_pmf <- log(tilted_hazard) + c(0, head(expected_survival, -1))
  path <- .process_hazard_path(baseline, eta)
  expect_equal(as.numeric(path$log_survival), expected_survival, tolerance = 1e-12)
  expect_equal(as.numeric(path$log_pmf), expected_pmf, tolerance = 1e-12)
})
