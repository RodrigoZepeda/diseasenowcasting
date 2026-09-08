# =============================================================================
# Linelist retractions: the confirmation (cure-model) observation block
# =============================================================================
# Covers the data preparation (as-of masking, same-period drop, sufficient
# statistics), the exact reduction to the ordinary count model, the cure block
# itself against a hand-written reference, recovery of `p` and `g_C` from
# simulated data, and the row-by-row predictive thinning.
# =============================================================================

# ── data preparation ─────────────────────────────────────────────────────────

test_that("a retraction column switches on the cure block and builds its sufficient statistics", {
  simulated <- simulate_retraction_linelist()
  tn <- as_revision_tbl_now(simulated$linelist, simulated$now)
  engine <- suppressMessages(suppressWarnings(diseasenowcasting:::prepare_from_tbl_now(
    tn, model(), now = simulated$now, revision_mode = "retraction_only")))$data

  expect_equal(engine$is_linelist_retraction, 1L)
  expect_gt(engine$n_retracted, 0)
  expect_gt(engine$n_standing, 0)
  # Every retracted row is counted once in the lag table; every standing row once
  # in the age table.  With no censoring every row is "exact", so the pooled tables
  # account for all of them.
  expect_equal(engine$n_censored, 0)
  expect_equal(sum(engine$retract_table[, "count"]), engine$n_retracted)
  expect_equal(sum(engine$standing_table[, "count"]), engine$n_standing)
  # `case_counts` is every row (the k_t the count block models); `standing_counts`
  # is the smaller currently-on-the-books total.
  expect_equal(sum(engine$case_counts), engine$n_retracted + engine$n_standing)
  expect_equal(sum(engine$standing_counts), engine$n_standing)
  expect_lt(sum(engine$standing_counts), sum(engine$case_counts))
  # g_C lives on {1, 2, ...}: no zero lag survives.
  expect_true(all(engine$retract_table[, "lag"] >= 1))
})

test_that("retractions dated after `now` are masked, leaving the row standing", {
  simulated <- simulate_retraction_linelist()
  linelist  <- simulated$linelist
  # Pull `now` back so a chunk of the retractions has not happened yet.
  early_now <- simulated$now - 20
  tn <- as_revision_tbl_now(linelist, early_now)
  engine <- suppressMessages(suppressWarnings(diseasenowcasting:::prepare_from_tbl_now(
    tn, model(), now = early_now, revision_mode = "retraction_only")))$data

  in_view <- linelist$onset <= early_now & linelist$reported <= early_now
  retracted_by_now <- sum(in_view & !is.na(linelist$retracted) & linelist$retracted <= early_now)
  expect_equal(engine$n_retracted, retracted_by_now)
  # Rows whose retraction is still in the future are STANDING, not dropped.
  expect_equal(engine$n_retracted + engine$n_standing, sum(in_view))
})

test_that("same-period retractions are dropped from the data entirely", {
  simulated <- simulate_retraction_linelist(n_days = 30)
  linelist  <- simulated$linelist
  linelist$retracted[1:5] <- linelist$reported[1:5]         # lag 0
  tn <- as_revision_tbl_now(linelist, simulated$now)
  engine <- suppressMessages(suppressWarnings(diseasenowcasting:::prepare_from_tbl_now(
    tn, model(), now = simulated$now, revision_mode = "retraction_only")))$data

  in_view <- linelist$onset <= simulated$now & linelist$reported <= simulated$now
  expect_equal(engine$n_retracted + engine$n_standing, sum(in_view) - 5L)
})

test_that("a tbl_now with no revision process is a plain count fit", {
  # There is no column ARGUMENT to get wrong any more -- the revision process is
  # read off the object -- so the failure mode this used to guard (a misspelled
  # `retraction_date =`) cannot happen.  What is still worth pinning is the
  # no-op: an object that carries no revision process must not acquire one.
  simulated <- simulate_retraction_linelist(n_days = 20)
  tn <- as_retraction_tbl_now(simulated$linelist, simulated$now)
  expect_false(isTRUE(tbl.now::has_revision(tn)))
  engine <- suppressWarnings(diseasenowcasting:::prepare_from_tbl_now(
    tn, model(), now = simulated$now))$data
  expect_equal(engine$is_linelist_retraction, 0L)
})

# ── reduction to the ordinary count model ────────────────────────────────────

test_that("an all-NA retraction column gives exactly the ordinary count fit", {
  simulated <- simulate_retraction_linelist(n_days = 45)
  linelist  <- simulated$linelist
  linelist$retracted <- as.Date(NA)
  tn  <- as_revision_tbl_now(linelist, simulated$now)
  mdl <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay())

  plain <- suppressMessages(suppressWarnings(nowcast(
    tn, mdl, now = simulated$now, type = "one_stage",
    temporal_effects = "none", n_draws = 50, seed = 5)))
  with_column <- suppressMessages(suppressWarnings(nowcast(
    tn, mdl, now = simulated$now, type = "one_stage",
    temporal_effects = "none", n_draws = 50, seed = 5)))

  expect_equal(with_column@engine$is_linelist_retraction, 0L)
  expect_equal(plain@fits[[1]]$nll, with_column@fits[[1]]$nll)
})

test_that("pinning p = 1 with observed retractions errors clearly", {
  simulated <- simulate_retraction_linelist(n_days = 40)
  tn  <- as_revision_tbl_now(simulated$linelist, simulated$now)
  mdl <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
               revision = revision_process(p = 1))
  expect_error(
    suppressMessages(suppressWarnings(nowcast(
      tn, mdl, now = simulated$now, type = "one_stage",
      temporal_effects = "none", n_draws = 10, seed = 5))),
    "no report is ever retracted")
})

# ── the cure block itself ────────────────────────────────────────────────────

test_that(".loglik_retraction matches a hand-written Berkson-Gage likelihood", {
  confirm_p    <- 0.8
  retract_mean <- 3
  retract_sd   <- 2
  retract_fns  <- diseasenowcasting:::.delay_distribution_functions(
    1L, log(retract_mean), retract_sd)

  lags        <- c(1, 2, 3, 5)
  lag_counts  <- c(4, 9, 6, 2)
  ages        <- c(1, 2, 4, 8)
  age_counts  <- c(20, 15, 30, 40)

  actual <- diseasenowcasting:::.loglik_retraction(
    retract_fns, is_nonparametric = 0L,
    retract_lags = lags, retract_lag_counts = lag_counts,
    standing_ages = ages, standing_age_counts = age_counts,
    n_retracted = sum(lag_counts), confirm_p = confirm_p, split_lag = 2)

  # Reference: g_C(c) = F(c) - F(c-1) with F(0) = 0, Sbar_C(j) = 1 - F(j).
  log_location <- log(retract_mean) - 0.5 * log1p(retract_sd^2 / retract_mean^2)
  log_scale    <- sqrt(log1p(retract_sd^2 / retract_mean^2))
  cdf <- function(x) ifelse(x <= 0, 0, plnorm(x, log_location, log_scale))
  expected <- sum(lag_counts) * log(1 - confirm_p) +
    sum(lag_counts * log(cdf(lags) - cdf(lags - 1))) +
    sum(age_counts * log(confirm_p + (1 - confirm_p) * (1 - cdf(ages))))

  expect_equal(as.numeric(actual), expected, tolerance = 1e-8)
})

test_that("the cure block is inert when nothing has been retracted", {
  retract_fns <- diseasenowcasting:::.delay_distribution_functions(1L, log(3), 2)
  # p = 1 and no retractions: 0 * log(0) must be 0, not NaN.
  value <- diseasenowcasting:::.loglik_retraction(
    retract_fns, is_nonparametric = 0L,
    retract_lags = numeric(0), retract_lag_counts = numeric(0),
    standing_ages = c(1, 5), standing_age_counts = c(10, 10),
    n_retracted = 0, confirm_p = 1, split_lag = 2)
  expect_true(is.finite(value))
  expect_equal(as.numeric(value), 0)
})

test_that("rho(j) rises from p at age 0 towards 1 as the report matures", {
  survival_fn <- function(age) exp(-age / 2)
  rho <- diseasenowcasting:::.retraction_genuine_probability(0:10, 0.8, survival_fn)
  expect_equal(rho[1], 0.8)                        # a brand-new report: the prior
  expect_true(all(diff(rho) > 0))                  # evidence accumulates
  expect_lt(1 - rho[11], 0.02)                     # a mature report is genuine
})

test_that("the empirical p estimate corrects the naive rate upward in retraction", {
  # 100 retracted, 900 standing, but most standing rows are brand new -- the naive
  # rate 100/1000 = 10% understates the true retraction rate.
  lags       <- c(1, 2, 3);  lag_counts  <- c(40, 40, 20)
  ages       <- c(0, 1, 5);  age_counts  <- c(600, 100, 200)
  p_hat <- diseasenowcasting:::.empirical_confirmation_probability(
    lags, lag_counts, ages, age_counts)
  expect_lt(p_hat, 0.90)                           # more retraction than naive 10%
  expect_gt(p_hat, 0.50)
})

# ── recovery on simulated data ───────────────────────────────────────────────

test_that("p and the settled counts are recovered from a simulated linelist", {
  skip_on_cran()
  simulated <- simulate_retraction_linelist(n_days = 80, p_true = 0.85, seed = 4)
  tn <- as_revision_tbl_now(simulated$linelist, simulated$now)
  fitted <- suppressMessages(suppressWarnings(nowcast(
    tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
    now = simulated$now, type = "one_stage",
    temporal_effects = "none", n_draws = 400, seed = 9)))

  expect_equal(fitted@fits[[1]]$reconstruct$retraction$p, simulated$p_true, tolerance = 0.05)

  # Settled truth: cases never retracted, by event time.
  settled <- simulated$linelist[is.na(simulated$linelist$retracted), , drop = FALSE]
  truth <- as.numeric(table(factor(as.character(settled$onset),
                                   levels = as.character(simulated$origin + seq_len(80) - 1))))
  prediction <- predict(fitted)
  bounds <- apply(prediction@draws, 2, quantile, c(0.025, 0.975), na.rm = TRUE)
  coverage <- mean(truth >= bounds[1, ] & truth <= bounds[2, ])
  expect_gt(coverage, 0.85)
})

test_that("every supported retraction-delay family fits and agrees on p", {
  skip_on_cran()
  simulated <- simulate_retraction_linelist(n_days = 60, p_true = 0.85, seed = 6)
  tn <- as_revision_tbl_now(simulated$linelist, simulated$now)

  fitted_p <- vapply(list(lognormal_delay(), gamma_delay(),
                          generalized_gamma_delay(), dirichlet_delay(bins = 8)),
                     function(retract_delay) {
    fitted <- suppressMessages(suppressWarnings(nowcast(
      tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
                revision = revision_process(revision_delay = retract_delay)),
      now = simulated$now, type = "one_stage",
      temporal_effects = "none", n_draws = 50, seed = 3)))
    fitted@fits[[1]]$reconstruct$retraction$p
  }, numeric(1))

  expect_true(all(abs(fitted_p - simulated$p_true) < 0.06))
  expect_lt(diff(range(fitted_p)), 0.03)           # the families agree with each other
})

# ── the predictive ───────────────────────────────────────────────────────────

test_that("the predictive thins the standing rows rather than starting from every row", {
  simulated <- simulate_retraction_linelist(n_days = 50, seed = 8)
  tn <- as_revision_tbl_now(simulated$linelist, simulated$now)
  fitted <- suppressMessages(suppressWarnings(nowcast(
    tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
    now = simulated$now, type = "one_stage",
    temporal_effects = "none", n_draws = 200, seed = 2)))
  prediction <- predict(fitted)

  # A settled (fully observed) event time gets essentially no future additions, so
  # its nowcast must sit at or below the standing count -- never at the gross row
  # count, which is what ignoring retractions would give.
  standing_total <- rowSums(fitted@engine$standing_counts)
  gross_total    <- rowSums(fitted@engine$case_counts)
  early <- 5
  expect_lt(median(prediction@draws[, early]), gross_total[early])
  expect_lt(abs(median(prediction@draws[, early]) - standing_total[early]),
            0.15 * standing_total[early])
})

test_that(".thin_standing_rows accumulates repeated cells instead of overwriting them", {
  # Two ages in the same (time, stratum) cell: the retained counts must add up.
  standing_rows <- matrix(c(1, 1, 0, 100,
                            1, 1, 5, 100), nrow = 2, byrow = TRUE,
                          dimnames = list(NULL, c("cell", "stratum", "age", "count")))
  set.seed(1)
  retained <- diseasenowcasting:::.thin_standing_rows(standing_rows, rho = rep(1, 6),
                                                      n_time = 3, n_strata = 1)
  expect_equal(retained[1, 1], 200)
  expect_equal(sum(retained[-1, ]), 0)
})

# ── censoring: the four observation patterns ─────────────────────────────────

# Add report / retraction censoring flags to a simulated linelist.  A censored
# report date is recorded at its UPPER BOUND, capped so it never lands after the
# retraction (a case cannot be withdrawn before it is filed) or after `now`; a
# censored retraction date is likewise an upper bound capped at `now`.
censor_linelist <- function(linelist, now, report_frac = 0, retract_frac = 0, seed = 21) {
  set.seed(seed)
  linelist$is_censored <- runif(nrow(linelist)) < report_frac
  linelist$q_bound <- !is.na(linelist$retracted) & runif(nrow(linelist)) < retract_frac
  bumped <- linelist$is_censored
  linelist$reported[bumped] <- pmin(
    linelist$reported[bumped] + 2L, now,
    ifelse(is.na(linelist$retracted[bumped]), now, linelist$retracted[bumped] - 1L))
  linelist$retracted[linelist$q_bound] <- pmin(linelist$retracted[linelist$q_bound] + 2L, now)
  linelist
}

fit_censored <- function(linelist, now, ...) {
  tn <- as_revision_tbl_now(
    linelist, now, is_censored_report = is_censored,
    is_censored_revision = q_bound
  )
  suppressMessages(suppressWarnings(nowcast(tn,
    model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
          revision = revision_process(revision_delay = dirichlet_revision(bins = 8))),
    now = now,
    type = "one_stage", temporal_effects = "none", n_draws = 100, seed = 6, ...)))
}

test_that("revision censoring is read only from tbl_now metadata", {
  simulated <- simulate_retraction_linelist(n_days = 45, seed = 16)
  rows <- simulated$linelist
  rows$q_bound <- !is.na(rows$retracted)
  data <- as_revision_tbl_now(
    rows, simulated$now, is_censored_revision = q_bound
  )
  specification <- model(
    nb_likelihood(), ar1_epidemic(), lognormal_delay(), revision_process()
  )

  expect_identical(tbl.now::get_is_censored_revision(data), "q_bound")
  expect_false("revision_censored" %in% names(formals(nowcast)))
  expect_error(
    nowcast(data, specification, revision_censored = "q_bound"),
    "not a.*nowcast.*argument"
  )

  engine <- suppressMessages(prepare_from_tbl_now(
    data, specification, now = simulated$now,
    revision_mode = "retraction_only"
  ))$data
  expect_gt(engine$n_censored, 0)
})

test_that("a point-valued censoring interval reproduces the exact-row likelihood", {
  # `is_censored` means the delay is known only to lie in [0, j], so marking a row
  # censored genuinely widens it -- EXCEPT when j = 0, where the interval is the
  # single point 0.  The censored kernels must then reproduce the exact-row terms
  # they replace, so the two log-likelihoods agree to machine precision.
  #
  # This compares the OBJECTIVE at a shared parameter vector rather than the fitted
  # nll: several priors are data-informed (the Beta on p is centred on the observed
  # retraction rate, the Dirichlet alphas on the observed lag profile), and those
  # summaries are built from the exact tables alone, so the two fits legitimately
  # see slightly different priors.  The likelihood is the claim being tested.
  simulated <- simulate_retraction_linelist(n_days = 45, seed = 15)
  linelist  <- simulated$linelist
  # Give a slice of the cases a same-day report so a point interval exists at all.
  same_day <- seq(1, nrow(linelist), by = 7)
  linelist$reported[same_day] <- linelist$onset[same_day]
  linelist$retracted[same_day] <- pmax(linelist$retracted[same_day],
                                       linelist$reported[same_day] + 1)
  linelist$q_bound <- FALSE

  retraction_model <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
                            revision = revision_process())
  engine_for <- function(is_censored) {
    rows <- linelist
    rows$is_censored <- is_censored
    tn <- as_revision_tbl_now(
      rows, simulated$now, is_censored_report = is_censored,
      is_censored_revision = q_bound
    )
    suppressMessages(suppressWarnings(diseasenowcasting:::prepare_from_tbl_now(
      tn, retraction_model, now = simulated$now,
      revision_mode = "retraction_only")))$data
  }
  exact_engine <- engine_for(rep(FALSE, nrow(linelist)))
  point_engine <- engine_for(linelist$reported == linelist$onset)
  expect_gt(point_engine$n_censored, 0)               # the patterns really are used
  expect_equal(sum(point_engine$case_counts), sum(exact_engine$case_counts))

  shared_priors <- default_priors(retraction_model, exact_engine)
  exact_objective <- diseasenowcasting:::build_joint_obj(
    exact_engine, shared_priors, use_random = FALSE)$obj
  point_objective <- diseasenowcasting:::build_joint_obj(
    point_engine, shared_priors, use_random = FALSE)$obj

  expect_identical(names(exact_objective$par), names(point_objective$par))
  at <- exact_objective$par
  expect_equal(point_objective$fn(at), exact_objective$fn(at), tolerance = 1e-8)
})

test_that("the censored kernel equals the exact-row decomposition on a point interval", {
  # Kernel-level version of the same identity, with no fitting in the way: on
  # [a, a] the standing kernel must be log g_D(a) + log h(d* - a) and the retracted
  # one log(1 - p) + log g_D(a) + log g_C(b - a) -- exactly the two pieces an exact
  # row contributes through the delay block and the cure block.
  appearance_pmf <- c(0.1, 0.2, 0.3, 0.25, 0.15)
  retract_pmf    <- c(0, 0.5, 0.3, 0.2, 0)
  retract_cdf    <- cumsum(retract_pmf)
  confirm_p <- 0.8
  h <- confirm_p + (1 - confirm_p) * (1 - retract_cdf)
  make_pattern <- function(retracted, appear, withdraw, horizon) matrix(
    c(1, retracted, appear, appear, withdraw, 0, horizon, 1), nrow = 1,
    dimnames = list(NULL, c("stratum", "retracted", "appear_lower", "appear_upper",
                            "withdraw", "withdraw_censored", "horizon", "count")))

  standing <- diseasenowcasting:::.loglik_retraction_censored(
    make_pattern(0, 2, 0, 4), appearance_pmf, retract_pmf, retract_cdf, confirm_p)
  expect_equal(as.numeric(standing),
               log(appearance_pmf[3]) + log(h[3]), tolerance = 1e-10)   # a = 2, h(4 - 2)

  retracted <- diseasenowcasting:::.loglik_retraction_censored(
    make_pattern(1, 1, 3, 6), appearance_pmf, retract_pmf, retract_cdf, confirm_p)
  expect_equal(as.numeric(retracted),
               log(1 - confirm_p) + log(appearance_pmf[2]) + log(retract_pmf[3]),
               tolerance = 1e-10)                                       # a = 1, g_C(3 - 1)
})

test_that("all four observation patterns fit and recover p", {
  skip_on_cran()
  simulated <- simulate_retraction_linelist(n_days = 80, p_true = 0.85, seed = 16)
  configurations <- list(c(0, 0), c(0.15, 0), c(0, 0.15), c(0.15, 0.15))
  fitted_p <- vapply(configurations, function(fractions) {
    linelist <- censor_linelist(simulated$linelist, simulated$now, fractions[1], fractions[2])
    fit_censored(linelist, simulated$now)@fits[[1]]$reconstruct$retraction$p
  }, numeric(1))
  expect_true(all(abs(fitted_p - simulated$p_true) < 0.05))
  expect_lt(diff(range(fitted_p)), 0.02)      # censoring costs precision, not accuracy
})

test_that("a censored report is bounded by its own retraction date", {
  # Rule 1 of the brief: when the report delay is censored at j_R but the
  # retraction is exact at withdrawal delay b, the appearance can only be as late
  # as b - 1.  The kernel must truncate at min(j_R, b - 1) -- so raising j_R past
  # b - 1 changes nothing.
  appearance_pmf <- c(0.1, 0.2, 0.3, 0.25, 0.15)          # g_D(0..4)
  retract_pmf    <- c(0, 0.5, 0.3, 0.2, 0)                # g_C(0..4), g_C(0) = 0
  retract_cdf    <- cumsum(retract_pmf)
  pattern <- function(appear_upper) matrix(
    c(1, 1, 0, appear_upper, 3, 0, 10, 1), nrow = 1,
    dimnames = list(NULL, c("stratum", "retracted", "appear_lower", "appear_upper",
                            "withdraw", "withdraw_censored", "horizon", "count")))

  at_bound <- diseasenowcasting:::.loglik_retraction_censored(
    pattern(2), appearance_pmf, retract_pmf, retract_cdf, confirm_p = 0.8)
  beyond_bound <- diseasenowcasting:::.loglik_retraction_censored(
    pattern(4), appearance_pmf, retract_pmf, retract_cdf, confirm_p = 0.8)
  expect_equal(at_bound, beyond_bound)

  # And it equals the hand-written sum over a = 0 .. b - 1 = 2.
  expected <- log(0.2 * sum(appearance_pmf[1:3] * retract_pmf[c(4, 3, 2)]))
  expect_equal(as.numeric(at_bound), expected, tolerance = 1e-10)
})

test_that("a censored retraction integrates g_C up to its bound", {
  # Rule 2: report exact at a, retraction known only to have happened by B, so the
  # lag contributes G_C(B - a) rather than g_C(b - a).
  appearance_pmf <- c(0.1, 0.2, 0.3, 0.25, 0.15)
  retract_pmf    <- c(0, 0.5, 0.3, 0.2, 0)
  retract_cdf    <- cumsum(retract_pmf)
  pattern <- matrix(c(1, 1, 1, 1, 4, 1, 10, 3), nrow = 1,
    dimnames = list(NULL, c("stratum", "retracted", "appear_lower", "appear_upper",
                            "withdraw", "withdraw_censored", "horizon", "count")))
  actual <- diseasenowcasting:::.loglik_retraction_censored(
    pattern, appearance_pmf, retract_pmf, retract_cdf, confirm_p = 0.8)
  expected <- 3 * log(0.2 * appearance_pmf[2] * retract_cdf[4])   # a = 1, G_C(4 - 1)
  expect_equal(as.numeric(actual), expected, tolerance = 1e-10)
})

test_that("a censored standing row sums the appearance mass against h", {
  appearance_pmf <- c(0.1, 0.2, 0.3, 0.25, 0.15)
  retract_pmf    <- c(0, 0.5, 0.3, 0.2, 0)
  retract_cdf    <- cumsum(retract_pmf)
  confirm_p <- 0.8
  pattern <- matrix(c(1, 0, 0, 3, 0, 0, 4, 2), nrow = 1,
    dimnames = list(NULL, c("stratum", "retracted", "appear_lower", "appear_upper",
                            "withdraw", "withdraw_censored", "horizon", "count")))
  actual <- diseasenowcasting:::.loglik_retraction_censored(
    pattern, appearance_pmf, retract_pmf, retract_cdf, confirm_p)
  h <- confirm_p + (1 - confirm_p) * (1 - retract_cdf)
  expected <- 2 * log(sum(appearance_pmf[1:4] * h[c(5, 4, 3, 2)]))  # a = 0..3, h(4 - a)
  expect_equal(as.numeric(actual), expected, tolerance = 1e-10)
})

# ── stratum-varying p ────────────────────────────────────────────────────────

fit_two_site <- function(simulated, stratified_p) {
  tn <- as_revision_tbl_now(simulated$linelist, simulated$now, strata = site)
  suppressMessages(suppressWarnings(nowcast(tn,
    model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
          revision = revision_process(revision_delay = dirichlet_revision(bins = 8),
                                              stratified_p = stratified_p)),
    now = simulated$now, type = "one_stage",
    temporal_effects = "none", n_draws = 100, seed = 4)))
}

test_that("stratified_p recovers per-stratum confirmation probabilities", {
  skip_on_cran()
  simulated <- simulate_two_site_linelist()
  stratified <- fit_two_site(simulated, TRUE)
  shared     <- fit_two_site(simulated, FALSE)

  fitted_p <- stratified@fits[[1]]$reconstruct$retraction$p_by_stratum
  expect_length(fitted_p, 2L)
  expect_true(all(abs(sort(fitted_p) - sort(simulated$p_true)) < 0.05))

  # The shared fit is forced into a compromise strictly between the two truths,
  # and pays for it in likelihood.
  shared_p <- shared@fits[[1]]$reconstruct$retraction$p_by_stratum
  expect_equal(shared_p[1], shared_p[2])
  expect_gt(shared_p[1], min(simulated$p_true))
  expect_lt(shared_p[1], max(simulated$p_true))
  expect_lt(stratified@fits[[1]]$nll, shared@fits[[1]]$nll)
})

test_that("stratified_p is a no-op on a single stratum", {
  simulated <- simulate_retraction_linelist(n_days = 40, seed = 17)
  tn <- as_revision_tbl_now(simulated$linelist, simulated$now)
  nll <- vapply(c(FALSE, TRUE), function(stratified) {
    suppressMessages(suppressWarnings(nowcast(tn,
      model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
            revision = revision_process(stratified_p = stratified)),
      now = simulated$now, type = "one_stage",
      temporal_effects = "none", n_draws = 20, seed = 3)))@fits[[1]]$nll
  }, numeric(1))
  expect_equal(nll[1], nll[2])
})

# ── mathematical correctness ─────────────────────────────────────────────────

# Posterior draws of the confirmation probability, taken from the same Laplace
# approximation `predict()` samples.  `parameters()` cannot be used here: it reports NA
# standard errors for every joint AR1 / HSGP fit, retraction or not.
posterior_confirm_p <- function(fitted, n_draws = 400L) {
  objective <- fitted@fits[[1]]$obj
  mode_par  <- objective$env$last.par.best
  precision <- methods::as(objective$he(mode_par), "sparseMatrix")
  parameter_draws <- diseasenowcasting:::.sample_mvnorm_precision(
    as.numeric(mode_par), precision, n_draws)
  stats::plogis(parameter_draws[which(names(mode_par) == "logit_confirm_p")[1], ])
}

test_that("the cure block's gradient matches a numeric one at the mode", {
  # Catches a wrong Jacobian on the logit transform, which no point-recovery test
  # can: a bias in the transform shifts the mode without breaking the fit.
  simulated <- simulate_retraction_linelist(n_days = 40, seed = 18)
  tn <- as_revision_tbl_now(simulated$linelist, simulated$now)
  fitted <- suppressMessages(suppressWarnings(nowcast(tn,
    model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
    now = simulated$now, type = "one_stage",
    temporal_effects = "none", n_draws = 10, seed = 3)))

  objective <- fitted@fits[[1]]$obj
  mode_par  <- objective$env$last.par.best
  analytic  <- objective$gr(mode_par)
  retraction_index <- grep("confirm_p|retract", names(mode_par))
  expect_gt(length(retraction_index), 0)

  step <- 1e-5
  numeric_gradient <- vapply(retraction_index, function(index) {
    shifted_up <- shifted_down <- mode_par
    shifted_up[index]   <- shifted_up[index]   + step
    shifted_down[index] <- shifted_down[index] - step
    (objective$fn(shifted_up) - objective$fn(shifted_down)) / (2 * step)
  }, numeric(1))
  expect_equal(as.numeric(analytic)[retraction_index], numeric_gradient, tolerance = 1e-4)
})

test_that("the gamma frailty cancels out of the row-level split (Theorem 2)", {
  # The claim behind "no quadrature": CONDITIONAL on the row total k, the split
  # across types is Binomial(k, kappa_1 / K) and does not depend on the frailty
  # shape r.  It has to be conditional -- the marginal spread of the ratio differs
  # between an over-dispersed and a nearly-Poisson total simply because k does.
  skip_on_cran()
  set.seed(19)
  kappa <- c(30, 70)
  conditional_split <- function(nb_size, fixed_total = 100L) {
    frailty <- rgamma(4e5, nb_size, nb_size)
    first   <- rpois(4e5, frailty * kappa[1])
    second  <- rpois(4e5, frailty * kappa[2])
    first[first + second == fixed_total]
  }
  tight <- conditional_split(50)      # nearly Poisson
  loose <- conditional_split(1.5)     # heavily over-dispersed
  expect_gt(length(tight), 200); expect_gt(length(loose), 200)

  # Both must match Binomial(100, 0.3): mean 30, sd sqrt(100 * 0.3 * 0.7) = 4.58.
  expect_equal(mean(tight), 30, tolerance = 0.5)
  expect_equal(mean(loose), 30, tolerance = 0.5)
  expect_equal(sd(tight), sqrt(100 * 0.3 * 0.7), tolerance = 0.5)
  expect_equal(sd(loose), sqrt(100 * 0.3 * 0.7), tolerance = 0.5)
})

test_that("p is calibrated across repeated simulations", {
  # A coverage-based stand-in for simulation-based calibration: vary the truth over
  # a spread of plausible values, refit, and check the 90% credible interval for p
  # covers it about the right share of the time.  Laplace intervals are
  # approximate, so the band is deliberately generous -- this catches a systematic
  # mis-centring or a badly scaled posterior, not a few percent of miscalibration.
  skip_on_cran()
  true_p <- c(0.70, 0.78, 0.85, 0.90, 0.94, 0.75, 0.88, 0.82)
  covered <- vapply(seq_along(true_p), function(replicate_index) {
    simulated <- simulate_retraction_linelist(n_days = 50, p_true = true_p[replicate_index],
                                              seed = 100 + replicate_index)
    tn <- as_revision_tbl_now(simulated$linelist, simulated$now)
    fitted <- suppressMessages(suppressWarnings(nowcast(tn,
      model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
            revision = revision_process(revision_delay = dirichlet_revision(bins = 8))),
      now = simulated$now, type = "one_stage",
      temporal_effects = "none", n_draws = 20, seed = 3)))
    interval <- stats::quantile(posterior_confirm_p(fitted), c(0.05, 0.95), names = FALSE)
    true_p[replicate_index] >= interval[1] && true_p[replicate_index] <= interval[2]
  }, logical(1))
  expect_gt(mean(covered), 0.6)
})

test_that("the linelist and count-cumulative models agree on the settled mean", {
  # Section 9 is the IMAGE likelihood of section 8: aggregating a linelist to a
  # count-cumulative stream throws away which rows were retracted, keeping only the
  # net increments.  Both must target the same lambda_t, with the linelist tighter.
  skip_on_cran()
  simulated <- simulate_retraction_linelist(n_days = 45, p_true = 0.85, seed = 20)
  linelist  <- simulated$linelist
  now <- simulated$now

  linelist_fit <- suppressMessages(suppressWarnings(nowcast(
    as_revision_tbl_now(linelist, now),
    model(poisson_likelihood(), ar1_epidemic(), lognormal_delay()),
    now = now, type = "one_stage",
    temporal_effects = "none", n_draws = 100, seed = 3)))

  # Aggregate to the cumulative curve C_t(d) seen at each report date.
  event_dates  <- sort(unique(linelist$onset))
  report_dates <- sort(unique(c(linelist$reported, linelist$retracted[!is.na(linelist$retracted)])))
  report_dates <- report_dates[report_dates <= now]
  cumulative <- do.call(rbind, lapply(event_dates, function(event_date) {
    same_event <- linelist[linelist$onset == event_date, , drop = FALSE]
    counts <- vapply(report_dates, function(as_of)
      sum(same_event$reported <= as_of) -
      sum(!is.na(same_event$retracted) & same_event$retracted <= as_of), numeric(1))
    data.frame(event = event_date, report = report_dates, n = counts)
  }))
  cumulative <- cumulative[cumulative$report >= cumulative$event & cumulative$n >= 0, ]
  cumulative_tn <- suppressWarnings(tbl.now::tbl_now(cumulative, event_date = event,
    report_date = report, case_count = n, now = now,
    data_type = "count-cumulative", verbose = FALSE))
  cumulative_fit <- tryCatch(suppressMessages(suppressWarnings(nowcast(cumulative_tn,
    model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
          revision = revision_process()),
    now = now, type = "one_stage", temporal_effects = "none", n_draws = 100, seed = 3))),
    error = function(e) NULL)
  skip_if(is.null(cumulative_fit), "count-cumulative comparison fit did not converge")

  # Compare on the settled part of the series, where both are well determined.
  settled <- seq_len(30)
  linelist_lambda   <- rowSums(matrix(linelist_fit@fits[[1]]$lambda, ncol = 1))[settled]
  cumulative_lambda <- rowSums(matrix(cumulative_fit@fits[[1]]$lambda, ncol = 1))[settled]
  relative_gap <- mean(abs(linelist_lambda - cumulative_lambda) / pmax(cumulative_lambda, 1))
  expect_lt(relative_gap, 0.25)
})

# ── degeneracies and round-trips ─────────────────────────────────────────────

test_that("short follow-up widens the posterior for p rather than pretending precision", {
  # The classical cure-model failure mode: with every report younger than the bulk
  # of g_C, p and the tail of g_C trade off and p cannot be pinned down.  The model
  # should say so through a wider posterior, not report false precision.
  skip_on_cran()
  simulated <- simulate_retraction_linelist(n_days = 60, seed = 22)
  tn <- as_revision_tbl_now(simulated$linelist, simulated$now)
  posterior_spread <- function(now) {
    fitted <- suppressMessages(suppressWarnings(nowcast(tn,
      model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
      now = now, type = "one_stage",
      temporal_effects = "none", n_draws = 10, seed = 3)))
    stats::sd(posterior_confirm_p(fitted))
  }
  # `now` pulled back to just after the first reports: almost no follow-up.
  short <- posterior_spread(min(simulated$linelist$reported) + 3)
  long  <- posterior_spread(simulated$now)
  expect_gt(short, long)
})

test_that("update() carries the retraction settings and moves cases out of standing", {
  simulated <- simulate_retraction_linelist(n_days = 60, seed = 23)
  linelist  <- simulated$linelist
  early_now <- simulated$now - 15
  tn <- as_revision_tbl_now(linelist, early_now)

  fitted <- suppressMessages(suppressWarnings(nowcast(tn,
    model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
    now = early_now, type = "one_stage",
    temporal_effects = "none", n_draws = 20, seed = 3)))
  expect_equal(fitted@revision_mode, "retraction_only")

  refit <- suppressMessages(suppressWarnings(
    update(fitted, new_data = as_revision_tbl_now(linelist, simulated$now),
           now = simulated$now, compute_surprise = FALSE)))
  expect_equal(refit@engine$is_linelist_retraction, 1L)
  # Retractions that landed between the two dates have left the standing pool.
  expect_gt(refit@engine$n_retracted, fitted@engine$n_retracted)
})
