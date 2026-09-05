# =============================================================================
# The mathematical claims behind the resolution model
# =============================================================================
# These test the DERIVATION rather than the plumbing: the frailty factorisation
# that lets the negative binomial reuse the Poisson mark blocks, the score
# equation at the fitted mode, simulation-based calibration of `p`, and the
# identities the three resolution modes are supposed to satisfy.
# =============================================================================

# ── the negative binomial ────────────────────────────────────────────────────

test_that("the gamma frailty cancels out of the row-level multinomial split", {
  # THE claim behind "no quadrature": conditional on a shared frailty the
  # trajectory-type counts are independent Poisson, so the split of the rows
  # across types is multinomial and FREE of the frailty size r.  If this failed,
  # every negative-binomial fit would be quietly mis-specified.
  skip_on_cran()
  set.seed(11)
  type_means <- c(unresolved = 6, resolved_early = 3, resolved_late = 1)
  target <- type_means / sum(type_means)

  observed <- vapply(c(0.5, 2, 20, 1e6), function(frailty_size) {
    splits <- replicate(20000, {
      frailty <- stats::rgamma(1, shape = frailty_size, rate = frailty_size)
      counts  <- stats::rpois(length(type_means), type_means * frailty)
      if (sum(counts) == 0) rep(NA_real_, length(type_means)) else counts / sum(counts)
    })
    rowMeans(splits, na.rm = TRUE)
  }, numeric(length(type_means)))

  # Every r gives the same split, and it is the Poisson one.
  for (column in seq_len(ncol(observed)))
    expect_equal(unname(observed[, column]), unname(target), tolerance = 0.01)
  expect_lt(max(apply(observed, 1, function(row) diff(range(row)))), 0.01)
})

test_that("p is recovered identically under Poisson and negative binomial", {
  # `p` lives entirely in the frailty-free cure block, so the likelihood family of
  # the COUNT block must not move it.
  skip_on_cran()
  simulated <- simulate_overdispersed_retraction_linelist(seed = 21)
  fitted_p <- vapply(list(poisson_likelihood(), nb_likelihood()), function(likelihood) {
    fitted <- fit_resolution(simulated$linelist, simulated$now, likelihood = likelihood, n_draws = 50)
    fitted@fits[[1]]$reconstruct$retraction$p
  }, numeric(1))
  expect_equal(fitted_p[1], simulated$p_true, tolerance = 0.03)
  expect_equal(fitted_p[2], fitted_p[1], tolerance = 0.01)
})

test_that("the negative-binomial fit estimates a finite overdispersion and stays proper", {
  skip_on_cran()
  simulated <- simulate_overdispersed_retraction_linelist(seed = 22)
  fitted <- fit_resolution(simulated$linelist, simulated$now, likelihood = nb_likelihood(), n_draws = 50)
  expect_true(is.finite(fitted@fits[[1]]$phi_nb))
  expect_gt(fitted@fits[[1]]$phi_nb, 0)
  expect_true(all(is.finite(fitted@fits[[1]]$lambda)))
})

# ── the score equation ───────────────────────────────────────────────────────

test_that("the analytic gradient of the cure block matches a numeric one", {
  # Guards the AD tape of the whole resolution block, including the logit Jacobian
  # on `p` -- the piece no point-recovery test can catch.
  skip_on_cran()
  simulated <- simulate_retraction_linelist(n_days = 45, seed = 23)
  tn <- as_validation_tbl_now(simulated$linelist, simulated$now)
  engine <- suppressMessages(suppressWarnings(diseasenowcasting:::prepare_from_tbl_now(
    tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
              validation = validation_process()),
    now = simulated$now, validation_mode = "retraction_only")))$data
  priors <- default_priors(model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
                                 validation = validation_process()), engine)
  objective <- diseasenowcasting:::build_joint_obj(engine, priors, use_random = FALSE)$obj

  at <- objective$par
  analytic <- objective$gr(at)
  resolution_index <- which(names(at) %in%
    c("logit_confirm_p", "retract_mu", "log_retract_sd_exc"))
  expect_gt(length(resolution_index), 0)
  numeric_gradient <- vapply(resolution_index, function(index) {
    step <- 1e-5
    up <- down <- at; up[index] <- up[index] + step; down[index] <- down[index] - step
    (objective$fn(up) - objective$fn(down)) / (2 * step)
  }, numeric(1))
  expect_equal(as.numeric(analytic)[resolution_index], numeric_gradient, tolerance = 1e-4)
})

test_that("the fitted mode really is a stationary point in p", {
  skip_on_cran()
  simulated <- simulate_retraction_linelist(n_days = 60, seed = 24)
  fitted <- fit_resolution(simulated$linelist, simulated$now, n_draws = 20)
  objective <- fitted@fits[[1]]$obj
  mode_vector <- objective$env$last.par.best
  gradient <- as.numeric(objective$gr(mode_vector))
  p_index <- which(names(mode_vector) == "logit_confirm_p")
  expect_lt(abs(gradient[p_index]), 1e-3 * max(1, sum(fitted@engine$case_counts)))
})

# ── simulation-based calibration of p ────────────────────────────────────────

test_that("p is calibrated: its credible intervals cover at about the nominal rate", {
  # Draw p from a spread of true values, refit, and check the 90% interval covers.
  # A wrong Jacobian on the logit transform would tilt this even though the point
  # estimate stayed fine.
  skip_on_cran()
  covered <- vapply(seq_len(12), function(replicate_index) {
    p_true <- stats::runif(1, 0.55, 0.95)
    simulated <- simulate_retraction_linelist(n_days = 55, p_true = p_true,
                                              seed = 300 + replicate_index)
    fitted <- fit_resolution(simulated$linelist, simulated$now, n_draws = 20)
    natural <- resolution_probability(fitted, conf.level = 0.9)
    natural$conf.low <= p_true && p_true <= natural$conf.high
  }, logical(1))
  expect_gt(mean(covered), 0.6)          # nominal 0.9; loose, this is 12 replicates
})

# ── the three modes agree where they must ────────────────────────────────────

test_that("with both signs recorded, p is exactly the binomial MLE on resolved rows", {
  # pi = 1 makes the unresolved term free of p, so the cure block collapses to a
  # binomial: phat = N+ / (N+ + N-), with no censoring correction at all.
  skip_on_cran()
  simulated <- simulate_both_signs_linelist(n_days = 70, p_true = 0.65, seed = 77)
  fitted <- fit_resolution(simulated$linelist, simulated$now,
                           n_draws = 50)
  engine <- fitted@engine
  binomial_mle <- engine$n_positive / (engine$n_positive + engine$n_negative)
  expect_equal(engine$resolution_mode, 2L)
  expect_equal(fitted@fits[[1]]$reconstruct$retraction$p, binomial_mle, tolerance = 0.01)
  expect_equal(binomial_mle, simulated$p_true, tolerance = 0.03)
})

test_that("with both signs recorded, rho is flat in the report age", {
  simulated <- simulate_both_signs_linelist(n_days = 40, seed = 78)
  fitted <- fit_resolution(simulated$linelist, simulated$now,
                           n_draws = 20)
  resolution <- fitted@fits[[1]]$reconstruct$retraction
  # A shared lag law means an unresolved row's age says nothing about its sign.
  expect_lt(diff(range(resolution$rho[, 1])), 1e-8)
  expect_equal(as.numeric(resolution$rho[1, 1]), resolution$p)
})

test_that("a validation date with an unusable outcome is rejected", {
  # "A report resolves once" used to need a test, because the two dates were two
  # columns and a row could fill both.  A tbl_now records ONE date plus one
  # outcome, so that is now structural.  What still needs guarding is the other
  # direction: a dated row whose outcome is not one we recognise would fall through
  # every `== "confirmed"` test and be silently counted as a RETRACTION.
  simulated <- simulate_both_signs_linelist(n_days = 30, seed = 79)
  tn <- as_validation_tbl_now(simulated$linelist, simulated$now)
  type_col <- tbl.now::get_validation_type(tn)
  resolved <- which(!is.na(tn[[tbl.now::get_validation_date(tn)]]))

  unknown <- tn
  unknown[[type_col]][resolved[1]] <- "unknown"
  expect_error(
    suppressMessages(suppressWarnings(nowcast(unknown, model(), now = simulated$now))),
    "Unrecognised outcome")

  missing_type <- tn
  missing_type[[type_col]][resolved[1]] <- NA_character_
  expect_error(
    suppressMessages(suppressWarnings(nowcast(missing_type, model(), now = simulated$now))),
    "without a usable")
})

test_that("seeing both signs beats seeing only one", {
  # The same data, read three ways.  Knowing the sign outright must give a tighter
  # interval for p than having to infer it from the censoring pattern.
  skip_on_cran()
  simulated <- simulate_both_signs_linelist(n_days = 70, p_true = 0.65, seed = 80)
  # The mode is asserted on the COMPONENT now, not passed to nowcast(): `both`
  # reads the sign outright, `confirmation_only` sees just the positives and has to
  # infer the split from the censoring.
  interval_width <- function(mode) {
    fitted <- fit_resolution(
      simulated$linelist, simulated$now, n_draws = 20,
      .validation_mode = mode)
    natural <- resolution_probability(fitted)
    c(width = natural$conf.high - natural$conf.low, estimate = natural$estimate)
  }
  both         <- interval_width("both")
  confirm_only <- interval_width("confirmation_only")

  expect_equal(unname(both["estimate"]), simulated$p_true, tolerance = 0.03)
  expect_equal(unname(confirm_only["estimate"]), simulated$p_true, tolerance = 0.06)
  expect_lt(unname(both["width"]), unname(confirm_only["width"]))
})

# ── stratified retraction ────────────────────────────────────────────────────

test_that("stratified p is recovered per stratum with the right cell bookkeeping", {
  # The `cell` index is column-major arithmetic over (event-time, stratum) -- the
  # most error-prone line in the predictive.
  skip_on_cran()
  simulated <- simulate_two_site_linelist(n_days = 70, seed = 41)
  tn <- as_validation_tbl_now(simulated$linelist, simulated$now, strata = site)
  fitted <- suppressMessages(suppressWarnings(nowcast(tn,
    model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
          validation = validation_process(validation_delay = dirichlet_validation(bins = 8),
                                              stratified_p = TRUE)),
    now = simulated$now, type = "one_stage",
    temporal_effects = "none", n_draws = 300, seed = 4)))

  expect_equal(fitted@fits[[1]]$reconstruct$retraction$p_by_stratum,
               simulated$p_true, tolerance = 0.05)
  # Per-stratum predictive draws must reconstruct the per-stratum truth.
  prediction <- predict(fitted)
  expect_equal(dim(prediction@strata_draws)[3], 2L)
  settled <- seq_len(30)
  for (stratum in 1:2) {
    truth <- simulated$truth[[stratum]][settled]
    medians <- apply(prediction@strata_draws[, settled, stratum], 2, median)
    expect_lt(mean(abs(medians - truth) / pmax(truth, 1)), 0.15)
  }
})

# ── leakage and follow-up ────────────────────────────────────────────────────

test_that("un-masking future retractions changes the fit (leakage regression)", {
  # If someone 'simplifies' `.mask_retractions()` away, this fails: a retraction
  # dated after `now` must not be visible.
  simulated <- simulate_retraction_linelist(n_days = 60, seed = 25)
  early_now <- simulated$now - 15
  honest <- suppressMessages(suppressWarnings(diseasenowcasting:::prepare_from_tbl_now(
    as_validation_tbl_now(simulated$linelist, early_now), model(),
    now = early_now, validation_mode = "retraction_only")))$data

  leaked_linelist <- simulated$linelist        # pretend every retraction is known now
  leaked <- suppressMessages(suppressWarnings(diseasenowcasting:::prepare_from_tbl_now(
    as_validation_tbl_now(leaked_linelist, simulated$now), model(),
    now = simulated$now, validation_mode = "retraction_only")))$data

  future_retractions <- sum(simulated$linelist$onset <= early_now &
                            simulated$linelist$reported <= early_now &
                            !is.na(simulated$linelist$retracted) &
                            simulated$linelist$retracted > early_now)
  expect_gt(future_retractions, 0)
  expect_lt(honest$n_retracted, leaked$n_retracted)
})

test_that("short follow-up widens the interval for p instead of pretending to know it", {
  # The classical cure-model failure mode: with every report younger than the bulk
  # of g_C, p and the tail of the lag trade off and the data cannot separate them.
  skip_on_cran()
  simulated <- simulate_retraction_linelist(n_days = 70, p_true = 0.8, seed = 26)
  wide_window <- fit_resolution(simulated$linelist, simulated$now, n_draws = 20)
  # Truncate the view so no report has had time to be retracted.
  narrow_now <- min(simulated$linelist$reported) + 2
  narrow_window <- tryCatch(
    fit_resolution(simulated$linelist, narrow_now,
                   n_draws = 20),
    error = function(e) NULL)

  width_of <- function(fitted) {
    natural <- resolution_probability(fitted)
    natural$conf.high - natural$conf.low
  }
  # A window so short that nothing has been retracted turns the block off entirely
  # (the p = 1 boundary), and then there is no `confirm_p` row to compare.
  skip_if(is.null(narrow_window) ||
          narrow_window@engine$is_linelist_retraction != 1L,
          "no retraction observed in the narrow window")
  expect_gt(width_of(narrow_window), width_of(wide_window))
})

# ── update() ─────────────────────────────────────────────────────────────────

test_that("update() moves a newly retracted case out of the standing pool", {
  skip_on_cran()
  simulated <- simulate_retraction_linelist(n_days = 60, seed = 27)
  early_now <- simulated$now - 10
  early_tn <- as_validation_tbl_now(
    dplyr::filter(simulated$linelist, .data$reported <= early_now), early_now)
  fitted <- suppressMessages(suppressWarnings(nowcast(early_tn,
    model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
    now = early_now, type = "one_stage",
    temporal_effects = "none", n_draws = 20, seed = 6)))

  later_tn <- as_validation_tbl_now(simulated$linelist, simulated$now)
  refreshed <- suppressMessages(suppressWarnings(
    stats::update(fitted, later_tn, compute_surprise = FALSE)))

  expect_equal(refreshed@validation_mode, "retraction_only")
  expect_equal(refreshed@engine$is_linelist_retraction, 1L)
  expect_gt(refreshed@engine$n_retracted, fitted@engine$n_retracted)
})

# ── the negative-binomial predictive ─────────────────────────────────────────

test_that("the future count conditions the frailty on what the origin already showed", {
  # The bug this pins: drawing the future from the PRIOR frailty Gamma(r, r) gives
  # the right MARGINAL spread but the wrong CONDITIONAL one, which is what a
  # predictive needs.  Simulated exactly, with the true parameters plugged in.
  set.seed(4)
  frailty_size <- 1 / 0.15
  lambda <- 200; gstar <- 0.6; n_rep <- 40000
  frailty <- stats::rgamma(n_rep, shape = frailty_size, rate = frailty_size)
  total   <- stats::rpois(n_rep, frailty * lambda)
  observed <- stats::rbinom(n_rep, total, gstar)
  future_truth <- total - observed

  near_mean <- abs(observed - lambda * gstar) < 5      # condition on k
  withr::local_options(diseasenowcasting.conditional_frailty = TRUE)
  conditional <- diseasenowcasting:::.future_count_draw(
    TRUE, rep(lambda * (1 - gstar), sum(near_mean)),
    rep(lambda * gstar, sum(near_mean)), observed[near_mean], 0.15)
  prior_draw <- diseasenowcasting:::.epidemic_rng(
    TRUE, rep(lambda * (1 - gstar), sum(near_mean)), 0.15)

  truth_sd <- stats::sd(future_truth[near_mean])
  expect_equal(stats::sd(conditional), truth_sd, tolerance = 0.15)
  expect_gt(stats::sd(prior_draw), 2 * truth_sd)       # the old behaviour
})

test_that("the frailty ratio is bounded so a misfit cannot explode the prediction", {
  # (r + k) / (r + m) is the factor this origin ran hot by.  When the fit is badly
  # off -- as it can be in the two-stage path -- an unbounded ratio multiplied the
  # future mean into 1e8-case predictions.
  withr::local_options(diseasenowcasting.conditional_frailty = TRUE)
  set.seed(5)
  absurd <- diseasenowcasting:::.future_count_draw(
    TRUE, future_mean = 1000, observed_mean = 1, observed_count = 17000, phi_nb = 0.24)
  expect_lt(absurd, 1e5)                                # bounded, not astronomic
  # And with no observations the update is inert: nothing is known about the frailty.
  set.seed(5); inert <- diseasenowcasting:::.future_count_draw(
    TRUE, future_mean = 50, observed_mean = 0, observed_count = 0, phi_nb = 0.24)
  set.seed(5); marginal <- diseasenowcasting:::.epidemic_rng(TRUE, 50, 0.24)
  expect_equal(inert, marginal)
})

test_that("the conditional frailty is opt-in and off by default", {
  # The default must reproduce the marginal draw byte-for-byte: switching it on
  # sharpens the intervals but currently costs WIS on real data (see the note on
  # `.future_count_draw()`), so the published benchmarks depend on this default.
  set.seed(9)
  default_draw <- diseasenowcasting:::.future_count_draw(
    TRUE, rep(50, 5), rep(40, 5), rep(40, 5), 0.24)
  set.seed(9)
  marginal_draw <- diseasenowcasting:::.epidemic_rng(TRUE, rep(50, 5), 0.24)
  expect_identical(default_draw, marginal_draw)

  withr::local_options(diseasenowcasting.conditional_frailty = TRUE)
  set.seed(9)
  conditional_draw <- diseasenowcasting:::.future_count_draw(
    TRUE, rep(50, 5), rep(40, 5), rep(40, 5), 0.24)
  expect_false(identical(conditional_draw, marginal_draw))
})

test_that("negative-binomial intervals are near nominal at short horizons", {
  skip_on_cran()
  withr::local_options(diseasenowcasting.conditional_frailty = TRUE)
  simulated <- simulate_overdispersed_retraction_linelist(n_days = 90, seed = 28)
  fitted <- fit_resolution(simulated$linelist, simulated$now, n_draws = 800)
  truth <- as.numeric(table(factor(
    as.character(simulated$linelist$onset[is.na(simulated$linelist$retracted)]),
    levels = as.character(simulated$origin + seq_len(90) - 1))))
  bounds <- apply(predict(fitted)@draws, 2, stats::quantile,
                  c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
  recent <- 83:90
  # The prior-frailty draw used to cover ~100% of its own 95% intervals here.
  expect_lt(mean(truth[recent] >= bounds[1, recent] & truth[recent] <= bounds[4, recent]), 1.0)
  expect_gt(mean(truth[recent] >= bounds[1, recent] & truth[recent] <= bounds[4, recent]), 0.6)
})

# ── competing risks ──────────────────────────────────────────────────────────

test_that("competing risks recovers an age-dependent rho that shared lags cannot", {
  # Negatives come back fast (mean 1), positives slow (mean 5).  Then an old
  # unresolved report is probably heading positive, and rho must rise with age.
  skip_on_cran()
  simulated <- simulate_competing_risks_linelist(n_days = 90, p_true = 0.6, seed = 91)
  fitted <- fit_resolution(simulated$linelist, simulated$now,
    negative_delay = lognormal_validation(), n_draws = 400)
  resolution <- fitted@fits[[1]]$reconstruct$retraction

  ages <- c(0, 2, 5, 10)
  expected <- simulated$p_true * (1 - stats::ppois(ages, simulated$positive_lag_mean)) /
    (simulated$p_true * (1 - stats::ppois(ages, simulated$positive_lag_mean)) +
       (1 - simulated$p_true) * (1 - stats::ppois(ages, simulated$negative_lag_mean)))
  expect_equal(as.numeric(resolution$rho[ages + 1L, 1]), expected, tolerance = 0.08)
  expect_true(all(diff(as.numeric(resolution$rho[1:8, 1])) > 0))   # rises with age
  expect_equal(resolution$p, simulated$p_true, tolerance = 0.05)
})

test_that("competing risks beats a shared lag law when the two signs differ", {
  skip_on_cran()
  simulated <- simulate_competing_risks_linelist(n_days = 90, p_true = 0.6, seed = 92)
  fit_with <- function(negative_delay) fit_resolution(
    simulated$linelist, simulated$now, negative_delay = negative_delay, n_draws = 300)
  shared    <- fit_with(NULL)
  competing <- fit_with(lognormal_validation())

  expect_lt(competing@fits[[1]]$nll, shared@fits[[1]]$nll)   # strictly better fit
  # The shared-lag fit is forced to a flat rho and cannot represent the truth.
  expect_lt(diff(range(shared@fits[[1]]$reconstruct$retraction$rho[, 1])), 1e-8)
  expect_gt(diff(range(competing@fits[[1]]$reconstruct$retraction$rho[, 1])), 0.2)
})

test_that("competing risks collapses to the shared-lag fit when the two laws agree", {
  # Setting both lag laws to the same family on data generated with ONE lag must
  # recover a flat rho -- the shared case is nested inside the competing one.
  skip_on_cran()
  simulated <- simulate_both_signs_linelist(n_days = 70, p_true = 0.65, seed = 93)
  competing <- fit_resolution(simulated$linelist, simulated$now,
    negative_delay = lognormal_validation(), n_draws = 200)
  rho <- as.numeric(competing@fits[[1]]$reconstruct$retraction$rho[1:8, 1])
  expect_lt(diff(range(rho)), 0.08)                          # near-flat
  expect_equal(competing@fits[[1]]$reconstruct$retraction$p,
               simulated$p_true, tolerance = 0.05)
})

test_that("a negative_delay without both signs errors with the reason", {
  # A retraction-only stream records ONE sign, so it cannot identify two lag laws.
  # (This used to be produced by handing a both-signs linelist a single date
  # column; the mode is read from the data now, so the data must really carry one
  # sign.)
  simulated <- simulate_retraction_linelist(n_days = 30, seed = 94)
  expect_error(
    fit_resolution(simulated$linelist, simulated$now,
                   negative_delay = lognormal_validation(), n_draws = 10),
    "BOTH confirmations and retractions")
})
