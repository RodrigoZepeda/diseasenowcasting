# =============================================================================
# Confirmation mode, count-incidence data, and the count-cumulative reduction
# =============================================================================
# Confirmation and retraction are two readings of one revision
# process (see the "Resolution processes" section of the Mathematics vignette):
# under retraction the observed dates are the NEGATIVE resolutions and evidence
# accumulates in favour of a surviving report; under confirmation they are the
# POSITIVE ones and evidence accumulates against an unresolved one.  These tests
# pin the parts where the two genuinely differ, and the parts where the aggregated
# (count-incidence) form must agree with the linelist exactly.
# =============================================================================

# event -> report -> confirmed.  A share `p_true` of reports is ever confirmed, and
# the confirmation lag K ~ Poisson(lag_mean) STARTS AT 0: a case may be confirmed
# in the same period it is reported, unlike a retraction.
simulate_confirmation_linelist <- function(n_days = 70, p_true = 0.7,
                                           lag_mean = 1.5, seed = 31) {
  set.seed(seed)
  origin <- as.Date("2023-01-01")
  lambda <- 30 * exp(0.8 * sin(2 * pi * seq_len(n_days) / 55))
  per_day <- lapply(seq_len(n_days), function(day) {
    n_gross <- rpois(1, lambda[day] / p_true)
    if (n_gross == 0) return(NULL)
    appearance  <- 1 + rpois(n_gross, 3)
    confirmable <- runif(n_gross) < p_true
    lag         <- rpois(n_gross, lag_mean)
    data.frame(
      onset     = origin + day - 1,
      reported  = origin + day - 1 + appearance,
      confirmed = as.Date(ifelse(confirmable,
                                 as.numeric(origin + day - 1 + appearance + lag), NA),
                          origin = "1970-01-01"))
  })
  list(linelist = do.call(rbind, per_day), now = origin + n_days - 1,
       origin = origin, p_true = p_true, lag_mean = lag_mean, n_days = n_days)
}

fit_confirmation <- function(linelist, now, ..., data_type = "linelist",
                             .revision = revision_process(
                               revision_delay = dirichlet_revision(bins = 8))) {
  # Since 2.2.0 the revision process is detected from the tbl_now, so the outcome
  # has to be ON the object: one `revision_date` plus a `revision_type`, folded
  # here from whichever of `confirmed` / `retracted` the simulator filled.
  confirmed <- if ("confirmed" %in% names(linelist)) linelist$confirmed else
    as.Date(rep(NA_real_, nrow(linelist)), origin = "1970-01-01")
  retracted <- if ("retracted" %in% names(linelist)) linelist$retracted else
    as.Date(rep(NA_real_, nrow(linelist)), origin = "1970-01-01")
  linelist$revision_date <- dplyr::coalesce(confirmed, retracted)
  linelist$revision_type <- ifelse(!is.na(confirmed), "confirmed",
                              ifelse(!is.na(retracted), "retracted", "pending"))
  # `now` is not pinned on the object: a tbl_now refuses to hold a revision dated
  # after its own `now` (tbl.now#51), and these simulators resolve cases past the
  # analysis date deliberately.  nowcast(now = ) does the as-of masking.
  tn <- suppressWarnings(tbl.now::tbl_now(linelist, event_date = onset,
          report_date = reported, revision_date = revision_date,
          revision_type = revision_type, data_type = data_type,
          verbose = FALSE, ...))
  suppressMessages(suppressWarnings(nowcast(tn,
    model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
          revision = .revision),
    now = now, type = "one_stage",
    temporal_effects = "none", n_draws = 200, seed = 7)))
}

# ── the confirmation mode itself ─────────────────────────────────────────────

test_that("confirmation mode recovers p and the confirmation-lag curve", {
  skip_on_cran()
  simulated <- simulate_confirmation_linelist()
  fitted <- fit_confirmation(simulated$linelist, simulated$now)
  resolution <- fitted@fits[[1]]$reconstruct$retraction

  expect_equal(resolution$lag_offset, 1L)        # lag support starts at 0
  expect_equal(resolution$resolved_weight, 1)    # a confirmed case is already in
  expect_equal(resolution$p, simulated$p_true, tolerance = 0.04)

  # rho(j) = p Sbar_K(j) / [(1 - p) + p Sbar_K(j)] for K ~ Poisson(lag_mean).
  ages <- 0:5
  survival <- 1 - stats::ppois(ages, simulated$lag_mean)
  expected_rho <- simulated$p_true * survival /
    (simulated$p_true * survival + (1 - simulated$p_true))
  expect_equal(as.numeric(resolution$rho[ages + 1L, 1]), expected_rho, tolerance = 0.03)
})

test_that("rho falls with report age under confirmation and rises under retraction", {
  # The sign flip is the whole difference on the prediction side: an unconfirmed
  # report looks worse the longer it waits, an unretracted one looks better.
  survival_fn <- function(age) exp(-age / 2)
  confirmation <- diseasenowcasting:::.retraction_genuine_probability(
    0:8, confirm_p = 0.7, survival_fn = survival_fn, lag_offset = 1L)
  retraction <- diseasenowcasting:::.retraction_genuine_probability(
    0:8, confirm_p = 0.7, survival_fn = survival_fn, lag_offset = 0L)

  expect_true(all(diff(confirmation) < 0))
  expect_true(all(diff(retraction) > 0))
  expect_lt(confirmation[9], 0.05)               # a long-unconfirmed report is lost
  expect_gt(retraction[9], 0.95)                 # a long-standing report is genuine
  expect_equal(retraction[1], 0.7)               # a brand-new report: the prior
})

test_that("same-period confirmations are kept, unlike same-period retractions", {
  simulated <- simulate_confirmation_linelist(n_days = 40, seed = 32)
  linelist  <- simulated$linelist
  in_view_rows <- linelist$onset <= simulated$now & linelist$reported <= simulated$now
  # Only a resolution VISIBLE by `now` can be same-period; one dated later is
  # masked to NA first, so it never reaches the same-period rule.
  same_period <- sum(in_view_rows & !is.na(linelist$confirmed) &
                       linelist$confirmed <= simulated$now &
                       linelist$confirmed == linelist$reported)
  expect_gt(same_period, 0)

  # The mode is a property of the DATA now, so the same column can no longer be
  # read both ways.  Label the identical resolutions once as confirmations and once
  # as retractions, which is what a stream recording each sign would look like.
  labelled <- function(outcome) {
    rows <- linelist
    rows$revision_date <- rows$confirmed
    rows$revision_type <- ifelse(is.na(rows$confirmed), "pending", outcome)
    suppressWarnings(tbl.now::tbl_now(rows, event_date = onset, report_date = reported,
      revision_date = revision_date, revision_type = revision_type,
      data_type = "linelist", verbose = FALSE))
  }
  as_confirmation <- suppressMessages(suppressWarnings(
    diseasenowcasting:::prepare_from_tbl_now(labelled("confirmed"), model(),
      now = simulated$now, revision_mode = "confirmation_only")))$data
  as_retraction <- suppressMessages(suppressWarnings(
    diseasenowcasting:::prepare_from_tbl_now(labelled("retracted"), model(),
      now = simulated$now, revision_mode = "retraction_only")))$data

  in_view <- sum(in_view_rows)
  expect_equal(as_confirmation$n_retracted + as_confirmation$n_standing, in_view)
  # Read as retractions, the same-period rows are structurally invisible and go.
  expect_equal(as_retraction$n_retracted + as_retraction$n_standing,
               in_view - same_period)
})

test_that("the confirmation predictive counts confirmed cases and thins the rest", {
  skip_on_cran()
  simulated <- simulate_confirmation_linelist(n_days = 70, seed = 33)
  fitted <- fit_confirmation(simulated$linelist, simulated$now)
  prediction <- predict(fitted)

  truth <- as.numeric(table(factor(
    as.character(simulated$linelist$onset[!is.na(simulated$linelist$confirmed)]),
    levels = as.character(prediction@event_dates))))
  bounds <- apply(prediction@draws, 2, quantile, c(0.025, 0.975), na.rm = TRUE)
  expect_gt(mean(truth >= bounds[1, ] & truth <= bounds[2, ]), 0.85)

  # A settled event time is already fully confirmed, so the nowcast must sit AT the
  # confirmed count -- never below it, since a confirmation is never undone here.
  confirmed_counts <- rowSums(fitted@engine$resolved_counts)
  medians <- apply(prediction@draws, 2, median, na.rm = TRUE)
  settled <- seq_len(30)
  expect_true(all(medians[settled] >= confirmed_counts[settled] - 1e-8))
  expect_lt(mean(abs(medians[settled] - truth[settled]) / pmax(truth[settled], 1)), 0.12)
})

test_that("a stream with no confirmation reduces under auto and uses the prior when asserted", {
  # ASSERTING confirmation mode on data where nothing is confirmed leaves the
  # target (the eventually-confirmed count) unidentified, so it is refused.  With
  # `mode = "auto"` the same data instead REDUCES -- nothing has resolved, so there
  # is no revision process to fit and the ordinary count model is the answer.
  # An assertion that cannot be satisfied is an error; an inference with no
  # evidence falls back.
  simulated <- simulate_confirmation_linelist(n_days = 30, seed = 34)
  linelist <- simulated$linelist
  linelist$confirmed <- as.Date(NA)

  # Asserting the mode is not refused: this is a Bayesian model, and a parameter
  # the data say nothing about is exactly what a prior is for.  The user has said
  # the process exists, so `p` is carried by its prior rather than the fit failing.
  asserted <- suppressMessages(suppressWarnings(fit_confirmation(
    linelist, simulated$now,
    .revision = revision_process(p = beta_prior(7, 3), mode = "confirmation_only"))))
  expect_equal(asserted@revision_mode, "confirmation_only")
  # With no data on `p`, the posterior sits where the prior put it.
  expect_equal(asserted@fits[[1]]$reconstruct$retraction$p, 0.7, tolerance = 0.15)

  reduced <- suppressMessages(suppressWarnings(
    fit_confirmation(linelist, simulated$now)))
  expect_equal(reduced@revision_mode, "none")
  expect_equal(reduced@engine$is_linelist_retraction, 0L)
})

test_that("a revision date with no revision_type is an error", {
  # tbl.now records ONE date plus an outcome, so "both dates at once" is no longer
  # expressible.  The invariant that replaces it: a row that HAS resolved but whose
  # sign is unknown cannot enter either lag law, so it must be refused rather than
  # silently dropped into one of them.  tbl.now warns at construction; this is the
  # second and final ask.
  simulated <- simulate_confirmation_linelist(n_days = 20, seed = 35)
  linelist  <- simulated$linelist
  linelist$revision_date <- linelist$confirmed
  linelist$revision_type <- ifelse(is.na(linelist$confirmed), "pending", "confirmed")
  # A resolved row whose outcome went missing.
  first_resolved <- which(!is.na(linelist$revision_date))[1]
  linelist$revision_type[first_resolved] <- NA_character_

  tn <- suppressWarnings(tbl.now::tbl_now(linelist, event_date = onset,
          report_date = reported, revision_date = revision_date,
          revision_type = revision_type,
          data_type = "linelist", verbose = FALSE))
  expect_error(
    suppressMessages(suppressWarnings(nowcast(tn, model(), now = simulated$now))),
    "without a usable")
})

# ── count-incidence is the same model, weighted ──────────────────────────────

test_that("count-incidence and linelist give bit-identical engines and likelihoods", {
  skip_on_cran()
  simulated <- simulate_confirmation_linelist(n_days = 60, seed = 36)
  linelist  <- simulated$linelist
  # dplyr::count, not stats::aggregate: the latter silently DROPS rows whose
  # grouping key is NA, i.e. every unresolved case.
  aggregated <- dplyr::count(linelist, .data$onset, .data$reported, .data$confirmed,
                             name = "n")
  expect_lt(nrow(aggregated), nrow(linelist))          # aggregation really happened
  expect_equal(sum(aggregated$n), nrow(linelist))      # and lost nothing

  retraction_model <- model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
    revision = revision_process(revision_delay = dirichlet_revision(bins = 8)))
  engine_of <- function(tn) suppressMessages(suppressWarnings(
    diseasenowcasting:::prepare_from_tbl_now(tn, retraction_model, now = simulated$now,
      revision_mode = "confirmation_only")))$data

  linelist_engine <- engine_of(suppressWarnings(tbl.now::tbl_now(linelist,
    event_date = onset, report_date = reported, now = simulated$now,
    data_type = "linelist", verbose = FALSE)))
  counted_engine <- engine_of(suppressWarnings(tbl.now::tbl_now(aggregated,
    event_date = onset, report_date = reported, case_count = n, now = simulated$now,
    data_type = "count-incidence", verbose = FALSE)))

  for (field in c("case_counts", "standing_table", "retract_table", "resolved_counts",
                  "standing_counts", "row_sums_exact", "n_standing", "n_retracted")) {
    expect_equal(linelist_engine[[field]], counted_engine[[field]], info = field)
  }

  # And the objectives agree at a shared parameter vector, not merely the summaries.
  shared_priors <- default_priors(retraction_model, linelist_engine)
  linelist_objective <- diseasenowcasting:::build_joint_obj(
    linelist_engine, shared_priors, use_random = FALSE)$obj
  counted_objective <- diseasenowcasting:::build_joint_obj(
    counted_engine, shared_priors, use_random = FALSE)$obj
  at <- linelist_objective$par
  expect_equal(counted_objective$fn(at), linelist_objective$fn(at), tolerance = 1e-10)
})

test_that("count-incidence works for the retraction mode too", {
  skip_on_cran()
  simulated <- simulate_retraction_linelist(n_days = 50, seed = 37)
  linelist  <- simulated$linelist
  aggregated <- dplyr::count(linelist, .data$onset, .data$reported, .data$retracted,
                             name = "n")
  engine_of <- function(tn) suppressMessages(suppressWarnings(
    diseasenowcasting:::prepare_from_tbl_now(tn, model(), now = simulated$now,
      revision_mode = "retraction_only")))$data
  linelist_engine <- engine_of(suppressWarnings(tbl.now::tbl_now(linelist,
    event_date = onset, report_date = reported, now = simulated$now,
    data_type = "linelist", verbose = FALSE)))
  counted_engine <- engine_of(suppressWarnings(tbl.now::tbl_now(aggregated,
    event_date = onset, report_date = reported, case_count = n, now = simulated$now,
    data_type = "count-incidence", verbose = FALSE)))
  expect_equal(linelist_engine$standing_table, counted_engine$standing_table)
  expect_equal(linelist_engine$retract_table, counted_engine$retract_table)
  expect_equal(linelist_engine$case_counts, counted_engine$case_counts)
})

# ── the count-cumulative reduction ───────────────────────────────────────────

test_that("count-cumulative confirmation is refused, with the reduction spelled out", {
  # Not unimplemented -- it REDUCES: a confirmed-only cumulative shows only the
  # confirmations, so the mean is lambda_t * G_{D+K}(d) and the ordinary count
  # model with the onset-to-confirmation delay is the right answer.
  simulated <- simulate_confirmation_linelist(n_days = 30, seed = 38)
  confirmed <- simulated$linelist[!is.na(simulated$linelist$confirmed), , drop = FALSE]
  cumulative <- dplyr::count(confirmed, event = .data$onset, report = .data$confirmed,
                             name = "n")
  tn <- suppressWarnings(tbl.now::tbl_now(cumulative, event_date = event,
    report_date = report, case_count = n, now = simulated$now,
    data_type = "count-cumulative", verbose = FALSE))
  # With the confirmations carried as a revision process, the refusal is eq.
  # `noconfirmcum`: a confirmation does not change a cumulative count, so its delay
  # parameters are unidentifiable.
  cumulative$revision_date <- cumulative$report
  cumulative$revision_type <- "confirmed"
  tn_validated <- suppressWarnings(tbl.now::tbl_now(cumulative, event_date = event,
    report_date = report, case_count = n, revision_date = revision_date,
    revision_type = revision_type, data_type = "count-cumulative", verbose = FALSE))
  expect_error(
    suppressMessages(suppressWarnings(nowcast(tn_validated, model(), now = simulated$now))),
    "cannot carry .*confirmed.* revisions")
})

test_that("the confirmed-only reduction matches an onset-to-confirmation count model", {
  # The claim the error message makes, checked: nowcasting the confirmed count from
  # a confirmed-only stream is the ordinary model with the confirmation date in the
  # report slot.  Its fitted lambda must track the confirmation model's, which sees
  # strictly more (it also sees the unconfirmed reports).
  skip_on_cran()
  simulated <- simulate_confirmation_linelist(n_days = 60, seed = 39)
  linelist  <- simulated$linelist

  full <- fit_confirmation(linelist, simulated$now)

  confirmed_only <- linelist[!is.na(linelist$confirmed) &
                               linelist$confirmed <= simulated$now, , drop = FALSE]
  confirmed_only$reported <- confirmed_only$confirmed
  reduced_tn <- suppressWarnings(tbl.now::tbl_now(confirmed_only, event_date = onset,
    report_date = reported, now = simulated$now, data_type = "linelist", verbose = FALSE))
  reduced <- suppressMessages(suppressWarnings(nowcast(reduced_tn,
    model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
    now = simulated$now, type = "one_stage", temporal_effects = "none",
    n_draws = 200, seed = 7)))

  settled <- seq_len(40)
  full_lambda    <- as.numeric(full@fits[[1]]$lambda)[settled]
  reduced_lambda <- as.numeric(reduced@fits[[1]]$lambda)[settled]
  expect_lt(mean(abs(full_lambda - reduced_lambda) / pmax(reduced_lambda, 1)), 0.20)
})

# ── parameters() reports p on the natural scale ────────────────────────────────────

test_that("parameters() gives a usable interval for p", {
  skip_on_cran()
  simulated <- simulate_confirmation_linelist(n_days = 60, seed = 40)
  fitted <- fit_confirmation(simulated$linelist, simulated$now)
  estimates <- parameters(fitted)

  # The bug this guards: `diag()` on the sparse inverse Hessian used to error, and
  # the tryCatch turned EVERY standard error into NA.
  expect_gt(mean(!is.na(estimates$std.error)), 0.9)

  natural <- resolution_probability(fitted)
  expect_equal(nrow(natural), 1L)
  expect_gt(natural$conf.low, 0); expect_lt(natural$conf.high, 1)
  expect_lt(natural$conf.low, simulated$p_true)
  expect_gt(natural$conf.high, simulated$p_true)
  # The natural-scale row is the transformed logit interval, exactly.
  logit_row <- estimates[estimates$term == "logit_confirm_p", ]
  expect_equal(natural$estimate, stats::plogis(logit_row$estimate))
  expect_equal(natural$conf.low, stats::plogis(logit_row$conf.low))
})

test_that("parameters() names one p per stratum under stratified_p", {
  skip_on_cran()
  simulated <- simulate_two_site_linelist(n_days = 60, seed = 41)
  tn <- as_revision_tbl_now(simulated$linelist, simulated$now, strata = site)
  fitted <- suppressMessages(suppressWarnings(nowcast(tn,
    model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
          revision = revision_process(revision_delay = dirichlet_revision(bins = 8),
                                              stratified_p = TRUE)),
    now = simulated$now, type = "one_stage",
    temporal_effects = "none", n_draws = 50, seed = 4)))
  estimates <- parameters(fitted)
  natural <- estimates[grepl("^prob_[a-z_]+\\[", estimates$term), ]
  expect_equal(nrow(natural), 2L)
  # Rows are labelled and ordered by stratum level (A, B), so they line up with
  # `p_true` as given -- NOT with a sorted copy of it.
  expect_equal(natural$term, c("prob_not_retracted[A]", "prob_not_retracted[B]"))
  expect_true(all(natural$conf.low < simulated$p_true))
  expect_true(all(natural$conf.high > simulated$p_true))
})

test_that("future confirmations are pending as of the backtest date", {
  rows <- data.frame(
    onset = as.Date("2020-01-01"),
    reported = as.Date("2020-01-02"),
    revision_date = as.Date(c("2020-01-03", "2020-01-05", NA)),
    revision_type = c("confirmed", "confirmed", "pending"),
    n = c(2L, 3L, 5L)
  )
  data <- suppressWarnings(tbl.now::tbl_now(
    rows, event_date = onset, report_date = reported, case_count = n,
    revision_date = revision_date, revision_type = revision_type,
    units = "days", data_type = "count-incidence", verbose = FALSE
  ))
  specification <- model(
    poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
    revision_process(mode = "confirmation_only")
  )
  engine <- suppressMessages(prepare_from_tbl_now(
    data, specification, now = as.Date("2020-01-03"),
    revision_mode = "confirmation_only"
  ))$data

  # Only the first laboratory result is known by January 3. The later result is
  # still pending, exactly like the structurally unconfirmed Probable row.
  expect_equal(sum(engine$n_positive_by_stratum), 2)
  expect_equal(sum(engine$standing_counts), 8)

  # Full-data backtest truth for confirmation-only data is confirmed cases only.
  truth <- diseasenowcasting:::.revision_truth_source(data)
  expect_equal(sum(truth$n), 5)
})

test_that("confirmation revision works in one-stage and stepwise two-stage fits", {
  skip_on_cran()
  simulated <- simulate_confirmation_linelist(n_days = 40, seed = 42)
  rows <- simulated$linelist
  rows$revision_date <- rows$confirmed
  rows$revision_type <- ifelse(is.na(rows$confirmed), "pending", "confirmed")
  data <- suppressWarnings(tbl.now::tbl_now(
    rows, event_date = onset, report_date = reported,
    revision_date = revision_date, revision_type = revision_type,
    data_type = "linelist", verbose = FALSE
  ))
  specification <- model(
    poisson_likelihood(), ar1_epidemic(phi = 0.7, sigma = 0.1),
    lognormal_delay(),
    revision_process(lognormal_revision(), mode = "confirmation_only")
  )
  fit_with <- function(type) suppressMessages(suppressWarnings(nowcast(
    data, specification, now = simulated$now, type = type, K = 3,
    temporal_effects = "none", n_draws = 20, seed = 43
  )))

  joint <- fit_with("one_stage")
  staged <- fit_with("two_stage")

  expect_equal(joint@rung, "onestage")
  expect_length(joint@fits, 1L)
  expect_equal(staged@rung, "multi")
  expect_gt(length(staged@fits), 1L)
  expect_true(all(vapply(staged@fits, function(x) x$opt$convergence == 0L,
                         logical(1))))
  # Reporting-delay parameters are imputed/fixed in Stage 2. Revision delay
  # and p remain free and are sampled from each Stage-2 posterior.
  expect_true(all(vapply(staged@fits,
    function(x) isTRUE(x$priors$delay_mu$is_constant == 1L), logical(1))))
  expect_true(all(vapply(staged@fits,
    function(x) isTRUE(x$priors$retract_mu$is_constant == 0L), logical(1))))
  expect_true(all(vapply(staged@fits,
    function(x) isTRUE(x$priors$confirm_p$is_constant == 0L), logical(1))))
  expect_true(all(is.finite(median(joint))))
  expect_true(all(is.finite(median(staged))))
})

test_that("both-sign revision works in one-stage and stepwise two-stage fits", {
  skip_on_cran()
  simulated <- simulate_both_signs_linelist(n_days = 40, seed = 48)
  data <- as_revision_tbl_now(simulated$linelist, simulated$now)
  specification <- model(
    poisson_likelihood(), ar1_epidemic(phi = 0.7, sigma = 0.1),
    lognormal_delay(),
    revision_process(lognormal_revision(), mode = "both")
  )
  fit_with <- function(type) suppressMessages(suppressWarnings(nowcast(
    data, specification, now = simulated$now, type = type, K = 3,
    temporal_effects = "none", n_draws = 20, seed = 49
  )))

  joint <- fit_with("one_stage")
  staged <- fit_with("two_stage")

  expect_equal(joint@rung, "onestage")
  expect_equal(staged@rung, "multi")
  expect_gt(length(staged@fits), 1L)
  expect_true(all(vapply(staged@fits,
    function(x) isTRUE(x$priors$delay_mu$is_constant == 1L), logical(1))))
  expect_true(all(vapply(staged@fits,
    function(x) isTRUE(x$priors$retract_mu$is_constant == 0L), logical(1))))
  expect_true(all(vapply(staged@fits,
    function(x) isTRUE(x$priors$confirm_p$is_constant == 0L), logical(1))))
  expect_true(all(is.finite(median(joint))))
  expect_true(all(is.finite(median(staged))))
})
