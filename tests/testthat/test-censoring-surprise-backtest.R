# Tests for: backtest max_delay cutoff, surprise-at-update warnings, and
# censored-delay (m_censored) support.

suppressMessages(library(tbl.now))

# ── TASK 1: backtest max_delay ───────────────────────────────────────────────

test_that("backtest excludes evaluation dates without complete truth", {
  set.seed(1)
  start <- as.Date("2020-01-01")
  df <- data.frame(onset = start + rep(0:70, each = 3))
  df$reported <- df$onset + rpois(nrow(df), 3)
  tn <- tbl_now(df, event_date = onset, report_date = reported,
                data_type = "linelist", verbose = FALSE)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  bt  <- suppressWarnings(backtest(tn, mdl, n_dates = 5, type = "one_stage",
                                   n_draws = 100, seed = 1))
  # The most recent evaluation date must leave room for its truth to complete.
  last_eval   <- max(as.Date(bt@results$date_run))
  last_report <- max(df$reported)
  expect_true(last_eval < last_report)
})

test_that("backtest with max_delay = Inf keeps user-supplied recent dates", {
  set.seed(2)
  start <- as.Date("2020-01-01")
  df <- data.frame(onset = start + rep(0:50, each = 3))
  df$reported <- df$onset + rpois(nrow(df), 2)
  tn <- tbl_now(df, event_date = onset, report_date = reported,
                data_type = "linelist", verbose = FALSE)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  recent <- start + c(45, 49)
  bt <- suppressWarnings(backtest(tn, mdl, dates = recent, max_delay = Inf,
                                  type = "one_stage", n_draws = 100, seed = 1))
  expect_setequal(as.Date(unique(bt@results$date_run)), recent)
})

# ── TASK 3: censored delays (m_censored) ─────────────────────────────────────

test_that("censor_delays_above flags long-delay reports", {
  df <- data.frame(onset = as.Date("2020-01-01") + c(0, 0, 1, 2),
                   reported = as.Date("2020-01-01") + c(1, 5, 2, 300))
  tn <- tbl_now(df, event_date = onset, report_date = reported,
                data_type = "linelist", verbose = FALSE)
  tn2 <- censor_delays_above(tn, max_delay = 60, quiet = TRUE)
  cc  <- tbl.now::get_is_censored(tn2)
  expect_true(length(cc) == 1L)
  expect_equal(sum(as.logical(tn2[[cc]])), 1L)            # only the 300-day report
})

test_that("a censored tbl_now feeds m_censored through to the engine", {
  set.seed(3)
  start <- as.Date("2020-01-01")
  df <- data.frame(onset = start + rep(0:40, each = 3))
  df$reported <- df$onset + rpois(nrow(df), 2)
  df$reported[1] <- df$onset[1] + 200                     # outlier
  tn  <- tbl_now(df, event_date = onset, report_date = reported,
                 data_type = "linelist", verbose = FALSE)
  tn_c <- censor_delays_above(tn, max_delay = 30, quiet = TRUE)
  mdl  <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  prep <- prepare_from_tbl_now(tn_c, mdl)
  expect_true(length(prep$data$obs_delays_cens) >= 1L)    # censored delays present
  expect_true(sum(prep$data$row_sums_cens) >= 1)
})

test_that("censoring an outlier delay changes the fitted delay distribution", {
  set.seed(4)
  start <- as.Date("2020-01-01")
  df <- data.frame(onset = start + rep(0:40, each = 4))
  df$reported <- df$onset + rpois(nrow(df), 2)
  df$reported[1] <- df$onset[1] + 250                     # extreme outlier delay
  tn   <- tbl_now(df, event_date = onset, report_date = reported,
                  data_type = "linelist", verbose = FALSE)
  tn_c <- censor_delays_above(tn, max_delay = 30, quiet = TRUE)
  mdl  <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  nc_plain <- nowcast(tn,   mdl, type = "one_stage", n_draws = 150,
                      temporal_effects = "none", seed = 1)
  nc_cens  <- nowcast(tn_c, mdl, type = "one_stage", n_draws = 150,
                      temporal_effects = "none", seed = 1)
  # The outlier inflates the plain delay mean; censoring shrinks it.
  expect_lt(coef(nc_cens)["delay_mu"], coef(nc_plain)["delay_mu"])
  expect_true(is.finite(coef(nc_cens)["delay_mu"]))
})

# ── TASK 2: surprise at update ───────────────────────────────────────────────

test_that("surprise() flags a count direction (high/low) and respects level", {
  tn <- .make_synth_tblnow(Tn = 60L, seed = 5)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 200, seed = 1)
  # An absurdly high count at a mid event-time should flag "high"
  s  <- surprise(nc, data.frame(event_index = 30, count = 1e5),
                 type = "count", level = 0.99, n_draws = 150, seed = 2)
  expect_true("direction" %in% names(s$count_surprise))
  expect_equal(s$count_surprise$direction[1], "high")
  expect_true(s$count_surprise$is_surprising[1])
})

test_that("surprise() flags a surprisingly long delay", {
  tn <- .make_synth_tblnow(Tn = 60L, seed = 6)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 200, seed = 1)
  s  <- surprise(nc, data.frame(delay = c(2, 300)),
                 type = "delay", level = 0.99, n_draws = 150, seed = 2)
  expect_true("direction" %in% names(s$delay_surprise))
  long_row <- s$delay_surprise[s$delay_surprise$delay == 300, ]
  expect_equal(long_row$direction, "long")
  expect_true(long_row$is_surprising)
})

test_that("delay surprise works for the Dirichlet (non-parametric) delay", {
  # Regression: the Dirichlet branch of the delay-surprise modal-pmf computation
  # used to reference an undefined `rc`/`parlist` and error.
  tn <- .make_synth_tblnow(Tn = 60L, seed = 6)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay()),
                type = "one_stage", n_draws = 150, seed = 1)
  s  <- surprise(nc, data.frame(delay = c(2, 5, 12, 300)),
                 type = "delay", level = 0.99, n_draws = 120, seed = 2)
  ds <- s$delay_surprise
  expect_equal(nrow(ds), 4L)
  expect_true(all(c("mean_tail_prob", "relative_surprise", "direction", "is_surprising") %in% names(ds)))
  expect_true(all(is.finite(ds$mean_tail_prob)))               # no NA/crash
  expect_true(all(diff(ds$mean_tail_prob) <= 1e-8))            # P(D >= d) is non-increasing in d
  expect_equal(ds$direction[ds$delay == 300], "long")          # a 300-unit delay is surprisingly long
})

test_that("update() warns about a surprising new delay and stores the result", {
  set.seed(7); start <- as.Date("2020-01-01")
  df <- data.frame(onset = start + rep(0:40, each = 4))
  df$reported <- df$onset + rpois(nrow(df), 2)
  tn <- tbl_now(df, event_date = onset, report_date = reported,
                data_type = "linelist", verbose = FALSE)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", now = start + 40, n_draws = 200,
                temporal_effects = "none", seed = 1)
  # New data: a report for an existing event-time with a 300-day delay
  new <- data.frame(onset = start + 41, reported = start + 41 + 300)

  # A SINGLE warning, naming the delay in the data's units ("days") and only
  # flagging the too-LONG delay (no count/epidemic surprise).
  w <- testthat::capture_warnings(
    nc2 <- update(nc, new, now = start + 41, temporal_effects = "none"))
  expect_length(w, 1L)
  expect_match(w, "Surprising reporting delay of .* days")
  expect_match(w, "longer than the model expects")
  expect_match(w, "extreme_values\\(nc\\)")          # code hint to see the surprises

  ev <- extreme_values(nc2)
  expect_s3_class(ev, "data.frame")                    # tidy table of flagged surprises
  expect_gte(nrow(ev), 1L)                             # the 300-day delay is flagged
  expect_true(all(ev$surprise == "delay"))             # delay-only (no epidemic/count surprise)
  expect_true(all(ev$direction == "long"))             # only too-long delays
})

test_that("update(compute_surprise = FALSE) is silent and still refits", {
  set.seed(8); start <- as.Date("2020-01-01")
  df <- data.frame(onset = start + rep(0:30, each = 4))
  df$reported <- df$onset + rpois(nrow(df), 2)
  tn <- tbl_now(df, event_date = onset, report_date = reported,
                data_type = "linelist", verbose = FALSE)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", now = start + 30, n_draws = 150,
                temporal_effects = "none", seed = 1)
  new <- data.frame(onset = start + 31, reported = start + 31 + 300)
  nc2 <- suppressMessages(update(nc, new, now = start + 31,
                                 compute_surprise = FALSE, temporal_effects = "none"))
  expect_null(extreme_values(nc2))
  expect_true(is.finite(coef(nc2)["delay_mu"]))
})
