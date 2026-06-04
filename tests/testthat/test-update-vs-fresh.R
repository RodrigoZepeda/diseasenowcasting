# Test: update() on new data ≈ fresh nowcast() on the combined dataset.
#
# Why this matters: update() warm-starts from the previous fit, so it may reach
# a slightly different optimum than a cold fit.  We want to confirm that the
# estimates are *in the same ballpark* -- not that they are numerically identical.
# The comparison is done at well-observed event times (far from the right-censored
# edge), where both fits see the same data and the posterior is tight enough for a
# meaningful comparison.

suppressMessages(library(tbl.now))

# Shared data generator for this test file: bell-shaped epidemic + LogNormal delays
.make_split_tblnow <- function(Tn, split_day, peak = 40, ctr = 40, wid = 15,
                               base = 3, mu_log = log(5), sigma = 3, seed = 42) {
  set.seed(seed)
  start  <- as.Date("2023-01-01")
  ln     <- lognormal_native(mu_log, sigma)
  rows   <- list()
  for (t in seq_len(Tn)) {
    n <- rpois(1, peak * exp(-0.5 * ((t - ctr) / wid)^2) + base)
    if (n > 0) for (i in seq_len(n)) {
      d <- max(0L, round(rlnorm(1, ln$log_location, ln$log_scale)))
      rows[[length(rows) + 1]] <- data.frame(onset    = start + (t - 1L),
                                             reported = start + (t - 1L) + d)
    }
  }
  all_rows <- do.call(rbind, rows)
  all_rows <- all_rows[all_rows$reported <= start + (Tn - 1L), ]

  old_rows <- all_rows[all_rows$reported <= start + (split_day - 1L), ]
  new_rows <- all_rows[all_rows$reported >  start + (split_day - 1L), ]

  make_tn <- function(df) tbl_now(df, event_date = onset, report_date = reported,
                                  data_type = "linelist", verbose = FALSE)
  list(full = make_tn(all_rows), old = make_tn(old_rows), new_rows = new_rows)
}

# ── update() ≈ fresh nowcast(): structure ─────────────────────────────────────

test_that("update() target matches a fresh nowcast() on the same data", {
  dat <- .make_split_tblnow(Tn = 70L, split_day = 50L)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())

  nc_fresh   <- suppressMessages(nowcast(dat$full, mdl, type = "one_stage",
                                         n_draws = 200L, seed = 1L,
                                         temporal_effects = "none"))
  nc_old     <- suppressMessages(nowcast(dat$old,  mdl, type = "one_stage",
                                         n_draws = 200L, seed = 1L,
                                         temporal_effects = "none"))
  nc_updated <- suppressWarnings(update(nc_old, dat$new_rows, compute_surprise = FALSE))

  # Both nowcasts cover the same full series
  expect_equal(nc_updated@target, nc_fresh@target)

  # Updated object is a valid nowcast
  expect_true(S7::S7_inherits(nc_updated, diseasenowcasting:::nowcast_class))
  expect_length(nc_updated@fits, 1L)
})

# ── update() ≈ fresh nowcast(): delay parameter estimates ─────────────────────

test_that("update() and fresh nowcast() recover similar delay parameters", {
  dat <- .make_split_tblnow(Tn = 70L, split_day = 50L)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())

  nc_fresh   <- suppressMessages(nowcast(dat$full, mdl, type = "one_stage",
                                         n_draws = 200L, seed = 1L,
                                         temporal_effects = "none"))
  nc_old     <- suppressMessages(nowcast(dat$old,  mdl, type = "one_stage",
                                         n_draws = 200L, seed = 1L,
                                         temporal_effects = "none"))
  nc_updated <- suppressWarnings(update(nc_old, dat$new_rows, compute_surprise = FALSE))

  cf_fresh   <- coef(nc_fresh)
  cf_updated <- coef(nc_updated)

  # delay_mu estimates should be within 1 log-unit of each other -- both fits
  # see the same delay data, so their delay estimates should agree well.
  expect_true(abs(cf_fresh["delay_mu"] - cf_updated["delay_mu"]) < 1.0,
              label = "delay_mu: fresh vs updated within 1 unit")
  expect_true(cf_fresh["delay_mu"]   > 0, label = "fresh delay_mu is positive")
  expect_true(cf_updated["delay_mu"] > 0, label = "updated delay_mu is positive")
})

# ── update() ≈ fresh nowcast(): posterior incidence summaries ─────────────────

test_that("update() and fresh nowcast() give consistent posterior medians", {
  dat <- .make_split_tblnow(Tn = 70L, split_day = 50L)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())

  nc_fresh   <- suppressMessages(nowcast(dat$full, mdl, type = "one_stage",
                                         n_draws = 300L, seed = 1L,
                                         temporal_effects = "none"))
  nc_old     <- suppressMessages(nowcast(dat$old,  mdl, type = "one_stage",
                                         n_draws = 300L, seed = 1L,
                                         temporal_effects = "none"))
  nc_updated <- suppressWarnings(update(nc_old, dat$new_rows, compute_surprise = FALSE))

  med_fresh   <- median(nc_fresh,   seed = 42L)
  med_updated <- median(nc_updated, seed = 42L)

  # Check at event times near the epidemic peak (well-observed, not right-censored).
  # Both models see the same data at these times, so their medians should agree
  # within a factor of 2 -- a generous but meaningful tolerance.
  check_times <- 25:45
  log_ratio <- log(med_fresh[check_times] / med_updated[check_times])
  pos_ratio  <- log_ratio[is.finite(log_ratio) & med_fresh[check_times] > 1]

  expect_true(all(abs(pos_ratio) < log(2)),
              label = "posterior medians at the epidemic peak agree within a factor of 2")
})

# ── update() ≈ fresh nowcast(): CI containment ────────────────────────────────

test_that("fresh nowcast median falls within the updated nowcast's 95% CI", {
  dat <- .make_split_tblnow(Tn = 70L, split_day = 50L)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())

  nc_fresh   <- suppressMessages(nowcast(dat$full, mdl, type = "one_stage",
                                         n_draws = 300L, seed = 1L,
                                         temporal_effects = "none"))
  nc_old     <- suppressMessages(nowcast(dat$old,  mdl, type = "one_stage",
                                         n_draws = 300L, seed = 1L,
                                         temporal_effects = "none"))
  nc_updated <- suppressWarnings(update(nc_old, dat$new_rows, compute_surprise = FALSE))

  # Well-observed event times: check that the fresh posterior median
  # falls inside the updated nowcast's 95% posterior credible interval.
  check_times <- 25:45
  med_fresh   <- median(nc_fresh,   seed = 42L)[check_times]
  q_updated   <- quantile(nc_updated, probs = c(0.025, 0.975), seed = 42L)[check_times, ]

  in_ci <- med_fresh >= q_updated[, 1] & med_fresh <= q_updated[, 2]
  # Require at least 80% of the checked event times to be contained
  expect_true(mean(in_ci) >= 0.8,
              label = "fresh median falls within updated 95% CI at >=80% of checked times")
})
