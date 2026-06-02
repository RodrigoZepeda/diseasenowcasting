# Tests for: automatic temporal effects, deterministic full-grid covariates,
# and per-stratum + global predict()/summary() output.

suppressMessages(library(tbl.now))

# ── Helper: daily synthetic linelist ─────────────────────────────────────────
.daily_linelist <- function(Tn = 40L, seed = 1, start = as.Date("2020-01-01")) {
  set.seed(seed)
  rows <- list()
  for (t in seq_len(Tn)) {
    n <- rpois(1, 8 + 5 * sin(2 * pi * t / 14))
    if (n > 0) for (i in seq_len(n)) {
      d <- rpois(1, 2)
      rows[[length(rows) + 1]] <- data.frame(onset = start + (t - 1),
                                             reported = start + (t - 1) + d)
    }
  }
  do.call(rbind, rows)
}

# ── 1. Automatic temporal effects ────────────────────────────────────────────

test_that("nowcast() auto-adds temporal effects (message + covariates)", {
  tnw <- .make_synth_tblnow(Tn = 60L, seed = 1)
  expect_message(
    nc <- nowcast(tnw, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                  type = "one_stage", n_draws = 100, seed = 1),
    regexp = "temporal effects"
  )
  expect_gt(nc@engine$P, 0L)            # covariates were added
})

test_that("temporal_effects = 'none' disables automatic effects", {
  tnw <- .make_synth_tblnow(Tn = 60L, seed = 2)
  nc <- nowcast(tnw, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 100, temporal_effects = "none", seed = 1)
  expect_equal(nc@engine$P, 0L)         # no covariates
})

test_that("user-supplied temporal effects are respected (no double-add, no message)", {
  d  <- .daily_linelist(Tn = 40L, seed = 3)
  tn <- tbl_now(d, event_date = onset, report_date = reported,
                data_type = "linelist", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
    compute_temporal_effects()
  expect_no_message(
    nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                  type = "one_stage", n_draws = 100,
                  now = as.Date("2020-02-09"), seed = 1)
  )
  expect_gt(nc@engine$P, 0L)
})

# ── 2. Deterministic full-grid covariates (nowcast past last observation) ─────

test_that(".temporal_effect_matrix spans the full grid and matches wday", {
  d  <- .daily_linelist(Tn = 20L, seed = 4)
  tn <- tbl_now(d, event_date = onset, report_date = reported,
                data_type = "linelist", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE, seasons = 52)) |>
    compute_temporal_effects()

  # nowcast as of a date AFTER the last observation
  now <- as.Date("2020-01-25")
  prep <- prepare_from_tbl_now(tn, model(), now = now)
  X <- prep$data$X
  expect_equal(nrow(X), prep$data$max_time)
  expect_false(any(rowSums(abs(X)) == 0))    # every event-time has covariates

  # day-of-week column equals lubridate-style wday on the grid
  grid <- as.Date("2020-01-01") + (seq_len(prep$data$max_time) - 1L)
  expect_equal(as.numeric(X[, ".event_day_of_week"]),
               as.numeric(as.POSIXlt(grid)$wday + 1))
})

test_that("nowcast works when there are no recent observations before `now`", {
  # Last observation well before `now`: no need to complete with zeros.
  d  <- .daily_linelist(Tn = 15L, seed = 5)            # onsets up to 2020-01-15
  tn <- tbl_now(d, event_date = onset, report_date = reported,
                data_type = "linelist", verbose = FALSE)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 100,
                now = as.Date("2020-01-25"), seed = 1)   # 10 days past last onset
  s <- summary(predict(nc, seed = 2))
  expect_true(all(is.finite(s$median)))
  expect_equal(nc@target, nc@engine$max_time)
})

# ── 3. predict() / summary() expose per-stratum AND global ────────────────────

test_that("predict() carries both the global total and per-stratum draws", {
  # Two-stratum synthetic
  set.seed(6); start <- as.Date("2023-01-01"); ln <- lognormal_native(log(4), 3)
  rows <- list()
  gen <- function(g, peak, amp) for (t in 1:60) {
    n <- rpois(1, amp * exp(-0.5 * ((t - peak) / 12)^2) + 3)
    if (n > 0) for (i in seq_len(n)) {
      dd <- max(0L, round(rlnorm(1, ln$log_location, ln$log_scale)))
      rows[[length(rows) + 1]] <<- data.frame(onset = start + t - 1,
                                              reported = start + t - 1 + dd, grp = g)
    }
  }
  gen("A", 30, 30); gen("B", 40, 18)
  d2 <- do.call(rbind, rows); d2 <- d2[d2$reported <= start + 59, ]
  tn <- tbl_now(d2, event_date = onset, report_date = reported, strata = grp,
                data_type = "linelist", verbose = FALSE)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 150, seed = 1)

  pr <- predict(nc, seed = 2)
  # Global total draws present
  expect_true(is.matrix(pr@draws))
  expect_equal(ncol(pr@draws), nc@target)
  # Per-stratum draws present
  expect_false(is.null(pr@strata_draws))
  expect_equal(dim(pr@strata_draws)[3], 2L)
  # The global equals the sum over strata (within Monte-Carlo error -> exact here)
  total_from_strata <- pr@strata_draws[, , 1] + pr@strata_draws[, , 2]
  expect_equal(unname(pr@draws), unname(total_from_strata), tolerance = 1e-8)
})

test_that("summary(predict()) returns per-stratum blocks plus a Total block", {
  set.seed(7); start <- as.Date("2023-01-01"); ln <- lognormal_native(log(4), 3)
  rows <- list()
  gen <- function(g, peak, amp) for (t in 1:60) {
    n <- rpois(1, amp * exp(-0.5 * ((t - peak) / 12)^2) + 3)
    if (n > 0) for (i in seq_len(n)) {
      dd <- max(0L, round(rlnorm(1, ln$log_location, ln$log_scale)))
      rows[[length(rows) + 1]] <<- data.frame(onset = start + t - 1,
                                              reported = start + t - 1 + dd, grp = g)
    }
  }
  gen("A", 30, 30); gen("B", 40, 18)
  d2 <- do.call(rbind, rows); d2 <- d2[d2$reported <= start + 59, ]
  tn <- tbl_now(d2, event_date = onset, report_date = reported, strata = grp,
                data_type = "linelist", verbose = FALSE)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 150, seed = 1)
  s <- summary(predict(nc, seed = 2))
  expect_true(all(c("stratum", "event_date") %in% names(s)))
  expect_setequal(unique(s$stratum), c("A", "B", "Total"))
  expect_equal(nrow(s), nc@target * 3L)
  expect_true(all(is.finite(s$median)))
})

test_that("unstratified summary() has no stratum column but carries event dates", {
  tnw <- .make_synth_tblnow(Tn = 60L, seed = 8)
  nc  <- nowcast(tnw, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 type = "one_stage", n_draws = 120, seed = 1)
  s <- summary(predict(nc, seed = 2))
  expect_false("stratum" %in% names(s))
  expect_equal(nrow(s), nc@target)
})
