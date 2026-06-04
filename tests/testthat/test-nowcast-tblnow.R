# nowcast() on a tbl_now: fit + lazy predict + accessors + update.

test_that("nowcast() fits a tbl_now and the accessors work", {
  tn  <- .make_synth_tblnow(Tn = 80L, seed = 2)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  nc  <- nowcast(tn, mdl, type = "one_stage", n_draws = 400, seed = 1)

  expect_true(S7::S7_inherits(nc, diseasenowcasting:::nowcast_class))
  expect_length(nc@fits, 1L)

  # coef() = parameter estimates
  cf <- coef(nc)
  expect_true(all(c("delay_mu", "delay_sigma", "mu_intercept") %in% names(cf)))
  expect_true(is.finite(cf["delay_mu"]))

  # latent incidence summaries (one value per event-time)
  lat_mean   <- mean(nc, seed = 2)
  lat_median <- median(nc, seed = 2)
  expect_length(lat_mean, nc@target)
  expect_true(all(lat_mean > 0))
  q <- quantile(nc, probs = c(0.1, 0.5, 0.9), seed = 2)
  expect_equal(ncol(q), 3L); expect_equal(nrow(q), nc@target)

  # predict() = posterior-predictive nowcast (lazy)
  pr <- predict(nc, seed = 3)
  expect_true(S7::S7_inherits(pr, diseasenowcasting:::nowcast_prediction_class))
  s <- summary(pr)
  expect_true(all(c("mean", "median", "sd", "q2.5", "q97.5", ".event_num") %in% names(s)))
  expect_equal(nrow(s), nc@target)
})

test_that("a `now` passed to nowcast() overrides the tbl_now's own `now`", {
  tn <- .make_synth_tblnow(Tn = 80L, seed = 2)
  # Attach a `now` to the tbl_now itself, then pass a DIFFERENT, earlier now.
  tbl_now_date <- max(tn[[tbl.now::get_event_date(tn)]], na.rm = TRUE)
  tn <- suppressWarnings(tbl.now::change_now(tn, tbl_now_date))
  passed_now <- tbl_now_date - 20

  # Engine layer: prepared `now` follows the passed argument, not the tbl_now's.
  prep <- diseasenowcasting:::prepare_from_tbl_now(
    tn, model(), now = passed_now)
  expect_equal(as.Date(prep$now), as.Date(passed_now))
  expect_false(isTRUE(as.Date(prep$now) == as.Date(tbl_now_date)))

  # Full nowcast(): the stored as-of date is the passed one.
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", now = passed_now, n_draws = 200, seed = 1)
  expect_equal(as.Date(nc@now), as.Date(passed_now))
})

test_that("two-stage nowcast on a tbl_now pools imputations", {
  tn  <- .make_synth_tblnow(Tn = 90L, seed = 5)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  nc  <- nowcast(tn, mdl, type = "two_stage", K = 5, n_draws = 400, seed = 1)
  expect_equal(nc@type, "two_stage")
  expect_true(length(nc@fits) >= 1L)
  expect_true(all(is.finite(summary(predict(nc, seed = 2))$median)))
})

test_that("update() merges new data and refits to a longer series", {
  set.seed(7); start <- as.Date("2023-01-01")
  ln <- lognormal_native(log(5), 4)
  gen <- function(t1, t2) {
    rows <- list()
    for (t in t1:t2) { n <- rpois(1, 40 * exp(-0.5 * ((t - 60) / 18)^2) + 5)
      if (n > 0) for (i in seq_len(n)) { d <- max(0L, round(rlnorm(1, ln$log_location, ln$log_scale)))
        rows[[length(rows) + 1]] <- data.frame(onset = start + (t - 1), reported = start + (t - 1) + d) } }
    do.call(rbind, rows)
  }
  d_old <- gen(1, 70); d_old <- d_old[d_old$reported <= start + 69, ]
  d_all <- gen(1, 85); d_all <- d_all[d_all$reported <= start + 84, ]
  new_rows <- d_all[d_all$reported > start + 69, ]

  tn_old <- tbl.now::tbl_now(d_old, event_date = onset, report_date = reported, data_type = "linelist")
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  nc  <- nowcast(tn_old, mdl, type = "one_stage", n_draws = 300, seed = 1)
  # update() now warns about surprising new data; not what this test checks.
  nc2 <- suppressWarnings(update(nc, new_rows, compute_surprise = FALSE))

  expect_true(S7::S7_inherits(nc2, diseasenowcasting:::nowcast_class))
  expect_true(nc2@target > nc@target)                 # series grew
  expect_true(is.finite(coef(nc2)["delay_mu"]))
})
