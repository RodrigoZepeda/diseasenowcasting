# =============================================================================
# Shared simulators for the resolution-process tests
# =============================================================================
# testthat sources each test file in its own environment, so anything used by
# more than one of them lives here.
# =============================================================================

# Simulate a linelist from the marked point process of the vignette: settled
# genuine mean `lambda_t`, gross reports Poisson(lambda_t / p), appearance delay
# 1 + Poisson(3), and a retraction 1 + Poisson(2) periods later for the erroneous
# ones.  Returns the frame plus the true settled count per event time.
simulate_retraction_linelist <- function(n_days = 70, p_true = 0.85, seed = 1) {
  set.seed(seed)
  origin <- as.Date("2023-01-01")
  lambda <- 25 * exp(0.8 * sin(2 * pi * seq_len(n_days) / 55))
  per_day <- lapply(seq_len(n_days), function(day) {
    n_gross <- rpois(1, lambda[day] / p_true)
    if (n_gross == 0) return(NULL)
    appearance <- 1 + rpois(n_gross, 3)
    genuine    <- runif(n_gross) < p_true
    lag        <- 1 + rpois(n_gross, 2)
    data.frame(
      onset     = origin + day - 1,
      reported  = origin + day - 1 + appearance,
      retracted = as.Date(ifelse(genuine, NA,
                                 as.numeric(origin + day - 1 + appearance + lag)),
                          origin = "1970-01-01"))
  })
  linelist <- do.call(rbind, per_day)
  list(linelist = linelist, now = origin + n_days - 1, origin = origin, p_true = p_true)
}

# The simulators above record the OUTCOME as which of two date columns is filled
# (`confirmed` / `retracted`), because that is how the maths is written.  A
# `tbl_now` records it as ONE date plus an outcome, and since 2.2.0 that is the
# only representation `nowcast()` reads -- it detects the validation process from
# the object rather than taking a column-name argument.  This folds one into the
# other, so the simulators do not have to change.
as_validation_tbl_now <- function(linelist, now, ...) {
  confirmed <- if ("confirmed" %in% names(linelist)) linelist$confirmed else
    as.Date(rep(NA_real_, nrow(linelist)), origin = "1970-01-01")
  retracted <- if ("retracted" %in% names(linelist)) linelist$retracted else
    as.Date(rep(NA_real_, nrow(linelist)), origin = "1970-01-01")

  linelist$validation_date <- dplyr::coalesce(confirmed, retracted)
  linelist$validation_type <- ifelse(!is.na(confirmed), "confirmed",
                              ifelse(!is.na(retracted), "retracted", "pending"))

  # NOTE: `now` is deliberately NOT pinned here.  A tbl_now refuses to hold a
  # validation dated after its own `now` (tbl.now#51), and the simulators resolve
  # cases past the analysis date on purpose -- that is what the as-of masking in
  # prepare_from_tbl_now() exists to handle.  So let the object infer `now` from
  # the data and pass the ANALYSIS date to nowcast(now = ) instead.
  suppressWarnings(tbl.now::tbl_now(
    linelist, event_date = onset, report_date = reported,
    validation_date = validation_date, validation_type = validation_type,
    data_type = "linelist", verbose = FALSE, ...))
}

# An ordinary two-date tbl_now, for the tests that deliberately carry NO
# validation process.
as_retraction_tbl_now <- function(linelist, now) {
  suppressWarnings(tbl.now::tbl_now(linelist, event_date = onset, report_date = reported,
                                    now = now, data_type = "linelist", verbose = FALSE))
}


simulate_two_site_linelist <- function(n_days = 70, p_true = c(0.92, 0.70), seed = 11) {
  set.seed(seed)
  origin <- as.Date("2023-01-01")
  per_site <- lapply(seq_along(p_true), function(site) {
    site_rows <- lapply(seq_len(n_days), function(day) {
      n_gross <- rpois(1, 28 * exp(0.7 * sin(2 * pi * day / 55)) / p_true[site])
      if (n_gross == 0) return(NULL)
      appearance <- 1 + rpois(n_gross, 3)
      genuine    <- runif(n_gross) < p_true[site]
      lag        <- 1 + rpois(n_gross, 2)
      data.frame(site = LETTERS[site], onset = origin + day - 1,
                 reported = origin + day - 1 + appearance,
                 retracted = as.Date(ifelse(genuine, NA,
                              as.numeric(origin + day - 1 + appearance + lag)),
                              origin = "1970-01-01"))
    })
    do.call(rbind, site_rows)
  })
  linelist <- do.call(rbind, per_site)
  # Settled truth per (site, event time), on the full 1..n_days grid so it lines up
  # with the predictive columns.
  event_grid <- origin + seq_len(n_days) - 1
  truth <- lapply(LETTERS[seq_along(p_true)], function(site_label) {
    settled <- linelist |>
      dplyr::filter(.data$site == site_label, is.na(.data$retracted)) |>
      dplyr::count(.data$onset, name = "n")
    counts <- settled$n[match(event_grid, settled$onset)]
    counts[is.na(counts)] <- 0
    as.numeric(counts)
  })
  list(linelist = linelist, now = origin + n_days - 1, p_true = p_true,
       truth = truth, event_grid = event_grid)
}


# Overdispersed generator: a per-origin gamma frailty multiplies the gross report
# rate, so the counts are genuinely negative-binomial rather than Poisson.
simulate_overdispersed_retraction_linelist <- function(n_days = 80, p_true = 0.8,
                                                       phi = 0.15, seed = 21) {
  set.seed(seed)
  origin <- as.Date("2023-01-01")
  lambda <- 60 * exp(0.8 * sin(2 * pi * seq_len(n_days) / 60))
  per_day <- lapply(seq_len(n_days), function(day) {
    frailty <- stats::rgamma(1, shape = 1 / phi, rate = 1 / phi)
    n_gross <- stats::rpois(1, frailty * lambda[day] / p_true)
    if (n_gross == 0) return(NULL)
    appearance <- 1 + stats::rpois(n_gross, 3)
    genuine    <- stats::runif(n_gross) < p_true
    lag        <- 1 + stats::rpois(n_gross, 2)
    data.frame(onset = origin + day - 1, reported = origin + day - 1 + appearance,
               retracted = as.Date(ifelse(genuine, NA,
                 as.numeric(origin + day - 1 + appearance + lag)), origin = "1970-01-01"))
  })
  list(linelist = do.call(rbind, per_day), now = origin + n_days - 1,
       origin = origin, p_true = p_true, phi = phi)
}

# event -> report -> (confirmed OR retracted): ONE resolution at lag R ~ Poisson,
# whose sign is positive with probability p.  Both dates are recorded.
simulate_both_signs_linelist <- function(n_days = 70, p_true = 0.65,
                                         lag_mean = 1.5, seed = 77) {
  set.seed(seed)
  origin <- as.Date("2023-01-01")
  lambda <- 40 * exp(0.8 * sin(2 * pi * seq_len(n_days) / 55))
  per_day <- lapply(seq_len(n_days), function(day) {
    n_gross <- stats::rpois(1, lambda[day] / p_true)
    if (n_gross == 0) return(NULL)
    appearance <- 1 + stats::rpois(n_gross, 3)
    resolution_lag <- stats::rpois(n_gross, lag_mean)      # may be 0
    positive <- stats::runif(n_gross) < p_true
    resolved <- origin + day - 1 + appearance + resolution_lag
    data.frame(onset = origin + day - 1, reported = origin + day - 1 + appearance,
      confirmed = as.Date(ifelse(positive,  as.numeric(resolved), NA), origin = "1970-01-01"),
      retracted = as.Date(ifelse(!positive, as.numeric(resolved), NA), origin = "1970-01-01"))
  })
  list(linelist = do.call(rbind, per_day), now = origin + n_days - 1,
       origin = origin, p_true = p_true, lag_mean = lag_mean)
}

# One fitting entry point for all three modes.
fit_resolution <- function(linelist, now, likelihood = nb_likelihood(),
                           n_draws = 200, validation_delay = NULL,
                           .validation_mode = "auto", ...) {
  tn <- as_validation_tbl_now(linelist, now)
  validation_delay <- validation_delay %||% dirichlet_validation(bins = 8)
  suppressMessages(suppressWarnings(nowcast(tn,
    model(likelihood, ar1_epidemic(), lognormal_delay(),
          validation = validation_process(validation_delay,
                                          mode = .validation_mode)),
    now = now, type = "one_stage", temporal_effects = "none",
    n_draws = n_draws, seed = 7, ...)))
}

# The natural-scale resolution probability row of `parameters()`.  Its NAME depends on
# what is being modelled (`prob_not_retracted` vs `prob_confirmed`), so tests ask
# for it by role rather than hard-coding either.
resolution_probability <- function(fitted, conf.level = 0.95) {
  estimates <- parameters(fitted, conf.level = conf.level)
  estimates[grepl("^prob_(confirmed|not_retracted)$", estimates$term), , drop = FALSE]
}
