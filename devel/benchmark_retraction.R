# benchmark_retraction.R
#
# Does the linelist retraction (cure) model actually pay for itself?
#
# We take three real linelists -- dengue (weekly), mpox (daily), covid Colombia
# (daily) -- and INJECT retractions: a random share (< 20%) of the cases are
# declared erroneous and given a retraction date `report + C`, C ~ 1 + Poisson.
# The truth is then the number of NEVER-retracted cases per event time, which is
# what a settled surveillance register would eventually show.
#
# Three ways to nowcast that truth:
#
#   retraction  the new model -- pass `retraction_date`; every row informs the
#               appearance delay, retracted rows inform g_C, standing rows are a
#               censored (cure) sample, and the predictive thins the standing
#               rows row by row.
#   keep_all    ignore the retraction column and nowcast every row.  Over-counts:
#               it targets the gross report total, not the settled one.
#   drop_known  ignore the retraction column but delete the rows already known to
#               be retracted.  Under-counts at recent event times, because the
#               retractions that will cancel today's reports have not landed yet
#               -- this is the "multiply by the confirmed fraction" mistake in
#               its most tempting form.
#
# `retraction` should beat both on WIS and get closer to nominal coverage.
#
# Flat, top-to-bottom, run it and read the tables at the bottom.

rm(list = ls())

library(dplyr)
library(tidyr)
library(tbl.now)
library(diseasenowcasting)

# ---- run controls -----------------------------------------------------------
RETRACT_SHARE <- 0.15      # < 20%, per the brief
RETRACT_LAG   <- 2         # C ~ 1 + Poisson(RETRACT_LAG)
N_DATES       <- 6         # as-of dates per disease
N_DRAWS       <- 1000
HORIZONS      <- 0:6       # event times scored, counted back from `now`
SEED          <- 20260726L
PROBS  <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
OUT    <- "devel/results"

set.seed(SEED)
options(max.print = 100000)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# DATA -- one linelist per disease, with an injected retraction date
# =============================================================================
# Each element: a data.frame with columns onset / reported / retracted (Date or
# NA), plus the event unit and the as-of dates to evaluate at.

datasets <- list()

# -- dengue: already a linelist, weekly ---------------------------------------
dengue_line <- tbl.now::denguedat |>
  transmute(onset = as.Date(.data$onset_week), reported = as.Date(.data$report_week)) |>
  filter(.data$onset >= as.Date("2005-01-01"), .data$reported >= .data$onset)
datasets$dengue <- list(line = dengue_line, unit = "week", lag_scale = 1)

# -- mpox: aggregated counts, daily -- expand to one row per case -------------
mpox_line <- as.data.frame(tbl.now::mpoxdat) |>
  transmute(onset = as.Date(.data$dx_date), reported = as.Date(.data$dx_report_date),
            n = as.integer(.data$n)) |>
  filter(.data$reported >= .data$onset) |>
  tidyr::uncount(.data$n)
datasets$mpox <- list(line = mpox_line, unit = "day", lag_scale = RETRACT_LAG)

# -- covid Colombia: aggregated counts, daily -- expand to one row per case ----
covid_line <- as.data.frame(tbl.now::covid_colombia) |>
  transmute(onset = as.Date(.data$diagnosis_date), reported = as.Date(.data$notification_date),
            n = as.integer(.data$n)) |>
  filter(.data$reported >= .data$onset) |>
  tidyr::uncount(.data$n)
datasets$covid <- list(line = covid_line, unit = "day", lag_scale = RETRACT_LAG)

# -- inject retractions -------------------------------------------------------
# A case is erroneous with probability RETRACT_SHARE and is then retracted
# `C ~ 1 + Poisson(lag_scale)` event-units after its report.  On the weekly dengue
# grid the retraction date must land in a LATER week, so the lag is in weeks.
for (disease in names(datasets)) {
  line      <- datasets[[disease]]$line
  step_days <- if (datasets[[disease]]$unit == "week") 7L else 1L
  erroneous <- runif(nrow(line)) < RETRACT_SHARE
  lag_units <- 1L + rpois(nrow(line), datasets[[disease]]$lag_scale)
  line$retracted <- as.Date(NA)
  line$retracted[erroneous] <- line$reported[erroneous] + step_days * lag_units[erroneous]
  datasets[[disease]]$line      <- line
  datasets[[disease]]$erroneous <- erroneous
  cat(sprintf("%-7s %7d cases, %5.1f%% retracted, %s grid, %s .. %s\n",
              disease, nrow(line), 100 * mean(erroneous), datasets[[disease]]$unit,
              min(line$onset), max(line$onset)))
}

# -- as-of dates: spread ACROSS the series, not bunched at its end --------------
# This matters more than it looks.  Anchoring the as-of dates near the last onset
# puts every evaluation in the epidemic TAIL -- COVID Colombia runs ~55 cases/day
# there against a median of 591 and a peak of 19,428 -- and WIS is on the scale of
# the counts, so the whole table lands an order of magnitude below the published
# Benchmark vignette (covid WIS ~839) for no reason but where it was measured.
# Quantiles of the ONSET dates put the evaluation where the epidemic actually is.
# (Onsets, not reports: mpox's reports run months past its final onset, and an
# as-of date out there scores event times whose truth is identically zero, which
# flatters every method equally.)
for (disease in names(datasets)) {
  onsets <- sort(datasets[[disease]]$line$onset)
  datasets[[disease]]$now_dates <-
    unique(as.Date(stats::quantile(onsets, seq(0.35, 0.92, length.out = N_DATES), type = 1)))
}

# =============================================================================
# TRUTH -- settled genuine cases per event time
# =============================================================================
# The value the register converges to: cases that are reported at some point and
# never retracted.  Independent of the as-of date, so computed once per disease.
for (disease in names(datasets)) {
  line  <- datasets[[disease]]$line
  kept  <- line[is.na(line$retracted), , drop = FALSE]
  datasets[[disease]]$truth <- kept |>
    count(.data$onset, name = "truth") |>
    arrange(.data$onset)
}

# =============================================================================
# RUN -- three methods x three diseases x N_DATES as-of dates
# =============================================================================
results <- list()

for (disease in names(datasets)) {
  line      <- datasets[[disease]]$line
  unit      <- datasets[[disease]]$unit
  truth_tbl <- datasets[[disease]]$truth
  step_days <- if (unit == "week") 7L else 1L

  for (now_date in as.list(datasets[[disease]]$now_dates)) {
    # As-of view.  `retracted` is left intact in the frame -- the package masks
    # retractions dated after `now` itself, which is exactly the behaviour we
    # want to exercise here.
    as_of <- line[line$onset <= now_date & line$reported <= now_date, , drop = FALSE]

    for (method in c("retraction", "keep_all", "drop_known")) {
      rows <- switch(method,
        retraction = as_of,
        keep_all   = as_of,
        drop_known = as_of[is.na(as_of$retracted) | as_of$retracted > now_date, , drop = FALSE])

      tn <- suppressWarnings(tbl.now::tbl_now(
        rows[, c("onset", "reported", "retracted")],
        event_date = onset, report_date = reported, now = now_date,
        data_type = "linelist", verbose = FALSE))     # units inferred (weekly for dengue)

      # g_C is Dirichlet, not lognormal.  rho(j) = p / (p + (1-p)(1 - G_C(j))) is
      # applied to every standing case, so at high counts a SHAPE error in g_C
      # biases the nowcast by more than its Monte-Carlo noise: on a COVID peak of
      # ~8000 cases/day a lognormal fitted to this 1 + Poisson(2) lag left a ~0.9%
      # bias and lost nominal coverage, while the Dirichlet recovered rho to four
      # decimals and covered 7/7 horizons.
      fitted <- tryCatch(suppressMessages(suppressWarnings(
        nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
                          confirmation = confirmation_process(
                            retract_delay = dirichlet_retraction(bins = 10))),
                now = now_date, n_draws = N_DRAWS, seed = SEED,
                retraction_date = if (method == "retraction") "retracted" else NULL)
      )), error = function(e) e)

      if (inherits(fitted, "error")) {
        cat(sprintf("  [fail] %s / %s / %s : %s\n", disease, now_date, method,
                    conditionMessage(fitted)))
        next
      }

      prediction  <- predict(fitted)
      # The engine's OWN grid, not a hand-rolled one: `drop_known` fits a different
      # row set, so its earliest onset -- and hence its grid origin -- need not match
      # the other two methods'.
      event_dates <- prediction@event_dates
      scored_idx  <- ncol(prediction@draws) - HORIZONS
      scored_idx  <- scored_idx[scored_idx >= 1L]

      for (column in scored_idx) {
        event_date  <- event_dates[column]
        truth_value <- truth_tbl$truth[match(event_date, truth_tbl$onset)]
        if (is.na(truth_value)) truth_value <- 0
        draws <- prediction@draws[, column]
        draws <- draws[is.finite(draws)]
        if (!length(draws)) next
        quantile_values <- as.numeric(quantile(draws, PROBS, na.rm = TRUE))

        # Weighted interval score: (1/(K + 1/2)) * [ |y - median|/2 + sum_k
        # (alpha_k/2) * IS_{alpha_k} ], the standard quantile decomposition.
        alphas <- 2 * PROBS[PROBS < 0.5]
        interval_scores <- numeric(length(alphas))
        for (k in seq_along(alphas)) {
          lower <- quantile_values[k]
          upper <- quantile_values[length(PROBS) - k + 1L]
          interval_scores[k] <- (upper - lower) +
            (2 / alphas[k]) * (lower - truth_value) * (truth_value < lower) +
            (2 / alphas[k]) * (truth_value - upper) * (truth_value > upper)
        }
        median_value <- quantile_values[PROBS == 0.5]
        wis <- (abs(truth_value - median_value) / 2 + sum(alphas / 2 * interval_scores)) /
          (length(alphas) + 0.5)

        results[[length(results) + 1L]] <- data.frame(
          disease = disease, now = now_date, method = method,
          horizon = ncol(prediction@draws) - column,
          truth = truth_value, median = median_value, wis = wis,
          # Relative WIS makes the three diseases comparable despite counts that
          # differ by three orders of magnitude.
          rel_wis = wis / pmax(truth_value, 1),
          in50 = truth_value >= quantile_values[PROBS == 0.25] &
                 truth_value <= quantile_values[PROBS == 0.75],
          in95 = truth_value >= quantile_values[1] &
                 truth_value <= quantile_values[length(PROBS)],
          p_hat = if (method == "retraction")
            fitted@fits[[1]]$reconstruct$retraction$p else NA_real_)
      }
      cat(sprintf("  done %s / %s / %s\n", disease, now_date, method))
    }
  }
}

scores <- bind_rows(results)

# =============================================================================
# TABLES
# =============================================================================
cat("\n=== per disease x method ===\n")
summary_by_disease <- scores |>
  group_by(.data$disease, .data$method) |>
  summarise(wis = mean(.data$wis), rel_wis = mean(.data$rel_wis),
            mean_truth = mean(.data$truth),
            cov50 = mean(.data$in50), cov95 = mean(.data$in95),
            rel_bias = mean((.data$median - .data$truth) / pmax(.data$truth, 1)),
            p_hat = mean(.data$p_hat, na.rm = TRUE), n = dplyr::n(), .groups = "drop") |>
  arrange(.data$disease, .data$wis)
print(as.data.frame(summary_by_disease), digits = 3)

cat("\n=== pooled over diseases ===\n")
summary_pooled <- scores |>
  group_by(.data$method) |>
  summarise(wis = mean(.data$wis), rel_wis = mean(.data$rel_wis),
            cov50 = mean(.data$in50), cov95 = mean(.data$in95),
            rel_bias = mean((.data$median - .data$truth) / pmax(.data$truth, 1)),
            .groups = "drop") |>
  arrange(.data$wis)
print(as.data.frame(summary_pooled), digits = 3)

cat("\n=== WIS by horizon (0 = the as-of event time) ===\n")
summary_by_horizon <- scores |>
  group_by(.data$method, .data$horizon) |>
  summarise(wis = mean(.data$wis), .groups = "drop") |>
  pivot_wider(names_from = "method", values_from = "wis")
print(as.data.frame(summary_by_horizon), digits = 3)

cat("\n=== where the evaluation sits on each series ===\n")
print(as.data.frame(scores |>
  group_by(.data$disease) |>
  summarise(as_of = paste(format(sort(unique(.data$now))), collapse = " "),
            mean_scored_truth = mean(.data$truth), .groups = "drop")), digits = 4)

cat(sprintf("\ntrue confirmation probability p = %.3f\n", 1 - RETRACT_SHARE))
saveRDS(scores, file.path(OUT, "benchmark_retraction_scores.rds"))

# =============================================================================
# Conditional-frailty check, 2026-07-26
# =============================================================================
# `predict()` draws the count still to come from the PRIOR frailty Gamma(r, r).
# That is the right marginal spread but the wrong conditional one, and the fix
# (posterior Gamma(r + k_t, r + E[observed])) is implemented behind
# `options(diseasenowcasting.conditional_frailty = TRUE)`.  Re-running this
# benchmark with it ON:
#
#   disease     method   wis(off)  wis(on)   cov95(off)  cov95(on)
#     covid retraction     11.1     160.4       0.976       1.000
#     covid drop_known    333.6     467.4       0.024       0.024
#     covid   keep_all    818.1     969.2       0.000       0.000
#    dengue retraction     10.5      12.7       0.905       0.905
#      mpox retraction      9.6      13.4       1.000       0.881
#
# Worse on every disease at essentially unchanged coverage.  On data simulated
# FROM the model the same switch fixes the calibration outright (50% coverage
# 0.77 -> 0.51).  The difference is misspecification: on real data the epidemic
# mean is never exactly right, and the prior draw's extra width was absorbing
# that.  Hence the option defaults OFF and the table below is the shipped
# behaviour.
#
# =============================================================================
# Results, 2026-07-26 (RETRACT_SHARE = 0.15, N_DATES = 6, HORIZONS = 0:6,
#                      Dirichlet g_C, 42 scored points per disease x method)
# =============================================================================
#
#  disease     method    wis rel_wis mean_truth  cov50  cov95 rel_bias p_hat
#    covid retraction  11.13  0.0626     4593.1 0.4286 0.9762  0.00116 0.850
#    covid drop_known 333.58  0.1204     4593.1 0.0238 0.0238  0.07537    --
#    covid   keep_all 818.10  0.2334     4593.1 0.0000 0.0000  0.17757    --
#   dengue retraction  10.54  0.0694      146.5 0.5476 0.9048 -0.02206 0.850
#   dengue drop_known  13.14  0.0895      146.5 0.3333 0.5238  0.04862    --
#   dengue   keep_all  24.42  0.1824      146.5 0.1429 0.2857  0.15200    --
#     mpox retraction   9.63  0.2737       44.9 0.5000 1.0000  0.07912 0.852
#     mpox drop_known  10.27  0.3134       44.9 0.4524 0.9048  0.21970    --
#     mpox   keep_all  11.12  0.3395       44.9 0.3571 0.7857  0.25724    --
#
#  pooled:  retraction  10.4  (cov50 0.492, cov95 0.960, bias 0.019)
#           drop_known 119.0  (cov50 0.270, cov95 0.484, bias 0.115)
#           keep_all   284.5  (cov50 0.167, cov95 0.357, bias 0.196)
#
# Reading:
#  * SCALE CHECK.  `keep_all` on covid scores WIS 818, against the ~839 the
#    published Benchmark vignette reports for the best model on the same disease.
#    That is the sanity check that the evaluation now sits where the epidemic is
#    (mean scored truth 4593/day).  An earlier run of this script anchored its
#    as-of dates at the end of the onset series and scored ~55 cases/day, which
#    pulled every WIS down by an order of magnitude and made the table look
#    incomparable to the vignette.  Nothing about the scoring changed -- only
#    where it was measured.
#  * p is recovered on every disease: 0.850, 0.850, 0.852 against a true 0.850.
#  * The retraction model is the only one anywhere near nominal coverage
#    (0.49 / 0.96 against 50% / 95%).  Its WIS advantage grows with the counts,
#    from ~1.1x on mpox (45/day) to ~30x on covid (4593/day), because the two
#    baselines are BIASED and bias scales with the count while their intervals do
#    not.
#  * The 0.00 coverage of `keep_all` on covid is not a scoring artefact.  It is
#    biased +17.8% on a mean truth of 4593, i.e. ~815 cases, and at horizons >= 1
#    covid's short reporting delay leaves 1 - G_D(d*) ~ 0, so the ordinary count
#    model emits an essentially zero-width interval at the observed count.  A
#    systematically wrong point prediction with no width covers nothing.  That is
#    what ignoring a retraction column does, not a defect of the count model.
#  * `drop_known` is unbiased on SETTLED event times -- once an origin is old, the
#    retracted rows really are gone -- and wrong at recent ones, where the
#    retractions that will cancel today's reports have not landed.  Hence its
#    WIS-by-horizon column decays from 356 at horizon 0 to 6 at horizon 6, while
#    `keep_all` stays wrong everywhere.
