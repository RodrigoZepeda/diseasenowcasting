# backtest_flusight_p.R
#
# Is `p` better fixed at the empirical down-revision rate, or estimated?
#
# Session 2 decided this from a SINGLE as-of date, which is not enough. This
# scores both arms with backtest() over several as-of dates and several states.
#
# Two data-preparation rules, both properties of the FluSight file, not the model:
#
#  1. WINDOW to the snapshot era. `target_end_date` starts 2022-02-05 but `as_of`
#     only 2023-09-23, so every earlier event week is left-truncated on the report
#     axis -- on Texas, 126 of 197 event weeks first appear at delay > 1 week, up
#     to 145 weeks. Nothing can fit that.
#  2. TRUNCATE to the last event date. FluSight publishes only completed weeks, so
#     the newest snapshot is dated after the newest event week. `now` follows the
#     report axis, so the grid gains a final week that cannot have been reported at
#     any delay yet; its cumulative is 0 and the epidemic mean collapses to explain
#     it. Dropping reports beyond the last event date removes the phantom without
#     touching `now` or anything inside the package.
#
# Flat, top-to-bottom. Run it and read the tables at the bottom.

rm(list = ls())

library(dplyr)
library(tbl.now)
pkgload::load_all(".", quiet = TRUE)   # the WORKING TREE, not the installed package

SEED    <- 20260901
STATES  <- c("Texas", "California", "New York", "Florida")
N_DATES <- 6
N_DRAWS <- 500
START   <- as.Date("2023-09-23")       # the first as_of in the file

set.seed(SEED)

score_rows <- list()
p_rows     <- list()

for (state in STATES) {
  raw <- tbl.now::flusight |>
    filter(location_name == state,
           target_end_date >= START,        # rule 1: window to the snapshot era
           as_of >= START)
  # Rule 2: truncate to the last event date.
  #
  # KNOWN COST, measured on Texas: this drops ONE snapshot (2025-11-12) but SEVEN
  # event weeks (2025-09-27 .. 2025-11-08), because `as_of` has a ~7-week
  # publication gap over the off-season (2025-09-24 -> 2025-11-12) and that final
  # snapshot is the only one carrying the October-November weeks.
  #
  # It also does NOT remove the trailing empty grid cell it was meant to. `now`
  # follows the REPORT axis, and `align_weeks()` displaces the two axes by
  # different amounts (event Saturdays back 6 days, report Wednesdays back 3), so a
  # 4-day gap becomes a 7-day one -- exactly one extra event-time. No data-side
  # filter reaches that; see devel/HANDOFF_validation.md.
  raw <- raw |>
    filter(as_of <= max(target_end_date))

  tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
                case_count = observation, data_type = "count-cumulative",
                verbose = FALSE)
  tn <- align_weeks(tn, date_col = "report_date")

  # The empirical centre, on the model's own denominator: `alpha_t^d = mu_t g_D(d)`
  # counts every appearance including delay 0, so first reports belong in it.
  grouped <- raw |> arrange(target_end_date, as_of) |> group_by(target_end_date)
  first_reports <- grouped |> slice(1) |> ungroup() |>
    summarise(total = sum(observation)) |> pull(total)
  revisions <- grouped |> mutate(delta = observation - lag(observation)) |>
    ungroup() |> filter(!is.na(delta))
  up   <- sum(revisions$delta[revisions$delta > 0])
  down <- -sum(revisions$delta[revisions$delta < 0])
  p_empirical <- 1 - down / (first_reports + up)

  p_rows[[state]] <- data.frame(
    state = state, p_empirical = round(p_empirical, 4),
    first_reports = first_reports, up = up, down = down,
    event_weeks = length(unique(raw$target_end_date)))

  # Arm 1 is the package default (p fixed at the empirical rate); arm 2 frees it
  # under a Beta centred on that same rate, so the two differ ONLY in whether the
  # likelihood is allowed to move `p`.  They are run as SEPARATE backtests because
  # score() labels a model by its components, and both arms would otherwise carry
  # the identical label "AR1/poisson/LogNormal" and be collapsed into one row.
  arms <- list(
    `p fixed`     = confirmation_process(),
    `p estimated` = confirmation_process(
                      p = beta_prior(p_empirical * 10, (1 - p_empirical) * 10)))

  for (arm_name in names(arms)) {
    mdl <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
                 confirmation = arms[[arm_name]])
    bt <- tryCatch(
      backtest(tn, mdl, n_dates = N_DATES, type = "one_stage",
               n_draws = N_DRAWS, temporal_effects = "none", seed = SEED),
      error = function(e) { message("[", state, "/", arm_name, "] ", conditionMessage(e)); NULL })
    if (is.null(bt)) next
    scored <- score(bt, report = FALSE)
    scored$state <- state
    scored$arm   <- arm_name
    score_rows[[length(score_rows) + 1L]] <- scored
  }
}

p_table     <- do.call(rbind, p_rows)
score_table <- do.call(rbind, score_rows)
score_table <- score_table[, c("state", "arm", "wis", "ape", "mse", "coverage_50", "coverage_90", "n")]

cat("\n=== empirical down-revision rate, windowed + truncated ===\n")
print(p_table, row.names = FALSE)

cat("\n=== backtest scores, ", N_DATES, " as-of dates per state ===\n", sep = "")
print(score_table, row.names = FALSE, digits = 4)

cat("\n=== mean over states ===\n")
print(score_table |>
        group_by(arm) |>
        summarise(wis = mean(wis), ape = mean(ape), mse = mean(mse),
                  cov50 = mean(coverage_50), cov90 = mean(coverage_90),
                  .groups = "drop"),
      digits = 4)

saveRDS(list(p = p_table, scores = score_table),
        file.path("devel", "backtest_flusight_p.rds"))
