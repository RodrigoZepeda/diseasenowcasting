# benchmark_auto.R
# -----------------------------------------------------------------------------
# Focused check of auto_nowcast() against the diseasenowcasting own models ONLY
# (no NobBS / epinowcast / baselinenowcast).  For each disease we fit the full
# 3x3 own-model grid AND auto_nowcast() at a handful of as-of dates, score the
# d*=0 nowcast against the eventual truth (WIS), and verify two claims:
#
#   1. auto_nowcast() CONVERGES on every date where any own model converges.
#   2. auto_nowcast() lands in the TOP spots of the WIS ranking.
#
# Run:  Rscript devel/benchmark_auto.R
#       RUN_COVID=FALSE Rscript devel/benchmark_auto.R    # skip the slow daily series
#
# NOTE: auto_nowcast() runs its OWN selection backtest over the data you hand it,
# ignoring `now` for selection.  So to benchmark it fairly at an as-of date we
# pass it data TRUNCATED to that date (event & report <= date_run); otherwise its
# model selection would peek at the future.
# -----------------------------------------------------------------------------
rm(list = ls())

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(lubridate)
library(tbl.now)
library(diseasenowcasting)
library(scoringutils)
library(foreach)
library(future)
library(doFuture)

# ---- run controls -----------------------------------------------------------
N_DATES   <- as.integer(Sys.getenv("N_DATES", "4"))     # as-of eval dates per disease
N_DRAWS   <- as.integer(Sys.getenv("N_DRAWS", "800"))   # draws for the final fits
WORKERS   <- as.integer(Sys.getenv("WORKERS", "8"))
K_IMPUTE  <- 25L                                        # two-stage delay imputations

# auto_nowcast() selection backtest (kept cheap; the winner is refit at N_DRAWS
# with the full K_IMPUTE).  The selection grid is fit over AUTO_NDATES dates with
# AUTO_KSELECT imputations -- the cost driver on a long series, so keep it small.
AUTO_NDATES <- as.integer(Sys.getenv("AUTO_NDATES", "6"))
AUTO_NDRAWS <- as.integer(Sys.getenv("AUTO_NDRAWS", "400"))
AUTO_KSELECT <- as.integer(Sys.getenv("AUTO_KSELECT", "5"))

RUN_DENGUE <- as.logical(Sys.getenv("RUN_DENGUE", "TRUE"))
RUN_COVID  <- as.logical(Sys.getenv("RUN_COVID",  "TRUE"))
RUN_MPOX   <- as.logical(Sys.getenv("RUN_MPOX",   "TRUE"))

PROBS  <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
QCOLS  <- c("q2.5", "q5", "q10", "q25", "q50", "q75", "q90", "q95", "q97.5")
SEED_FIT <- 23875L

# =============================================================================
# DATA -- build each disease's tbl_now, eval dates, truth and own-model grid
# =============================================================================
D <- list()

epidemic_components <- list(hsgp_epidemic(), ar1_epidemic(), sir_epidemic(N_pop = 5e6))
delay_components    <- list(lognormal_delay(), generalized_gamma_delay(), dirichlet_delay())
own_models_list <- cross2(epidemic_components, delay_components) |>
  map(~ model(likelihood = nb_likelihood(), epidemic = .x[[1]], delay = .x[[2]]))

if (RUN_DENGUE) {
  data(denguedat)
  tbl <- denguedat |>
    tbl_now(event_date = onset_week, report_date = report_week, data_type = "linelist") |>
    to_count("count-incidence") |>
    add_temporal_effects(temporal_effects(seasons = 52)) |>
    compute_temporal_effects()

  ev <- tbl |> pull(get_event_date(tbl))
  full_grid     <- seq(min(ev), max(ev), by = "1 week")
  interior_grid <- full_grid[2:(length(full_grid) - 1)]
  set.seed(8765L)
  dates <- sort(base::sample(interior_grid, N_DATES), decreasing = TRUE)

  D$dengue <- list(tbl = tbl, event_col = get_event_date(tbl), report_col = get_report_date(tbl),
                   truth = tbl |> get_latest_reported_cases() |> as_tibble(),
                   dates = dates, own_models = own_models_list)
}

if (RUN_COVID) {
  data(covid_colombia)
  tbl <- covid_colombia |>
    group_by(notification_date, diagnosis_date) |>
    summarise(n = sum(n), .groups = "drop") |>
    tbl_now(event_date = notification_date, report_date = diagnosis_date,
            case_count = n, data_type = "count-incidence") |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
    compute_temporal_effects()

  full_grid     <- seq(ymd("2020-03-09"), ymd("2020-03-09") + years(2), by = "1 day")
  interior_grid <- full_grid[2:(length(full_grid) - 1)]
  set.seed(5432L)
  dates <- sort(base::sample(interior_grid, N_DATES), decreasing = TRUE)

  D$covid <- list(tbl = tbl, event_col = get_event_date(tbl), report_col = get_report_date(tbl),
                  truth = tbl |> get_latest_reported_cases() |> as_tibble(),
                  dates = dates, own_models = own_models_list)
}

if (RUN_MPOX) {
  data(mpoxdat)
  tbl <- mpoxdat |>
    tbl_now(event_date = dx_date, report_date = dx_report_date, data_type = "linelist") |>
    to_count("count-incidence") |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
    compute_temporal_effects()

  ev <- tbl |> pull(get_event_date(tbl))
  full_grid     <- seq(ymd("2022-07-12"), max(ev), by = "1 day")
  interior_grid <- full_grid[2:(length(full_grid) - 1)]
  set.seed(2109L)
  dates <- sort(base::sample(interior_grid, N_DATES), decreasing = TRUE)

  D$mpox <- list(tbl = tbl, event_col = get_event_date(tbl), report_col = get_report_date(tbl),
                 truth = tbl |> get_latest_reported_cases() |> as_tibble(),
                 dates = dates, own_models = own_models_list)
}

all_long <- list()

# =============================================================================
# 1. OWN MODELS  -- the 3x3 grid, fit per (model, as-of date)
# =============================================================================
plan(multisession, workers = WORKERS)

for (nm in names(D)) {
  cli::cli_h1(paste("own models:", nm))

  tbl    <- D[[nm]]$tbl
  dates  <- D[[nm]]$dates
  models <- D[[nm]]$own_models
  ev     <- D[[nm]]$event_col
  truth  <- D[[nm]]$truth
  labels <- map_chr(models, ~ sprintf("own %s/%s/%s", .x@epidemic@name, .x@likelihood@name, .x@delay@name))

  task_grid <- expand_grid(model_idx = seq_along(models), date_run = dates)

  res <- foreach(idx = task_grid$model_idx, date_run = task_grid$date_run,
                 .combine = bind_rows, .options.future = list(seed = TRUE)) %dofuture% {
    m   <- models[[idx]]
    lbl <- labels[[idx]]
    tryCatch({
      t <- system.time({
        nc <- nowcast(tbl, m, type = "two_stage", now = date_run, K = K_IMPUTE,
                      n_draws = N_DRAWS, seed = SEED_FIT)
      })
      summ <- as.data.frame(predict(nc, summary = TRUE))
      row  <- summ[summ$event_date == date_run, , drop = FALSE]
      if (nrow(row) == 0) row <- summ[which.max(summ$.event_num), , drop = FALSE]
      obs <- truth |> filter(.data[[ev]] == date_run) |> pull(n)
      if (length(obs) == 0) return(NULL)
      tibble(model = lbl, date_run = date_run, observed = obs[1],
             quantile = PROBS, predicted = as.numeric(row[1, QCOLS]),
             secs = as.numeric(t["elapsed"]))
    }, error = function(e) NULL)
  }
  all_long[[length(all_long) + 1L]] <- res |> mutate(disease = nm)
}

# =============================================================================
# 2. AUTO_NOWCAST  -- one selection+refit per as-of date (data truncated to date)
# =============================================================================
# Sequential outer loop: each auto_nowcast() parallelises its own backtest over
# the multisession plan (so we must NOT nest it inside a %dofuture%).
for (nm in names(D)) {
  cli::cli_h1(paste("auto_nowcast:", nm))

  tbl   <- D[[nm]]$tbl
  dates <- D[[nm]]$dates
  ev    <- D[[nm]]$event_col
  rp    <- D[[nm]]$report_col
  truth <- D[[nm]]$truth

  rows <- list()
  # Index-based loop: `for (x in date_vector)` would strip the Date class and
  # hand auto_nowcast() a bare numeric `now`; `dates[di]` keeps it a Date.
  for (di in seq_along(dates)) {
    date_run <- dates[di]
    sub <- tbl |> filter(.data[[ev]] <= date_run, .data[[rp]] <= date_run)   # no future peeking

    out <- tryCatch({
      t <- system.time({
        nc <- auto_nowcast(sub, type = "two_stage", now = date_run,
                           n_dates = AUTO_NDATES, n_draws_select = AUTO_NDRAWS,
                           n_draws = N_DRAWS, K = K_IMPUTE, K_select = AUTO_KSELECT,
                           seed = SEED_FIT, verbose = FALSE)
      })
      summ <- as.data.frame(predict(nc, summary = TRUE))
      row  <- summ[summ$event_date == date_run, , drop = FALSE]
      if (nrow(row) == 0) row <- summ[which.max(summ$.event_num), , drop = FALSE]
      obs <- truth |> filter(.data[[ev]] == date_run) |> pull(n)
      if (length(obs) == 0) return(NULL)

      cli::cli_alert_info("  {as.character(date_run)}: auto chose {best_model_name(nc)} ({round(t['elapsed'],1)}s)")
      tibble(model = "auto_nowcast", date_run = date_run, observed = obs[1],
             quantile = PROBS, predicted = as.numeric(row[1, QCOLS]),
             secs = as.numeric(t["elapsed"]), chosen = best_model_name(nc))
    }, error = function(e) { cli::cli_alert_danger("  {as.character(date_run)}: auto FAILED -- {conditionMessage(e)}"); NULL })

    rows[[length(rows) + 1L]] <- out
  }
  all_long[[length(all_long) + 1L]] <- bind_rows(rows) |> mutate(disease = nm)
}

plan(sequential)
long_df <- bind_rows(all_long)

# =============================================================================
# 3. CONVERGENCE CHECK  -- did auto converge wherever the own models did?
# =============================================================================
cli::cli_h1("Convergence")
for (nm in names(D)) {
  L     <- long_df |> filter(disease == nm)
  dates <- D[[nm]]$dates
  own   <- L |> filter(grepl("^own ", model))
  auto  <- L |> filter(model == "auto_nowcast")

  per_date <- tibble(date_run = dates) |>
    mutate(n_own_ok = map_int(date_run, ~ own  |> filter(date_run == .x) |> distinct(model) |> nrow()),
           auto_ok  = map_lgl(date_run, ~ any(auto$date_run == .x)))

  violations <- per_date |> filter(n_own_ok > 0, !auto_ok)
  cli::cli_h3(nm)
  print(as.data.frame(per_date), row.names = FALSE)
  if (nrow(violations) == 0)
    cli::cli_alert_success("auto converged on every date where an own model converged.")
  else
    cli::cli_alert_danger("auto FAILED on {nrow(violations)} date(s) where own models converged.")
}

# =============================================================================
# 4. RANKING  -- WIS over the common dates (own grid + auto together)
# =============================================================================
cli::cli_h1("WIS ranking (own grid + auto, common dates)")
rank_tbl <- list()
for (nm in names(D)) {
  L <- long_df |> filter(disease == nm)
  if (nrow(L) == 0) next

  # common dates: those where every model (incl. auto) produced a forecast
  per_model_dates <- L |> distinct(model, date_run) |> group_by(model) |>
    summarise(dates = list(date_run), .groups = "drop")
  common <- Reduce(intersect, map(per_model_dates$dates, as.character))
  if (length(common) == 0) { cli::cli_alert_warning("{nm}: no common dates across all models."); next }

  sub <- L |> filter(as.character(date_run) %in% common)
  tab <- sub |>
    as_forecast_quantile(observed = "observed", predicted = "predicted",
                         quantile_level = "quantile", forecast_unit = c("model", "date_run")) |>
    score() |> summarise_scores(by = "model") |>
    transmute(model, wis = round(wis, 1), cov50 = round(interval_coverage_50, 2),
              cov90 = round(interval_coverage_90, 2)) |>
    arrange(wis) |>
    mutate(rank = row_number(), disease = nm, n_dates = length(common))

  cli::cli_h3(paste(nm, "-", length(common), "common dates"))
  print(as.data.frame(tab |> select(rank, model, wis, cov50, cov90)), row.names = FALSE)
  auto_rank <- tab |> filter(model == "auto_nowcast") |> pull(rank)
  if (length(auto_rank))
    cli::cli_alert_info("auto_nowcast WIS rank: {auto_rank} / {nrow(tab)}")
  rank_tbl[[length(rank_tbl) + 1L]] <- tab
}

ranks <- bind_rows(rank_tbl)
saveRDS(long_df, file.path(tempdir(), "benchmark_auto_long.rds"))
saveRDS(ranks,   file.path(tempdir(), "benchmark_auto_ranks.rds"))
cli::cli_alert_success("Done. Saved long + ranks to {tempdir()}.")
