# =============================================================================
# benchmark_full.R — ONE-FILE benchmark: diseasenowcasting vs NobBS vs Epinowcast
# =============================================================================
# Reproduces the original (good) benchmark results -- HSGP wins -- in a single
# runnable file, using the HIGH-LEVEL API:
#
#   * Own models: the FULL grid (HSGP / AR(1) / SIR  x  {LogNormal, Gamma,
#     Generalized-Gamma, Dirichlet}) via a single `backtest()` call.  The
#     seasonal / day-of-week covariates are supplied by COMPUTING the temporal
#     effects on the tbl_now (backtest() -> nowcast() -> prepare_from_tbl_now()
#     turns the effect columns into the design matrix automatically -- there is
#     no separate `X` to pass).  Defaults already match the original benchmark:
#       type = "two_stage", K = 25, nb_likelihood() phi = lognormal(log 20, 0.5).
#   * Comparison: NobBS (1) + Epinowcast (3 variants) per disease.
#   * Scoring: d*=0, restricted to the COMMON evaluation-date set, WIS via
#     scoringutils.
#
# Why the earlier simplified Benchmark-vignette code gave different (worse)
# results: it fit only 3 own models, a single Epinowcast variant, and -- the
# subtle one -- attached the temporal-effect SPEC to a raw linelist without
# compute_temporal_effects(), so the seasonal covariates may not have been used.
#
# Requirements: diseasenowcasting INSTALLED (so parallel workers can library it),
#   plus NobBS, epinowcast (+ cmdstanr), scoringutils.
#
#   Rscript devel/benchmark_full.R
#   N_DATES=6 RUN_COVID=FALSE RUN_MPOX=FALSE Rscript devel/benchmark_full.R   # smoke
#
# Output: a ranked WIS table per disease + devel/results/benchmark_scores.rds.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(purrr)
  library(lubridate); library(tbl.now); library(diseasenowcasting)
  library(scoringutils)
  library(foreach); library(future); library(doFuture)
  library(NobBS); library(epinowcast)
})
options(cmdstanr_warn_inits = FALSE)
suppressMessages(epinowcast::enw_set_cache(
  tools::R_user_dir(package = "epinowcast", "cache"), type = "session"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
OUT <- "devel/results"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ── Run controls (env-overridable) ───────────────────────────────────────────
N_DATES     <- as.integer(Sys.getenv("N_DATES", "50"))
RUN_DENGUE  <- as.logical(Sys.getenv("RUN_DENGUE", "TRUE"))
RUN_COVID   <- as.logical(Sys.getenv("RUN_COVID",  "TRUE"))
RUN_MPOX    <- as.logical(Sys.getenv("RUN_MPOX",   "TRUE"))
N_DRAWS     <- as.integer(Sys.getenv("N_DRAWS", "1000"))
WORKERS     <- as.integer(Sys.getenv("TUNE_WORKERS", "8"))
MODELS_SET  <- Sys.getenv("MODELS", "all")              # "all" or "fast"
COVID_RDS   <- Sys.getenv("COVID_RDS",
  "/Users/rodzepeda/Documents/diseasenowcast2/devel/covid_colombia_aggregated.rds")

# Identical seeds + N_DATES to the originals so the own and comparison models are
# evaluated on the SAME dates (-> the common-set scoring is non-empty).
DATE_SEED <- c(dengue = 8765L, covid = 5432L, mpox = 2109L)
PROBS  <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
QCOLS  <- c("q2.5", "q5", "q10", "q25", "q50", "q75", "q90", "q95", "q97.5")

plan(multisession, workers = WORKERS)

run_silent <- function(expr) {
  null_con <- file(nullfile(), open = "wt"); sink(null_con, type = "output")
  on.exit({ sink(NULL); close(null_con) }, add = TRUE)
  withCallingHandlers(suppressWarnings(suppressMessages(expr)),
                      message = function(m) invokeRestart("muffleMessage"),
                      warning = function(w) invokeRestart("muffleWarning"))
}

# Sample N_DATES interior evaluation dates (same logic + seed as the originals).
sample_dates <- function(min_date, max_date, unit, seed) {
  grid <- seq(min_date, max_date, by = paste0("1 ", unit))
  grid <- grid[2:(length(grid) - 1)]
  set.seed(seed)
  sort(base::sample(grid, N_DATES), decreasing = TRUE)
}

# Pivot any d*=0 row set (q2.5..q97.5 + `model`/`date_run`/`observed`) to long form.
to_long_quantile <- function(df) {
  df %>%
    pivot_longer(all_of(QCOLS), names_to = "quantile", values_to = "predicted") %>%
    mutate(quantile = setNames(PROBS, QCOLS)[quantile])
}

# =============================================================================
# OWN MODELS  -- one backtest() call covers the full grid and all dates
# =============================================================================
own_model_list <- function(disease, N_pop) {
  delays <- if (MODELS_SET == "fast") list(lognormal_delay(), dirichlet_delay())
            else list(lognormal_delay(), generalized_gamma_delay(), dirichlet_delay())
  epidemics <- list(hsgp_epidemic(), ar1_epidemic(), sir_epidemic(N_pop = N_pop))
  models <- list()
  for (epidemic in epidemics) for (delay in delays)
    models[[length(models) + 1]] <- model(nb_likelihood(), epidemic, delay)
  models
}

# d*=0 own-model nowcasts as a long table.  backtest() uses two-stage + K = 25 +
# the default NB phi prior (all defaults), and the tbl_now's COMPUTED temporal
# effects as covariates.  The newest event per (model, date_run) is the d*=0
# nowcast; `final` is its eventual truth.
own_d0_long <- function(tbl, dates, models) {
  bt <- backtest(tbl, models = models, dates = dates,
                 n_draws = N_DRAWS, seed = 23875L)        # two_stage, K = 25 are defaults
  bt@results %>%
    group_by(model, date_run) %>%
    slice_max(.event_num, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(model = paste0("own ", model), date_run, observed = final,
              q2.5, q5, q10, q25, q50, q75, q90, q95, q97.5) %>%
    to_long_quantile()
}

# =============================================================================
# COMPARISON MODELS  -- NobBS + 3 Epinowcast variants, fit per date
# =============================================================================
# NobBS d*=0 nowcast for one date, as a long table.
nobbs_d0_long <- function(tbl, date_run, event_col, report_col, truth, nobbs_units) {
  tryCatch({
    sub <- tbl %>% filter(.data[[event_col]] <= date_run, .data[[report_col]] <= date_run) %>%
      as.data.frame() %>% dplyr::select(all_of(c(event_col, report_col, "n"))) %>% uncount(n)
    fit <- suppressMessages(NobBS::NobBS(
      data = sub, now = date_run, units = nobbs_units,
      onset_date = event_col, report_date = report_col,
      moving_window = 50, specs = list(dist = "NB", quantiles = PROBS)))
    est <- fit$estimates %>% as.data.frame() %>% filter(as.Date(onset_date) == date_run)
    obs <- truth %>% filter(.data[[event_col]] == date_run) %>% pull(n)
    if (nrow(est) == 0 || length(obs) == 0) return(NULL)
    tibble(model = "NobBS", date_run = date_run, observed = obs[1],
           quantile = PROBS, predicted = as.numeric(est[1, paste0("q_", PROBS)]))
  }, error = function(e) NULL)
}

# Epinowcast: fit the shared preprocessing once, then each expectation variant.
# `variants` is a named list of expectation formulas (NULL = default model).
enw_d0_long <- function(tbl, date_run, event_col, report_col, truth, timestep, variants) {
  min_date  <- min(tbl[[event_col]], na.rm = TRUE)
  max_delay <- min(30L, as.integer(difftime(date_run, min_date,
                                            units = if (timestep == "week") "weeks" else "days")))
  if (max_delay < 1L) return(NULL)
  p_data <- tryCatch({
    tbl %>% filter(.data[[event_col]] <= date_run, .data[[report_col]] <= date_run) %>%
      to_count(to = "count-cumulative") %>% as_tibble() %>%
      transmute(reference_date = .data[[event_col]], report_date = .data[[report_col]], confirm = n) %>%
      enw_complete_dates(max_delay = max_delay, timestep = timestep) %>%
      enw_preprocess_data(max_delay = max_delay, timestep = timestep)
  }, error = function(e) NULL)
  if (is.null(p_data)) return(NULL)
  obs <- truth %>% filter(.data[[event_col]] == date_run) %>% pull(n)
  if (length(obs) == 0) return(NULL)

  out <- list()
  for (label in names(variants)) {
    out[[label]] <- tryCatch({
      args <- list(data = p_data, model = enw_model(verbose = FALSE),
                   fit = enw_fit_opts(sampler = enw_pathfinder, refresh = 0, show_messages = FALSE))
      if (!is.null(variants[[label]]))
        args$expectation <- enw_expectation(r = variants[[label]], data = p_data)
      nc   <- suppressMessages(suppressWarnings(do.call(epinowcast, args)))
      summ <- epinowcast:::summary.epinowcast(nc, type = "nowcast", probs = PROBS) %>% as_tibble() %>%
        filter(as.Date(reference_date) == date_run)
      if (nrow(summ) == 0) NULL else
        tibble(model = label, date_run = date_run, observed = obs[1],
               quantile = PROBS, predicted = as.numeric(summ[1, paste0("q", PROBS * 100)]))
    }, error = function(e) NULL)
  }
  bind_rows(out)
}

# =============================================================================
# SCORING  -- d*=0, COMMON date set, WIS via scoringutils
# =============================================================================
score_long <- function(long, common_only = TRUE) {
  # Common date set: keep dates where every RELIABLE model (the own models +
  # NobBS) produced a nowcast.  Epinowcast's pathfinder fits fail occasionally;
  # requiring them too can silently collapse the common set to a handful of
  # unrepresentative dates (which is what made NobBS spuriously "win" before).
  # Epinowcast is still scored, but only on the dates where it succeeded.
  #
  # With `common_only = FALSE` the common-date filter is NOT applied: every model
  # is scored on ALL the dates IT produced (pre-filter).  The per-model `n_dates`
  # column then reveals coverage gaps -- a model with n_dates < N_DATES failed on
  # the missing ones (this is what shrinks the common set).
  reliable     <- long %>% filter(!grepl("Epinowcast", model))
  n_reliable   <- dplyr::n_distinct(reliable$model)
  common_dates <- reliable %>% distinct(model, date_run) %>% count(date_run) %>%
    filter(n == n_reliable) %>% pull(date_run)
  if (common_only) {
    long <- long %>% filter(date_run %in% common_dates)
    cli::cli_alert_info("post-filter: scoring on {length(common_dates)} common date(s)")
  } else {
    cli::cli_alert_info("pre-filter: scoring each model on ALL its available dates")
  }
  if (nrow(long) == 0) return(NULL)

  per_model_n <- long %>% distinct(model, date_run) %>% count(model, name = "n_dates")
  long %>%
    as_forecast_quantile(observed = "observed", predicted = "predicted",
      quantile_level = "quantile", forecast_unit = c("model", "date_run")) %>%
    score() %>%
    summarise_scores(by = "model") %>%
    transmute(model, wis = round(wis, 1), over = round(overprediction, 1),
              under = round(underprediction, 1), disp = round(dispersion, 1),
              bias = round(bias, 2), cov50 = round(interval_coverage_50, 2),
              cov90 = round(interval_coverage_90, 2)) %>%
    left_join(per_model_n, by = "model") %>%
    arrange(wis)
}

# =============================================================================
# PER-DISEASE DRIVER
# =============================================================================
run_disease <- function(disease, tbl, N_pop, unit, timestep, nobbs_units,
                        enw_variants, min_eval, max_eval) {
  cli::cli_h1(toupper(disease))
  event_col  <- get_event_date(tbl); report_col <- get_report_date(tbl)
  truth      <- tbl %>% get_latest_reported_cases() %>% as_tibble()
  dates      <- sample_dates(min_eval, max_eval, unit, DATE_SEED[[disease]])

  # Own models: a single backtest() call (parallel over models x dates inside).
  own_long <- own_d0_long(tbl, dates, own_model_list(disease, N_pop))

  # Comparison models: one worker per date (NobBS + the 3 epinowcast variants).
  comp_long <- foreach(k = seq_along(dates), .combine = bind_rows,
                       .options.future = list(seed = TRUE)) %dofuture% {
    run_silent(bind_rows(
      nobbs_d0_long(tbl, dates[k], event_col, report_col, truth, nobbs_units),
      enw_d0_long(tbl, dates[k], event_col, report_col, truth, timestep, enw_variants)))
  }

  long        <- bind_rows(own_long, comp_long)
  long_tagged <- long %>% mutate(disease = disease)          # raw, pre-filter
  tab         <- score_long(long, common_only = TRUE)        # post-filter (common set)
  tab_all     <- score_long(long, common_only = FALSE)       # pre-filter (per-model dates)
  tag <- function(x) if (is.null(x)) NULL else x %>% mutate(disease = disease)

  if (is.null(tab)) {
    cli::cli_warn("{disease}: no common d*=0 targets.")
    return(list(long = long_tagged, scores = NULL, scores_prefilter = tag(tab_all)))
  }
  cli::cli_h3("{disease}: post-filter (common date set)")
  print(as.data.frame(tab), row.names = FALSE)
  cli::cli_h3("{disease}: pre-filter (each model on all its dates; n_dates shows coverage)")
  print(as.data.frame(tab_all), row.names = FALSE)

  best_own <- tab %>% filter(grepl("^own ", model)) %>% slice(1)
  nobbs_wis <- tab %>% filter(model == "NobBS") %>% pull(wis)
  cli::cli_alert_success(
    "{disease}: best own = {best_own$model} (WIS {best_own$wis}); ",
    "beats NobBS ({nobbs_wis %||% NA}) = {isTRUE(best_own$wis <= (nobbs_wis %||% Inf))}")
  list(long = long_tagged, scores = tag(tab), scores_prefilter = tag(tab_all))
}

# =============================================================================
# RUN
# =============================================================================
all_results <- list()   # per disease: list(long, scores, scores_prefilter)

if (RUN_DENGUE) {
  data(denguedat)
  tbl <- denguedat %>%
    tbl_now(event_date = onset_week, report_date = report_week, data_type = "linelist") %>%
    to_count("count-incidence") %>%
    add_temporal_effects(temporal_effects(seasons = 52)) %>% compute_temporal_effects()
  d0 <- min(as_tibble(tbl)[[get_event_date(tbl)]]); d1 <- max(as_tibble(tbl)[[get_event_date(tbl)]])
  all_results[["dengue"]] <- run_disease(
    "dengue", tbl, N_pop = 5e6, unit = "week", timestep = "week", nobbs_units = "1 week",
    enw_variants = list("Epinowcast (weekly RE)" = ~ 1 + (1 | week),
                        "Epinowcast (default)"   = NULL,
                        "Epinowcast (rw)"        = ~ 1 + rw(week)),
    min_eval = d0, max_eval = d1)
}

if (RUN_COVID) {
  tbl <- read_rds(COVID_RDS) %>%
    group_by(notification_date, diagnosis_date) %>% summarise(n = sum(n), .groups = "drop") %>%
    tbl_now(event_date = notification_date, report_date = diagnosis_date, case_count = n,
            data_type = "count-incidence") %>%
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) %>% compute_temporal_effects()
  all_results[["covid"]] <- run_disease(
    "covid", tbl, N_pop = 5e6, unit = "day", timestep = "day", nobbs_units = "1 day",
    enw_variants = list("Epinowcast (point effect)" = ~ 1 + day_of_week,
                        "Epinowcast (default)"      = NULL,
                        "Epinowcast (rw)"           = ~ 1 + rw(day)),
    min_eval = ymd("2020-03-09"), max_eval = ymd("2020-03-09") + years(2))
}

if (RUN_MPOX) {
  data(mpoxdat)
  tbl <- mpoxdat %>%
    tbl_now(event_date = dx_date, report_date = dx_report_date, data_type = "linelist") %>%
    to_count("count-incidence") %>%
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) %>% compute_temporal_effects()
  all_results[["mpox"]] <- run_disease(
    "mpox", tbl, N_pop = 5e6, unit = "day", timestep = "day", nobbs_units = "1 day",
    enw_variants = list("Epinowcast (point effect)" = ~ 1 + day_of_week,
                        "Epinowcast (default)"      = NULL,
                        "Epinowcast (rw)"           = ~ 1 + rw(day)),
    min_eval = ymd("2022-07-12"), max_eval = max(as_tibble(tbl)[[get_event_date(tbl)]]))
}

plan(sequential)

# Post-filter (common date set) scores -- the headline table.
scores_df <- bind_rows(lapply(all_results, `[[`, "scores"))
write_rds(scores_df, file.path(OUT, "benchmark_scores.rds"))

# Pre-filter scores -- every model scored on ALL the dates it produced; the
# per-model `n_dates` column shows which models dropped dates (n_dates < N_DATES).
scores_prefilter_df <- bind_rows(lapply(all_results, `[[`, "scores_prefilter"))
write_rds(scores_prefilter_df, file.path(OUT, "benchmark_scores_prefilter.rds"))

# Raw pre-filter predictions (one row per model x date x quantile), unfiltered --
# everything needed to re-score or audit which model produced which date.
long_df <- bind_rows(lapply(all_results, `[[`, "long"))
write_rds(long_df, file.path(OUT, "benchmark_long.rds"))

cli::cli_alert_success(
  "Saved to {OUT}/: benchmark_scores.rds ({nrow(scores_df)} rows, common set), ",
  "benchmark_scores_prefilter.rds ({nrow(scores_prefilter_df)} rows, all dates), ",
  "benchmark_long.rds ({nrow(long_df)} raw prediction rows).")
