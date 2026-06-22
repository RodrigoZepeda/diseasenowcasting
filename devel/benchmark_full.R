# =============================================================================
# benchmark_full.R
# -----------------------------------------------------------------------------
# A FLAT, top-to-bottom benchmark of diseasenowcasting against NobBS, epinowcast
# and baselinenowcast on three real surveillance series (dengue, COVID-19, mpox).
# Each method estimates the d*=0 nowcast (the newest, most-censored event-time)
# at 50 historical "as-of" dates per disease; we then compare each estimate with
# the count that eventually arrived, scored with the Weighted Interval Score.
# Every individual fit is also timed (system.time), so the score tables carry a
# `secs` column = the average wall-clock seconds per fit for each model.
#
# This file is deliberately written as a DATA-ANALYSIS SCRIPT: no helper
# functions, read it straight through.  It runs the four methods in order so a
# reviewer (incl. the epinowcast / baselinenowcast authors) can audit the
# fairness of every configuration in one linear pass:
#
#     SETUP -> DATA -> 1. diseasenowcasting -> 2. NobBS -> 3. epinowcast
#           -> 4. baselinenowcast -> SCORING -> SAVE
#
# Fairness notes:
#   * Every method is given the SAME temporal effects the own models use:
#     day-of-week for the daily series (COVID, mpox), and epinowcast's weekly
#     index for dengue (epinowcast has no week-of-year/epiweek column, so we use
#     its running `week` as the trend term).
#   * epinowcast gets a 3 (delay: LogNormal/Gamma/Nonparametric) x 3 (expectation:
#     random-walk+effect / point effect / random effect) grid.
#   * baselinenowcast is a minimal non-parametric baseline with no temporal-effect
#     machinery, so it is run as-is.
#
# Requirements: diseasenowcasting INSTALLED, plus NobBS, epinowcast (+ cmdstanr),
#   baselinenowcast, scoringutils, tbl.now.
#   Rscript devel/benchmark_full.R
#   N_DATES=3 RUN_COVID=FALSE RUN_MPOX=FALSE Rscript devel/benchmark_full.R   # smoke
# =============================================================================

# ---- libraries --------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(purrr)
  library(lubridate); library(tbl.now); library(diseasenowcasting)
  library(scoringutils)
  library(foreach); library(future); library(doFuture)
  library(NobBS); library(epinowcast); library(baselinenowcast)
})
options(cmdstanr_warn_inits = FALSE)
suppressMessages(epinowcast::enw_set_cache(
  tools::R_user_dir(package = "epinowcast", "cache"), type = "session"))

# ---- run controls (env-overridable) -----------------------------------------
N_DATES     <- as.integer(Sys.getenv("N_DATES", "50"))
N_DRAWS     <- as.integer(Sys.getenv("N_DRAWS", "1000"))
WORKERS     <- as.integer(Sys.getenv("TUNE_WORKERS", "8"))
RUN_DENGUE  <- as.logical(Sys.getenv("RUN_DENGUE", "TRUE"))
RUN_COVID   <- as.logical(Sys.getenv("RUN_COVID",  "TRUE"))
RUN_MPOX    <- as.logical(Sys.getenv("RUN_MPOX",   "TRUE"))
# Two-stage delay-imputation spread floors for the own models (tightened from the
# package defaults, which over-disperse low-count daily data; see git history).
FLOOR_SIG_FRAC <- as.numeric(Sys.getenv("FLOOR_SIG_FRAC", "0.08"))
FLOOR_MU       <- as.numeric(Sys.getenv("FLOOR_MU",       "0.08"))
# epinowcast sampler: Pathfinder (fast variational, default) or full NUTS (slow).
ENW_SAMPLER <- Sys.getenv("ENW_SAMPLER", "pathfinder")
# epinowcast / baselinenowcast cannot scale to a 20-year weekly series the way
# the RTMB own models can: a full-series fit takes minutes and OOM-crashes the
# parallel workers.  So we give them the most recent ENW_WINDOW reference
# time-steps before each as-of date -- ample for the delay + recent trend (and
# more generous than NobBS's 50-step moving window).  This is also how a real
# epinowcast user would run it.  Raise it if you have the time/RAM.
ENW_WINDOW  <- as.integer(Sys.getenv("ENW_WINDOW", "150"))
COVID_RDS   <- Sys.getenv("COVID_RDS",
  "/Users/rodzepeda/Documents/diseasenowcast2/devel/covid_colombia_aggregated.rds")
OUT <- Sys.getenv("BENCH_OUT", "devel/results")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---- constants --------------------------------------------------------------
PROBS  <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
QCOLS  <- c("q2.5", "q5", "q10", "q25", "q50", "q75", "q90", "q95", "q97.5")
DATE_SEED <- c(dengue = 8765L, covid = 5432L, mpox = 2109L)   # same dates as the originals
SEED_FIT  <- 23875L
if (ENW_SAMPLER == "pathfinder") {
  ENW_FIT <- enw_fit_opts(sampler = epinowcast::enw_pathfinder, refresh = 0, show_messages = FALSE)
} else {
  ENW_FIT <- enw_fit_opts(sampler = epinowcast::enw_sample, refresh = 0, show_messages = FALSE,
               chains = 2, parallel_chains = 2, iter_warmup = 500, iter_sampling = 500,
               adapt_delta = 0.95)
}

plan(multisession, workers = WORKERS)

# =============================================================================
# DATA  -- build each disease's tbl_now (with temporal effects), evaluation
#          dates, truth, own-model grid, and epinowcast variant grid.
# =============================================================================
# Everything a disease needs is stuffed into one list `D[[disease]]`, so the
# method sections below can loop `for (nm in names(D))` without copy-paste.
D <- list()

if (RUN_DENGUE) {
  data(denguedat)
  tbl <- denguedat |>
    tbl_now(event_date = onset_week, report_date = report_week, data_type = "linelist") |>
    to_count("count-incidence") |>
    add_temporal_effects(temporal_effects(seasons = 52)) |> compute_temporal_effects()
  ev  <- as_tibble(tbl)[[get_event_date(tbl)]]
  D$dengue <- list(
    tbl = tbl, unit = "week", timestep = "week", nobbs_units = "1 week", N_pop = 5e6,
    min_eval = min(ev), max_eval = max(ev),
    # epinowcast has NO week-of-year column for dengue seasonality, so its three
    # expectations are trend models on the running `week` index:
    enw_expect = list("RW" = ~ 1 + rw(week), "point" = ~ 1 + week, "RE" = ~ 1 + (1 | week)))
}
if (RUN_COVID) {
  tbl <- read_rds(COVID_RDS) |>
    group_by(notification_date, diagnosis_date) |> summarise(n = sum(n), .groups = "drop") |>
    tbl_now(event_date = notification_date, report_date = diagnosis_date, case_count = n,
            data_type = "count-incidence") |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |> compute_temporal_effects()
  D$covid <- list(
    tbl = tbl, unit = "day", timestep = "day", nobbs_units = "1 day", N_pop = 5e6,
    min_eval = ymd("2020-03-09"), max_eval = ymd("2020-03-09") + years(2),
    enw_expect = list("RW" = ~ 1 + rw(day) + day_of_week, "point" = ~ 1 + day_of_week,
                      "RE" = ~ 1 + (1 | day_of_week)))
}
if (RUN_MPOX) {
  data(mpoxdat)
  tbl <- mpoxdat |>
    tbl_now(event_date = dx_date, report_date = dx_report_date, data_type = "linelist") |>
    to_count("count-incidence") |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |> compute_temporal_effects()
  ev  <- as_tibble(tbl)[[get_event_date(tbl)]]
  D$mpox <- list(
    tbl = tbl, unit = "day", timestep = "day", nobbs_units = "1 day", N_pop = 5e6,
    min_eval = ymd("2022-07-12"), max_eval = max(ev),
    enw_expect = list("RW" = ~ 1 + rw(day) + day_of_week, "point" = ~ 1 + day_of_week,
                      "RE" = ~ 1 + (1 | day_of_week)))
}

# For each disease add: the event/report column names, the eventual-truth table,
# the sampled evaluation dates, the own-model grid, and the epinowcast variant
# grid (3 delays x the 3 expectations defined above).
for (nm in names(D)) {
  tbl <- D[[nm]]$tbl
  D[[nm]]$event_col  <- get_event_date(tbl)
  D[[nm]]$report_col <- get_report_date(tbl)
  D[[nm]]$truth      <- tbl |> get_latest_reported_cases() |> as_tibble()
  # N_DATES interior evaluation dates (drop the first/last grid point), reproducibly.
  grid <- seq(D[[nm]]$min_eval, D[[nm]]$max_eval, by = paste0("1 ", D[[nm]]$unit))
  grid <- grid[2:(length(grid) - 1)]
  set.seed(DATE_SEED[[nm]])
  D[[nm]]$dates <- sort(base::sample(grid, N_DATES), decreasing = TRUE)
  # own-model grid: {HSGP, AR1, SIR} x {LogNormal, GenGamma, Dirichlet}
  D[[nm]]$own_models <- list()
  for (ep in list(hsgp_epidemic(), ar1_epidemic(), sir_epidemic(N_pop = D[[nm]]$N_pop)))
    for (dl in list(lognormal_delay(), generalized_gamma_delay(), dirichlet_delay()))
      D[[nm]]$own_models[[length(D[[nm]]$own_models) + 1L]] <- model(nb_likelihood(), ep, dl)
  # epinowcast grid: 3 delay families x the 3 expectation formulas above.
  delay_map <- c(LogNormal = "lognormal", Gamma = "gamma", Nonparametric = "nonparametric")
  D[[nm]]$enw_grid <- list()
  for (delay_name in names(delay_map))
    for (exp_name in names(D[[nm]]$enw_expect))
      D[[nm]]$enw_grid[[sprintf("Epinowcast (%s, %s)", delay_name, exp_name)]] <-
        list(dist = delay_map[[delay_name]], r = D[[nm]]$enw_expect[[exp_name]])
}

# Accumulators for every method's d*=0 predictions (long: model/date_run/observed/
# quantile/predicted/disease).
all_long <- list()

# =============================================================================
# 1. DISEASENOWCASTING  -- fit every (model, date) and time each fit
# =============================================================================
# This is exactly what backtest() does internally -- one nowcast() per model x
# date, in parallel -- but written out so we can time each individual fit (the
# `secs` column below).  Two-stage K = 25 imputation + the default NB phi prior
# are the nowcast() defaults; the tbl_now's COMPUTED temporal effects become
# covariates automatically.  The d*=0 nowcast is the newest event-time; `observed`
# is its eventual truth.
for (nm in names(D)) {
  cli::cli_h1(paste("diseasenowcasting:", nm))
  tbl <- D[[nm]]$tbl; dates <- D[[nm]]$dates; models <- D[[nm]]$own_models
  ev <- D[[nm]]$event_col; truth <- D[[nm]]$truth
  labels <- vapply(models, function(m)
    sprintf("own %s/%s/%s", m@epidemic@name, m@likelihood@name, m@delay@name), character(1))
  grid <- expand.grid(mi = seq_along(models), di = seq_along(dates))
  res <- foreach(g = seq_len(nrow(grid)), .combine = bind_rows,
                 .options.future = list(seed = TRUE)) %dofuture% {
    m <- models[[grid$mi[g]]]; date_run <- dates[grid$di[g]]
    out <- tryCatch({
      t <- system.time(nc <- nowcast(tbl, m, type = "two_stage", now = date_run, K = 25L,
                         n_draws = N_DRAWS, floor_sig_frac = FLOOR_SIG_FRAC,
                         floor_mu = FLOOR_MU, seed = SEED_FIT))
      summ <- as.data.frame(predict(nc, summary = TRUE))
      row  <- summ[summ$event_date == date_run, , drop = FALSE]
      if (nrow(row) == 0) row <- summ[which.max(summ$.event_num), , drop = FALSE]
      obs  <- truth |> filter(.data[[ev]] == date_run) |> pull(n)
      if (length(obs) == 0) NULL else
        tibble(model = labels[grid$mi[g]], date_run = date_run, observed = obs[1],
               quantile = PROBS, predicted = as.numeric(row[1, QCOLS]),
               secs = as.numeric(t["elapsed"]))
    }, error = function(e) NULL)
    out
  }
  all_long[[length(all_long) + 1L]] <- res |> mutate(disease = nm)
}

# =============================================================================
# 2. NobBS  -- one fit per (disease, date)
# =============================================================================
# Give every method section a FRESH worker pool.  A persistent worker that has
# run one engine (NobBS' JAGS) crashes when later handed another (epinowcast's
# CmdStan) -- the two native back-ends do not coexist in one process.  Killing
# the workers (`plan(sequential)`) and respawning forces clean processes.  A bare
# re-`plan(multisession)` is NOT enough; it reuses the existing workers.
plan(sequential); plan(multisession, workers = WORKERS)
for (nm in names(D)) {
  cli::cli_h1(paste("NobBS:", nm))
  tbl <- D[[nm]]$tbl; dates <- D[[nm]]$dates
  ev <- D[[nm]]$event_col; rp <- D[[nm]]$report_col; truth <- D[[nm]]$truth
  units <- D[[nm]]$nobbs_units
  res <- foreach(k = seq_along(dates), .combine = bind_rows,
                 .options.future = list(seed = TRUE)) %dofuture% {
    date_run <- dates[k]
    out <- tryCatch({
      sub <- tbl |> filter(.data[[ev]] <= date_run, .data[[rp]] <= date_run) |>
        as.data.frame() |> dplyr::select(all_of(c(ev, rp, "n"))) |> uncount(n)
      tt  <- system.time(fit <- suppressMessages(NobBS::NobBS(
        data = sub, now = date_run, units = units, onset_date = ev, report_date = rp,
        moving_window = 50, specs = list(dist = "NB", quantiles = PROBS))))
      est <- fit$estimates |> as.data.frame() |> filter(as.Date(onset_date) == date_run)
      obs <- truth |> filter(.data[[ev]] == date_run) |> pull(n)
      if (nrow(est) == 0 || length(obs) == 0) NULL else
        tibble(model = "NobBS", date_run = date_run, observed = obs[1], secs = as.numeric(tt["elapsed"]),
               quantile = PROBS, predicted = as.numeric(est[1, paste0("q_", PROBS)]))
    }, error = function(e) NULL)
    out
  }
  all_long[[length(all_long) + 1L]] <- res |> mutate(disease = nm)
}

# =============================================================================
# 3. EPINOWCAST  -- the 3 (delay) x 3 (expectation) grid, one preprocessing per
#                   (disease, date), then each variant fit on it.
# =============================================================================
# First COMPILE epinowcast's Stan model once here in the main process.  Without
# this, the parallel workers below all try to compile it simultaneously on first
# use and crash ("Future ... interrupted").  One warm fit fills the on-disk cache
# that every worker then reuses.  (The fit itself is thrown away.)
cli::cli_h1("epinowcast: pre-compiling the Stan model (one warm fit)")
pc_nm    <- names(D)[1]
pc_tbl   <- D[[pc_nm]]$tbl; pc_ev <- D[[pc_nm]]$event_col; pc_rp <- D[[pc_nm]]$report_col
pc_ts    <- D[[pc_nm]]$timestep; pc_date <- D[[pc_nm]]$dates[1]
pc_lo    <- pc_date - ENW_WINDOW * (if (pc_ts == "week") 7L else 1L)
pc_delay <- min(30L, as.integer(difftime(pc_date, max(min(pc_tbl[[pc_ev]], na.rm = TRUE), pc_lo),
                  units = if (pc_ts == "week") "weeks" else "days")))
pc_data  <- pc_tbl |> filter(.data[[pc_ev]] <= pc_date, .data[[pc_ev]] > pc_lo, .data[[pc_rp]] <= pc_date) |>
  to_count(to = "count-cumulative") |> as_tibble() |>
  transmute(reference_date = .data[[pc_ev]], report_date = .data[[pc_rp]], confirm = n) |>
  enw_complete_dates(max_delay = pc_delay, timestep = pc_ts) |>
  enw_preprocess_data(max_delay = pc_delay, timestep = pc_ts)
try(suppressMessages(suppressWarnings(epinowcast(
  data = pc_data, reference = enw_reference(parametric = ~1, distribution = "lognormal", data = pc_data),
  model = enw_model(verbose = FALSE),
  expectation = enw_expectation(r = D[[pc_nm]]$enw_expect[["RW"]], data = pc_data), fit = ENW_FIT))),
  silent = TRUE)

plan(sequential); plan(multisession, workers = WORKERS)   # fresh workers (no JAGS baggage)
for (nm in names(D)) {
  cli::cli_h1(paste("epinowcast:", nm))
  tbl <- D[[nm]]$tbl; dates <- D[[nm]]$dates; ts <- D[[nm]]$timestep
  ev <- D[[nm]]$event_col; rp <- D[[nm]]$report_col; truth <- D[[nm]]$truth
  grid <- D[[nm]]$enw_grid
  res <- foreach(k = seq_along(dates), .combine = bind_rows,
                 .options.future = list(seed = TRUE)) %dofuture% {
    date_run  <- dates[k]
    win_lo    <- date_run - ENW_WINDOW * (if (ts == "week") 7L else 1L)   # recent window
    min_date  <- max(min(tbl[[ev]], na.rm = TRUE), win_lo)
    max_delay <- min(30L, as.integer(difftime(date_run, min_date,
                       units = if (ts == "week") "weeks" else "days")))
    if (max_delay < 1L) return(NULL)
    p_data <- tryCatch(
      tbl |> filter(.data[[ev]] <= date_run, .data[[ev]] > win_lo, .data[[rp]] <= date_run) |>
        to_count(to = "count-cumulative") |> as_tibble() |>
        transmute(reference_date = .data[[ev]], report_date = .data[[rp]], confirm = n) |>
        enw_complete_dates(max_delay = max_delay, timestep = ts) |>
        enw_preprocess_data(max_delay = max_delay, timestep = ts),
      error = function(e) NULL)
    if (is.null(p_data)) return(NULL)
    obs <- truth |> filter(.data[[ev]] == date_run) |> pull(n)
    if (length(obs) == 0) return(NULL)
    # Fit every variant on the shared preprocessing.
    rows <- list()
    for (label in names(grid)) {
      spec <- grid[[label]]
      rows[[label]] <- tryCatch({
        ref <- if (identical(spec$dist, "nonparametric"))
                 enw_reference(parametric = ~0, non_parametric = ~ 1 + delay, data = p_data)
               else
                 enw_reference(parametric = ~1, distribution = spec$dist, data = p_data)
        tt  <- system.time(nc <- suppressMessages(suppressWarnings(epinowcast(
          data = p_data, reference = ref, model = enw_model(verbose = FALSE),
          expectation = enw_expectation(r = spec$r, data = p_data), fit = ENW_FIT))))
        summ <- epinowcast:::summary.epinowcast(nc, type = "nowcast", probs = PROBS) |>
          as_tibble() |> filter(as.Date(reference_date) == date_run)
        if (nrow(summ) == 0) NULL else
          tibble(model = label, date_run = date_run, observed = obs[1], secs = as.numeric(tt["elapsed"]),
                 quantile = PROBS, predicted = as.numeric(summ[1, paste0("q", PROBS * 100)]))
      }, error = function(e) NULL)
    }
    bind_rows(rows)
  }
  all_long[[length(all_long) + 1L]] <- res |> mutate(disease = nm)
}

# =============================================================================
# 4. BASELINENOWCAST  -- minimal non-parametric baseline, one fit per
#                        (disease, date).  No temporal effects (by design).
# =============================================================================
plan(sequential); plan(multisession, workers = WORKERS)   # fresh workers
for (nm in names(D)) {
  cli::cli_h1(paste("baselinenowcast:", nm))
  tbl <- D[[nm]]$tbl; dates <- D[[nm]]$dates; ts <- D[[nm]]$timestep
  ev <- D[[nm]]$event_col; rp <- D[[nm]]$report_col; truth <- D[[nm]]$truth
  res <- foreach(k = seq_along(dates), .combine = bind_rows,
                 .options.future = list(seed = TRUE)) %dofuture% {
    date_run <- dates[k]
    win_lo   <- date_run - ENW_WINDOW * (if (ts == "week") 7L else 1L)   # same recent window as epinowcast
    out <- tryCatch({
      sub <- tbl |> filter(.data[[ev]] <= date_run, .data[[ev]] > win_lo, .data[[rp]] <= date_run)
      rt  <- tbl_now_to_baselinenowcast(sub, format = "matrix", verbose = FALSE)
      tt  <- system.time(draws <- suppressMessages(suppressWarnings(baselinenowcast(rt, draws = N_DRAWS))))
      # d*=0 = the newest reference date (= date_run when present).
      d0_date <- if (any(as.Date(draws$reference_date) == date_run)) date_run
                 else max(as.Date(draws$reference_date))
      d0 <- draws |> filter(as.Date(reference_date) == d0_date) |> pull(pred_count)
      obs <- truth |> filter(.data[[ev]] == date_run) |> pull(n)
      if (length(d0) == 0 || length(obs) == 0) NULL else
        tibble(model = "baselinenowcast", date_run = date_run, observed = obs[1], secs = as.numeric(tt["elapsed"]),
               quantile = PROBS, predicted = as.numeric(quantile(d0, probs = PROBS, na.rm = TRUE)))
    }, error = function(e) NULL)
    out
  }
  all_long[[length(all_long) + 1L]] <- res |> mutate(disease = nm)
}

plan(sequential)
long_df <- bind_rows(all_long)

# =============================================================================
# SCORING  -- four tables per disease, on each comparator's common date set.
# =============================================================================
# Views:
#   own        : every diseasenowcasting model, on the dates the own models share.
#   nobbs      : own + NobBS, on the dates both produced.
#   epinowcast : own + the best-converging epinowcast variant(s) (ties kept).
#   baselinenowcast : own + baselinenowcast, on the dates both produced.
# We also record per-model convergence (how many dates each produced) for the
# vignette footnotes.
scores_list <- list(); conv_list <- list()
for (nm in names(D)) {
  L <- long_df |> filter(disease == nm)
  if (nrow(L) == 0) next
  all_dates  <- L |> distinct(date_run) |> pull()
  own_models <- sort(unique(L$model[grepl("^own ", L$model)]))
  own_dates  <- Reduce(intersect, lapply(own_models, function(m)
                  L |> filter(model == m) |> distinct(date_run) |> pull(date_run)))

  # Pick the comparator models + common dates for each of the four views.
  enw_models <- unique(L$model[grepl("^Epinowcast", L$model)])
  enw_n      <- vapply(enw_models, function(m) L |> filter(model == m) |>
                  distinct(date_run) |> nrow(), integer(1))
  best_enw   <- if (length(enw_n)) names(enw_n)[enw_n == max(enw_n)] else character(0)

  views <- list(
    own        = list(models = own_models, dates = own_dates),
    nobbs      = list(models = c(own_models, "NobBS"),
                      dates  = intersect(own_dates, L |> filter(model == "NobBS") |>
                                 distinct(date_run) |> pull(date_run))),
    epinowcast = list(models = c(own_models, best_enw),
                      dates  = Reduce(intersect, c(list(own_dates), lapply(best_enw, function(m)
                                 L |> filter(model == m) |> distinct(date_run) |> pull(date_run))))),
    baselinenowcast = list(models = c(own_models, "baselinenowcast"),
                      dates  = intersect(own_dates, L |> filter(model == "baselinenowcast") |>
                                 distinct(date_run) |> pull(date_run))))

  for (v in names(views)) {
    spec <- views[[v]]
    sub  <- L |> filter(model %in% spec$models, date_run %in% spec$dates)
    if (nrow(sub) == 0) next
    # Average wall-clock seconds per fit, by model (secs is repeated across a
    # fit's quantile rows, so de-duplicate to one row per model x date first).
    secs_by_model <- sub |> distinct(model, date_run, secs) |>
      group_by(model) |> summarise(secs = round(mean(secs, na.rm = TRUE), 2), .groups = "drop")
    tab <- sub |>
      as_forecast_quantile(observed = "observed", predicted = "predicted",
        quantile_level = "quantile", forecast_unit = c("model", "date_run")) |>
      score() |> summarise_scores(by = "model") |>
      transmute(model, wis = round(wis, 1), over = round(overprediction, 1),
                under = round(underprediction, 1), disp = round(dispersion, 1),
                bias = round(bias, 2), cov50 = round(interval_coverage_50, 2),
                cov90 = round(interval_coverage_90, 2)) |>
      left_join(secs_by_model, by = "model") |>
      arrange(wis) |>
      mutate(disease = nm, view = v, common_dates = length(spec$dates))
    scores_list[[length(scores_list) + 1L]] <- tab
    cli::cli_h3(paste(nm, "-", v, "-", length(spec$dates), "dates"))
    print(as.data.frame(tab |> select(model, wis, cov90, secs)), row.names = FALSE)
  }

  # Convergence: how many of the N_DATES each competitor produced.  We list EVERY
  # comparator we attempted -- the full epinowcast grid, NobBS, baselinenowcast --
  # so a variant that failed on *all* dates still shows up as 0/N (and isn't
  # silently dropped), which matters for the fairness footnotes.
  attempted <- c(names(D[[nm]]$enw_grid), "NobBS", "baselinenowcast")
  conv_list[[length(conv_list) + 1L]] <- tibble(model = attempted) |>
    left_join(L |> filter(model %in% attempted) |>
                distinct(model, date_run) |> count(model, name = "n_dates"), by = "model") |>
    mutate(n_dates = coalesce(n_dates, 0L), total = length(all_dates), failed = total - n_dates,
           note = ifelse(failed > 0, sprintf("did not converge on %d/%d dates", failed, total), ""),
           disease = nm) |> arrange(desc(n_dates))
}

scores_df <- bind_rows(scores_list)
conv_df   <- bind_rows(conv_list)

# =============================================================================
# SAVE
# =============================================================================
write_rds(scores_df, file.path(OUT, "benchmark_scores.rds"))
write_rds(conv_df,   file.path(OUT, "benchmark_convergence.rds"))
write_rds(long_df,   file.path(OUT, "benchmark_long.rds"))
cli::cli_alert_success(paste0(
  "Saved to ", OUT, "/: benchmark_scores.rds (", nrow(scores_df), " rows, 4 tables/disease), ",
  "benchmark_convergence.rds (", nrow(conv_df), " rows), ",
  "benchmark_long.rds (", nrow(long_df), " raw prediction rows)."))
