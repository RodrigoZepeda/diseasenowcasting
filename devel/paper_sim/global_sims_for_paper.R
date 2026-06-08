# =============================================================================
# 1. SETUP
# =============================================================================
set.seed(25789)
library(diseasenowcasting)
library(tbl.now)
library(dplyr)
library(tidyr)
library(future)
library(furrr)        # parallel map over evaluation dates
library(scoringutils)
library(NobBS)
epinowcast::enw_set_cache(tempdir(), type = 'session')
library(epinowcast)

future::plan(multisession, 
             workers = max(parallel::detectCores(logical = TRUE) - 1, 1))

N_DATES  <- 50
PROBS    <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
QCOLS    <- c("q2.5", "q5", "q10", "q25", "q50", "q75", "q90", "q95", "q97.5")

# The diseasenowcasting models we benchmark (extend as you like).
OWN_MODELS <- list(
  "own HSGP/LogNormal" = model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
  "own HSGP/GenGamma"  = model(nb_likelihood(), hsgp_epidemic(), generalized_gamma_delay()),
  "own AR1/LogNormal"  = model(nb_likelihood(), ar1_epidemic(),  lognormal_delay())
)

# Reproducible evaluation-date samplers (interior points only).
sample_dates <- function(tn, seed, unit) {
  ev    <- tn[[get_event_date(tn)]]
  grid  <- seq(min(ev, na.rm = TRUE), max(ev, na.rm = TRUE), by = paste("1", unit))
  grid  <- grid[-c(1, length(grid))]
  sort(base::sample(grid, N_DATES))
}

# =============================================================================
# 2. HELPERS -- each returns d*=0 quantiles in the shared long format.
#    Long format columns: method, date_run, observed, predicted, quantile_level
# =============================================================================

# Long-format helper: keep the NEWEST event (d*=0) per (model, date_run) from a
# diseasenowcasting backtest() result and pivot the quantile columns.
own_to_long <- function(bt_results) {
  bt_results |>
    group_by(model, date_run) |>
    slice_max(.event_num, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(method = model, date_run, observed = final,
              q2.5, q5, q10, q25, q50, q75, q90, q95, q97.5) |>
    pivot_longer(all_of(QCOLS), names_to = "qname", values_to = "predicted") |>
    mutate(quantile_level = PROBS[match(qname, QCOLS)], qname = NULL)
}

# diseasenowcasting: one backtest() call covers all OWN_MODELS and all dates.
run_own <- function(tn, dates) {
  bt <- backtest(tn, models = OWN_MODELS, dates = dates,
                 type = "two_stage", K = 25, n_draws = 1000, seed = 42)
  own_to_long(bt@results)
}

# NobBS: fit per date, take the d*=0 row (event_date == now).
run_nobbs <- function(tn, dates, unit_label) {
  ev_col  <- get_event_date(tn); rp_col <- get_report_date(tn)
  truth   <- get_latest_reported_cases(tn) |> as_tibble()
  res <- furrr::future_map_dfr(dates, function(d) {
    tryCatch({
      sub <- tn |> filter(.data[[ev_col]] <= d, .data[[rp_col]] <= d) |>
        as.data.frame() |> select(all_of(c(ev_col, rp_col, "n"))) |> uncount(n)
      fit <- NobBS::NobBS(data = sub, now = d, units = unit_label,
                          onset_date = ev_col, report_date = rp_col,
                          moving_window = 50,
                          specs = list(dist = "NB", quantiles = PROBS))
      est <- fit$estimates |> as.data.frame() |>
        filter(as.Date(onset_date) == d)             # d*=0 nowcast
      obs <- truth |> filter(.data[[ev_col]] == d) |> pull(n)
      if (nrow(est) == 0 || length(obs) == 0) return(NULL)
      tibble(method = "NobBS", date_run = d,
             observed = obs[1], quantile_level = PROBS,
             predicted = as.numeric(est[1, paste0("q_", PROBS)]))
    }, error = function(e) NULL)
  }, .options = furrr::furrr_options(seed = TRUE))
  res
}

# epinowcast: fit per date (pathfinder for speed), take the d*=0 reference date.
run_enw <- function(tn, dates, timestep) {
  ev_col <- get_event_date(tn); rp_col <- get_report_date(tn)
  truth  <- get_latest_reported_cases(tn) |> as_tibble()
  res <- furrr::future_map_dfr(dates, function(d) {
    tryCatch({
      max_delay <- min(30L, as.integer(difftime(d, min(tn[[ev_col]], na.rm = TRUE),
                                                units = if (timestep == "week") "weeks" else "days")))
      if (max_delay < 1L) return(NULL)
      cum <- tn |> filter(.data[[ev_col]] <= d, .data[[rp_col]] <= d) |>
        to_count(to = "count-cumulative") |> as_tibble() |>
        transmute(reference_date = .data[[ev_col]], report_date = .data[[rp_col]],
                  confirm = n)
      pd  <- cum |> enw_complete_dates(max_delay = max_delay, timestep = timestep) |>
        enw_preprocess_data(max_delay = max_delay, timestep = timestep)
      nc  <- epinowcast(data = pd, model = enw_model(verbose = FALSE),
                        fit = enw_fit_opts(sampler = enw_pathfinder,
                                           refresh = 0, show_messages = FALSE))
      sm  <- summary(nc, type = "nowcast", probs = PROBS) |> as_tibble() |>
        filter(as.Date(reference_date) == d)         # d*=0 nowcast
      obs <- truth |> filter(.data[[ev_col]] == d) |> pull(n)
      if (nrow(sm) == 0 || length(obs) == 0) return(NULL)
      tibble(method = "epinowcast", date_run = d,
             observed = obs[1], quantile_level = PROBS,
             predicted = as.numeric(sm[1, paste0("q", PROBS * 100)]))
    }, error = function(e) NULL)
  }, .options = furrr::furrr_options(seed = TRUE))
  res
}

# =============================================================================
# 3. RUN -- loop over diseases
# =============================================================================
build <- list(
  dengue = function() tbl_now(denguedat, event_date = onset_week,
                              report_date = report_week, data_type = "linelist",
                              verbose = FALSE, 
                              t_effects = temporal_effects(seasons = 52)),
  mpox   = function() tbl_now(mpoxdat, event_date = dx_date,
                              report_date = dx_report_date,
                              data_type = "linelist", verbose = FALSE,
                              t_effects = temporal_effects(day_of_week = T)),
  covid  = function() tbl_now(covid_colombia, event_date = notification_date,
                              report_date = diagnosis_date, strata = sex, case_count = n,
                              data_type = "count-incidence", verbose = FALSE,
                              t_effects = temporal_effects(day_of_week = T))
)
date_seed  <- c(dengue = 8765L, mpox = 2109L, covid = 5432L)
unit_lab   <- c(dengue = "1 week", mpox = "1 day", covid = "1 day")
unit_step  <- c(dengue = "week", mpox = "day", covid = "day")

all_long <- list()
for (dis in names(build)) {
  tn    <- build[[dis]]()
  dates <- sample_dates(tn, date_seed[[dis]], unit_step[[dis]])
  
  long <- bind_rows(
    run_own(tn, dates),
    run_nobbs(tn, dates, unit_lab[[dis]]),
    run_enw(tn, dates, unit_step[[dis]])
  )
  all_long[[dis]] <- long |> mutate(disease = dis)
  message("Done: ", dis)
}

# =============================================================================
# 4. SCORE -- common date set per disease, then WIS + coverage via scoringutils
# =============================================================================
score_disease <- function(long) {
  # Common date set: keep only date_runs where EVERY method produced a nowcast.
  ok <- long |> distinct(method, date_run) |> count(date_run) |>
    filter(n == n_distinct(long$method)) |> pull(date_run)
  long <- long |> filter(date_run %in% ok)
  
  fc <- as_forecast_quantile(long, observed = "observed", predicted = "predicted",
                             quantile_level = "quantile_level",
                             forecast_unit = c("method", "date_run"))
  sc <- score(fc)
  summarise_scores(sc, by = "method") |>
    transmute(model = method, wis,
              over = overprediction, under = underprediction, disp = dispersion,
              bias, cov50 = interval_coverage_50, cov90 = interval_coverage_90,
              n_dates = length(ok)) |>
    arrange(wis)
}

scores_df <- bind_rows(lapply(names(all_long), function(dis)
  score_disease(all_long[[dis]]) |> mutate(disease = dis)))

future::plan(sequential)

# Save so the vignette tables can be rebuilt from this object.
saveRDS(scores_df, "comparison_scores.rds")