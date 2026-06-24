# benchmark_full.R
rm(list = ls())

library(dplyr) 
library(tidyr) 
library(tibble) 
library(readr) 
library(purrr)
library(lubridate) 
library(tbl.now) 
library(diseasenowcasting)
library(scoringutils)
library(foreach)
library(future)
library(doFuture)
library(NobBS)
library(epinowcast)
library(baselinenowcast)

options(cmdstanr_warn_inits = FALSE)
epinowcast::enw_set_cache(tools::R_user_dir(package = "epinowcast", "cache"), type = "session")

# ---- run controls -------- ---- ---- ---- ---- ---- ---- ---- ---- ----  
N_DATES     <- 2 
N_DRAWS     <- 1000
WORKERS     <- 16 
RUN_DENGUE  <- TRUE 
RUN_COVID   <- TRUE 
RUN_MPOX    <- TRUE 
# Two-stage delay-imputation spread floors for the own models.  These are now the
# package defaults (they avoid over-dispersing low-count daily data); passed
# explicitly here to document the value the package is backtested at.
FLOOR_SIG_FRAC <- 0.08
FLOOR_MU       <- 0.08
ENW_SAMPLER    <- "pathfinder"  # epinowcast sampler: Pathfinder (fast variational, default) or full NUTS (slow).
# epinowcast / baselinenowcast cannot scale to a 20-year weekly series the way
# the RTMB own models can: a full-series fit takes minutes and OOM-crashes the
# parallel workers.  So we give them the most recent ENW_WINDOW reference
# time-steps before each as-of date -- ample for the delay + recent trend (and
# more generous than NobBS's 50-step moving window).  This is also how a real
# epinowcast user would run it.  Raise it if you have the time/RAM.
ENW_WINDOW  <- 150 
COVID_RDS   <- Sys.getenv("COVID_RDS",
  "/Users/rodzepeda/Documents/diseasenowcast2/devel/covid_colombia_aggregated.rds")
OUT         <- tempdir()#"devel/results"

#Create the output dir
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---- constants --------------------------------------------------------------
PROBS  <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
QCOLS  <- c("q2.5", "q5", "q10", "q25", "q50", "q75", "q90", "q95", "q97.5")
SEED_FIT  <- 23875L
if (ENW_SAMPLER == "pathfinder") {
  ENW_FIT <- enw_fit_opts(sampler = epinowcast::enw_pathfinder, refresh = 0, show_messages = FALSE)
} else {
  ENW_FIT <- enw_fit_opts(sampler = epinowcast::enw_sample, refresh = 0, show_messages = FALSE,
               chains = 2, parallel_chains = 2, iter_warmup = 500, iter_sampling = 500,
               adapt_delta = 0.95)
}

# =============================================================================
# DATA  -- build each disease's tbl_now (with temporal effects), evaluation
#          dates, truth, own-model grid, and epinowcast variant grid.
# =============================================================================
D <- list()

if (RUN_DENGUE) {
  
  set.seed(8765L) 
  
  data(denguedat)
  
  # 1. Prepare the Data
  tbl <- denguedat |>
    tbl_now(event_date = onset_week, report_date = report_week, data_type = "linelist") |>
    to_count("count-incidence") |>
    add_temporal_effects(temporal_effects(seasons = 52)) |> 
    compute_temporal_effects()
  
  ev <- tbl |> pull(get_event_date(tbl))
  min_eval <- min(ev)
  max_eval <- max(ev)
  
  # 2. Sample Evaluation Dates Reproducibly
  full_grid <- seq(min_eval, max_eval, by = "1 week")
  # Drop the first and last points
  interior_grid <- full_grid[2:(length(full_grid) - 1)] 
  
  
  sampled_dates <- sort(base::sample(interior_grid, N_DATES), decreasing = TRUE)
  
  # 3. Build the diseasenowcasting Model Grid
  # Define components cleanly, then map over all combinations
  epidemic_components <- list(
    hsgp_epidemic(), 
    ar1_epidemic(), 
    sir_epidemic(N_pop = 5e6)
  )
  
  delay_components <- list(
    lognormal_delay(), 
    generalized_gamma_delay(), 
    dirichlet_delay()
  )
  
  # cross2 creates all pairs, map applies the model constructor
  own_models_list <- cross2(epidemic_components, delay_components) |>
    map(~ model(
      likelihood = nb_likelihood(), 
      epidemic   = .x[[1]], 
      delay      = .x[[2]]
    ))
  
  # 4. Build the epinowcast Model Grid
  enw_expectations <- list(
    "RW"    = ~ 1 + rw(week), 
    "point" = ~ 1 + week, 
    "RE"    = ~ 1 + (1 | week)
  )
  
  delay_map <- c(
    "LogNormal"     = "lognormal", 
    "Gamma"         = "gamma", 
    "Nonparametric" = "nonparametric"
  )
  
  # Create a flat dataframe of combinations, then map to a named list
  enw_combinations <- expand_grid(
    delay_name = names(delay_map),
    exp_name   = names(enw_expectations)
  )
  
  enw_grid_list <- map2(
    enw_combinations$delay_name,
    enw_combinations$exp_name,
    function(d_name, e_name) {
      list(
        dist = delay_map[[d_name]],
        r    = enw_expectations[[e_name]]
      )
    }
  ) |> 
    set_names(sprintf("Epinowcast (%s, %s)", enw_combinations$delay_name, enw_combinations$exp_name))
  
  # 5. Assemble the Final Object
  D$dengue <- list(
    tbl         = tbl, 
    unit        = "week", 
    timestep    = "week",
    N_pop       = 5e6,
    min_eval    = min_eval, 
    max_eval    = max_eval,
    nobbs_units = "1 week",
    event_col   = get_event_date(tbl),
    report_col  = get_report_date(tbl),
    truth       = tbl |> get_latest_reported_cases() |> as_tibble(),
    dates       = sampled_dates,
    own_models  = own_models_list,
    enw_grid    = enw_grid_list
  )
}

if (RUN_COVID) {
  
  data(covid_colombia)
  
  # 1. Prepare the Data
  tbl <- covid_colombia |>
    group_by(notification_date, diagnosis_date) |> 
    summarise(n = sum(n), .groups = "drop") |> 
    tbl_now(
      event_date  = notification_date, 
      report_date = diagnosis_date, 
      case_count  = n,
      data_type   = "count-incidence"
    ) |> 
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |> 
    compute_temporal_effects()
  
  min_eval <- ymd("2020-03-09")
  max_eval <- ymd("2020-03-09") + years(2)
  
  # 2. Sample Evaluation Dates Reproducibly
  full_grid <- seq(min_eval, max_eval, by = "1 day")
  interior_grid <- full_grid[2:(length(full_grid) - 1)]
  
  set.seed(5432L) # covid seed
  sampled_dates <- sort(base::sample(interior_grid, N_DATES), decreasing = TRUE)
  
  # 3. Build the diseasenowcasting Model Grid
  epidemic_components <- list(
    hsgp_epidemic(), 
    ar1_epidemic(), 
    sir_epidemic(N_pop = 5e6)
  )
  
  delay_components <- list(
    lognormal_delay(), 
    generalized_gamma_delay(), 
    dirichlet_delay()
  )
  
  own_models_list <- cross2(epidemic_components, delay_components) |>
    map(~ model(
      likelihood = nb_likelihood(), 
      epidemic   = .x[[1]], 
      delay      = .x[[2]]
    ))
  
  # 4. Build the epinowcast Model Grid
  enw_expectations <- list(
    "RW"    = ~ 1 + rw(day) + day_of_week, 
    "point" = ~ 1 + day_of_week,
    "RE"    = ~ 1 + (1 | day_of_week)
  )
  
  delay_map <- c(
    "LogNormal"     = "lognormal", 
    "Gamma"         = "gamma", 
    "Nonparametric" = "nonparametric"
  )
  
  enw_combinations <- expand_grid(
    delay_name = names(delay_map),
    exp_name   = names(enw_expectations)
  )
  
  enw_grid_list <- map2(
    enw_combinations$delay_name,
    enw_combinations$exp_name,
    function(d_name, e_name) {
      list(
        dist = delay_map[[d_name]],
        r    = enw_expectations[[e_name]]
      )
    }
  ) |> 
    set_names(sprintf("Epinowcast (%s, %s)", enw_combinations$delay_name, enw_combinations$exp_name))
  
  # 5. Assemble the Final Object
  D$covid <- list(
    tbl         = tbl, 
    unit        = "day", 
    timestep    = "day", 
    min_eval    = min_eval, 
    max_eval    = max_eval,
    N_pop       = 5e6,
    nobbs_units = "1 day", 
    event_col   = get_event_date(tbl),
    report_col  = get_report_date(tbl),
    truth       = tbl |> get_latest_reported_cases() |> as_tibble(),
    dates       = sampled_dates,
    own_models  = own_models_list,
    enw_grid    = enw_grid_list
  )
}

if (RUN_MPOX) {
  
  data(mpoxdat)
  
  # 1. Prepare the Data
  tbl <- mpoxdat |>
    tbl_now(event_date = dx_date, report_date = dx_report_date, data_type = "linelist") |>
    to_count("count-incidence") |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |> 
    compute_temporal_effects()
  
  ev <- tbl |> pull(get_event_date(tbl))
  min_eval <- ymd("2022-07-12")
  max_eval <- max(ev)
  
  # 2. Sample Evaluation Dates Reproducibly
  full_grid <- seq(min_eval, max_eval, by = "1 day")
  interior_grid <- full_grid[2:(length(full_grid) - 1)]
  
  set.seed(2109L) # mpox seed
  sampled_dates <- sort(base::sample(interior_grid, N_DATES), decreasing = TRUE)
  
  # 3. Build the diseasenowcasting Model Grid
  epidemic_components <- list(
    hsgp_epidemic(), 
    ar1_epidemic(), 
    sir_epidemic(N_pop = 5e6)
  )
  
  delay_components <- list(
    lognormal_delay(), 
    generalized_gamma_delay(), 
    dirichlet_delay()
  )
  
  own_models_list <- cross2(epidemic_components, delay_components) |>
    map(~ model(
      likelihood = nb_likelihood(), 
      epidemic   = .x[[1]], 
      delay      = .x[[2]]
    ))
  
  # 4. Build the epinowcast Model Grid
  enw_expectations <- list(
    "RW"    = ~ 1 + rw(day) + day_of_week, 
    "point" = ~ 1 + day_of_week,
    "RE"    = ~ 1 + (1 | day_of_week)
  )
  
  delay_map <- c(
    "LogNormal"     = "lognormal", 
    "Gamma"         = "gamma", 
    "Nonparametric" = "nonparametric"
  )
  
  enw_combinations <- expand_grid(
    delay_name = names(delay_map),
    exp_name   = names(enw_expectations)
  )
  
  enw_grid_list <- map2(
    enw_combinations$delay_name,
    enw_combinations$exp_name,
    function(d_name, e_name) {
      list(
        dist = delay_map[[d_name]],
        r    = enw_expectations[[e_name]]
      )
    }
  ) |> 
    set_names(sprintf("Epinowcast (%s, %s)", enw_combinations$delay_name, enw_combinations$exp_name))
  
  # 5. Assemble the Final Object
  D$mpox <- list(
    tbl         = tbl, 
    unit        = "day", 
    timestep    = "day", 
    N_pop       = 5e6,
    nobbs_units = "1 day", 
    event_col   = get_event_date(tbl),
    report_col  = get_report_date(tbl),
    truth       = tbl |> get_latest_reported_cases() |> as_tibble(),
    min_eval    = min_eval, 
    max_eval    = max_eval,
    dates       = sampled_dates,
    own_models  = own_models_list,
    enw_grid    = enw_grid_list
  )
}

# Accumulators for every method's d*=0 predictions (long: model/date_run/observed/
# quantile/predicted/disease).
all_long <- list()

# =============================================================================
# 1. DISEASENOWCASTING
# =============================================================================
for (nm in names(D)) {
  cli::cli_h1(paste("diseasenowcasting:", nm))
  
  tbl    <- D[[nm]]$tbl
  dates  <- D[[nm]]$dates
  models <- D[[nm]]$own_models
  ev     <- D[[nm]]$event_col
  truth  <- D[[nm]]$truth
  
  labels    <- map_chr(models, ~ sprintf("own %s/%s/%s", .x@epidemic@name, .x@likelihood@name, .x@delay@name))
  
  task_grid <- expand_grid(model_idx = seq_along(models), date_run = dates)
  
  res <- foreach(
    idx      = task_grid$model_idx, 
    date_run = task_grid$date_run, 
    .combine = bind_rows,
    .options.future = list(seed = TRUE)
  ) %dofuture% {
    
    m   <- models[[idx]]
    lbl <- labels[[idx]]
    
    out <- tryCatch({
      t <- system.time({
        nc <- nowcast(
          tbl, m, type = "two_stage", now = date_run, K = 25L,
          n_draws = N_DRAWS, floor_sig_frac = FLOOR_SIG_FRAC,
          floor_mu = FLOOR_MU, seed = SEED_FIT
        )
      })
      
      summ <- as.data.frame(predict(nc, summary = TRUE))
      row  <- summ[summ$event_date == date_run, , drop = FALSE]
      if (nrow(row) == 0) row <- summ[which.max(summ$.event_num), , drop = FALSE]
      
      obs <- truth |> filter(.data[[ev]] == date_run) |> pull(n)
      
      if (length(obs) == 0) return(NULL)
      
      tibble(
        model = lbl, date_run = date_run, observed = obs[1],
        quantile = PROBS, predicted = as.numeric(row[1, QCOLS]), secs = as.numeric(t["elapsed"])
      )
    }, error = function(e) NULL)
    
    out
  }
  
  all_long[[length(all_long) + 1L]] <- res |> mutate(disease = nm)
}

# =============================================================================
# 2. NobBS
# =============================================================================
plan(sequential); plan(multisession, workers = WORKERS)

for (nm in names(D)) {
  cli::cli_h1(paste("NobBS:", nm))
  
  tbl   <- D[[nm]]$tbl
  dates <- D[[nm]]$dates
  ev    <- D[[nm]]$event_col
  rp    <- D[[nm]]$report_col
  truth <- D[[nm]]$truth
  units <- D[[nm]]$nobbs_units
  
  res <- foreach(
    date_run = dates, 
    .combine = bind_rows,
    .options.future = list(seed = TRUE)
  ) %dofuture% {
    
    tryCatch({
      sub <- tbl |> 
        filter(.data[[ev]] <= date_run, .data[[rp]] <= date_run) |>
        as.data.frame() |> 
        select(all_of(c(ev, rp, "n"))) |> 
        uncount(n)
      
      tt <- system.time({
        fit <- suppressMessages(NobBS::NobBS(
          data = sub, now = date_run, units = units, onset_date = ev, report_date = rp,
          moving_window = 50, specs = list(dist = "NB", quantiles = PROBS)
        ))
      })
      
      est <- fit$estimates |> as.data.frame() |> filter(as.Date(onset_date) == date_run)
      obs <- truth |> filter(.data[[ev]] == date_run) |> pull(n)
      
      if (nrow(est) == 0 || length(obs) == 0) return(NULL)
      
      tibble(
        model = "NobBS", date_run = date_run, observed = obs[1], secs = as.numeric(tt["elapsed"]),
        quantile = PROBS, predicted = as.numeric(est[1, paste0("q_", PROBS)])
      )
    }, error = function(e) NULL)
  }
  
  all_long[[length(all_long) + 1L]] <- res |> mutate(disease = nm)
}

# =============================================================================
# 3. EPINOWCAST
# =============================================================================
cli::cli_h1("epinowcast: pre-compiling the Stan model (one warm fit)")
pc_nm    <- names(D)[1]
pc_tbl   <- D[[pc_nm]]$tbl
pc_ev    <- D[[pc_nm]]$event_col
pc_rp    <- D[[pc_nm]]$report_col
pc_ts    <- D[[pc_nm]]$timestep
pc_date  <- D[[pc_nm]]$dates[1]
pc_lo    <- pc_date - ENW_WINDOW * (if (pc_ts == "week") 7L else 1L)

pc_delay <- min(30L, as.integer(difftime(pc_date, max(min(pc_tbl[[pc_ev]], na.rm = TRUE), pc_lo), 
                                         units = if (pc_ts == "week") "weeks" else "days")))

pc_data <- pc_tbl |> 
  filter(.data[[pc_ev]] <= pc_date, .data[[pc_ev]] > pc_lo, .data[[pc_rp]] <= pc_date) |>
  to_count(to = "count-cumulative") |> 
  as_tibble() |>
  transmute(reference_date = .data[[pc_ev]], report_date = .data[[pc_rp]], confirm = n) |>
  enw_complete_dates(max_delay = pc_delay, timestep = pc_ts) |>
  enw_preprocess_data(max_delay = pc_delay, timestep = pc_ts)

try(suppressMessages(suppressWarnings(epinowcast(
  data = pc_data, reference = enw_reference(parametric = ~1, distribution = "lognormal", data = pc_data),
  model = enw_model(verbose = FALSE),
  expectation = enw_expectation(r = D[[pc_nm]]$enw_expect[["RW"]], data = pc_data), fit = ENW_FIT
))), silent = TRUE)

plan(sequential); plan(multisession, workers = WORKERS)

for (nm in names(D)) {
  cli::cli_h1(paste("epinowcast:", nm))
  
  tbl   <- D[[nm]]$tbl
  dates <- D[[nm]]$dates
  ts    <- D[[nm]]$timestep
  ev    <- D[[nm]]$event_col
  rp    <- D[[nm]]$report_col
  truth <- D[[nm]]$truth
  grid  <- D[[nm]]$enw_grid
  
  res <- foreach(
    date_run = dates, 
    .combine = bind_rows,
    .options.future = list(seed = TRUE)
  ) %dofuture% {
    
    win_lo    <- date_run - ENW_WINDOW * (if (ts == "week") 7L else 1L)
    min_date  <- max(min(tbl[[ev]], na.rm = TRUE), win_lo)
    max_delay <- min(30L, as.integer(difftime(date_run, min_date, units = if (ts == "week") "weeks" else "days")))
    
    if (max_delay < 1L) return(NULL)
    
    p_data <- tryCatch({
      
      tbl |> 
        filter(.data[[ev]] <= date_run, .data[[ev]] > win_lo, .data[[rp]] <= date_run) |>
        to_count(to = "count-cumulative") |> 
        as_tibble() |>
        transmute(reference_date = .data[[ev]], report_date = .data[[rp]], confirm = n) |>
        enw_complete_dates(max_delay = max_delay, timestep = ts) |>
        enw_preprocess_data(max_delay = max_delay, timestep = ts)
      
    }, error = function(e) NULL)
    
    if (is.null(p_data)) return(NULL)
    
    obs <- truth |> filter(.data[[ev]] == date_run) |> pull(n)
    if (length(obs) == 0) return(NULL)
    
    # Map across all enw configurations natively
    imap(grid, function(spec, label) {
      tryCatch({
        ref <- if (identical(spec$dist, "nonparametric")) {
          enw_reference(parametric = ~0, non_parametric = ~ 1 + delay, data = p_data)
        } else {
          enw_reference(parametric = ~1, distribution = spec$dist, data = p_data)
        }
        
        tt <- system.time({
          nc <- suppressMessages(suppressWarnings(epinowcast(
            data = p_data, reference = ref, model = enw_model(verbose = FALSE),
            expectation = enw_expectation(r = spec$r, data = p_data), fit = ENW_FIT
          )))
        })
        
        summ <- epinowcast:::summary.epinowcast(nc, type = "nowcast", probs = PROBS) |>
          as_tibble() |> 
          filter(as.Date(reference_date) == date_run)
        
        if (nrow(summ) == 0) return(NULL)
        
        tibble(
          model = label, date_run = date_run, observed = obs[1], secs = as.numeric(tt["elapsed"]),
          quantile = PROBS, predicted = as.numeric(summ[1, paste0("q", PROBS * 100)])
        )
      }, error = function(e) NULL)
    }) |> list_rbind()
  }
  
  all_long[[length(all_long) + 1L]] <- res |> mutate(disease = nm)
}

# =============================================================================
# 4. BASELINENOWCAST
# =============================================================================
plan(sequential); plan(multisession, workers = WORKERS)

for (nm in names(D)) {
  cli::cli_h1(paste("baselinenowcast:", nm))
  
  tbl   <- D[[nm]]$tbl
  dates <- D[[nm]]$dates
  ts    <- D[[nm]]$timestep
  ev    <- D[[nm]]$event_col
  rp    <- D[[nm]]$report_col
  truth <- D[[nm]]$truth
  
  res <- foreach(
    date_run = dates, 
    .combine = bind_rows,
    .options.future = list(seed = TRUE)
  ) %dofuture% {
    
    win_lo <- date_run - ENW_WINDOW * (if (ts == "week") 7L else 1L)
    
    tryCatch({
      sub <- tbl |> filter(.data[[ev]] <= date_run, .data[[ev]] > win_lo, .data[[rp]] <= date_run)
      rt  <- tbl_now_to_baselinenowcast(sub, format = "matrix", verbose = FALSE)
      
      tt <- system.time({
        draws <- suppressMessages(suppressWarnings(baselinenowcast(rt, draws = N_DRAWS)))
      })
      
      d0_date <- if (any(as.Date(draws$reference_date) == date_run)) date_run else max(as.Date(draws$reference_date))
      d0 <- draws |> filter(as.Date(reference_date) == d0_date) |> pull(pred_count)
      obs <- truth |> filter(.data[[ev]] == date_run) |> pull(n)
      
      if (length(d0) == 0 || length(obs) == 0) return(NULL)
      
      tibble(
        model = "baselinenowcast", date_run = date_run, observed = obs[1], secs = as.numeric(tt["elapsed"]),
        quantile = PROBS, predicted = as.numeric(quantile(d0, probs = PROBS, na.rm = TRUE))
      )
    }, error = function(e) NULL)
  }
  
  all_long[[length(all_long) + 1L]] <- res |> mutate(disease = nm)
}

plan(sequential)
long_df <- bind_rows(all_long)

# =============================================================================
# SCORING
# =============================================================================
scores_list <- list()
conv_list   <- list()

for (nm in names(D)) {
  L <- long_df |> filter(disease == nm)
  if (nrow(L) == 0) next
  
  all_dates  <- L |> distinct(date_run) |> pull()
  own_models <- sort(unique(L$model[grepl("^own ", L$model)]))
  own_dates  <- Reduce(intersect, map(own_models, ~ L |> filter(model == .x) |> distinct(date_run) |> pull(date_run)))
  
  enw_models <- unique(L$model[grepl("^Epinowcast", L$model)])
  enw_n      <- map_int(enw_models, ~ L |> filter(model == .x) |> distinct(date_run) |> nrow())
  best_enw   <- if (length(enw_n)) names(enw_n)[enw_n == max(enw_n)] else character(0)
  
  views <- list(
    own = list(
      models = own_models, 
      dates = own_dates
    ),
    nobbs = list(
      models = c(own_models, "NobBS"),
      dates  = intersect(own_dates, L |> filter(model == "NobBS") |> distinct(date_run) |> pull(date_run))
    ),
    epinowcast = list(
      models = c(own_models, best_enw),
      dates  = Reduce(intersect, c(list(own_dates), map(best_enw, ~ L |> filter(model == .x) |> distinct(date_run) |> pull(date_run))))
    ),
    baselinenowcast = list(
      models = c(own_models, "baselinenowcast"),
      dates  = intersect(own_dates, L |> filter(model == "baselinenowcast") |> distinct(date_run) |> pull(date_run))
    )
  )
  
  for (v in names(views)) {
    spec <- views[[v]]
    sub  <- L |> filter(model %in% spec$models, date_run %in% spec$dates)
    if (nrow(sub) == 0) next
    
    secs_by_model <- sub |> 
      distinct(model, date_run, secs) |>
      group_by(model) |> 
      summarise(secs = round(mean(secs, na.rm = TRUE), 2), .groups = "drop")
    
    tab <- sub |>
      as_forecast_quantile(
        observed = "observed", predicted = "predicted",
        quantile_level = "quantile", forecast_unit = c("model", "date_run")
      ) |>
      score() |> 
      summarise_scores(by = "model") |>
      transmute(
        model, 
        wis   = round(wis, 1), 
        over  = round(overprediction, 1),
        under = round(underprediction, 1), 
        disp  = round(dispersion, 1),
        bias  = round(bias, 2), 
        cov50 = round(interval_coverage_50, 2),
        cov90 = round(interval_coverage_90, 2)
      ) |>
      left_join(secs_by_model, by = "model") |>
      arrange(wis) |>
      mutate(disease = nm, view = v, common_dates = length(spec$dates))
    
    scores_list[[length(scores_list) + 1L]] <- tab
    cli::cli_h3(paste(nm, "-", v, "-", length(spec$dates), "dates"))
    print(as.data.frame(tab |> select(model, wis, cov90, secs)), row.names = FALSE)
  }
  
  attempted <- c(names(D[[nm]]$enw_grid), "NobBS", "baselinenowcast")
  conv_list[[length(conv_list) + 1L]] <- tibble(model = attempted) |>
    left_join(
      L |> filter(model %in% attempted) |> distinct(model, date_run) |> count(model, name = "n_dates"), 
      by = "model"
    ) |>
    mutate(
      n_dates = coalesce(n_dates, 0L), 
      total   = length(all_dates), 
      failed  = total - n_dates,
      note    = ifelse(failed > 0, sprintf("did not converge on %d/%d dates", failed, total), ""),
      disease = nm
    ) |> 
    arrange(desc(n_dates))
}

scores_df <- bind_rows(scores_list)
conv_df   <- bind_rows(conv_list)

# =============================================================================
# SAVE
# =============================================================================
write_rds(scores_df, file.path(OUT, "benchmark_scores.rds"))
write_rds(conv_df,   file.path(OUT, "benchmark_convergence.rds"))
write_rds(long_df,   file.path(OUT, "benchmark_long.rds"))
cli::cli_alert_success(sprintf(
  "Saved to %s/: benchmark_scores.rds (%d rows), benchmark_convergence.rds (%d rows), benchmark_long.rds (%d raw prediction rows).",
  OUT, nrow(scores_df), nrow(conv_df), nrow(long_df)
))