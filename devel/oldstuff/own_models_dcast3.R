# =============================================================================
# own_models_diseasenowcasting.R — diseasenowcasting (RTMB) port of diseasenowcast2/devel/own_models.R
# =============================================================================
# Parallel (foreach %dofuture% over evaluation dates).  Produces the SAME
# per-disease *_diseasenowcasting_steps.rds outputs (same columns + same evaluation dates
# via the shared seeds) so the scoring pipeline can compare diseasenowcasting vs NobBS vs
# Epinowcast unchanged.  Also records per-(disease, model, date) wall-time +
# series length for the fit-time-vs-series-length analysis.
#
# Requires diseasenowcasting INSTALLED (R CMD INSTALL diseasenowcasting) so parallel workers can
# library(diseasenowcasting).  Env vars (all optional):
#   N_DATES=50  RUN_DENGUE/RUN_COVID/RUN_MPOX=TRUE  K_MULTISAMPLE=25
#   TUNE_WORKERS=8  N_DRAWS_PER=200  MODELS=all|fast
#   COVID_RDS=<path to covid_colombia_aggregated.rds>
# Output (under devel/results/): {dengue,covid,mpox}_diseasenowcasting_steps.rds + diseasenowcasting_timing.rds
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(purrr)
  library(lubridate); library(tbl.now); library(diseasenowcasting)
  library(foreach); library(future); library(doFuture)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
OUT <- "devel"
dir.create(file.path(OUT, "results"), showWarnings = FALSE, recursive = TRUE)

N_DATES        <- as.integer(Sys.getenv("N_DATES", "50"))
RUN_DENGUE     <- as.logical(Sys.getenv("RUN_DENGUE", "TRUE"))
RUN_COVID      <- as.logical(Sys.getenv("RUN_COVID",  "TRUE"))
RUN_MPOX       <- as.logical(Sys.getenv("RUN_MPOX",   "TRUE"))
K_MULTISAMPLE  <- as.integer(Sys.getenv("K_MULTISAMPLE", "25"))
N_DRAWS_PER    <- as.integer(Sys.getenv("N_DRAWS_PER", "200"))
TUNE_WORKERS   <- as.integer(Sys.getenv("TUNE_WORKERS", "8"))
MODELS_SET     <- Sys.getenv("MODELS", "all")
PHI_PRIOR      <- lognormal_prior(log(20), 0.5)
DENGUE_DATE_SEED <- 8765L; COVID_DATE_SEED <- 5432L; MPOX_DATE_SEED <- 2109L

plan(multisession, workers = TUNE_WORKERS)
WALL_START <- Sys.time()

spec <- function(mdl, label = NULL) list(mdl = mdl, label = label %||% mdl@epidemic@name)
delays_for <- function(disease) {
  if (MODELS_SET == "fast") return(list(lognormal_delay(), dirichlet_delay()))
  if (disease == "covid") list(lognormal_delay(), generalized_gamma_delay(), dirichlet_delay())
  else list(lognormal_delay(), gamma_delay(), generalized_gamma_delay(), dirichlet_delay())
}
model_list <- function(disease, N_pop) {
  epis <- list(hsgp_epidemic(), ar1_epidemic(), sir_epidemic(N_pop = N_pop))
  out <- list(); for (ep in epis) for (dl in delays_for(disease))
    out[[length(out) + 1]] <- spec(model(nb_likelihood(), ep, dl)); out
}
run_silent <- function(expr) {
  o <- file(nullfile(), open = "wt"); sink(o, type = "output"); on.exit({sink(NULL); close(o)}, add = TRUE)
  withCallingHandlers(suppressWarnings(suppressMessages(expr)),
                      message = function(m) invokeRestart("muffleMessage"))
}

# X builders
X_fourier4 <- function(max_time) { tvec <- seq_len(max_time)
  do.call(cbind, lapply(1:4, function(k) cbind(sin(2*pi*k*tvec/52), cos(2*pi*k*tvec/52)))) }
X_dow <- function(max_time) { dow <- ((seq_len(max_time) - 1) %% 7) + 1
  matrix(as.numeric(model.matrix(~ factor(dow) - 1)[, -7, drop = FALSE]), nrow = max_time) }

# ── Generic per-disease runner (parallel over dates) ──────────────────────────
run_disease <- function(disease, tbl, event_col, dates, models, unit_fn, X_builder, max_time_fun) {
  cli::cli_h1(toupper(disease))
  min_date  <- tbl |> as_tibble() |> summarise(m = min(!!as.symbol(event_col))) |> pull(m)
  truth_tbl <- tbl %>% get_latest_reported_cases()

  res <- foreach(k = seq_along(dates), .options.future = list(seed = TRUE)) %dofuture% {
    run_silent({
      date_run <- dates[k]
      model_tbl <- suppressWarnings(tbl %>%
        filter(!!as.symbol(event_col) <= date_run, !!as.symbol(get_report_date(tbl)) <= date_run))
      max_time <- max_time_fun(date_run, min_date)
      if (!is.finite(max_time) || max_time < 5) return(NULL)
      d_star <- matrix(rev(seq_len(max_time)) - 1L, ncol = 1L)
      m <- model_tbl %>% mutate(.delay = .delay + 1L, .event_num = .event_num + 1L) %>%
        as_tibble() %>% dplyr::select(.event_num, n, .delay) %>%
        arrange(.event_num, .delay) %>% as.matrix() %>% cbind(1L)
      if (nrow(m) == 0) return(NULL)
      X <- X_builder(max_time)
      reported_truth <- truth_tbl %>% filter(!!as.symbol(event_col) <= date_run) %>%
        as_tibble() %>% dplyr::select(.event_num, final = n)
      grid <- tibble(.event_num = 0:(max_time - 1L)) %>%
        mutate(!!as.symbol(event_col) := min_date + unit_fn(.event_num))

      rows <- list(); tim <- list()
      for (sp in models) {
        mdl <- sp$mdl; epi_lbl <- sp$label
        t0 <- Sys.time()
        ms <- tryCatch(nowcast_twostage(mdl, m, X = X, max_time = max_time, d_star = d_star,
                         K = K_MULTISAMPLE, n_draws_per = N_DRAWS_PER, phi = PHI_PRIOR, seed = 23875L),
                       error = function(e) NULL)
        el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        tim[[length(tim)+1]] <- tibble(disease = disease,
          model = paste0(epi_lbl, "_", mdl@likelihood@name, "_", mdl@delay@name),
          date_run = date_run, max_time = max_time, n_rows = nrow(m), elapsed_s = el,
          rung = ms$rung %||% NA_character_, n_samp = ms$n_samp %||% NA_integer_,
          per_fit_s = el / max(1L, (ms$n_samp %||% 1L) + 2L))
        if (is.null(ms) || is.null(ms$nowcast)) next
        rows[[length(rows)+1]] <- ms$nowcast %>%
          mutate(epidemic_label = epi_lbl, nb_label = mdl@likelihood@name,
                 delay_label = mdl@delay@name, date_run = date_run) %>%
          left_join(grid, by = ".event_num") %>%
          left_join(reported_truth, by = ".event_num") %>%
          mutate(final = replace_na(final, 0)) %>%
          filter(!is.na(!!as.symbol(event_col)))
      }
      list(rows = bind_rows(rows), timing = bind_rows(tim))
    })
  }

  rows_all <- bind_rows(map(res, "rows"));  tim_all <- bind_rows(map(res, "timing"))
  if (nrow(rows_all) == 0) { cli::cli_warn("{disease}: no fits."); return(tim_all) }
  all_steps <- rows_all %>% mutate(t = .event_num) %>% filter(!is.na(mean))
  list(all_steps = all_steps, dates = dates, event_date_col = event_col) %>%
    write_rds(file.path(OUT, "results", paste0(disease, "_diseasenowcasting_steps.rds")))
  cli::cli_alert_success("{disease}: {nrow(all_steps)} rows -> {disease}_diseasenowcasting_steps.rds")
  tim_all
}

TIMING <- list()

if (RUN_DENGUE) {
  data(denguedat)
  tbl_dengue <- denguedat %>%
    tbl_now(event_date = onset_week, report_date = report_week, data_type = "linelist") |>
    to_count("count-incidence") %>%
    add_temporal_effects(temporal_effects(seasons = 52)) |> compute_temporal_effects()
  ec <- get_event_date(tbl_dengue)
  d0 <- tbl_dengue |> pull(ec) |> min(); d1 <- tbl_dengue |> pull(ec) |> max()
  dts <- seq(d0, d1, by = paste0("1 ", get_event_units(tbl_dengue)))
  set.seed(DENGUE_DATE_SEED); dts <- dts[2:(length(dts)-1)] |> base::sample(size = N_DATES) |> sort(decreasing = TRUE)
  TIMING[["dengue"]] <- run_disease("dengue", tbl_dengue, ec, dts, model_list("dengue", 5e6),
    function(x) weeks(x), X_fourier4, function(dr, md) as.numeric(difftime(dr, md, units = "weeks")) + 1L)
}
if (RUN_COVID) {
  COVID_RDS <- Sys.getenv("COVID_RDS",
    "/Users/rodzepeda/Documents/diseasenowcast2/devel/covid_colombia_aggregated.rds")
  tbl_covid <- read_rds(COVID_RDS) %>%
    group_by(notification_date, diagnosis_date) %>% summarise(n = sum(n), .groups = "drop") %>%
    tbl_now(event_date = notification_date, report_date = diagnosis_date, case_count = n,
            data_type = "count-incidence") %>%
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |> compute_temporal_effects()
  ec <- get_event_date(tbl_covid); d0 <- ymd("2020-03-09"); d1 <- d0 + years(2)
  dts <- seq(d0, d1, by = paste0("1 ", get_event_units(tbl_covid)))
  set.seed(COVID_DATE_SEED); dts <- dts[2:(length(dts)-1)] |> base::sample(size = N_DATES) |> sort(decreasing = TRUE)
  TIMING[["covid"]] <- run_disease("covid", tbl_covid, ec, dts, model_list("covid", 5e6),
    function(x) days(x), X_dow, function(dr, md) as.numeric(dr - md + 1))
}
if (RUN_MPOX) {
  data(mpoxdat)
  tbl_mpox <- mpoxdat %>%
    tbl_now(event_date = dx_date, report_date = dx_report_date, data_type = "linelist") |>
    to_count("count-incidence") %>%
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |> compute_temporal_effects()
  ec <- get_event_date(tbl_mpox); d0 <- ymd("2022-07-12"); d1 <- tbl_mpox |> pull(ec) |> max()
  dts <- seq(d0, d1, by = paste0("1 ", get_event_units(tbl_mpox)))
  set.seed(MPOX_DATE_SEED); dts <- dts[2:(length(dts)-1)] |> base::sample(size = N_DATES) |> sort(decreasing = TRUE)
  TIMING[["mpox"]] <- run_disease("mpox", tbl_mpox, ec, dts, model_list("mpox", 5e6),
    function(x) days(x), X_dow, function(dr, md) as.numeric(dr - md + 1))
}

timing <- bind_rows(TIMING)
write_rds(timing, file.path(OUT, "results", "diseasenowcasting_timing.rds"))
if (nrow(timing) > 0) {
  wall_min <- as.numeric(difftime(Sys.time(), WALL_START, units = "mins"))
  cpu_min  <- sum(timing$elapsed_s) / 60
  cli::cli_h1("TIMING")
  cat(sprintf("fits=%d | ACTUAL wall-clock=%.1f min (%d workers) | cumulative compute=%.1f min | parallel speedup=%.1fx\n",
              nrow(timing), wall_min, TUNE_WORKERS, cpu_min, cpu_min / max(wall_min, 1e-6)))
  cat(sprintf("per (model,date) call: median=%.1fs mean=%.1fs (mean>median => slow SIR/GG/long-series tail)\n",
              median(timing$elapsed_s), mean(timing$elapsed_s)))
  cat(sprintf("per inner RTMB fit: median=%.2fs\n", median(timing$per_fit_s, na.rm = TRUE)))
  print(timing %>% group_by(disease, model) %>%
        summarise(n = n(), med_call_s = round(median(elapsed_s),1), .groups="drop") %>%
        arrange(desc(med_call_s)) %>% head(8) %>% as.data.frame())
}
cli::cli_alert_success("DONE")
