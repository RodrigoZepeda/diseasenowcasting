#!/usr/bin/env Rscript

# Checkpointed stress test over every FluSight state-level series and US.
# This is a robustness run, not a package test or final comparative evaluation.

suppressPackageStartupMessages({
  library(dplyr)
  library(tbl.now)
  library(RTMB)
  pkgload::load_all(".", quiet = TRUE)
})
source("devel/skellam_prototypes/flusight_asof_data.R")
source("devel/skellam_prototypes/rtmb_identifiability_models.R")

epidemic <- Sys.getenv("EPIDEMIC", "ar")
delay_family <- Sys.getenv("DELAY", "lognormal")
H <- as.integer(Sys.getenv("SETTLEMENT_WEEKS", "26"))
n_draws <- as.integer(Sys.getenv("N_DRAWS", "250"))
target_max_age <- as.integer(Sys.getenv("TARGET_MAX_AGE", "4"))
seed <- as.integer(Sys.getenv("SEED", "20260904"))
set.seed(seed)

locations <- sort(unique(tbl.now::flusight$location_name))
location_env <- Sys.getenv("LOCATIONS", "")
if (nzchar(location_env))
  locations <- trimws(strsplit(location_env, ",", fixed = TRUE)[[1L]])
exclude <- trimws(strsplit(Sys.getenv("EXCLUDE_LOCATIONS", ""), ",",
                          fixed = TRUE)[[1L]])
exclude <- exclude[nzchar(exclude)]
locations <- setdiff(locations, exclude)

origins <- as.Date(c("2025-01-02", "2025-02-15", "2025-03-29",
                     "2025-04-26", "2025-05-31"))
origin_env <- Sys.getenv("NOWS", "")
if (nzchar(origin_env))
  origins <- as.Date(strsplit(origin_env, ",", fixed = TRUE)[[1L]])

clocks <- c("calendar", "compressed")
models <- strsplit(
  Sys.getenv("MODELS", "cumulative_poisson,cumulative_nb,hurdle_ztnb"),
  ",", fixed = TRUE
)[[1L]]
models <- intersect(trimws(models),
                    c("cumulative_poisson", "cumulative_nb", "hurdle_ztnb"))
if (!length(models)) stop("MODELS did not contain a supported model.")
cumulative_random <- tolower(Sys.getenv("CUMULATIVE_RANDOM", "true")) %in%
  c("true", "1", "yes")
strategy_tag <- if (cumulative_random) "laplace_cumulative" else "map"
result_dir <- file.path("devel/skellam_prototypes/results",
                        paste0("all_locations_H", H))
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
checkpoint_path <- file.path(
  result_dir,
  paste0("checkpoint_", epidemic, "_", delay_family, "_H", H, "_",
         strategy_tag, "_", paste(models, collapse = "-") , ".rds")
)

empty_checkpoint <- function() {
  list(
    config = list(epidemic = epidemic, delay = delay_family, H = H,
                  origins = origins, clocks = clocks, models = models,
                  locations = locations, seed = seed,
                  cumulative_random = cumulative_random),
    completed_jobs = character(),
    diagnostics = list(),
    scores = list(),
    failures = list(),
    started_at = Sys.time(),
    updated_at = Sys.time()
  )
}

checkpoint <- if (file.exists(checkpoint_path)) readRDS(checkpoint_path) else
  empty_checkpoint()
config_fields <- c("epidemic", "delay", "H", "origins", "clocks", "models",
                   "locations", "seed", "cumulative_random")
if (!identical(checkpoint$config[config_fields],
               empty_checkpoint()$config[config_fields])) {
  stop("Existing checkpoint configuration differs. Move it or choose a new configuration.")
}

save_checkpoint <- function() {
  checkpoint$updated_at <<- Sys.time()
  saveRDS(checkpoint, checkpoint_path)
}

levels <- c(0.05, seq(0.1, 0.9, 0.1), 0.95)
quantile_score <- function(q, y, probs) {
  2 * mean(ifelse(y >= q, probs * (y - q), (1 - probs) * (q - y)))
}
score_draws <- function(draws, truth) {
  q <- stats::quantile(draws, levels, names = FALSE, type = 8)
  data.frame(
    median = stats::median(draws),
    wis = quantile_score(q, truth, levels),
    absolute_error = abs(stats::median(draws) - truth),
    coverage90 = truth >= q[1L] && truth <= q[length(q)]
  )
}

cat(sprintf("Preparing %d FluSight locations through H=%d...\n",
            length(locations), H))
prepared <- prepare_flusight_asof(
  locations, start = as.Date("2023-09-02"), settlement_horizon = H
)

jobs <- tidyr::expand_grid(
  location = locations,
  origin = origins,
  clock = clocks
) |>
  mutate(job_id = paste(.data$location, .data$origin, .data$clock, sep = "|"))
remaining <- jobs |>
  filter(!.data$job_id %in% checkpoint$completed_jobs)
total_jobs <- nrow(jobs)
run_started_elapsed <- proc.time()[["elapsed"]]
initial_done <- total_jobs - nrow(remaining)

cat(sprintf("Starting/resuming %d jobs (%d already complete), %d model fits.\n",
            nrow(remaining), initial_done, nrow(remaining) * length(models)))

for (job_index in seq_len(nrow(remaining))) {
  job <- remaining[job_index, ]
  origin <- as.Date(job$origin, origin = "1970-01-01")
  job_started_elapsed <- proc.time()[["elapsed"]]
  panel <- tryCatch(
    asof_panel(prepared, job$location, origin, job$clock),
    error = identity
  )

  if (inherits(panel, "error")) {
    checkpoint$failures[[length(checkpoint$failures) + 1L]] <- data.frame(
      location = job$location, origin = origin, clock = job$clock,
      model = NA_character_, stage = "asof_panel",
      error = conditionMessage(panel)
    )
  } else {
    leakage_ok <- panel$latest_report_date <= origin &&
      max(panel$cells$event_date_actual) <= origin &&
      max(panel$cells$report_date_actual) <= origin
    data <- make_identifiability_data(panel, epidemic)
    targets <- panel$cells |>
      group_by(.data$event_num, .data$event_date_actual) |>
      slice_max(.data$report_num, n = 1L, with_ties = FALSE) |>
      ungroup() |>
      filter(.data$delay <= target_max_age) |>
      inner_join(
        prepared$truth |>
          filter(.data$location_name == job$location) |>
          select("event_date_actual", "event_year", "truth"),
        by = "event_date_actual"
      )

    # One empirical score per target and job.
    if (nrow(targets)) {
      for (target_index in seq_len(nrow(targets))) {
        target <- targets[target_index, ]
        baseline <- empirical_multiplier_draws(
          panel, target$event_num, target$delay, target$cumulative, n_draws
        )
        checkpoint$scores[[length(checkpoint$scores) + 1L]] <- cbind(
          data.frame(location = job$location, origin = origin,
                     clock = job$clock, model = "empirical_multiplier",
                     event_date = target$event_date_actual,
                     event_year = target$event_year, age = target$delay,
                     current = target$cumulative, truth = target$truth),
          score_draws(baseline, target$truth)
        )
      }
    }

    for (model in models) {
      fit <- tryCatch({
        build_identifiability_model(
          data, model, delay_family, delay_family,
          use_random = cumulative_random &&
            model %in% c("cumulative_poisson", "cumulative_nb")
        ) |>
          fit_identifiability_model()
      }, error = identity)

      if (inherits(fit, "error")) {
        checkpoint$failures[[length(checkpoint$failures) + 1L]] <- data.frame(
          location = job$location, origin = origin, clock = job$clock,
          model = model, stage = "fit", error = conditionMessage(fit)
        )
        next
      }

      finite_fit <- is.finite(fit$fit$objective) &&
        is.finite(fit$max_gradient) &&
        all(is.finite(fit$fit$par)) &&
        all(is.finite(fit$components$mu)) &&
        all(is.finite(fit$components$q_C))
      checkpoint$diagnostics[[length(checkpoint$diagnostics) + 1L]] <- data.frame(
        location = job$location, origin = origin, clock = job$clock,
        model = model, nll = fit$fit$objective,
        convergence = fit$fit$convergence,
        max_gradient = fit$max_gradient,
        finite_fit = finite_fit,
        leakage_ok = leakage_ok,
        retract_mass = plogis(fit$par_list$retract_mass_raw),
        q_terminal = fit$components$terminal_retention,
        n_cells = length(data$cumulative)
      )

      if (!finite_fit || !nrow(targets)) next
      reconstruction <- switch(
        model,
        cumulative_poisson = "anchored",
        cumulative_nb = "direct",
        hurdle_ztnb = "anchored"
      )
      for (target_index in seq_len(nrow(targets))) {
        target <- targets[target_index, ]
        draws <- tryCatch(
          simulate_terminal_count(
            fit, target$event_num, target$cumulative, target$delay,
            n_draws, reconstruction,
            previous_nonzero = target$previous_nonzero
          ),
          error = identity
        )
        if (inherits(draws, "error") || is.null(draws) ||
            any(!is.finite(draws))) {
          checkpoint$failures[[length(checkpoint$failures) + 1L]] <- data.frame(
            location = job$location, origin = origin, clock = job$clock,
            model = model, stage = "reconstruction",
            error = if (inherits(draws, "error")) conditionMessage(draws) else
              "Non-finite or missing draws"
          )
          next
        }
        checkpoint$scores[[length(checkpoint$scores) + 1L]] <- cbind(
          data.frame(location = job$location, origin = origin,
                     clock = job$clock,
                     model = paste(model, reconstruction, sep = "_"),
                     event_date = target$event_date_actual,
                     event_year = target$event_year, age = target$delay,
                     current = target$cumulative, truth = target$truth),
          score_draws(draws, target$truth)
        )
      }
    }
  }

  checkpoint$completed_jobs <- unique(c(checkpoint$completed_jobs, job$job_id))
  save_checkpoint()

  done <- initial_done + job_index
  elapsed <- proc.time()[["elapsed"]] - run_started_elapsed
  jobs_this_run <- job_index
  seconds_per_job <- elapsed / jobs_this_run
  eta_seconds <- seconds_per_job * (total_jobs - done)
  cat(sprintf(
    "[%d/%d] %s | %s | %s: %.1fs; ETA %.1f min; failures=%d\n",
    done, total_jobs, job$location, as.character(origin), job$clock,
    proc.time()[["elapsed"]] - job_started_elapsed,
    eta_seconds / 60, length(checkpoint$failures)
  ))
  flush.console()
}

diagnostics <- bind_rows(checkpoint$diagnostics)
scores <- bind_rows(checkpoint$scores)
failures <- bind_rows(checkpoint$failures)
summary <- diagnostics |>
  group_by(.data$clock, .data$model) |>
  summarise(
    fits = n(), finite = sum(.data$finite_fit),
    optimizer_converged = sum(.data$convergence == 0),
    gradient_below_01 = sum(.data$max_gradient < 0.1),
    leakage_passed = sum(.data$leakage_ok),
    median_gradient = median(.data$max_gradient),
    max_gradient = max(.data$max_gradient),
    .groups = "drop"
  )
score_summary <- scores |>
  group_by(.data$clock, .data$model) |>
  summarise(n = n(), wis = mean(.data$wis),
            mae = mean(.data$absolute_error),
            coverage90 = mean(.data$coverage90), .groups = "drop")

final_path <- file.path(
  result_dir,
  paste0("all_locations_", epidemic, "_", delay_family, "_H", H, "_",
         strategy_tag, "_", paste(models, collapse = "-"), ".rds")
)
saveRDS(list(summary = summary, score_summary = score_summary,
             diagnostics = diagnostics, scores = scores, failures = failures,
             config = checkpoint$config), final_path)
utils::write.csv(diagnostics,
                 sub("\\.rds$", "_diagnostics.csv", final_path),
                 row.names = FALSE)
utils::write.csv(failures,
                 sub("\\.rds$", "_failures.csv", final_path),
                 row.names = FALSE)

cat("\nStability summary:\n")
print(summary)
cat("\nScore summary:\n")
print(score_summary)
cat(sprintf("\nFailures: %d\nSaved %s\n", nrow(failures), final_path))
