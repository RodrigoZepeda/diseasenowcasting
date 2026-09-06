#!/usr/bin/env Rscript

# Checkpointed comparison of HSGP and SIR epidemic processes for the two
# signed-hurdle count-cumulative observation laws. AR(1) is intentionally not
# refit here: its matching calendar-time results are retained by
# render_count_cumulative_state_results.R and merged by the comparison renderer.

suppressPackageStartupMessages({
  library(dplyr)
  library(tbl.now)
  pkgload::load_all(".", quiet = TRUE)
})
source("devel/skellam_prototypes/flusight_asof_data.R")

H <- as.integer(Sys.getenv("SETTLEMENT_WEEKS", "26"))
n_draws <- as.integer(Sys.getenv("N_DRAWS", "250"))
target_max_age <- as.integer(Sys.getenv("TARGET_MAX_AGE", "4"))
seed <- as.integer(Sys.getenv("SEED", "20260905"))
if (!is.finite(H) || H < 1L) stop("SETTLEMENT_WEEKS must be positive.")
if (!is.finite(n_draws) || n_draws < 2L) stop("N_DRAWS must be at least 2.")

locations <- sort(unique(tbl.now::flusight$location_name))
location_env <- Sys.getenv("LOCATIONS", "")
if (nzchar(location_env))
  locations <- trimws(strsplit(location_env, ",", fixed = TRUE)[[1L]])
origins <- as.Date(c("2025-01-02", "2025-02-15", "2025-03-29",
                     "2025-04-26", "2025-05-31"))
origin_env <- Sys.getenv("NOWS", "")
if (nzchar(origin_env))
  origins <- as.Date(strsplit(origin_env, ",", fixed = TRUE)[[1L]])

supported_epidemics <- c("hsgp", "sir")
epidemics <- trimws(strsplit(
  Sys.getenv("EPIDEMICS", paste(supported_epidemics, collapse = ",")),
  ",", fixed = TRUE
)[[1L]])
epidemics <- intersect(epidemics, supported_epidemics)
models <- c("hurdle_ztnb", "hurdle_ztpoisson")
if (!length(locations) || !length(origins) || anyNA(origins) ||
    !length(epidemics)) stop("Invalid comparison scope.")

scope_tag <- Sys.getenv(
  "RUN_TAG",
  paste0("epidemic-comparison_loc", length(locations), "_orig", length(origins),
         "_calendar_seed", seed)
)
scope_tag <- gsub("[^A-Za-z0-9_.-]", "-", scope_tag)
result_dir <- file.path("devel", "count_cumulative_integration_results")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
checkpoint_path <- file.path(
  result_dir, paste0("checkpoint_", scope_tag, "_H", H, "_draws", n_draws,
                     "_", paste(epidemics, collapse = "-"), ".rds")
)

config <- list(
  H = H, n_draws = n_draws, target_max_age = target_max_age, seed = seed,
  locations = locations, origins = origins, clock = "calendar",
  models = models, epidemics = epidemics
)
new_checkpoint <- function() list(
  config = config, completed = character(), diagnostics = list(),
  scores = list(), predictions = list(), failures = list(),
  started_at = Sys.time(), updated_at = Sys.time()
)
checkpoint <- if (file.exists(checkpoint_path)) readRDS(checkpoint_path) else
  new_checkpoint()
if (!identical(checkpoint$config, config))
  stop("Existing checkpoint configuration differs from this run.")
save_checkpoint <- function() {
  checkpoint$updated_at <<- Sys.time()
  saveRDS(checkpoint, checkpoint_path)
}

make_production_tbl <- function(panel) {
  cells <- panel$cells |>
    transmute(
      event_model = as.Date("2000-01-01") + 7L * .data$event_num,
      report_model = as.Date("2000-01-01") + 7L * .data$report_num,
      cumulative = as.numeric(.data$cumulative)
    )
  tbl.now::tbl_now(
    cells, event_date = event_model, report_date = report_model,
    case_count = cumulative, data_type = "count-cumulative",
    event_units = "weeks", report_units = "weeks",
    now = as.Date("2000-01-01") + 7L * panel$latest_report_num,
    verbose = FALSE
  )
}

make_specification <- function(model_name, epidemic_name) {
  epidemic <- switch(
    epidemic_name,
    hsgp = hsgp_epidemic(),
    sir = sir_epidemic(),
    stop("Unsupported epidemic process: ", epidemic_name)
  )
  model(
    poisson_likelihood(), epidemic, lognormal_delay(),
    count_cumulative = count_cumulative_process(
      observation = model_name, retraction_delay = lognormal_delay(),
      settlement = H, movement_previous = 0
    )
  )
}

probabilities <- c(0.05, seq(0.1, 0.9, 0.1), 0.95)
score_draws <- function(draws, truth) {
  quantiles <- stats::quantile(draws, probabilities, names = FALSE, type = 8)
  quantile_score <- 2 * mean(ifelse(
    truth >= quantiles, probabilities * (truth - quantiles),
    (1 - probabilities) * (quantiles - truth)
  ))
  data.frame(
    median = stats::median(draws), wis = quantile_score,
    absolute_error = abs(stats::median(draws) - truth),
    coverage90 = truth >= quantiles[1L] && truth <= quantiles[length(quantiles)]
  )
}

cat(sprintf("Preparing %d locations through H=%d for calendar-time comparison...\n",
            length(locations), H))
prepared <- prepare_flusight_asof(
  locations, start = as.Date("2023-09-02"), settlement_horizon = H
)
jobs <- tidyr::expand_grid(location = locations, origin = origins) |>
  mutate(job_index = dplyr::row_number())
tasks <- tidyr::crossing(
  job_index = jobs$job_index, epidemic = epidemics, model = models
) |>
  left_join(jobs, by = "job_index") |>
  mutate(task_id = paste(.data$location, .data$origin, .data$epidemic,
                         .data$model, sep = "|")) |>
  filter(!.data$task_id %in% checkpoint$completed)
total_tasks <- nrow(jobs) * length(epidemics) * length(models)
initial_done <- total_tasks - nrow(tasks)
run_started <- proc.time()[["elapsed"]]
cat(sprintf("Starting/resuming %d tasks (%d complete; %d total).\n",
            nrow(tasks), initial_done, total_tasks))

for (task_index in seq_len(nrow(tasks))) {
  task <- tasks[task_index, ]
  origin <- as.Date(task$origin, origin = "1970-01-01")
  warning_messages <- character()
  started <- proc.time()[["elapsed"]]
  result <- tryCatch(withCallingHandlers({
    panel <- asof_panel(prepared, task$location, origin, "calendar")
    leakage_ok <- panel$latest_report_date <= origin &&
      max(panel$cells$event_date_actual) <= origin &&
      max(panel$cells$report_date_actual) <= origin
    targets <- panel$cells |>
      group_by(.data$event_num, .data$event_date_actual) |>
      slice_max(.data$report_num, n = 1L, with_ties = FALSE) |>
      ungroup() |>
      filter(.data$delay <= target_max_age) |>
      inner_join(
        prepared$truth |>
          filter(.data$location_name == task$location) |>
          select("event_date_actual", "event_year", "truth"),
        by = "event_date_actual"
      ) |>
      select("event_num", "event_date_actual", "event_year", "delay",
             current = "cumulative", "truth")
    fitted <- nowcast(
      make_production_tbl(panel),
      make_specification(task$model, task$epidemic),
      type = "one_stage", temporal_effects = "none", n_draws = n_draws,
      seed = seed + task$job_index
    )
    production_fit <- fitted@fits[[1L]]
    cumulative <- production_fit$reconstruct$count_cumulative
    prediction <- predict(
      fitted, n_draws = n_draws,
      seed = seed + 100000L + task$job_index
    )
    if (any(!is.finite(prediction@draws)))
      stop("Non-finite production prediction draws.")
    tidied <- tbl.now::tidy(prediction, level = 0.90) |>
      mutate(event_num = as.integer(round(
        as.numeric(.data$event_date - as.Date("2000-01-01")) / 7
      ))) |>
      inner_join(targets, by = "event_num") |>
      transmute(
        location = task$location, origin = origin, clock = "calendar",
        epidemic = task$epidemic, model = task$model,
        event_date = .data$event_date_actual, event_year = .data$event_year,
        age = .data$delay, current = .data$current, truth = .data$truth,
        estimate = .data$estimate, lower90 = .data$conf.low,
        upper90 = .data$conf.high, level = .data$level
      )
    if (!nrow(tidied) || any(!is.finite(tidied$estimate)) ||
        any(!is.finite(tidied$lower90)) || any(!is.finite(tidied$upper90)))
      stop("Tidy prediction intervals were empty or non-finite.")

    minimum_event <- min(panel$cells$event_num)
    score_rows <- lapply(seq_len(nrow(targets)), function(i) {
      target <- targets[i, ]
      column <- target$event_num - minimum_event + 1L
      if (column < 1L || column > ncol(prediction@draws)) return(NULL)
      cbind(
        data.frame(
          location = task$location, origin = origin, clock = "calendar",
          epidemic = task$epidemic, model = task$model,
          event_date = target$event_date_actual, event_year = target$event_year,
          age = target$delay, current = target$current, truth = target$truth
        ),
        score_draws(prediction@draws[, column], target$truth)
      )
    })
    finite_fit <- is.finite(production_fit$nll) &&
      is.finite(production_fit$max_gradient) &&
      all(is.finite(production_fit$par)) &&
      all(is.finite(production_fit$lambda)) &&
      all(is.finite(cumulative$q_C))
    diagnostic <- data.frame(
      location = task$location, origin = origin, clock = "calendar",
      epidemic = task$epidemic, model = task$model,
      nll = production_fit$nll, convergence = production_fit$convergence,
      max_gradient = production_fit$max_gradient,
      gradient_status = production_fit$gradient_status,
      finite_fit = finite_fit, finite_prediction = TRUE,
      leakage_ok = leakage_ok, use_random = isTRUE(production_fit$use_random),
      fit_seconds = proc.time()[["elapsed"]] - started
    )
    list(prediction = tidied, scores = bind_rows(score_rows),
         diagnostic = diagnostic)
  }, warning = function(condition) {
    warning_messages <<- unique(c(warning_messages, conditionMessage(condition)))
    invokeRestart("muffleWarning")
  }), error = identity)

  if (inherits(result, "error")) {
    checkpoint$failures[[length(checkpoint$failures) + 1L]] <- data.frame(
      location = task$location, origin = origin, clock = "calendar",
      epidemic = task$epidemic, model = task$model,
      error = conditionMessage(result),
      warnings = paste(warning_messages, collapse = " | ")
    )
  } else {
    result$prediction$warnings <- paste(warning_messages, collapse = " | ")
    result$diagnostic$warnings <- paste(warning_messages, collapse = " | ")
    checkpoint$predictions[[length(checkpoint$predictions) + 1L]] <-
      result$prediction
    checkpoint$scores[[length(checkpoint$scores) + 1L]] <- result$scores
    checkpoint$diagnostics[[length(checkpoint$diagnostics) + 1L]] <-
      result$diagnostic
  }
  checkpoint$completed <- unique(c(checkpoint$completed, task$task_id))
  save_checkpoint()

  done <- initial_done + task_index
  elapsed <- proc.time()[["elapsed"]] - run_started
  eta <- if (task_index) elapsed / task_index * (total_tasks - done) else NA_real_
  cat(sprintf(
    "[%d/%d] %s | %s | %s | %s: %.1fs; ETA %.1f min; failures=%d\n",
    done, total_tasks, task$location, origin, task$epidemic, task$model,
    proc.time()[["elapsed"]] - started, eta / 60, length(checkpoint$failures)
  ))
  flush.console()
}

diagnostics <- bind_rows(checkpoint$diagnostics)
scores <- bind_rows(checkpoint$scores)
predictions <- bind_rows(checkpoint$predictions)
failures <- bind_rows(checkpoint$failures)
summary <- diagnostics |>
  group_by(.data$epidemic, .data$model) |>
  summarise(
    fits = n(), finite = sum(.data$finite_fit & .data$finite_prediction),
    optimizer_converged = sum(.data$convergence == 0L),
    gradient_below_01 = sum(.data$max_gradient <= 0.1),
    leakage_passed = sum(.data$leakage_ok),
    median_gradient = median(.data$max_gradient),
    max_gradient = max(.data$max_gradient), .groups = "drop"
  )
score_summary <- scores |>
  group_by(.data$epidemic, .data$model) |>
  summarise(
    targets = n(), mean_wis = mean(.data$wis),
    mean_absolute_error = mean(.data$absolute_error),
    median_absolute_error = median(.data$absolute_error),
    median_signed_error = median(.data$median - .data$truth),
    coverage90 = mean(.data$coverage90), .groups = "drop"
  )
state_scores <- scores |>
  group_by(.data$location, .data$epidemic, .data$model) |>
  summarise(
    targets = n(), mean_wis = mean(.data$wis),
    mean_absolute_error = mean(.data$absolute_error),
    median_absolute_error = median(.data$absolute_error),
    median_signed_error = median(.data$median - .data$truth),
    coverage90 = mean(.data$coverage90), .groups = "drop"
  )

stem <- file.path(result_dir, paste0(scope_tag, "_H", H, "_draws", n_draws,
                                    "_", paste(epidemics, collapse = "-")))
saveRDS(list(
  summary = summary, score_summary = score_summary, state_scores = state_scores,
  diagnostics = diagnostics, scores = scores, predictions = predictions,
  failures = failures, config = config
), paste0(stem, ".rds"))
utils::write.csv(diagnostics, paste0(stem, "_diagnostics.csv"), row.names = FALSE)
utils::write.csv(scores, paste0(stem, "_scores.csv"), row.names = FALSE)
utils::write.csv(predictions, paste0(stem, "_predictions.csv"), row.names = FALSE)
utils::write.csv(state_scores, paste0(stem, "_state_scores.csv"), row.names = FALSE)
utils::write.csv(failures, paste0(stem, "_failures.csv"), row.names = FALSE)
cat("\nStability summary:\n")
print(summary)
cat("\nScore summary:\n")
print(score_summary)
cat(sprintf("\nFailures: %d\nSaved %s.rds\n", nrow(failures), stem))

gate_failed <- nrow(failures) > 0L || any(
  !diagnostics$finite_fit | !diagnostics$finite_prediction |
    !diagnostics$leakage_ok | diagnostics$convergence != 0L |
    diagnostics$max_gradient > 0.1
)
if (gate_failed)
  stop("Epidemic-process comparison gate failed; inspect saved diagnostics.")
