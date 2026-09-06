#!/usr/bin/env Rscript

# Checkpointed production-API integration gate for count-cumulative models.
#
# The default run crosses every FluSight state-level series plus the US, five
# historical origins, both calendar and compressed publication clocks, and all
# four count-cumulative variants.  Environment variables make small diagnostic
# runs possible without changing the scientific defaults, for example:
#
#   LOCATIONS=Texas NOWS=2025-05-31 N_DRAWS=40 \
#     Rscript devel/run_count_cumulative_integration_gate.R
#
# This script deliberately reuses only the vetted as-of/clock construction from
# the prototype directory.  Every fit and prediction below goes through the
# installed package implementation via model(), nowcast(), and predict().

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
set.seed(seed)

locations <- sort(unique(tbl.now::flusight$location_name))
location_env <- Sys.getenv("LOCATIONS", "")
if (nzchar(location_env))
  locations <- trimws(strsplit(location_env, ",", fixed = TRUE)[[1L]])
exclude <- trimws(strsplit(Sys.getenv("EXCLUDE_LOCATIONS", ""), ",",
                          fixed = TRUE)[[1L]])
locations <- setdiff(locations, exclude[nzchar(exclude)])

origins <- as.Date(c("2025-01-02", "2025-02-15", "2025-03-29",
                     "2025-04-26", "2025-05-31"))
origin_env <- Sys.getenv("NOWS", "")
if (nzchar(origin_env))
  origins <- as.Date(strsplit(origin_env, ",", fixed = TRUE)[[1L]])

clocks <- c("calendar", "compressed")
clock_env <- Sys.getenv("CLOCKS", "")
if (nzchar(clock_env))
  clocks <- intersect(trimws(strsplit(clock_env, ",", fixed = TRUE)[[1L]]),
                      clocks)

supported_models <- c("cumulative_poisson", "cumulative_nb",
                      "hurdle_ztnb", "hurdle_ztpoisson")
models <- trimws(strsplit(
  Sys.getenv("MODELS", paste(supported_models, collapse = ",")),
  ",", fixed = TRUE
)[[1L]])
models <- intersect(models, supported_models)
if (!length(locations)) stop("No locations selected.")
if (!length(origins) || anyNA(origins)) stop("NOWS must contain valid dates.")
if (!length(clocks)) stop("CLOCKS did not contain calendar or compressed.")
if (!length(models)) stop("MODELS did not contain a supported model.")

config_tag <- paste(models, collapse = "-")
scope_tag <- Sys.getenv(
  "RUN_TAG",
  paste0("loc", length(locations), "_orig", length(origins), "_clock",
         paste(substr(clocks, 1L, 3L), collapse = "-"), "_seed", seed)
)
scope_tag <- gsub("[^A-Za-z0-9_.-]", "-", scope_tag)
result_dir <- file.path("devel", "count_cumulative_integration_results")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
checkpoint_path <- file.path(
  result_dir,
  paste0("checkpoint_", scope_tag, "_H", H, "_draws", n_draws, "_",
         config_tag, ".rds")
)

new_checkpoint <- function() {
  list(
    config = list(H = H, n_draws = n_draws, target_max_age = target_max_age,
                  seed = seed, locations = locations, origins = origins,
                  clocks = clocks, models = models),
    completed_jobs = character(), diagnostics = list(), scores = list(),
    failures = list(), started_at = Sys.time(), updated_at = Sys.time()
  )
}
checkpoint <- if (file.exists(checkpoint_path)) readRDS(checkpoint_path) else
  new_checkpoint()
if (!identical(checkpoint$config, new_checkpoint()$config)) {
  stop("Existing checkpoint configuration differs; move it or change the run configuration.")
}

# After a diagnosed prediction bug has been fixed and every recorded failure
# has passed the targeted retry script, discard the affected jobs' partial
# outputs and run those jobs again in full. The original checkpoint is copied
# once before mutation so this repair is recoverable and auditable.
retry_verified_jobs <- tolower(Sys.getenv(
  "RETRY_VERIFIED_FAILURE_JOBS", "false"
)) %in% c("true", "1", "yes")
if (retry_verified_jobs && length(checkpoint$failures)) {
  retry_path <- file.path(
    result_dir, "retry_prediction_failures_after_log_pmf_fix.csv"
  )
  if (!file.exists(retry_path))
    stop("Targeted retry results are missing: ", retry_path)
  retry_results <- utils::read.csv(retry_path, stringsAsFactors = FALSE)
  if (!nrow(retry_results) || !all(retry_results$passed))
    stop("Not every targeted prediction retry passed; checkpoint was not changed.")

  failure_rows <- dplyr::bind_rows(checkpoint$failures)
  retry_job_ids <- unique(paste(
    failure_rows$location, as.character(as.Date(failure_rows$origin,
                                                 origin = "1970-01-01")),
    failure_rows$clock, sep = "|"
  ))
  original_checkpoint <- paste0(checkpoint_path, ".before_log_pmf_retry")
  if (!file.exists(original_checkpoint) &&
      !file.copy(checkpoint_path, original_checkpoint)) {
    stop("Could not preserve the original checkpoint before retry cleanup.")
  }
  keep_rows_outside_retry <- function(rows) {
    if (!length(rows)) return(rows)
    Filter(function(row) {
      row_id <- paste(
        row$location,
        as.character(as.Date(row$origin, origin = "1970-01-01")),
        row$clock, sep = "|"
      )
      !row_id %in% retry_job_ids
    }, rows)
  }
  checkpoint$completed_jobs <- setdiff(
    checkpoint$completed_jobs, retry_job_ids
  )
  checkpoint$diagnostics <- keep_rows_outside_retry(checkpoint$diagnostics)
  checkpoint$scores <- keep_rows_outside_retry(checkpoint$scores)
  checkpoint$failures <- keep_rows_outside_retry(checkpoint$failures)
  checkpoint$updated_at <- Sys.time()
  saveRDS(checkpoint, checkpoint_path)
  cat(sprintf(
    "Verified fix: reset %d affected jobs; original checkpoint saved as %s.\n",
    length(retry_job_ids), original_checkpoint
  ))
}
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

make_specification <- function(model_name) {
  observation <- switch(
    model_name,
    cumulative_poisson = "cumulative",
    cumulative_nb = "cumulative",
    hurdle_ztnb = "hurdle_ztnb",
    hurdle_ztpoisson = "hurdle_ztpoisson"
  )
  likelihood <- if (identical(model_name, "cumulative_nb"))
    nb_likelihood() else poisson_likelihood()
  report_delay <- lognormal_delay()
  model(
    likelihood, ar1_epidemic(), report_delay,
    count_cumulative = count_cumulative_process(
      observation = observation,
      retraction_delay = lognormal_delay(),
      settlement = H,
      movement_previous = 0
    )
  )
}

probabilities <- c(0.05, seq(0.1, 0.9, 0.1), 0.95)
quantile_score <- function(quantiles, truth, probs) {
  2 * mean(ifelse(truth >= quantiles, probs * (truth - quantiles),
                  (1 - probs) * (quantiles - truth)))
}
score_draws <- function(draws, truth) {
  quantiles <- stats::quantile(draws, probabilities, names = FALSE, type = 8)
  data.frame(
    median = stats::median(draws),
    wis = quantile_score(quantiles, truth, probabilities),
    absolute_error = abs(stats::median(draws) - truth),
    coverage90 = truth >= quantiles[1L] &&
      truth <= quantiles[length(quantiles)]
  )
}

warning_messages <- character()
capture_warnings <- function(expression) {
  withCallingHandlers(
    expression,
    warning = function(condition) {
      warning_messages <<- unique(c(warning_messages, conditionMessage(condition)))
      invokeRestart("muffleWarning")
    }
  )
}

cat(sprintf("Preparing %d FluSight locations through H=%d...\n",
            length(locations), H))
prepared <- prepare_flusight_asof(
  locations, start = as.Date("2023-09-02"), settlement_horizon = H
)

jobs <- tidyr::expand_grid(location = locations, origin = origins,
                           clock = clocks) |>
  mutate(job_id = paste(.data$location, .data$origin, .data$clock, sep = "|"))
remaining <- jobs |> filter(!.data$job_id %in% checkpoint$completed_jobs)
total_jobs <- nrow(jobs)
initial_done <- total_jobs - nrow(remaining)
run_started <- proc.time()[["elapsed"]]
cat(sprintf("Starting/resuming %d jobs (%d complete), %d production fits.\n",
            nrow(remaining), initial_done, nrow(remaining) * length(models)))

for (job_index in seq_len(nrow(remaining))) {
  job <- remaining[job_index, ]
  origin <- as.Date(job$origin, origin = "1970-01-01")
  job_started <- proc.time()[["elapsed"]]
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
    production_tbl <- make_production_tbl(panel)
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

    for (model_name in models) {
      warning_messages <- character()
      specification <- make_specification(model_name)
      fit_started <- proc.time()[["elapsed"]]
      fitted <- tryCatch(
        capture_warnings(nowcast(
          production_tbl, specification, type = "one_stage",
          temporal_effects = "none", n_draws = n_draws,
          seed = seed + job_index
        )),
        error = identity
      )
      if (inherits(fitted, "error")) {
        checkpoint$failures[[length(checkpoint$failures) + 1L]] <- data.frame(
          location = job$location, origin = origin, clock = job$clock,
          model = model_name, stage = "fit",
          error = conditionMessage(fitted)
        )
        next
      }

      production_fit <- fitted@fits[[1L]]
      cumulative <- production_fit$reconstruct$count_cumulative
      finite_fit <- is.finite(production_fit$nll) &&
        is.finite(production_fit$max_gradient) &&
        all(is.finite(production_fit$par)) &&
        all(is.finite(production_fit$lambda)) &&
        all(is.finite(cumulative$q_C))
      prediction <- tryCatch(
        capture_warnings(predict(
          fitted, n_draws = n_draws,
          seed = seed + 100000L + job_index
        )),
        error = identity
      )
      finite_prediction <- !inherits(prediction, "error") &&
        all(is.finite(prediction@draws))
      checkpoint$diagnostics[[length(checkpoint$diagnostics) + 1L]] <- data.frame(
        location = job$location, origin = origin, clock = job$clock,
        model = model_name, nll = production_fit$nll,
        convergence = production_fit$convergence,
        max_gradient = production_fit$max_gradient,
        gradient_status = production_fit$gradient_status,
        finite_fit = finite_fit, finite_prediction = finite_prediction,
        leakage_ok = leakage_ok,
        use_random = isTRUE(production_fit$use_random),
        retraction_mass = cumulative$retraction_mass,
        q_C_min = min(cumulative$q_C),
        q_C_max = max(cumulative$q_C),
        q_terminal = cumulative$terminal_retention,
        reconstruction = if (inherits(prediction, "error")) NA_character_ else
          prediction@cumulative_reconstruction,
        negative_projection_count = if (inherits(prediction, "error")) NA_integer_ else
          prediction@negative_projection_count,
        n_cells = sum(fitted@engine$observation_mask),
        fit_seconds = proc.time()[["elapsed"]] - fit_started,
        warnings = paste(warning_messages, collapse = " | ")
      )
      if (!finite_prediction) {
        checkpoint$failures[[length(checkpoint$failures) + 1L]] <- data.frame(
          location = job$location, origin = origin, clock = job$clock,
          model = model_name, stage = "prediction",
          error = if (inherits(prediction, "error")) conditionMessage(prediction) else
            "Non-finite production prediction draws"
        )
        next
      }
      if (!nrow(targets)) next

      minimum_event <- min(panel$cells$event_num)
      for (target_index in seq_len(nrow(targets))) {
        target <- targets[target_index, ]
        prediction_column <- target$event_num - minimum_event + 1L
        if (prediction_column < 1L ||
            prediction_column > ncol(prediction@draws)) next
        draws <- prediction@draws[, prediction_column]
        checkpoint$scores[[length(checkpoint$scores) + 1L]] <- cbind(
          data.frame(location = job$location, origin = origin,
                     clock = job$clock, model = model_name,
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
  elapsed <- proc.time()[["elapsed"]] - run_started
  eta_seconds <- elapsed / job_index * (total_jobs - done)
  cat(sprintf(
    "[%d/%d] %s | %s | %s: %.1fs; ETA %.1f min; failures=%d\n",
    done, total_jobs, job$location, as.character(origin), job$clock,
    proc.time()[["elapsed"]] - job_started, eta_seconds / 60,
    length(checkpoint$failures)
  ))
  flush.console()
}

diagnostics <- dplyr::bind_rows(checkpoint$diagnostics)
scores <- dplyr::bind_rows(checkpoint$scores)
failures <- dplyr::bind_rows(checkpoint$failures)
summary <- diagnostics |>
  group_by(.data$clock, .data$model) |>
  summarise(
    fits = n(), finite = sum(.data$finite_fit & .data$finite_prediction),
    optimizer_converged = sum(.data$convergence == 0),
    gradient_below_01 = sum(.data$max_gradient <= 0.1),
    leakage_passed = sum(.data$leakage_ok),
    laplace_fits = sum(.data$use_random),
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
  paste0("production_", scope_tag, "_H", H, "_draws", n_draws, "_",
         config_tag, ".rds")
)
saveRDS(list(summary = summary, score_summary = score_summary,
             diagnostics = diagnostics, scores = scores, failures = failures,
             config = checkpoint$config), final_path)
utils::write.csv(diagnostics, sub("\\.rds$", "_diagnostics.csv", final_path),
                 row.names = FALSE)
utils::write.csv(scores, sub("\\.rds$", "_scores.csv", final_path),
                 row.names = FALSE)
utils::write.csv(failures, sub("\\.rds$", "_failures.csv", final_path),
                 row.names = FALSE)

cat("\nProduction stability summary:\n")
print(summary)
cat("\nScore summary:\n")
print(score_summary)
cat(sprintf("\nFailures: %d\nSaved %s\n", nrow(failures), final_path))

gate_failed <- nrow(failures) > 0L || any(
  !diagnostics$finite_fit |
    !diagnostics$finite_prediction |
    !diagnostics$leakage_ok |
    diagnostics$convergence != 0L |
    diagnostics$max_gradient > 0.1
)
if (gate_failed) {
  stop("Production count-cumulative integration gate failed; inspect the saved diagnostics.")
}
