#!/usr/bin/env Rscript

# Reproduce every prediction failure recorded by the full count-cumulative
# integration checkpoint. This is a targeted verification step before the full
# runner is resumed; it does not modify the checkpoint.

suppressPackageStartupMessages({
  library(dplyr)
  library(tbl.now)
  pkgload::load_all(".", quiet = TRUE)
})
source("devel/skellam_prototypes/flusight_asof_data.R")

checkpoint_path <- file.path(
  "devel", "count_cumulative_integration_results",
  paste0(
    "checkpoint_loc53_orig5_clockcal-com_seed20260905_H26_draws250_",
    "cumulative_poisson-cumulative_nb-hurdle_ztnb-hurdle_ztpoisson.rds"
  )
)
checkpoint <- readRDS(checkpoint_path)
failures <- dplyr::bind_rows(checkpoint$failures) |>
  filter(.data$stage == "prediction") |>
  distinct(.data$location, .data$origin, .data$clock, .data$model)
if (!nrow(failures)) stop("The checkpoint contains no prediction failures to retry.")

H <- as.integer(checkpoint$config$H)
n_draws <- as.integer(checkpoint$config$n_draws)
seed <- as.integer(checkpoint$config$seed)
all_jobs <- tidyr::expand_grid(
  location = checkpoint$config$locations,
  origin = checkpoint$config$origins,
  clock = checkpoint$config$clocks
)

prepared <- prepare_flusight_asof(
  unique(failures$location), start = as.Date("2023-09-02"),
  settlement_horizon = H
)

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
  model(
    likelihood, ar1_epidemic(), lognormal_delay(),
    count_cumulative = count_cumulative_process(
      observation = observation,
      retraction_delay = lognormal_delay(), settlement = H,
      movement_previous = 0
    )
  )
}

results <- vector("list", nrow(failures))
for (failure_index in seq_len(nrow(failures))) {
  failed <- failures[failure_index, ]
  origin <- as.Date(failed$origin, origin = "1970-01-01")
  original_job_index <- which(
    all_jobs$location == failed$location &
      all_jobs$origin == origin &
      all_jobs$clock == failed$clock
  )
  warnings <- character()
  started <- proc.time()[["elapsed"]]
  result <- tryCatch(withCallingHandlers({
    panel <- asof_panel(prepared, failed$location, origin, failed$clock)
    fitted <- nowcast(
      make_production_tbl(panel), make_specification(failed$model),
      type = "one_stage", temporal_effects = "none", n_draws = n_draws,
      seed = seed + original_job_index
    )
    prediction <- predict(
      fitted, n_draws = n_draws,
      seed = seed + 100000L + original_job_index
    )
    list(
      passed = all(is.finite(prediction@draws)),
      convergence = fitted@fits[[1L]]$convergence,
      max_gradient = fitted@fits[[1L]]$max_gradient,
      projections = prediction@negative_projection_count,
      error = ""
    )
  }, warning = function(condition) {
    warnings <<- unique(c(warnings, conditionMessage(condition)))
    invokeRestart("muffleWarning")
  }), error = function(condition) list(
    passed = FALSE, convergence = NA_integer_, max_gradient = NA_real_,
    projections = NA_integer_, error = conditionMessage(condition)
  ))
  results[[failure_index]] <- data.frame(
    location = failed$location, origin = origin, clock = failed$clock,
    model = failed$model, original_job_index = original_job_index,
    passed = result$passed, convergence = result$convergence,
    max_gradient = result$max_gradient, projections = result$projections,
    elapsed_seconds = proc.time()[["elapsed"]] - started,
    warnings = paste(warnings, collapse = " | "), error = result$error
  )
  cat(sprintf(
    "[%d/%d] %s | %s | %s | %s: %s\n",
    failure_index, nrow(failures), failed$location, origin, failed$clock,
    failed$model, if (result$passed) "PASS" else "FAIL"
  ))
  flush.console()
}

results <- dplyr::bind_rows(results)
output <- file.path(
  "devel", "count_cumulative_integration_results",
  "retry_prediction_failures_after_log_pmf_fix.csv"
)
utils::write.csv(results, output, row.names = FALSE)
cat(sprintf(
  "Retried %d failures: %d passed, %d failed. Wrote %s\n",
  nrow(results), sum(results$passed), sum(!results$passed), output
))
if (any(!results$passed)) stop("At least one targeted prediction retry failed.")

