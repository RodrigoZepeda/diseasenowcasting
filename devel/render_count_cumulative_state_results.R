#!/usr/bin/env Rscript

# Refit the completed production-gate configurations to retain tidy 90%
# intervals, then render state-faceted diagnostic plots and per-state scores.
# The main stability runner intentionally stores compact scores rather than
# posterior draws, so this pass is checkpointed independently.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tbl.now)
  pkgload::load_all(".", quiet = TRUE)
})
source("devel/skellam_prototypes/flusight_asof_data.R")

result_dir <- file.path("devel", "count_cumulative_integration_results")
stem <- paste0(
  "production_loc53_orig5_clockcal-com_seed20260905_H26_draws250_",
  "cumulative_poisson-cumulative_nb-hurdle_ztnb-hurdle_ztpoisson"
)
full_result_path <- file.path(result_dir, paste0(stem, ".rds"))
if (!file.exists(full_result_path))
  stop("The full production gate has not produced its final RDS yet.")
full_result <- readRDS(full_result_path)
if (nrow(full_result$failures))
  stop("The full production gate has failures; interval rendering was not started.")

config <- full_result$config
H <- as.integer(config$H)
n_draws <- as.integer(config$n_draws)
seed <- as.integer(config$seed)
target_max_age <- as.integer(config$target_max_age)
jobs <- tidyr::expand_grid(
  location = config$locations, origin = config$origins, clock = config$clocks
) |>
  mutate(job_index = dplyr::row_number())

interval_checkpoint_path <- file.path(
  result_dir, "state_interval_render_checkpoint.rds"
)
new_interval_checkpoint <- function() list(
  config = config, completed = character(), predictions = list(),
  failures = list(), started_at = Sys.time(), updated_at = Sys.time()
)
interval_checkpoint <- if (file.exists(interval_checkpoint_path))
  readRDS(interval_checkpoint_path) else new_interval_checkpoint()
if (!identical(interval_checkpoint$config, config))
  stop("The interval-render checkpoint belongs to a different gate configuration.")
save_interval_checkpoint <- function() {
  interval_checkpoint$updated_at <<- Sys.time()
  saveRDS(interval_checkpoint, interval_checkpoint_path)
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
  model(
    likelihood, ar1_epidemic(), lognormal_delay(),
    count_cumulative = count_cumulative_process(
      observation = observation,
      retraction_delay = lognormal_delay(), settlement = H,
      movement_previous = 0
    )
  )
}

cat("Preparing full FluSight clock arms for interval rendering...\n")
prepared <- prepare_flusight_asof(
  config$locations, start = as.Date("2023-09-02"), settlement_horizon = H
)

models <- config$models
tasks <- tidyr::crossing(job_index = jobs$job_index, model = models) |>
  left_join(jobs, by = "job_index") |>
  mutate(task_id = paste(.data$location, .data$origin, .data$clock,
                         .data$model, sep = "|")) |>
  filter(!.data$task_id %in% interval_checkpoint$completed)
run_started <- proc.time()[["elapsed"]]
initial_done <- nrow(jobs) * length(models) - nrow(tasks)
cat(sprintf("Starting/resuming %d interval tasks (%d complete).\n",
            nrow(tasks), initial_done))

for (task_index in seq_len(nrow(tasks))) {
  task <- tasks[task_index, ]
  origin <- as.Date(task$origin, origin = "1970-01-01")
  warnings <- character()
  started <- proc.time()[["elapsed"]]
  result <- tryCatch(withCallingHandlers({
    panel <- asof_panel(prepared, task$location, origin, task$clock)
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
      make_production_tbl(panel), make_specification(task$model),
      type = "one_stage", temporal_effects = "none", n_draws = n_draws,
      seed = seed + task$job_index
    )
    prediction <- predict(
      fitted, n_draws = n_draws,
      seed = seed + 100000L + task$job_index
    )
    # Use tbl.now's cross-engine tidy contract to obtain the median and interval.
    tidied <- tbl.now::tidy(prediction, level = 0.90) |>
      mutate(
        event_num = as.integer(round(
          as.numeric(.data$event_date - as.Date("2000-01-01")) / 7
        ))
      ) |>
      inner_join(targets, by = "event_num") |>
      transmute(
        location = task$location, origin = origin, clock = task$clock,
        model = task$model, event_date = .data$event_date_actual,
        event_year = .data$event_year, age = .data$delay,
        current = .data$current, truth = .data$truth,
        estimate = .data$estimate, lower90 = .data$conf.low,
        upper90 = .data$conf.high, level = .data$level
      )
    if (!nrow(tidied) || any(!is.finite(tidied$estimate)) ||
        any(!is.finite(tidied$lower90)) || any(!is.finite(tidied$upper90))) {
      stop("Tidy prediction intervals were empty or non-finite.")
    }
    tidied
  }, warning = function(condition) {
    warnings <<- unique(c(warnings, conditionMessage(condition)))
    invokeRestart("muffleWarning")
  }), error = identity)

  if (inherits(result, "error")) {
    interval_checkpoint$failures[[length(interval_checkpoint$failures) + 1L]] <-
      data.frame(
        location = task$location, origin = origin, clock = task$clock,
        model = task$model, error = conditionMessage(result),
        warnings = paste(warnings, collapse = " | ")
      )
  } else {
    result$warnings <- paste(warnings, collapse = " | ")
    interval_checkpoint$predictions[[length(interval_checkpoint$predictions) + 1L]] <-
      result
  }
  interval_checkpoint$completed <- unique(c(
    interval_checkpoint$completed, task$task_id
  ))
  save_interval_checkpoint()

  done <- initial_done + task_index
  elapsed <- proc.time()[["elapsed"]] - run_started
  eta <- elapsed / task_index * (nrow(jobs) * length(models) - done)
  cat(sprintf(
    "[%d/%d] %s | %s | %s | %s: %.1fs; ETA %.1f min; failures=%d\n",
    done, nrow(jobs) * length(models), task$location, origin, task$clock,
    task$model, proc.time()[["elapsed"]] - started, eta / 60,
    length(interval_checkpoint$failures)
  ))
  flush.console()
}

interval_failures <- dplyr::bind_rows(interval_checkpoint$failures)
if (nrow(interval_failures)) {
  utils::write.csv(
    interval_failures,
    file.path(result_dir, "state_interval_render_failures.csv"),
    row.names = FALSE
  )
  stop("Interval rendering had failures; plots were not generated.")
}

predictions <- dplyr::bind_rows(interval_checkpoint$predictions)
prediction_path <- file.path(result_dir, "state_tidy_predictions.csv")
utils::write.csv(predictions, prediction_path, row.names = FALSE)

scores <- as.data.frame(full_result$scores)
state_metrics <- scores |>
  mutate(
    signed_error = .data$median - .data$truth,
    absolute_error = abs(.data$signed_error)
  ) |>
  group_by(.data$location, .data$clock, .data$model) |>
  summarise(
    targets = n(), mean_wis = mean(.data$wis),
    mean_absolute_error = mean(.data$absolute_error),
    median_absolute_error = median(.data$absolute_error),
    median_signed_error = median(.data$signed_error),
    coverage90 = mean(.data$coverage90), .groups = "drop"
  )
metric_path <- file.path(result_dir, "state_score_summary.csv")
utils::write.csv(state_metrics, metric_path, row.names = FALSE)

plot_dir <- file.path(result_dir, "state_plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
for (clock_name in unique(predictions$clock)) {
  for (model_name in unique(predictions$model)) {
    plot_data <- predictions |>
      filter(.data$clock == clock_name, .data$model == model_name)
    observed_truth <- plot_data |>
      distinct(.data$location, .data$origin, .data$event_date,
               .data$current, .data$truth)
    figure <- ggplot(plot_data, aes(x = .data$event_date)) +
      geom_linerange(
        aes(ymin = .data$lower90, ymax = .data$upper90,
            colour = "Nowcast 90% interval"),
        alpha = 0.45, linewidth = 0.25
      ) +
      geom_point(aes(y = .data$estimate, colour = "Nowcast median"),
                 size = 0.55) +
      geom_point(
        data = observed_truth,
        aes(x = .data$event_date, y = .data$current,
            colour = "Observed at origin"),
        shape = 1, size = 0.7, inherit.aes = FALSE
      ) +
      geom_point(
        data = observed_truth,
        aes(x = .data$event_date, y = .data$truth, colour = "Retrospective truth"),
        shape = 4, size = 0.7, stroke = 0.35, inherit.aes = FALSE
      ) +
      facet_wrap(vars(.data$location), ncol = 6L, scales = "free_y") +
      scale_colour_manual(values = c(
        "Observed at origin" = "#333333", "Nowcast median" = "#1769AA",
        "Nowcast 90% interval" = "#72A7D3", "Retrospective truth" = "#B22222"
      )) +
      labs(
        title = paste("Count-cumulative nowcasts:", model_name),
        subtitle = paste(clock_name, "clock; H =", H,
                         "; five origins; recent targets age 0-", target_max_age),
        x = "Event week", y = "Count", colour = NULL
      ) +
      theme_minimal(base_size = 8) +
      theme(
        legend.position = "top", panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 4.5),
        strip.text = element_text(size = 6)
      )
    output <- file.path(
      plot_dir, paste0("state_facets_", clock_name, "_", model_name, ".pdf")
    )
    ggsave(output, figure, width = 22, height = 30, units = "in", limitsize = FALSE)
  }
}

report_path <- file.path(result_dir, "COUNT_CUMULATIVE_STATE_RESULTS.md")
report_lines <- c(
  "# Count-cumulative full-sweep state results",
  "",
  sprintf("Finite settlement target: `C_t(%d)`. Intervals are 90%% intervals.", H),
  "",
  "The interval tables were produced with `tbl.now::tidy()` from 250",
  "posterior-predictive draws per fit. WIS and coverage are diagnostic",
  "composite-likelihood pseudo-posterior scores, not calibration proof.",
  "",
  "## Stability summary", "",
  capture.output(knitr::kable(as.data.frame(full_result$summary), digits = 4)),
  "", "## Aggregate score summary", "",
  capture.output(knitr::kable(as.data.frame(full_result$score_summary), digits = 4)),
  "", "## Per-state metrics", "",
  capture.output(knitr::kable(as.data.frame(state_metrics), digits = 4)),
  "", "## Facet-grid plots", "",
  paste0("- `state_plots/", basename(list.files(plot_dir, full.names = FALSE)), "`")
)
writeLines(report_lines, report_path)
cat("Wrote:\n", prediction_path, "\n", metric_path, "\n", report_path, "\n")
