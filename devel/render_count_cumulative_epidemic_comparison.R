#!/usr/bin/env Rscript

# Merge the retained AR(1) calendar results with the checkpointed HSGP/SIR
# comparison and render like-for-like aggregate, state and facet-grid outputs.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

result_dir <- file.path("devel", "count_cumulative_integration_results")
ar_result_path <- file.path(
  result_dir,
  paste0(
    "production_loc53_orig5_clockcal-com_seed20260905_H26_draws250_",
    "cumulative_poisson-cumulative_nb-hurdle_ztnb-hurdle_ztpoisson.rds"
  )
)
ar_prediction_path <- file.path(result_dir, "state_tidy_predictions.csv")
comparison_path <- file.path(
  result_dir,
  paste0(
    "epidemic-comparison_loc53_orig5_calendar_seed20260905_",
    "H26_draws250_hsgp-sir.rds"
  )
)
required <- c(ar_result_path, ar_prediction_path, comparison_path)
if (any(!file.exists(required))) {
  stop("Comparison inputs are not complete: ",
       paste(required[!file.exists(required)], collapse = ", "))
}

ar_result <- readRDS(ar_result_path)
comparison <- readRDS(comparison_path)
comparison_failures <- as.data.frame(comparison$failures)
if (nrow(comparison_failures))
  warning(nrow(comparison_failures),
          " comparison fit(s) failed; outputs will identify incomplete groups.")

models <- c("hurdle_ztnb", "hurdle_ztpoisson")
ar_predictions <- utils::read.csv(ar_prediction_path, stringsAsFactors = FALSE) |>
  filter(.data$clock == "calendar", .data$model %in% models) |>
  mutate(
    epidemic = "ar1", origin = as.Date(.data$origin),
    event_date = as.Date(.data$event_date)
  ) |>
  select(any_of(names(comparison$predictions)))
predictions <- bind_rows(ar_predictions, comparison$predictions) |>
  mutate(
    epidemic = factor(.data$epidemic, levels = c("ar1", "hsgp", "sir")),
    model = factor(.data$model, levels = models)
  )

ar_scores <- as.data.frame(ar_result$scores) |>
  filter(.data$clock == "calendar", .data$model %in% models) |>
  mutate(epidemic = "ar1")
scores <- bind_rows(ar_scores, comparison$scores) |>
  mutate(
    epidemic = factor(.data$epidemic, levels = c("ar1", "hsgp", "sir")),
    model = factor(.data$model, levels = models)
  )

aggregate_scores <- scores |>
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

aggregate_path <- file.path(result_dir, "epidemic_comparison_aggregate_scores.csv")
state_path <- file.path(result_dir, "epidemic_comparison_state_scores.csv")
prediction_path <- file.path(result_dir, "epidemic_comparison_tidy_predictions.csv")
utils::write.csv(aggregate_scores, aggregate_path, row.names = FALSE)
utils::write.csv(state_scores, state_path, row.names = FALSE)
utils::write.csv(predictions, prediction_path, row.names = FALSE)

plot_dir <- file.path(result_dir, "epidemic_comparison_plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
process_colours <- c(ar1 = "#1769AA", hsgp = "#7B1FA2", sir = "#00876C")
for (model_name in models) {
  plot_data <- predictions |>
    filter(.data$model == model_name)
  observed_truth <- plot_data |>
    distinct(.data$location, .data$origin, .data$event_date,
             .data$current, .data$truth)
  figure <- ggplot(plot_data, aes(x = .data$event_date)) +
    geom_linerange(
      aes(ymin = .data$lower90, ymax = .data$upper90,
          colour = .data$epidemic),
      position = position_dodge(width = 3), alpha = 0.42, linewidth = 0.28
    ) +
    geom_point(
      aes(y = .data$estimate, colour = .data$epidemic),
      position = position_dodge(width = 3), size = 0.58
    ) +
    geom_point(
      data = observed_truth,
      aes(x = .data$event_date, y = .data$current, shape = "Observed at origin"),
      colour = "#333333", size = 0.65, inherit.aes = FALSE
    ) +
    geom_point(
      data = observed_truth,
      aes(x = .data$event_date, y = .data$truth, shape = "Retrospective truth"),
      colour = "#B22222", size = 0.65, stroke = 0.35, inherit.aes = FALSE
    ) +
    facet_wrap(vars(.data$location), ncol = 6L, scales = "free_y") +
    scale_colour_manual(values = process_colours, drop = FALSE) +
    scale_shape_manual(values = c("Observed at origin" = 1,
                                  "Retrospective truth" = 4)) +
    labs(
      title = paste("Epidemic-process comparison:", model_name),
      subtitle = "Calendar clock; AR(1), HSGP and SIR; median and 90% interval",
      x = "Event week", y = "Count", colour = "Epidemic process", shape = NULL
    ) +
    theme_minimal(base_size = 8) +
    theme(
      legend.position = "top", panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 4.5),
      strip.text = element_text(size = 6)
    )
  ggsave(
    file.path(plot_dir, paste0("state_facets_epidemic_", model_name, ".pdf")),
    figure, width = 22, height = 30, units = "in", limitsize = FALSE
  )
}

report_path <- file.path(result_dir, "COUNT_CUMULATIVE_EPIDEMIC_COMPARISON.md")
report_lines <- c(
  "# Count-cumulative epidemic-process comparison",
  "",
  "Calendar-time comparison of AR(1), HSGP and SIR epidemic processes for",
  "the hurdle-ZTNB and hurdle-ZTPoisson observation laws. All other settings",
  "are held fixed (`H = 26`, five origins, 53 locations, 250 draws).",
  "",
  "Intervals were extracted through `tbl.now::tidy()`.",
  "Target counts are reported for every row so incomplete groups are visible.",
  if (nrow(comparison_failures)) paste0(
    "The comparison contains ", nrow(comparison_failures),
    " failed fit(s); their targets are omitted rather than imputed."
  ) else "All comparison fits completed.",
  "",
  if (nrow(comparison_failures)) "## Failed fits" else character(),
  if (nrow(comparison_failures)) "" else character(),
  if (nrow(comparison_failures)) capture.output(
    knitr::kable(comparison_failures)
  ) else character(),
  if (nrow(comparison_failures)) "" else character(),
  "## Aggregate scores", "",
  capture.output(knitr::kable(as.data.frame(aggregate_scores), digits = 4)),
  "", "## Per-state scores", "",
  capture.output(knitr::kable(as.data.frame(state_scores), digits = 4)),
  "", "## State-facet plots", "",
  paste0("- `epidemic_comparison_plots/",
         basename(list.files(plot_dir, full.names = FALSE)), "`")
)
writeLines(report_lines, report_path)
cat("Wrote:\n", aggregate_path, "\n", state_path, "\n",
    prediction_path, "\n", report_path, "\n")
