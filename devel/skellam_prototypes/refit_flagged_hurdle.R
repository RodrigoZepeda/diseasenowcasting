#!/usr/bin/env Rscript

# Strict refit of hurdle jobs flagged by either a nonzero optimizer code or a
# maximum gradient >= 0.1 in the all-location stability sweep.

suppressPackageStartupMessages({
  library(dplyr)
  library(tbl.now)
  library(RTMB)
  pkgload::load_all(".", quiet = TRUE)
})
source("devel/skellam_prototypes/flusight_asof_data.R")
source("devel/skellam_prototypes/rtmb_identifiability_models.R")

H <- as.integer(Sys.getenv("SETTLEMENT_WEEKS", "26"))
result_dir <- file.path("devel/skellam_prototypes/results",
                        paste0("all_locations_H", H))
result_path <- file.path(result_dir,
                         paste0("all_locations_ar_lognormal_H", H, ".rds"))
result <- readRDS(result_path)
diagnostics <- tibble::as_tibble(result$diagnostics)
flagged <- diagnostics |>
  filter(.data$model == "hurdle_ztnb",
         .data$convergence != 0 | .data$max_gradient >= 0.1) |>
  distinct(.data$location, .data$origin, .data$clock)

if (!nrow(flagged)) {
  cat("No hurdle fits require strict refitting.\n")
  quit(save = "no")
}

locations <- sort(unique(flagged$location))
cat(sprintf("Preparing %d locations and strictly refitting %d hurdle jobs.\n",
            length(locations), nrow(flagged)))
prepared <- prepare_flusight_asof(
  locations, start = as.Date("2023-09-02"), settlement_horizon = H
)

rows <- list()
for (i in seq_len(nrow(flagged))) {
  job <- flagged[i, ]
  target_location <- job$location[[1L]]
  target_origin <- as.Date(job$origin[[1L]])
  target_clock <- job$clock[[1L]]
  panel <- asof_panel(prepared, target_location, target_origin, target_clock)
  data <- make_identifiability_data(panel, "ar")
  fit <- build_identifiability_model(
    data, "hurdle_ztnb", "lognormal", "lognormal"
  ) |>
    fit_identifiability_model(
      control = list(iter.max = 4000, eval.max = 8000),
      polish_maxit = 5000L
    )
  rows[[i]] <- data.frame(
    location = target_location, origin = target_origin, clock = target_clock,
    original_convergence = diagnostics |>
      filter(.data$location == .env$target_location,
             .data$origin == .env$target_origin,
             .data$clock == .env$target_clock,
             .data$model == "hurdle_ztnb") |>
      pull(.data$convergence),
    original_gradient = diagnostics |>
      filter(.data$location == .env$target_location,
             .data$origin == .env$target_origin,
             .data$clock == .env$target_clock,
             .data$model == "hurdle_ztnb") |>
      pull(.data$max_gradient),
    strict_convergence = fit$fit$convergence,
    strict_gradient = fit$max_gradient,
    strict_nll = fit$fit$objective,
    q_terminal = fit$components$terminal_retention
  )
  cat(sprintf("[%d/%d] %s | %s | %s: code %d, gradient %.6g\n",
              i, nrow(flagged), target_location, target_origin, target_clock,
              fit$fit$convergence, fit$max_gradient))
}

out <- bind_rows(rows)
out_path <- file.path(result_dir, "strict_hurdle_refits.csv")
utils::write.csv(out, out_path, row.names = FALSE)
cat("Saved ", out_path, "\n", sep = "")
