#!/usr/bin/env Rscript

# Leakage-safe retrospective prototype for the asymptotically settled count.
# Example:
#   OMP_NUM_THREADS=1 SETTLEMENT_WEEKS=26 N_ORIGINS=2 \
#     Rscript devel/skellam_prototypes/run_asof_backtest.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tbl.now)
  library(RTMB)
  pkgload::load_all(".", quiet = TRUE)
})
source("devel/skellam_prototypes/flusight_asof_data.R")
source("devel/skellam_prototypes/rtmb_identifiability_models.R")

state <- Sys.getenv("STATE", "Texas")
epidemic <- Sys.getenv("EPIDEMIC", "ar")
delay_family <- Sys.getenv("DELAY", "lognormal")
settlement_horizon <- as.integer(Sys.getenv("SETTLEMENT_WEEKS", "52"))
target_max_age <- as.integer(Sys.getenv("TARGET_MAX_AGE", "4"))
n_origins <- as.integer(Sys.getenv("N_ORIGINS", "2"))
n_draws <- as.integer(Sys.getenv("N_DRAWS", "1000"))
seed <- as.integer(Sys.getenv("SEED", "20260904"))
clock_env <- Sys.getenv("CLOCKS", "calendar,compressed")
clocks <- intersect(strsplit(clock_env, ",", fixed = TRUE)[[1L]],
                    c("calendar", "compressed"))
if (!length(clocks)) stop("CLOCKS must contain calendar and/or compressed.")
set.seed(seed)

prepared <- prepare_flusight_asof(
  state, start = as.Date("2023-09-02"),
  settlement_horizon = settlement_horizon
)

origin_env <- Sys.getenv("NOWS", "")
if (nzchar(origin_env)) {
  origins <- as.Date(strsplit(origin_env, ",", fixed = TRUE)[[1L]])
} else {
  candidates <- prepared$raw |>
    distinct(.data$report_date_actual) |>
    arrange(.data$report_date_actual) |>
    filter(.data$report_date_actual >= as.Date("2024-01-01"),
           .data$report_date_actual < max(.data$report_date_actual)) |>
    pull(.data$report_date_actual)
  n_origins <- min(n_origins, length(candidates))
  positions <- unique(round(seq(1, length(candidates), length.out = n_origins)))
  origins <- candidates[positions]
}

levels <- c(0.05, seq(0.1, 0.9, 0.1), 0.95)
wis <- function(q, y, probs) {
  2 * mean(ifelse(y >= q, probs * (y - q), (1 - probs) * (q - y)))
}

score_draws <- function(draws, truth) {
  q <- stats::quantile(draws, levels, names = FALSE, type = 8)
  data.frame(
    median = stats::median(draws),
    wis = wis(q, truth, levels),
    absolute_error = abs(stats::median(draws) - truth),
    coverage90 = truth >= q[1L] && truth <= q[length(q)]
  )
}

score_rows <- list()
fit_rows <- list()
fits <- list()

cat(sprintf(
  "As-of settled-count prototype: %s; H=%d; origins=%s\n",
  state, settlement_horizon, paste(origins, collapse = ", ")
))

for (clock in clocks) {
  for (origin in origins) {
    # A base for-loop strips the Date class from a length-one element.
    origin <- as.Date(origin, origin = "1970-01-01")
    panel <- asof_panel(prepared, state, origin, clock)
    stopifnot(panel$latest_report_date <= origin,
              max(panel$cells$event_date_actual) <= origin,
              max(panel$cells$report_date_actual) <= origin)
    data <- make_identifiability_data(panel, epidemic)

    cat(sprintf("  %s @ %s (latest release %s): fitting cumulative Poisson/NB + hurdle",
                clock, as.character(origin),
                as.character(panel$latest_report_date)))
    model_fits <- list(
      cumulative_poisson = build_identifiability_model(
        data, "cumulative_poisson", delay_family, delay_family,
        use_random = TRUE
      ) |> fit_identifiability_model(),
      cumulative_nb = build_identifiability_model(
        data, "cumulative_nb", delay_family, delay_family,
        use_random = TRUE
      ) |> fit_identifiability_model(),
      hurdle_ztnb = build_identifiability_model(
        data, "hurdle_ztnb", delay_family, delay_family
      ) |> fit_identifiability_model()
    )
    cat("; reconstructing\n")
    fits[[paste(clock, origin, sep = "_")]] <- model_fits

    for (model_name in names(model_fits)) {
      fit <- model_fits[[model_name]]
      fit_rows[[length(fit_rows) + 1L]] <- data.frame(
        clock = clock,
        origin = origin,
        model = model_name,
        nll = fit$fit$objective,
        convergence = fit$fit$convergence,
        max_gradient = fit$max_gradient,
        retract_mass = plogis(fit$par_list$retract_mass_raw),
        q_terminal = fit$components$terminal_retention,
        n_cells = length(data$cumulative),
        latest_report_date = panel$latest_report_date
      )
    }

    targets <- panel$cells |>
      group_by(.data$event_num, .data$event_date_actual) |>
      slice_max(.data$report_num, n = 1L, with_ties = FALSE) |>
      ungroup() |>
      filter(.data$delay <= target_max_age) |>
      inner_join(
        prepared$truth |>
          filter(.data$location_name == state) |>
          select("event_date_actual", "event_year", "truth"),
        by = "event_date_actual"
      )

    for (i in seq_len(nrow(targets))) {
      target <- targets[i, ]
      baseline <- empirical_multiplier_draws(
        panel, target$event_num, target$delay, target$cumulative, n_draws
      )
      baseline_score <- score_draws(baseline, target$truth)
      score_rows[[length(score_rows) + 1L]] <- cbind(
        data.frame(clock = clock, origin = origin,
                   event_num = target$event_num,
                   event_date = target$event_date_actual,
                   event_year = target$event_year, age = target$delay,
                   current = target$cumulative, truth = target$truth,
                   model = "empirical_multiplier"),
        baseline_score
      )

      for (model_name in names(model_fits)) {
        fit <- model_fits[[model_name]]
        reconstructions <- switch(
          model_name,
          cumulative_poisson = c("anchored", "direct"),
          cumulative_nb = "direct",
          hurdle_ztnb = "anchored"
        )
        for (reconstruction in reconstructions) {
          draws <- simulate_terminal_count(
            fit, target$event_num, target$cumulative, target$delay,
            n_draws, reconstruction,
            previous_nonzero = target$previous_nonzero
          )
          model_score <- score_draws(draws, target$truth)
          score_rows[[length(score_rows) + 1L]] <- cbind(
            data.frame(clock = clock, origin = origin,
                       event_num = target$event_num,
                       event_date = target$event_date_actual,
                       event_year = target$event_year, age = target$delay,
                       current = target$cumulative, truth = target$truth,
                       model = paste(model_name, reconstruction, sep = "_")),
            model_score
          )
        }
      }
    }
  }
}

scores <- bind_rows(score_rows)
fit_diagnostics <- bind_rows(fit_rows)
summary <- scores |>
  group_by(.data$clock, .data$model) |>
  summarise(n = n(), wis = mean(.data$wis),
            mae = mean(.data$absolute_error),
            coverage90 = mean(.data$coverage90), .groups = "drop")

cat("\nFit diagnostics:\n")
print(fit_diagnostics)
cat("\nBacktest scores (2024 and 2025 final snapshots treated as truth):\n")
print(summary)

dir.create("devel/skellam_prototypes/results", showWarnings = FALSE)
output <- file.path(
  "devel/skellam_prototypes/results",
  paste0("asof_", gsub(" ", "_", tolower(state)), "_H",
         settlement_horizon, "_", epidemic, ".rds")
)
saveRDS(list(summary = summary, scores = scores,
             fit_diagnostics = fit_diagnostics, fits = fits,
             config = list(state = state, epidemic = epidemic,
                           delay = delay_family,
                           settlement_horizon = settlement_horizon,
                           target_max_age = target_max_age,
                           origins = origins, clocks = clocks,
                           n_draws = n_draws, seed = seed)), output)
cat("\nSaved ", output, "\n", sep = "")
