#!/usr/bin/env Rscript

# Retrospective evaluation for the asymptotically settled count. The final
# available value is treated as settled for event weeks in both 2024 and 2025.

suppressPackageStartupMessages({
  library(dplyr); library(tbl.now); library(RTMB)
  pkgload::load_all(".", quiet = TRUE)
})
source("devel/skellam_prototypes/flusight_data.R")
source("devel/skellam_prototypes/rtmb_count_cumulative.R")

state <- Sys.getenv("STATE", "Texas")
epidemic <- Sys.getenv("EPIDEMIC", "ar")
delay_family <- Sys.getenv("DELAY", "lognormal")
n_origins <- as.integer(Sys.getenv("N_ORIGINS", "4"))
n_draws <- as.integer(Sys.getenv("N_DRAWS", "1000"))
target_max_age <- as.integer(Sys.getenv("TARGET_MAX_AGE", "4"))
seed <- as.integer(Sys.getenv("SEED", "20260904"))
set.seed(seed)

prepared <- prepare_flusight(state)
full_panel <- state_panel(prepared, state)
settled <- prepared$settled |>
  filter(.data$location_name == state)

fit_one <- function(panel, observation) {
  build_rtmb_prototype(
    make_prototype_data(panel, epidemic = epidemic),
    observation = observation,
    appearance_delay = delay_family,
    retraction_delay = delay_family
  ) |>
    fit_rtmb_prototype()
}

settled_model_draws <- function(fit, event_index, n) {
  r <- reconstruct_prototype(fit)
  event_slot <- match(event_index, fit$data$event_levels)
  if (is.na(event_slot)) return(NULL)
  # The manuscript target: N_t^+ ~ Poisson(p * mu_t). The ZINB is the
  # observation law for revisions; it changes inference for mu_t but does not
  # redefine the settled estimand.
  rpois(n, lambda = max(r$p * r$mu[event_slot], 1e-10))
}

empirical_settled_draws <- function(prepared, state, event_year, age,
                                    current, n) {
  calibration <- prepared$settled |>
    filter(.data$location_name == state, .data$event_year < event_year) |>
    select("event_index", "settled_count") |>
    inner_join(
      prepared$data |>
        filter(.data$location_name == state, .data$delay == age) |>
        select("event_index", current_at_age = "observation"),
      by = "event_index"
    ) |>
    mutate(multiplier = .data$settled_count / .data$current_at_age) |>
    filter(is.finite(.data$multiplier), .data$multiplier >= 0)
  if (nrow(calibration) < 5L) return(rep(current, n))
  current * rep_len(calibration$multiplier, n)
}

wis <- function(q, y, levels)
  2 * mean(ifelse(y >= q, levels * (y - q), (1 - levels) * (q - y)))

clock <- prepared$clock
candidate_origins <- clock |>
  mutate(year = as.integer(format(.data$actual_week, "%Y"))) |>
  filter(.data$year %in% c(2024L, 2025L), .data$compressed_index >= 12L,
         .data$compressed_index <= max(.data$compressed_index) - 4L) |>
  pull(.data$compressed_index)
if (n_origins > length(candidate_origins)) n_origins <- length(candidate_origins)
origin_slots <- unique(round(seq(1, length(candidate_origins), length.out = n_origins)))
origins <- candidate_origins[origin_slots]
levels <- c(0.05, seq(0.1, 0.9, 0.1), 0.95)
rows <- list()

cat(sprintf("Settled-count evaluation for %s: %d origins; 2024 and 2025 final observations are truth.\n",
            state, length(origins)))
for (origin in origins) {
  training <- state_panel(prepared, state, report_cut = origin)
  cat(sprintf("  origin %d (%s): fitting", origin,
              as.character(clock$actual_week[origin + 1L])))
  fits <- list(skellam = fit_one(training, "skellam"),
               zinb = fit_one(training, "zinb"))
  cat("; scoring settled target\n")

  targets <- full_panel$cells |>
    filter(.data$report_index == origin, .data$age <= target_max_age) |>
    select("event_index", "event_week_actual", "age",
           current = "cumulative") |>
    mutate(event_year = as.integer(format(.data$event_week_actual, "%Y"))) |>
    filter(.data$event_year %in% c(2024L, 2025L)) |>
    inner_join(settled |> select("event_index", "settled_count"),
               by = "event_index")

  for (i in seq_len(nrow(targets))) {
    empirical <- empirical_settled_draws(
      prepared, state, targets$event_year[i], targets$age[i],
      targets$current[i], n_draws
    )
    q_emp <- quantile(empirical, levels, names = FALSE)
    for (model_name in names(fits)) {
      draws <- settled_model_draws(fits[[model_name]], targets$event_index[i],
                                   n_draws)
      if (is.null(draws)) next
      q <- quantile(draws, levels, names = FALSE)
      rows[[length(rows) + 1L]] <- data.frame(
        origin = origin,
        origin_date = clock$actual_week[origin + 1L],
        event_index = targets$event_index[i],
        event_date = targets$event_week_actual[i],
        event_year = targets$event_year[i],
        age = targets$age[i],
        current = targets$current[i],
        truth = targets$settled_count[i],
        model = model_name,
        wis_model = wis(q, targets$settled_count[i], levels),
        wis_empirical = wis(q_emp, targets$settled_count[i], levels),
        coverage90 = targets$settled_count[i] >= q[1L] &&
          targets$settled_count[i] <= q[length(q)]
      )
    }
  }
}

scores <- bind_rows(rows)
summary <- scores |>
  group_by(.data$model) |>
  summarise(n = n(), wis = mean(.data$wis_model),
            empirical_wis = mean(.data$wis_empirical),
            skill_vs_empirical = 1 - .data$wis / .data$empirical_wis,
            coverage90 = mean(.data$coverage90), .groups = "drop")
by_year <- scores |>
  group_by(.data$model, .data$event_year) |>
  summarise(n = n(), wis = mean(.data$wis_model),
            empirical_wis = mean(.data$wis_empirical),
            skill_vs_empirical = 1 - .data$wis / .data$empirical_wis,
            .groups = "drop")

cat("\nOverall settled-count scores:\n")
print(summary, row.names = FALSE, digits = 4)
cat("\nBy event year:\n")
print(by_year, row.names = FALSE, digits = 4)

dir.create("devel/skellam_prototypes/results", showWarnings = FALSE)
saveRDS(list(summary = summary, by_year = by_year, scores = scores,
             config = list(state = state, epidemic = epidemic,
                           delay = delay_family, n_origins = n_origins,
                           target_max_age = target_max_age, seed = seed)),
        file.path("devel/skellam_prototypes/results",
                  paste0("settled_", gsub(" ", "_", tolower(state)), "_",
                         epidemic, ".rds")))
