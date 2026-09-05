#!/usr/bin/env Rscript

suppressPackageStartupMessages({library(dplyr); library(tbl.now)})
source("devel/skellam_prototypes/flusight_data.R")

# Fifty states plus DC; omit the national aggregate and Puerto Rico here so the
# summary is genuinely state-level.
states <- setdiff(sort(unique(tbl.now::flusight$location_name)),
                  c("US", "Puerto Rico"))
prepared <- prepare_flusight(states)
out <- lapply(states, function(state) {
  x <- state_panel(prepared, state)
  data.frame(
    state = state,
    cells = nrow(x$cells),
    event_weeks = length(x$event_levels),
    p_empirical = x$p_empirical,
    signed_zero_rate = x$signed_zero_rate,
    first_reports = x$first_total,
    later_up = x$up,
    later_down = x$down,
    up_fraction_nonzero_mass = x$up / max(x$up + x$down, 1)
  )
}) |>
  bind_rows()

dir.create("devel/skellam_prototypes/results", showWarnings = FALSE)
write.csv(out, "devel/skellam_prototypes/results/flusight_state_diagnostics.csv",
          row.names = FALSE)

print(summary(out[, c("p_empirical", "signed_zero_rate",
                      "up_fraction_nonzero_mass")]))
cat("\nLowest and highest zero-rate states:\n")
print(bind_rows(slice_min(out, signed_zero_rate, n = 5),
                slice_max(out, signed_zero_rate, n = 5)), row.names = FALSE)
