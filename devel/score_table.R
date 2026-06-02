# =============================================================================
# score_table.R -- WIS decomposition + coverage + AE for the paper table
# =============================================================================
# Scores the d*=0 nowcast (event AT date_run vs eventual `final`) on a COMMON
# date set per disease, for the diseasenowcasting models AND the comparison
# methods (NobBS, Epinowcast).  Produces every column of tab:wis_combined:
#   WIS, Over, Under, Disp, Bias, IC 50%, IC 90%, AE.
#
#   Rscript devel/score_table.R
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(scoringutils)
})
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

OWN_DIR <- "devel/results"
CMP_DIR <- "/Users/rodzepeda/Documents/diseasenowcast2/devel/results"

qn <- c("q2.5","q5","q10","q25","q50","q75","q90","q95","q97.5")
ql <- c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975)

score_disease <- function(disease) {
  own <- readRDS(file.path(OWN_DIR, paste0(disease, "_dcast3_steps.rds")))
  cmp <- readRDS(file.path(CMP_DIR, paste0(disease, "_comparison_steps.rds")))
  ev  <- own$event_date_col

  make_model <- function(d) {
    epi <- d$epidemic_label; dly <- d$delay_label
    ifelse(grepl("NobBS|Epinowcast", epi), epi, paste(epi, dly, sep = "/"))
  }
  all_steps <- bind_rows(
    own$all_steps |> mutate(model = make_model(pick(everything())), final = replace_na(final, 0L)),
    cmp$all_steps |> mutate(model = make_model(pick(everything())), final = replace_na(final, 0L))
  )

  evs <- as.symbol(ev)
  tgt <- all_steps |> filter(as.numeric(date_run - !!evs) == 0, !is.na(median), is.finite(final))

  # Common date set across ALL models (fair comparison)
  nmod <- tgt |> distinct(model) |> nrow()
  keep <- tgt |> count(date_run) |> filter(n >= nmod) |> pull(date_run)
  tgt  <- tgt |> filter(date_run %in% keep)
  if (nrow(tgt) == 0) { cat(disease, ": no common dates\n"); return(NULL) }

  long <- tgt |>
    pivot_longer(all_of(qn), names_to = "qname", values_to = "predicted") |>
    mutate(quantile_level = ql[match(qname, qn)])

  sc <- long |>
    as_forecast_quantile(observed = "final", predicted = "predicted",
                         quantile_level = "quantile_level",
                         forecast_unit = c("date_run", "model")) |>
    score() |>
    summarise_scores(by = "model")

  # Absolute error of the median per model (AE)
  ae <- tgt |> group_by(model) |> summarise(ae = mean(abs(median - final)), .groups = "drop")

  out <- sc |> left_join(ae, by = "model") |>
    transmute(disease = disease, model,
              wis   = round(wis, 2),
              over  = round(overprediction, 2),
              under = round(underprediction, 2),
              disp  = round(dispersion, 2),
              bias  = round(bias, 2),
              ic50  = round(interval_coverage_50, 2),
              ic90  = round(interval_coverage_90, 2),
              ae    = round(ae, 2),
              n_dates = length(keep))
  out
}

results <- bind_rows(lapply(c("mpox", "dengue", "covid"), score_disease))
saveRDS(results, "devel/results/score_table.rds")

cat("\n================ FULL SCORE TABLE ================\n")
for (d in c("mpox", "dengue", "covid")) {
  cat("\n==== ", toupper(d), " (n_dates =", results$n_dates[results$disease==d][1], ") ====\n")
  print(as.data.frame(results |> filter(disease == d) |> arrange(model) |> select(-disease, -n_dates)),
        row.names = FALSE)
}
