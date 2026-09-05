# =============================================================================
# 41 -- AGGREGATE THE MULTI-STATE SWEEP.
#
# Reads every ms/<location>.rds written by 40_multistate.R and answers the two
# questions the sweep exists for.  Nothing here refits anything.
#
#   FIT     -- per location: does each model converge, does the hurdle
#              reproduce the exact-zero count, does PIT pass, is S_R(15) sane,
#              and (the point of script 39 over 38) is the increment mean
#              actually equal to alpha - omega?
#   FORECAST -- the four-way WIS ordering, pooled and per location, on
#              identical triples.  Pooled mean-WIS is dominated by the largest
#              locations, so the per-location WIN COUNT is reported alongside:
#              an ordering that holds in 45 of 53 locations is a fact, one that
#              holds only in the pooled mean is a statement about California.
#
# Run: Rscript devel/spa_diagnostics/41_ms_report.R [ORIGIN_WINDOW]
#   with ORIGIN_WINDOW = the number of most recent origins to restrict to,
#   so `12` reproduces the Texas comparison window exactly.  Default: all.
# =============================================================================
suppressMessages(library(dplyr))
DIR <- Sys.getenv("MSDIR", "devel/spa_diagnostics/ms")
NWIN <- suppressWarnings(as.integer(Sys.getenv("WINDOW", NA)))

fs <- list.files(DIR, pattern = "[.]rds$", full.names = TRUE)
res <- lapply(fs, readRDS)
bad <- Filter(function(x) !is.null(x$error), res)
ok  <- Filter(function(x) is.null(x$error) && !is.null(x$R), res)
cat(sprintf("=== %d locations on disk: %d complete, %d failed, %d with no triples ===\n",
            length(res), length(ok), length(bad), length(res) - length(ok) - length(bad)))
if (length(bad)) for (b in bad) cat(sprintf("  FAIL %s: %s\n", b$location, substr(b$error, 1, 160)))
if (!length(ok)) { cat("nothing to report yet\n"); quit(save = "no") }

R <- bind_rows(lapply(ok, `[[`, "R"))
G <- bind_rows(lapply(ok, `[[`, "gates"))
O <- bind_rows(lapply(ok, `[[`, "origins"))

if (!is.na(NWIN)) {
  keep <- O |> group_by(location) |> slice_max(origin, n = NWIN) |> select(location, origin)
  R <- R |> semi_join(keep, by = c("location", "origin"))
  cat(sprintf("restricted to the %d most recent origins per location\n", NWIN))
}
R$origin <- as.Date(R$origin, origin = "1970-01-01")
cat(sprintf("%d scored triples | %d locations | origins %s to %s\n\n",
            nrow(R), dplyr::n_distinct(R$location), min(R$origin), max(R$origin)))

MODS <- c("38" = "wis38", "36" = "wis36", "39" = "wis39", "37" = "wis37",
          "empirical" = "wis_emp", "persistence" = "wis_persist")
COVS <- c("38" = "cov90_38", "36" = "cov90_36", "39" = "cov90_39",
          "37" = "cov90_37", "empirical" = "cov90_emp", "persistence" = NA)

skill <- function(v, ref) 100 * (1 - mean(v) / mean(ref))

# ---- 1. pooled four-way, the table the Texas run produced --------------------
cat("=== POOLED FOUR-WAY (all triples, equal weight per triple) ===\n")
cat(sprintf("%-14s %8s %10s %8s   %s\n", "model", "WIS", "skill", "cov90", "mean structure"))
notes <- c("38" = "inflated by 1/(1-P0)", "36" = "free kappa*level^beta",
           "39" = "exactly mu_t q_C(d)", "37" = "exactly mu_t q_C(d), Skellam",
           "empirical" = "ratio lookup", "persistence" = "C0")
for (m in names(MODS)) {
  cv <- if (is.na(COVS[[m]])) NA_real_ else mean(R[[COVS[[m]]]])
  cat(sprintf("%-14s %8.3f %9.1f%% %8s   %s\n", paste("script", m), mean(R[[MODS[[m]]]]),
              skill(R[[MODS[[m]]]], R$wis_emp),
              if (is.na(cv)) "-" else sprintf("%.3f", cv), notes[[m]]))
}

# ---- 2. per-location: is the ordering a fact or a pooled-mean artefact? ------
per <- R |> group_by(location) |> summarise(n = n(),
  s38 = skill(wis38, wis_emp), s36 = skill(wis36, wis_emp),
  s39 = skill(wis39, wis_emp), s37 = skill(wis37, wis_emp),
  c38 = mean(cov90_38), c39 = mean(cov90_39), .groups = "drop")
# How many origins actually survived?  A model diverging drops the origin for
# all four, which keeps the four columns comparable WITHIN a location but makes
# a location with 9 surviving origins incomparable to one with 32.  Carry the
# count so a thin location cannot be read as a normal one.
if ("skipped" %in% names(O)) {
  keptn <- O |> group_by(location) |>
    summarise(orig_kept = sum(is.na(skipped)), orig_tot = n(), .groups = "drop")
  per <- per |> left_join(keptn, by = "location")
  thin <- per |> filter(orig_kept < 0.75 * orig_tot)
  if (nrow(thin)) {
    cat("\n!! locations with >25% of origins dropped -- skill not comparable:\n")
    print(as.data.frame(thin |> select(location, n, orig_kept, orig_tot, s38, s36, s39, s37) |>
      mutate(across(where(is.numeric), ~round(.x, 1)))), row.names = FALSE)
  }
  cat(sprintf("\n%d of %d origins dropped to divergence (%.1f%%), in %d locations\n",
      sum(!is.na(O$skipped)), nrow(O), 100 * mean(!is.na(O$skipped)),
      n_distinct(O$location[!is.na(O$skipped)])))
}
cat(sprintf("\n=== PER LOCATION (%d) ===\n", nrow(per)))
cat("skill vs the empirical baseline, positive = better\n")
cat(sprintf("  script 38 beats empirical in %2d/%d locations (median skill %+.1f%%)\n",
            sum(per$s38 > 0), nrow(per), median(per$s38)))
cat(sprintf("  script 36 beats empirical in %2d/%d               (median %+.1f%%)\n",
            sum(per$s36 > 0), nrow(per), median(per$s36)))
cat(sprintf("  script 39 beats empirical in %2d/%d               (median %+.1f%%)\n",
            sum(per$s39 > 0), nrow(per), median(per$s39)))
cat(sprintf("  script 37 beats empirical in %2d/%d               (median %+.1f%%)\n",
            sum(per$s37 > 0), nrow(per), median(per$s37)))
cat(sprintf("\n  38 beats 39 in %2d/%d locations   (the mean-preservation cost)\n",
            sum(per$s38 > per$s39), nrow(per)))
cat(sprintf("  38 beats 36 in %2d/%d locations   (what h_R structure buys)\n",
            sum(per$s38 > per$s36), nrow(per)))
cat(sprintf("  39 beats 37 in %2d/%d locations   (hurdle vs Skellam, same mean)\n",
            sum(per$s39 > per$s37), nrow(per)))
cat(sprintf("  90%% coverage: 38 median %.3f, 39 median %.3f (nominal 0.90)\n",
            median(per$c38), median(per$c39)))
cat("\nworst 8 locations for script 38:\n")
print(as.data.frame(per |> arrange(s38) |> head(8) |>
  mutate(across(where(is.numeric), ~round(.x, 2)))), row.names = FALSE)
cat("\nbest 8:\n")
print(as.data.frame(per |> arrange(desc(s38)) |> head(8) |>
  mutate(across(where(is.numeric), ~round(.x, 2)))), row.names = FALSE)

# ---- 3. by horizon and by target age ----------------------------------------
cat("\n=== BY HORIZON ===\n")
print(as.data.frame(R |> group_by(h) |> summarise(n = n(),
  m38 = mean(wis38), m36 = mean(wis36), m39 = mean(wis39), m37 = mean(wis37),
  emp = mean(wis_emp), pers = mean(wis_persist),
  sk38 = skill(wis38, wis_emp), cov38 = mean(cov90_38), .groups = "drop") |>
  mutate(across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)
cat("\n=== BY TARGET AGE ===\n")
print(as.data.frame(R |> mutate(age = cut(a_star, c(-1, 1, 2, 4, 8, 15))) |>
  group_by(age) |> summarise(n = n(),
  m38 = mean(wis38), m36 = mean(wis36), m39 = mean(wis39), m37 = mean(wis37),
  emp = mean(wis_emp), pers = mean(wis_persist),
  sk38 = skill(wis38, wis_emp), cov38 = mean(cov90_38), .groups = "drop") |>
  mutate(across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)

# ---- 4. season: the Texas window is late/off season, so split on it ----------
cat("\n=== BY SEASON OF THE ORIGIN ===\n")
print(as.data.frame(R |> mutate(mo = as.integer(format(origin, "%m")),
    season = ifelse(mo >= 11 | mo <= 3, "peak (Nov-Mar)", "off (Apr-Oct)")) |>
  group_by(season) |> summarise(n = n(), medC0 = median(C0),
  m38 = mean(wis38), m36 = mean(wis36), m39 = mean(wis39), m37 = mean(wis37),
  emp = mean(wis_emp), pers = mean(wis_persist),
  sk38 = skill(wis38, wis_emp), sk39 = skill(wis39, wis_emp),
  cov38 = mean(cov90_38), .groups = "drop") |>
  mutate(across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)

# ---- 5. the fit gates -------------------------------------------------------
cat("\n=== IN-SAMPLE GATES, script 38 vs 39, across locations ===\n")
for (mm in c("38", "39")) {
  g <- G |> filter(model == mm)
  cat(sprintf("\n-- script %s (%d locations) --\n", mm, nrow(g)))
  cat(sprintf("  exact zeros   observed median %.3f | model median %.3f | max |gap| %.3f\n",
              median(g$zero_obs), median(g$zero_mod), max(abs(g$zero_obs - g$zero_mod))))
  cat(sprintf("  P(move is UP) observed median %.3f | model median %.3f\n",
              median(g$up_obs), median(g$up_mod)))
  cat(sprintf("  S_R(15)       median %.3f  [%.3f, %.3f]\n",
              median(g$SR15), min(g$SR15), max(g$SR15)))
  cat(sprintf("  g_D P(0)      median %.3f  | mean delay median %.3f wk\n",
              median(g$gD0), median(g$gDmean)))
  cat(sprintf("  PIT KS p      median %.3f | p < 0.05 in %d of %d locations\n",
              median(g$ks_p), sum(g$ks_p < 0.05), nrow(g)))
  cat(sprintf("  tail ratio    |z|>20 median %.2f | |z|>50 median %.2f (1 = right)\n",
              median(g$tail20), median(g$tail50)))
  cat(sprintf("  mean E[Delta] vs alpha-omega: max abs error median %.3g, worst %.3g\n",
              median(g$mean_err_max), max(g$mean_err_max)))
  cat(sprintf("  magnitude inflation pi*E[M]/(alpha+omega): median %.2f, worst %.1f (1 = preserved)\n",
              median(g$infl_med), max(g$infl_max)))
  cat(sprintf("  pi clamped at 1 in a median of %.1f%% of cells\n", 100 * median(g$pi_clamped)))
}
cat("\n=== IN-SAMPLE logL, median across locations ===\n")
g38 <- G |> filter(model == "38"); g39 <- G |> filter(model == "39")
cat(sprintf("  36 %.1f | 37 %.1f | 38 %.1f | 39 %.1f\n",
            median(g38$logL36), median(g38$logL37), median(g38$logL), median(g39$logL)))
cat(sprintf("  38 has the highest in-sample logL in %d of %d locations\n",
            sum(g38$logL > pmax(g38$logL36, g38$logL37, g39$logL)), nrow(g38)))

saveRDS(list(R = R, G = G, O = O, per = per), file.path(DIR, "..", "ms_summary.rds"))
cat(sprintf("\nwrote %s\n", normalizePath(file.path(DIR, "..", "ms_summary.rds"), mustWork = FALSE)))
