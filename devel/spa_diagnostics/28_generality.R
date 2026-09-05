# =============================================================================
# 28 -- STAGE 0 of PLAN_increment_nowcasting.md: does the Texas increment
#       structure generalise, and are the revision bursts random or calendar?
#
# FINDINGS K was measured on one state and one arm.  Before building the hurdle
# model on top of it, check the four things the model design depends on:
#
#   A  the zero/jump structure          -- is it 77% zeros and a 3% heavy tail
#                                          everywhere, or is Texas special?
#   B  snapshot concentration           -- how sparse is pi_s, per state?
#   C  cross-state snapshot alignment   -- RISK 1 in the plan.  If the same
#                                          as_of dates are hot in every state,
#                                          the bursts are release policy and
#                                          pi_s wants a covariate, not a random
#                                          effect.
#   D  age loading and multiplicativity -- does participation depend on cohort
#                                          age (the w(a) term), and is the move
#                                          tighter on a relative scale?
#
# Run: NOT_CRAN=true Rscript devel/spa_diagnostics/28_generality.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)})
START <- as.Date("2023-09-23")
CAP   <- as.integer(Sys.getenv("CAP", "15"))

z <- tbl.now::flusight |>
  filter(target_end_date >= START, !is.na(observation)) |>
  mutate(d = floor(as.numeric(as_of - target_end_date) / 7)) |>
  filter(d >= 0, d <= CAP) |>
  arrange(location_name, target_end_date, as_of) |>
  group_by(location_name, target_end_date) |>
  mutate(z = observation - lag(observation), prev = lag(observation)) |>
  ungroup() |>
  filter(!is.na(z))                       # drop each cohort's first observation
cat(sprintf("=== post-first increments, %d locations, cap %d weeks: %d rows ===\n\n",
            n_distinct(z$location_name), CAP, nrow(z)))

# ---- A. the zero/jump structure, per state ----------------------------------
A <- z |> group_by(location_name) |>
  summarise(n = n(), pct_zero = 100 * mean(z == 0), pct_big = 100 * mean(abs(z) > 50),
            mean = mean(z), var = stats::var(z),
            vmr = stats::var(z) / pmax(abs(mean(z)), 1e-9), .groups = "drop") |>
  arrange(desc(n * 0 + pct_zero))
cat("=== A. zero fraction and heavy tail, all locations ===\n")
print(as.data.frame(A |> mutate(across(where(is.numeric), ~round(.x, 2)))),
      row.names = FALSE, max = 1e4)
cat(sprintf("\nzero fraction: median %.0f%%, range %.0f-%.0f%%\n",
            median(A$pct_zero), min(A$pct_zero), max(A$pct_zero)))
cat(sprintf("var/|mean|   : median %.0f  (Poisson would be 1)\n", median(A$vmr)))

# ---- B. how concentrated is the movement in snapshots? ----------------------
B <- z |> group_by(location_name, as_of) |>
  summarise(mass = sum(abs(z)), nz = sum(z != 0), n = n(), .groups = "drop_last") |>
  arrange(location_name, desc(mass)) |>
  summarise(snaps = n(), dead = sum(nz == 0),
            top3 = 100 * sum(mass[1:3]) / sum(mass),
            top6 = 100 * sum(mass[1:6]) / sum(mass),
            part_active = 100 * mean(nz[nz > 0] / n[nz > 0]), .groups = "drop")
cat("\n=== B. snapshot sparsity (per state) ===\n")
cat(sprintf("snapshots with zero movement : median %.0f of %.0f (%.0f%%)\n",
            median(B$dead), median(B$snaps), 100 * median(B$dead / B$snaps)))
cat(sprintf("top-3 snapshots hold         : median %.0f%% of the movement mass\n", median(B$top3)))
cat(sprintf("top-6 snapshots hold         : median %.0f%%\n", median(B$top6)))
cat(sprintf("cohort participation WITHIN an active snapshot: median %.0f%% of cohorts\n",
            median(B$part_active)))

# ---- C. are the same snapshots hot everywhere?  (plan risk 1) ---------------
# "did ANY cohort in this state move" saturates -- with ~14% participation over
# 15 cohorts a state moves somewhere almost surely.  Use the participation RATE
# per (state, snapshot), which is continuous, and test it against a null that
# keeps each state's own activity level but breaks the calendar alignment.
P <- z |> group_by(location_name, as_of) |>
  summarise(rate = mean(z != 0), mass = sum(abs(z)), n = n(), .groups = "drop")
W <- P |> select(location_name, as_of, rate) |>
  tidyr::pivot_wider(names_from = location_name, values_from = rate) |> arrange(as_of)
Wm <- as.matrix(W[, -1]); Wm <- Wm[, apply(Wm, 2, function(v) all(is.finite(v)) && stats::sd(v) > 0),
                                   drop = FALSE]
rho <- stats::cor(Wm); diag(rho) <- NA
obs_rho <- mean(rho, na.rm = TRUE)
set.seed(1); null_rho <- replicate(200, {
  Sh <- apply(Wm, 2, sample)                     # break alignment, keep marginals
  r <- stats::cor(Sh); diag(r) <- NA; mean(r, na.rm = TRUE)
})
cat("\n=== C. cross-state alignment: is a hot snapshot hot everywhere? ===\n")
cat(sprintf("mean pairwise correlation of the per-snapshot participation rate\n"))
cat(sprintf("  observed : %.3f\n", obs_rho))
cat(sprintf("  null     : %.3f  (sd %.3f, 200 within-state permutations)\n",
            mean(null_rho), stats::sd(null_rho)))
cat(sprintf("  z        : %.1f\n", (obs_rho - mean(null_rho)) / stats::sd(null_rho)))
Cn <- P |> group_by(as_of) |> summarise(states_moving = sum(rate > 0), states = n(),
                                        pooled_rate = stats::weighted.mean(rate, n),
                                        mass = sum(mass), .groups = "drop") |> arrange(desc(mass))
cat("\ntop snapshots by movement mass (pooled participation across all states):\n")
print(as.data.frame(head(Cn |> mutate(pooled_rate = round(pooled_rate, 3)), 10)), row.names = FALSE)
cat(sprintf("\npooled participation rate: median %.3f, max %.3f, ratio %.0fx\n",
            stats::median(Cn$pooled_rate), max(Cn$pooled_rate),
            max(Cn$pooled_rate) / max(stats::median(Cn$pooled_rate), 1e-9)))
cat(sprintf("snapshots above 3x the median pooled rate: %d of %d\n",
            sum(Cn$pooled_rate > 3 * stats::median(Cn$pooled_rate)), nrow(Cn)))

# ---- D. age loading, and relative vs absolute scale -------------------------
D <- z |> mutate(age = floor(as.numeric(as_of - target_end_date) / 7)) |>
  group_by(age) |> summarise(n = n(), pct_move = 100 * mean(z != 0),
                             med_abs = stats::median(abs(z[z != 0])), .groups = "drop")
cat("\n=== D. participation by cohort age (the w(a) term) ===\n")
print(as.data.frame(D |> mutate(across(where(is.numeric), ~round(.x, 1)))), row.names = FALSE, max = 1e4)
mv <- z |> filter(z != 0, !is.na(prev), prev > 0) |> group_by(location_name, as_of) |>
  filter(n() >= 5) |> summarise(sd_abs = stats::sd(z), sd_rel = stats::sd(z / prev),
                                cv_abs = stats::sd(z) / mean(abs(z)),
                                cv_rel = stats::sd(z / prev) / mean(abs(z / prev)), .groups = "drop")
cat(sprintf("\nwithin active (state, snapshot) cells with >=5 movers (%d cells):\n", nrow(mv)))
cat(sprintf("  median CV of ABSOLUTE change : %.2f\n", median(mv$cv_abs)))
cat(sprintf("  median CV of RELATIVE change : %.2f   <- smaller means multiplicative is right\n",
            median(mv$cv_rel)))
cat(sprintf("  relative is tighter in %.0f%% of cells\n", 100 * mean(mv$cv_rel < mv$cv_abs)))

saveRDS(list(A = A, B = B, C = Cn, D = D, mv = mv, cap = CAP,
             obs_rho = obs_rho, null_rho = null_rho),
        sprintf("devel/spa_diagnostics/generality_cap%d.rds", CAP))
