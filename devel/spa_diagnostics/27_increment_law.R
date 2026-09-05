# =============================================================================
# 27 -- WHAT THE INCREMENTS ACTUALLY LOOK LIKE, AND WHY THE SKELLAM CANNOT
#       REPRESENT THEM  (follows 26; bears on reviewer sections 15-17, 25, 30.4).
#
# Script 26 fit the retention curve nonparametrically and gained 9.8 nats over
# the 5-parameter lognormal-g_C model, landing on the same solution: over half of
# all reports retracted at lag 1, r(15) = 0.408.  Stage 3 there showed the fit
# expects 74,206 gross retractions against an observed net down-movement of
# 2,741 -- 27x.  This script asks why, from the data side, and the answer turns
# out to settle what the successor model has to change.
#
# Three checks, all on the w15 Texas arm:
#   A  the empirical cumulative trajectory by age -- is the count still arriving?
#   B  the law of the increments AFTER the first observation
#   C  posterior predictive check on the one feature that matters: how many
#      increments are exactly zero?
#
# Run: NOT_CRAN=true Rscript devel/spa_diagnostics/27_increment_law.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
STATE <- Sys.getenv("STATE", "Texas")
START <- as.Date("2023-09-23")

raw <- tbl.now::flusight |> filter(location_name == STATE, target_end_date >= START) |>
  mutate(d = floor(as.numeric(as_of - target_end_date) / 7)) |>
  filter(d <= 15, d >= 0) |> arrange(target_end_date, d)

# ---- A. the trajectory is flat: the count is complete at first publication ---
fin <- raw |> group_by(target_end_date) |> filter(d == max(d)) |> summarise(final = observation)
x <- raw |> inner_join(fin, by = "target_end_date") |> filter(final > 0) |>
  mutate(frac = observation / final)
cat("=== A. empirical C_t(a) / C_t(last observed), by age ===\n")
print(as.data.frame(x |> group_by(d) |> summarise(n = n(), mean = round(mean(frac), 4),
                                                  median = round(median(frac), 4))),
      row.names = FALSE, max = 1e4)
cat("\nThe mean trajectory starts at 1.01 and ends at 1.00: reporting is COMPLETE at\n")
cat("the first snapshot, and what follows is a ~2% net down-drift.  The model of\n")
cat("script 26 instead fits g_D with mean 1.79 wk and cancels the late arrivals\n")
cat("with 54% retraction at lag 1 -- same flat mean curve, two huge flows.\n")

# ---- B. the increments after first observation ------------------------------
z <- raw |> group_by(target_end_date) |>
  mutate(z = observation - lag(observation, default = 0), first = row_number() == 1) |> ungroup()
post <- z |> filter(!first)
cat(sprintf("\n=== B. the %d increments after first observation ===\n", nrow(post)))
cat(sprintf("mean            %8.3f\n", mean(post$z)))
cat(sprintf("variance        %8.1f\n", stats::var(post$z)))
cat(sprintf("var / |mean|    %8.0f\n", stats::var(post$z) / abs(mean(post$z))))
cat(sprintf("range           %8d .. %d\n", min(post$z), max(post$z)))
cat(sprintf("exactly zero    %8d (%.0f%%)\n", sum(post$z == 0), 100 * mean(post$z == 0)))
cat(sprintf("|z| > 50        %8d (%.0f%%)\n", sum(abs(post$z) > 50), 100 * mean(abs(post$z) > 50)))
cat("\nA Skellam(alpha, omega) has mean alpha-omega and variance alpha+omega.  To\n")
cat("reach mean 0.56 with variance 1635 it needs gross flows of ~1635 per interval\n")
cat("-- but at alpha+omega = 1635, P(increment = 0) is negligible, and 77% of the\n")
cat("increments are exactly zero.  The two requirements are incompatible.\n")

# ---- C. predictive check on the zeros ---------------------------------------
src <- readLines("devel/spa_diagnostics/26_free_retention.R")
eval(parse(text = paste(src[1:(which(grepl("^# ---- stage 1", src))[1] - 1L)], collapse = "\n")))
fr <- readRDS(sprintf("devel/spa_diagnostics/freeret_%s.rds", gsub(" ", "", STATE)))
Q <- q_pairs(fr$gD, fr$b, 0, fl$a, fl$b); mu <- fr$mu[fl$grp]
al <- mu * Q$qp; om <- mu * Q$qm
p0 <- exp(2 * sqrt(al * om) - (al + om)) * besselI(2 * sqrt(al * om), 0, expon.scaled = TRUE)
keep <- fl$a >= 0
cat(sprintf("\n=== C. predictive check at the script 26 fit ===\n"))
cat(sprintf("observed exactly zero       : %3d of %d (%.0f%%)\n",
            sum(fl$z[keep] == 0), sum(keep), 100 * mean(fl$z[keep] == 0)))
cat(sprintf("model expected exactly zero : %3.0f of %d (%.0f%%)\n",
            sum(p0[keep], na.rm = TRUE), sum(keep), 100 * mean(p0[keep], na.rm = TRUE)))
cat(sprintf("alpha+omega quantiles (25/50/75/95): %s\n",
            paste(round(stats::quantile(al[keep] + om[keep], c(.25, .5, .75, .95))), collapse = " ")))
cat("\nThe fit is a compromise: churn is pushed up until the zeros start to cost\n")
cat("more than the jumps gain, which lands at alpha+omega ~ 185 -- 9x short of the\n")
cat("dispersion needed for the jumps, and 7x too few zeros.  p is the only dial\n")
cat("this likelihood has for dispersion, so every dispersion source in the data,\n")
cat("administrative revisions included, is absorbed by pushing p down.\n")
