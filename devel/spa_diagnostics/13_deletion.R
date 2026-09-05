# =============================================================================
# Step 2: batch-deletion sensitivity.  Does removing a handful of extreme cells
# destroy the low-p optimum?
# =============================================================================
# Cells are ranked by INFLUENCE -- their contribution to the low-p vs high-p
# likelihood gap -- which is a fixed ranking, not one that depends on the p being
# evaluated.  For each p on the grid the profile is then recomputed with the top-k
# influential cells dropped from the sum.
#
# This holds the nuisance parameters at their full-data fitted values, so it asks
# "do the REMAINING cells still prefer low p?" rather than "what would a refit on
# the reduced data do?".  That is the right question for a deletion diagnostic, but
# it is not a refit and is not labelled as one.
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

START <- as.Date("2023-09-23"); STATE <- "Texas"
raw <- tbl.now::flusight |>
  filter(location_name == STATE, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))
tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
              case_count = observation, data_type = "count-cumulative",
              verbose = FALSE) |> align_weeks(date_col = "report_date")

cells_at <- function(p_fixed) {
  f <- suppressMessages(suppressWarnings(nowcast(
    tn, model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
              validation = validation_process(p = p_fixed)),
    type = "one_stage", n_draws = 20, temporal_effects = "none", seed = 1)))
  fit <- f@fits[[1]]; e <- f@engine; pr <- f@priors
  cD <- min(as.integer(e$max_conf_delay) - 1L, 15L)
  dfns <- diseasenowcasting:::.delay_distribution_functions(
    as.integer(e$delay_family), as.numeric(fit$delay_mu), as.numeric(fit$delay_sigma))
  acdf <- as.numeric(dfns$cdf(seq_len(cD + 1L))); gD <- c(acdf[1], diff(acdf))
  rmu <- as.numeric(fit$parList$retract_mu)
  rsd <- 0.01 + exp(as.numeric(fit$parList$log_retract_sd_exc))
  rfns <- diseasenowcasting:::.delay_distribution_functions(as.integer(pr$retract_family), rmu, rsd)
  gC <- c(0, diff(c(0, as.numeric(rfns$cdf(seq_len(cD))))))
  gW <- diseasenowcasting:::.convolve_delays(gD, gC)
  mu <- as.numeric(fit$lambda) / p_fixed
  out <- list()
  for (t in seq_len(e$max_time)) {
    h <- min(as.integer(e$d_star[t, 1]), cD); if (h < 0L) next
    for (d in 0:h) {
      z <- e$increment_array[t, d + 1L, 1]
      a <- mu[t] * gD[d + 1L]; w <- (1 - p_fixed) * mu[t] * gW[d + 1L]
      out[[length(out) + 1L]] <- c(t = t, d = d, s = t + d, z = z,
        ll = diseasenowcasting:::.log_skellam_increment(z, a, w, if (d == 0L) 0L else 1L))
    }
  }
  as.data.frame(do.call(rbind, out))
}

P_GRID <- c(0.10, 0.20, 0.40, 0.60, 0.80, 0.90, 0.96, 0.99)
per_p <- lapply(P_GRID, cells_at); names(per_p) <- as.character(P_GRID)

# Influence ranking: contribution to the low-vs-high gap (fixed, p-independent).
key   <- paste(per_p[["0.1"]]$t, per_p[["0.1"]]$d)
stopifnot(identical(key, paste(per_p[["0.96"]]$t, per_p[["0.96"]]$d)))
influence <- per_p[["0.1"]]$ll - per_p[["0.96"]]$ll
ord <- order(-influence)

cat("\n=== the 10 most influential cells (drivers of the low-p preference) ===\n")
top <- per_p[["0.1"]][ord[1:10], c("t", "d", "s", "z")]
top$influence <- round(influence[ord[1:10]], 1)
top$snapshot  <- format(per_p[["0.1"]]$t[ord[1:10]] * 0 +
                        (min(raw$target_end_date) + (top$s - 1L) * 7))
print(top, row.names = FALSE)

cat("\n=== profile in p after deleting the top-k influential cells ===\n")
cat(sprintf("%6s %10s %10s %10s %10s %10s\n", "p", "k=0", "k=5", "k=10", "k=25", "k=50"))
res <- list()
for (k in c(0, 5, 10, 25, 50)) {
  drop <- if (k == 0) integer(0) else ord[seq_len(k)]
  ll <- vapply(per_p, function(fr) sum(fr$ll[-drop]) , numeric(1))
  if (k == 0) ll <- vapply(per_p, function(fr) sum(fr$ll), numeric(1))
  res[[as.character(k)]] <- ll - max(ll)
}
for (i in seq_along(P_GRID))
  cat(sprintf("%6.2f %10.1f %10.1f %10.1f %10.1f %10.1f\n", P_GRID[i],
              res[["0"]][i], res[["5"]][i], res[["10"]][i], res[["25"]][i], res[["50"]][i]))
cat("\noptimum p after deletion:\n")
for (k in c("0","5","10","25","50"))
  cat(sprintf("  k=%-3s -> p = %.2f\n", k, P_GRID[which.max(res[[k]])]))
