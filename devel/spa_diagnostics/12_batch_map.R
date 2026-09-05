# =============================================================================
# Step 1: do the influential cells cluster into snapshot-level batch events?
# =============================================================================
# An ordinary reporting anomaly is indexed by EVENT time t.  An administrative
# batch is indexed by the SNAPSHOT it was published in, s = t + d.  If the
# influential cells share a handful of s values, that is independent evidence the
# event is administrative rather than individual-level reporting.
#
# Reference parameters come from the HIGH-p fit, not the free fit: section 15 of
# the plan warns not to detect a batch using parameters the batch itself distorted,
# and the free fit inflates mu_t precisely to make these cells unsurprising.
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

f <- suppressMessages(suppressWarnings(nowcast(
  tn, model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
            validation = validation_process(p = 0.9572)),
  type = "one_stage", n_draws = 20, temporal_effects = "none", seed = 1)))
fit <- f@fits[[1]]; e <- f@engine; pr <- f@priors
cD <- min(as.integer(e$max_conf_delay) - 1L, 15L)
p <- pr$confirm_p$fixed
dfns <- diseasenowcasting:::.delay_distribution_functions(
  as.integer(e$delay_family), as.numeric(fit$delay_mu), as.numeric(fit$delay_sigma))
acdf <- as.numeric(dfns$cdf(seq_len(cD + 1L))); gD <- c(acdf[1], diff(acdf))
rmu <- as.numeric(fit$parList$retract_mu)
rsd <- 0.01 + exp(as.numeric(fit$parList$log_retract_sd_exc))
rfns <- diseasenowcasting:::.delay_distribution_functions(as.integer(pr$retract_family), rmu, rsd)
gC <- c(0, diff(c(0, as.numeric(rfns$cdf(seq_len(cD))))))
gW <- diseasenowcasting:::.convolve_delays(gD, gC)
mu <- as.numeric(fit$lambda) / p

# Mid-P two-sided tail surprise S = -log(2 min(u, 1-u)) under Skellam(alpha, omega).
surprise <- function(z, a, w) {
  span <- max(60, ceiling(10 * sqrt(a + w + 1)))
  ctr  <- round(a - w)
  grid <- seq.int(min(ctr - span, z - 1), max(ctr + span, z + 1))
  lp   <- vapply(grid, function(k)
    diseasenowcasting:::.log_skellam_increment(k, a + 1e-10, w + 1e-10, 1L), numeric(1))
  pmf  <- exp(lp - max(lp)); pmf <- pmf / sum(pmf)
  below <- sum(pmf[grid < z]); at <- sum(pmf[grid == z])
  u <- below + 0.5 * at
  -log(max(min(2 * min(u, 1 - u), 1), 1e-300))
}

inc <- e$increment_array; ds <- e$d_star
min_event <- e$min_event; unit <- 7
rows <- list()
for (t in seq_len(e$max_time)) {
  h <- min(as.integer(ds[t, 1]), cD); if (h < 1L) next
  for (d in 1:h) {
    z <- inc[t, d + 1L, 1]
    a <- mu[t] * gD[d + 1L]; w <- (1 - p) * mu[t] * gW[d + 1L]
    rows[[length(rows) + 1L]] <- data.frame(
      t = t, d = d, s = t + d, z = z, alpha = a, omega = w,
      S = surprise(z, a, w))
  }
}
cells <- do.call(rbind, rows)
cells$event_date    <- min_event + (cells$t - 1L) * unit
cells$snapshot_date <- min_event + (cells$s - 1L) * unit

cat(sprintf("\n%d cells scored (d >= 1). Surprise S = -log(two-sided mid-P tail).\n", nrow(cells)))
cat(sprintf("S quantiles: %s\n",
            paste(sprintf("%.1f", quantile(cells$S, c(.5, .9, .99, 1))), collapse = "  ")))

top <- cells[order(-cells$S), ][1:25, ]
cat("\n=== the 25 most surprising cells under the high-p reference ===\n")
print(top[, c("t", "d", "s", "snapshot_date", "z", "alpha", "omega", "S")],
      row.names = FALSE, digits = 4)

cat("\n=== clustering by SNAPSHOT s (do different event times share a revision date?) ===\n")
clus <- top |>
  group_by(s, snapshot_date) |>
  summarise(n_flagged = n(), total_pos = sum(pmax(z, 0)), total_neg = sum(pmin(z, 0)),
            t_range = paste(min(t), max(t), sep = "-"), max_S = max(S), .groups = "drop") |>
  arrange(desc(n_flagged))
print(as.data.frame(clus), row.names = FALSE, digits = 4)

cat("\n=== all cells, grouped by snapshot: which snapshots carry the mass? ===\n")
by_s <- cells |>
  group_by(s, snapshot_date) |>
  summarise(n_cells = n(), n_flagged = sum(S > 20), total_abs = sum(abs(z)),
            max_S = max(S), .groups = "drop") |>
  arrange(desc(total_abs)) |> head(12)
print(as.data.frame(by_s), row.names = FALSE, digits = 4)
saveRDS(cells, "devel/spa_diagnostics/batch_cells.rds")
