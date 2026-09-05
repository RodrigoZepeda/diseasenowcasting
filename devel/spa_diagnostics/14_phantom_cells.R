# =============================================================================
# Is the low-p optimum an artefact of cells whose SNAPSHOT DOES NOT EXIST?
# =============================================================================
# FluSight does not publish every week: 63 snapshots cover 105 event weeks, with
# gaps of 14, 18, 21, 42 and 203 days (the off-season).  prepare_data() builds a
# DENSE (event-time x delay) increment array and zero-fills it, so a cell whose
# snapshot was never published enters the likelihood as "we observed zero change".
#
# For an event week first published at delay k, the model is therefore told:
#   delay 0..k-1 : observed zero        (FALSE -- nothing was published)
#   delay k      : +<the whole count>   (really the FIRST observation)
# which is exactly the +2512 pattern driving the pathology.
#
# Here the profile is recomputed over VALID cells only.  Nuisance parameters stay
# at their full-data fitted values, so this asks "do the real cells prefer low p?"
# -- not "what would a refit do?".
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
asof <- sort(unique(tn[[tbl.now::get_report_date(tn)]]))
ev   <- sort(unique(tn[[tbl.now::get_event_date(tn)]]))

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
      out[[length(out) + 1L]] <- c(t = t, d = d, z = z,
        real = as.numeric((ev[1] + (t - 1 + d) * 7) %in% asof),
        ll = diseasenowcasting:::.log_skellam_increment(z, a, w, if (d == 0L) 0L else 1L))
    }
  }
  as.data.frame(do.call(rbind, out))
}

P_GRID <- c(0.10, 0.20, 0.40, 0.60, 0.80, 0.90, 0.96, 0.99)
per_p  <- lapply(P_GRID, cells_at)

all_ll  <- vapply(per_p, function(fr) sum(fr$ll), numeric(1))
real_ll <- vapply(per_p, function(fr) sum(fr$ll[fr$real == 1]), numeric(1))
phan_ll <- vapply(per_p, function(fr) sum(fr$ll[fr$real == 0]), numeric(1))

cat(sprintf("\ncells: %d total, %d real, %d phantom\n",
            nrow(per_p[[1]]), sum(per_p[[1]]$real == 1), sum(per_p[[1]]$real == 0)))
cat("\n=== profile in p: all cells vs REAL cells only ===\n")
cat(sprintf("%6s %14s %14s %14s\n", "p", "all cells", "REAL only", "phantom only"))
for (i in seq_along(P_GRID))
  cat(sprintf("%6.2f %14.1f %14.1f %14.1f\n", P_GRID[i],
              all_ll[i] - max(all_ll), real_ll[i] - max(real_ll), phan_ll[i] - max(phan_ll)))
cat(sprintf("\noptimum with ALL cells      : p = %.2f\n", P_GRID[which.max(all_ll)]))
cat(sprintf("optimum with REAL cells only: p = %.2f\n", P_GRID[which.max(real_ll)]))
cat(sprintf("optimum with PHANTOMS only  : p = %.2f\n", P_GRID[which.max(phan_ll)]))
