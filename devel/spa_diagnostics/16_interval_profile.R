# =============================================================================
# The decisive test: profile p under the cadence-given INTERVAL likelihood.
# =============================================================================
# For each event week the observed delays are those carrying a snapshot.  Those
# delays partition the horizon into consecutive intervals (d_prev, d_next], and the
# observation for each is C_t(d_next) - C_t(d_prev).  Every piece of information is
# used exactly once, so section 20's double-counting rule holds by construction and
# the phantom cells simply never exist as observations.
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

q_plus <- function(a, b, gD, gC, p) {
  GC <- cumsum(gC); Gbar <- function(k) if (k < 0) 1 else 1 - GC[min(k, length(GC)-1L) + 1L]
  s <- 0; for (r in max(a + 1L, 0L):b) if (r + 1L <= length(gD))
    s <- s + gD[r + 1L] * (p + (1 - p) * Gbar(b - r)); s
}
q_minus <- function(a, b, gD, gC, p) {
  GC <- cumsum(gC); G <- function(k) if (k < 0) 0 else GC[min(k, length(GC)-1L) + 1L]
  if (a < 0) return(0)
  s <- 0; for (r in 0:a) if (r + 1L <= length(gD)) s <- s + gD[r + 1L]*(G(b - r) - G(a - r))
  (1 - p) * s
}

START <- as.Date("2023-09-23"); STATE <- "Texas"
raw <- tbl.now::flusight |>
  filter(location_name == STATE, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))
tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
              case_count = observation, data_type = "count-cumulative",
              verbose = FALSE) |> align_weeks(date_col = "report_date")
asof <- sort(unique(tn[[tbl.now::get_report_date(tn)]]))
ev   <- sort(unique(tn[[tbl.now::get_event_date(tn)]]))

P_GRID <- c(0.10, 0.20, 0.40, 0.60, 0.80, 0.90, 0.96, 0.99)
rows <- list()
for (p_fixed in P_GRID) {
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
  inc <- e$increment_array

  L_point <- L_interval <- 0; n_iv <- 0
  for (t in seq_len(e$max_time)) {
    h <- min(as.integer(e$d_star[t, 1]), cD); if (h < 0L) next
    # point likelihood, every dense cell (what the package does today)
    for (d in 0:h) {
      z <- inc[t, d + 1L, 1]
      L_point <- L_point + diseasenowcasting:::.log_skellam_increment(
        z, mu[t] * gD[d + 1L], (1 - p_fixed) * mu[t] * gW[d + 1L], if (d == 0L) 0L else 1L)
    }
    # interval likelihood, one term per OBSERVED snapshot
    obs_d <- (0:h)[(ev[1] + (t - 1 + 0:h) * 7) %in% asof]
    if (!length(obs_d)) next
    prev <- -1L
    for (b in obs_d) {
      z <- sum(inc[t, seq_len(b + 1L), 1]) - (if (prev < 0) 0 else sum(inc[t, seq_len(prev + 1L), 1]))
      a_iv <- mu[t] * q_plus(prev, b, gD, gC, p_fixed)
      w_iv <- mu[t] * q_minus(prev, b, gD, gC, p_fixed)
      L_interval <- L_interval + diseasenowcasting:::.log_skellam_increment(
        z, a_iv, w_iv, if (w_iv <= 0) 0L else 1L)
      n_iv <- n_iv + 1; prev <- b
    }
  }
  rows[[length(rows) + 1L]] <- data.frame(p = p_fixed, L_point = L_point,
                                          L_interval = L_interval, n_iv = n_iv,
                                          gD_median = exp(as.numeric(fit$delay_mu)), gC1 = gC[2])
  cat(sprintf("p=%.2f  L_point=%11.1f  L_interval=%11.1f  (%d interval obs)\n",
              p_fixed, L_point, L_interval, n_iv))
}
tab <- do.call(rbind, rows)
tab$rel_point    <- tab$L_point    - max(tab$L_point)
tab$rel_interval <- tab$L_interval - max(tab$L_interval)
cat("\n=== p-profile: dense point cells vs cadence-given intervals ===\n")
print(tab[, c("p", "rel_point", "rel_interval", "gD_median", "gC1")],
      row.names = FALSE, digits = 5)
cat(sprintf("\npoint    optimum: p = %.2f\ninterval optimum: p = %.2f\n",
            tab$p[which.max(tab$L_point)], tab$p[which.max(tab$L_interval)]))
saveRDS(tab, "devel/spa_diagnostics/interval_profile.rds")
