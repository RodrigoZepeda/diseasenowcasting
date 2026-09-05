# =============================================================================
# Phase 2: profile likelihood in p (deliverable D) + the churn mechanism (13).
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
invisible(capture.output(source("devel/spa_diagnostics/01_exact_reference.R")))

START <- as.Date("2023-09-23"); STATE <- "Texas"
raw <- tbl.now::flusight |>
  filter(location_name == STATE, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))
tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
              case_count = observation, data_type = "count-cumulative",
              verbose = FALSE) |> align_weeks(date_col = "report_date")

P_GRID <- c(0.05, 0.10, 0.15, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70,
            0.80, 0.90, 0.95, 0.96, 0.97, 0.98, 0.99)
rows <- list()

for (p_fixed in P_GRID) {
  fitted <- tryCatch(suppressMessages(suppressWarnings(nowcast(
    tn, model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
              validation = validation_process(p = p_fixed)),
    type = "one_stage", n_draws = 20, temporal_effects = "none", seed = 1))),
    error = function(e) { message("[p=", p_fixed, "] ", conditionMessage(e)); NULL })
  if (is.null(fitted)) next

  fit <- fitted@fits[[1]]; engine <- fitted@engine; priors <- fitted@priors
  conf_D <- min(as.integer(engine$max_conf_delay) - 1L, 15L)
  delay_fns <- diseasenowcasting:::.delay_distribution_functions(
    as.integer(engine$delay_family), as.numeric(fit$delay_mu), as.numeric(fit$delay_sigma))
  acdf <- as.numeric(delay_fns$cdf(seq_len(conf_D + 1L)))
  g_D  <- c(acdf[1], diff(acdf))
  rmu <- as.numeric(fit$parList$retract_mu)
  rsd <- 0.01 + exp(as.numeric(fit$parList$log_retract_sd_exc))
  rfns <- diseasenowcasting:::.delay_distribution_functions(as.integer(priors$retract_family), rmu, rsd)
  g_C <- c(0, diff(c(0, as.numeric(rfns$cdf(seq_len(conf_D))))))
  g_W <- diseasenowcasting:::.convolve_delays(g_D, g_C)

  lambda <- as.numeric(fit$lambda); mu_stream <- lambda / p_fixed
  eta_stream <- (1 - p_fixed) * mu_stream
  inc <- engine$increment_array; d_star <- engine$d_star

  L_spa <- L_exact <- A_total <- W_total <- abs_delta <- 0
  for (t in seq_len(engine$max_time)) {
    h <- min(as.integer(d_star[t, 1]), conf_D); if (h < 0L) next
    for (d in 0:h) {
      z <- inc[t, d + 1L, 1]; a <- mu_stream[t] * g_D[d + 1L]; w <- eta_stream[t] * g_W[d + 1L]
      bt <- if (d == 0L) 0L else 1L
      L_spa   <- L_spa   + diseasenowcasting:::.log_skellam_increment(z, a, w, bt)
      L_exact <- L_exact + if (bt == 0L) dpois(z, a + 1e-8, log = TRUE)
                           else log_skellam_exact(z, a + 1e-8, w + 1e-8)
      A_total <- A_total + a; W_total <- W_total + w; abs_delta <- abs_delta + abs(z)
    }
  }
  rows[[length(rows) + 1L]] <- data.frame(
    p = p_fixed, L_spa = L_spa, L_exact = L_exact,
    gD_median = exp(as.numeric(fit$delay_mu)), gC_median = exp(rmu),
    gC_mass_lag1 = g_C[2], mu_median = median(mu_stream),
    A_total = A_total, W_total = W_total,
    churn = (A_total + W_total) / max(abs_delta, 1), conv = fit$convergence)
  cat(sprintf("p=%.2f  L_spa=%12.2f  L_exact=%12.2f  gD_med=%6.2f gC_med=%6.3f churn=%8.1f\n",
              p_fixed, L_spa, L_exact, exp(as.numeric(fit$delay_mu)), exp(rmu),
              (A_total + W_total) / max(abs_delta, 1)))
}

prof <- do.call(rbind, rows)
prof$rel_spa   <- prof$L_spa   - max(prof$L_spa)
prof$rel_exact <- prof$L_exact - max(prof$L_exact)
cat("\n=== D. Profile likelihood in p (Texas), relative to each profile's max ===\n")
print(prof[, c("p", "rel_spa", "rel_exact", "gD_median", "gC_median", "gC_mass_lag1",
               "mu_median", "A_total", "W_total", "churn", "conv")],
      row.names = FALSE, digits = 4)
cat(sprintf("\nmax |rel_spa - rel_exact| across the grid = %.4f nats\n",
            max(abs(prof$rel_spa - prof$rel_exact))))
saveRDS(prof, "devel/spa_diagnostics/profile_texas.rds")
