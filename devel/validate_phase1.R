# =============================================================================
# Phase 1 oracle gate (dcast3): delay-only LogNormal vs the FIXED Stan engine.
#
# Compares dcast3::fit() (RTMB, per-time censoring) against the installed
# diseasenowcast2 fit_internal(delay_only = TRUE) MAP on windowed COVID data.
# Gate: worst |Δ|/|Stan| <= 2% on delay_mu and delay_sigma.
#
#   N_DATES=5 Rscript devel/validate_phase1.R   # from ~/Documents/dcast3
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(tbl.now); library(diseasenowcast2)   # Stan oracle
})
devtools::load_all(quiet = TRUE)                 # dcast3 (RTMB)

N_DATES <- as.integer(Sys.getenv("N_DATES", "5"))
DELAY_WINDOW <- as.integer(Sys.getenv("DELAY_WINDOW", "120"))
D2 <- "/Users/rodzepeda/Documents/diseasenowcast2"

covid_raw <- read_rds(file.path(D2, "devel/covid_colombia_aggregated.rds"))
tbl_covid <- covid_raw %>%
  group_by(notification_date, diagnosis_date) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  tbl_now(event_date = notification_date, report_date = diagnosis_date,
          case_count = n, data_type = "count-incidence") %>%
  add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
  compute_temporal_effects()
EV <- "notification_date"
alld <- sort(unique(readRDS(file.path(D2, "devel/results/covid_comparison_steps.rds"))$all_steps$date_run),
             decreasing = TRUE)
DATES <- alld[round(seq(1, length(alld), length.out = N_DATES))]

build_window <- function(dr, W = DELAY_WINDOW) {
  gmin  <- tbl_covid %>% as_tibble() %>% summarise(m = min(!!as.symbol(EV))) %>% pull(m)
  since <- max(gmin, dr - days(W - 1))
  mt <- suppressWarnings(tbl_covid %>%
    filter(!!as.symbol(EV) <= dr, !!as.symbol(EV) >= since,
           !!as.symbol(get_report_date(tbl_covid)) <= dr))
  max_time <- as.numeric(dr - since + 1)
  d_star <- matrix(rev(seq_len(max_time)) - 1L, ncol = 1L)
  m <- mt %>% as_tibble() %>%
    mutate(.event_num = as.integer(difftime(!!as.symbol(EV), since, units = "days"))) %>%
    mutate(.delay = .delay + 1L, .event_num = .event_num + 1L) %>%
    select(.event_num, n, .delay) %>% arrange(.event_num, .delay) %>%
    as.matrix() %>% cbind(1L)
  list(m = m, max_time = max_time, d_star = d_star)
}

FAMILY <- Sys.getenv("FAMILY", "lognormal")   # lognormal | gamma | gengamma
delay_s <- switch(FAMILY,
  lognormal = diseasenowcast2::lognormal_delay(),
  gamma     = diseasenowcast2::gamma_delay(),
  gengamma  = diseasenowcast2::generalized_gamma_delay())
delay_r <- switch(FAMILY,
  lognormal = dcast3::lognormal_delay(),
  gamma     = dcast3::gamma_delay(),
  gengamma  = dcast3::generalized_gamma_delay())
mdl_stan <- diseasenowcast2::model(diseasenowcast2::nb_likelihood(),
                                   diseasenowcast2::hsgp_epidemic(), delay_s)
mdl_r <- dcast3::model(dcast3::nb_likelihood(), dcast3::hsgp_epidemic(), delay_r)
cat("FAMILY:", FAMILY, "\n")

rows <- list()
for (di in seq_along(DATES)) {
  dr <- DATES[di]; B <- build_window(dr)
  cat(sprintf("[%d/%d] %s (max_time=%d)\n", di, length(DATES), format(dr), B$max_time))

  # --- Stan oracle (FIXED): delay-only MAP ---
  sd1 <- diseasenowcast2::data_to_stan(mdl_stan, B$m, max_time = B$max_time,
                                       d_star = B$d_star, delay_only = TRUE)
  pr1 <- diseasenowcast2::default_priors(mdl_stan, sd1)
  # Assert the prior families dcast3 assumes, so a future prior change fails
  # loudly here instead of silently mis-scoring. dist codes: 1=Normal (delay_mu),
  # 105=Gamma (delay_sigma); see prior_lpdf().
  stopifnot(pr1$prior_delay_param_1_dist %in% c(0L, 1L),       # Std/Normal log-mean
            pr1$prior_delay_param_2_dist == 105L)              # Gamma scale/SD
  sfit <- tryCatch(diseasenowcast2::fit_internal(mdl_stan, sd1, priors = pr1,
                                                 seed = 23875L, timeout = 90),
                   error = function(e) NULL)
  if (is.null(sfit)) { cat("  Stan NULL — skip\n"); next }
  mu_col  <- if (FAMILY == "gamma") "delay_mu_gamma[1]" else "delay_mu[1]"
  sig_col <- if (FAMILY == "gengamma") "delay_sigma_gengamma[1]" else "delay_sigma[1]"
  mode_tbl <- tryCatch(sfit$mode()$draws(format = "matrix"), error = function(e) NULL)
  if (is.null(mode_tbl) || !all(c(mu_col, sig_col) %in% colnames(mode_tbl))) { cat("  Stan mode NA — skip\n"); next }
  stan_mu    <- as.numeric(mode_tbl[1, mu_col])
  stan_sigma <- as.numeric(mode_tbl[1, sig_col])

  # --- dcast3 RTMB ---
  dat <- dcast3::prepare_data(mdl_r, B$m, max_time = B$max_time,
                              d_star = B$d_star, delay_only = TRUE)
  pr  <- dcast3::default_priors(mdl_r, dat)
  rf  <- dcast3::fit(mdl_r, dat, priors = pr)

  rel_mu  <- abs(rf$delay_mu - stan_mu) / abs(stan_mu + 1e-8)
  rel_sig <- abs(rf$delay_sigma - stan_sigma) / abs(stan_sigma + 1e-8)
  cat(sprintf("  Stan : mu=%.4f sigma=%.4f | RTMB: mu=%.4f sigma=%.4f | rel: %.2e / %.2e\n",
              stan_mu, stan_sigma, rf$delay_mu, rf$delay_sigma, rel_mu, rel_sig))
  rows[[length(rows) + 1]] <- tibble(date_run = dr, max_time = B$max_time,
                                     stan_mu, stan_sigma,
                                     rtmb_mu = rf$delay_mu, rtmb_sigma = rf$delay_sigma,
                                     rel_mu, rel_sig)
}

out <- if (length(rows)) bind_rows(rows) else tibble()
# Persist the artifact so committed numbers reflect the current (fixed) oracle.
saveRDS(out, sprintf("devel/validate_phase1_%s.rds", FAMILY))
cat("\n──────── summary ────────\n"); print(out)
if (nrow(out)) {
  cat(sprintf("\nWorst rel: mu=%.2e sigma=%.2e | gate(<=2%%): %s\n",
              max(out$rel_mu), max(out$rel_sig),
              if (max(out$rel_mu) <= 0.02 && max(out$rel_sig) <= 0.02) "PASS" else "FAIL"))
}
