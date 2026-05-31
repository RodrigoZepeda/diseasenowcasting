# =============================================================================
# Joint-fit parity gate (dcast3 vs Stan): HSGP + NB + LogNormal nowcast at d*=0
# on real COVID data with day-of-week covariates.
#
#   N_DATES=2 Rscript devel/validate_joint.R   # from ~/Documents/dcast3
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(lubridate); library(stringr)
  library(tbl.now); library(diseasenowcast2); library(posterior)
})
devtools::load_all(quiet = TRUE)
D2 <- "/Users/rodzepeda/Documents/diseasenowcast2"
N_DATES <- as.integer(Sys.getenv("N_DATES", "2"))
qprobs <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)

covid_raw <- read_rds(file.path(D2, "devel/covid_colombia_aggregated.rds"))
tbl_covid <- covid_raw %>% group_by(notification_date, diagnosis_date) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  tbl_now(event_date = notification_date, report_date = diagnosis_date,
          case_count = n, data_type = "count-incidence") %>%
  add_temporal_effects(temporal_effects(day_of_week = TRUE)) |> compute_temporal_effects()
EV <- "notification_date"
alld <- sort(unique(readRDS(file.path(D2, "devel/results/covid_comparison_steps.rds"))$all_steps$date_run))
# Short early series (max_time ~150-250) where both Laplace engines converge cleanly.
idx <- unique(pmin(length(alld), round(seq(4, 10, length.out = N_DATES))))
DATES <- alld[idx]

dow_X <- function(min_date, max_time) {
  dts <- seq(min_date, min_date + days(max_time - 1), by = "1 day")
  dws <- ((wday(min_date) - 2) %% 7) + 1
  X <- tibble(d = dts) %>% mutate(day_of_week = wday(d, week_start = dws), ones = 1) %>%
    pivot_wider(id_cols = d, names_from = day_of_week, values_from = ones, values_fill = 0) %>%
    select(-d) %>% select(matches(as.character(1:7))) %>% as.matrix()
  if (ncol(X) == 7) X <- X[, -ncol(X)]
  X
}
prep <- function(dr) {
  dr <- as.Date(dr)
  min_date <- as.Date(tbl_covid %>% as_tibble() %>% summarise(m = min(!!as.symbol(EV))) %>% pull(m))
  mt <- suppressWarnings(tbl_covid %>% filter(!!as.symbol(EV) <= dr,
        !!as.symbol(get_report_date(tbl_covid)) <= dr))
  max_time <- as.numeric(dr - min_date + 1); d_star <- matrix(rev(seq_len(max_time)) - 1L, ncol = 1L)
  m <- mt %>% mutate(.delay = .delay + 1L, .event_num = .event_num + 1L) %>% as_tibble() %>%
    select(.event_num, n, .delay) %>% arrange(.event_num, .delay) %>% as.matrix() %>% cbind(1L)
  list(m = m, max_time = max_time, d_star = d_star, X = dow_X(min_date, max_time))
}

mdl_s <- diseasenowcast2::model(diseasenowcast2::nb_likelihood(), diseasenowcast2::hsgp_epidemic(),
                                diseasenowcast2::lognormal_delay())
mdl_r <- dcast3::model(dcast3::nb_likelihood(), dcast3::hsgp_epidemic(), dcast3::lognormal_delay())
PHI_s <- diseasenowcast2::lognormal_prior(log(20), 0.5)
PHI_r <- dcast3::lognormal_prior(log(20), 0.5)

for (dr in DATES) {
  P <- prep(dr); Tn <- P$max_time
  cat(sprintf("\n=== %s (max_time=%d) ===\n", format(dr), Tn))

  # Stan joint + GQ nowcast
  sd <- diseasenowcast2::data_to_stan(mdl_s, P$m, X = P$X, max_time = Tn, d_star = P$d_star)
  prs <- diseasenowcast2::default_priors(mdl_s, sd, phi = PHI_s)
  rs <- tryCatch(diseasenowcast2::fit(mdl_s, sd, priors = prs, seed = 23875L, timeout = 300,
                                      tolerance_epidemic = max(30000, 50 * max(tapply(P$m[,2], P$m[,1], sum)))),
                 error = function(e) NULL)
  stan_q <- NULL
  if (!is.null(rs)) {
    gq <- tryCatch(diseasenowcast2::gq_internal(rs, seed = 4242L), error = function(e) NULL)
    if (!is.null(gq)) {
      dm <- tryCatch(diseasenowcast2::draws(gq, variables = "nowcast", format = "matrix"), error = function(e) NULL)
      if (!is.null(dm)) {
        idx <- as.integer(str_remove_all(colnames(dm), "nowcast\\[|\\]")); col <- which.max(idx)
        v <- as.numeric(dm[, col]); v <- v[is.finite(v)]
        stan_q <- quantile(v, qprobs)
      }
    }
  }

  # dcast3 joint + nowcast
  dat <- dcast3::prepare_data(mdl_r, P$m, X = P$X, max_time = Tn, d_star = P$d_star)
  prr <- dcast3::default_priors(mdl_r, dat, phi = PHI_r)
  rr <- tryCatch(dcast3::fit(mdl_r, dat, priors = prr), error = function(e) NULL)
  r_q <- if (!is.null(rr)) dcast3::nowcast(rr, n_draws = 2000, seed = 1)$quantiles else NULL

  cat("Stan  nowcast q:", if (is.null(stan_q)) "NULL" else paste(round(stan_q), collapse=" "), "\n")
  cat("dcast3 nowcast q:", if (is.null(r_q)) "NULL" else paste(round(r_q), collapse=" "), "\n")
  if (!is.null(stan_q) && !is.null(r_q)) {
    relmed <- abs(stan_q[5] - r_q[5]) / abs(stan_q[5] + 1)
    cat(sprintf("median Stan=%.0f dcast3=%.0f  rel=%.2f\n", stan_q[5], r_q[5], relmed))
  }
}
