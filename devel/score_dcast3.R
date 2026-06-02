# =============================================================================
# score_diseasenowcasting.R — WIS + coverage: diseasenowcasting vs NobBS vs Epinowcast
# =============================================================================
# Loads the diseasenowcasting nowcasts (devel/results/<disease>_diseasenowcasting_steps.rds) and the
# existing comparison nowcasts (NobBS + Epinowcast, from diseasenowcast2), pools
# them, and scores the d*=0 nowcast (event AT date_run vs its eventual `final`)
# with scoringutils on a COMMON date set — the same methodology as
# diseasenowcast2/devel/plots_wis.R::score_nowcast().  Reports WIS, its
# over/under/dispersion decomposition, and 50%/90% interval coverage per model.
#
#   Rscript devel/score_diseasenowcasting.R
#   CMP_DIR=/path/to/diseasenowcast2/devel/results LAGS="0 1 2" Rscript devel/score_diseasenowcasting.R
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(scoringutils); library(cli)
})
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

OWN_DIR <- "devel/results"
CMP_DIR <- Sys.getenv("CMP_DIR", "/Users/rodzepeda/Documents/diseasenowcast2/devel/results")
USE_COMMON_SET <- as.logical(Sys.getenv("USE_COMMON_SET", "TRUE"))
LAGS <- as.integer(strsplit(Sys.getenv("LAGS", "0"), "[ ,]+")[[1]])

.to_long_quantile <- function(df, ev) {
  qn <- c("q2.5","q5","q10","q25","q50","q75","q90","q95","q97.5")
  df %>% pivot_longer(all_of(qn), names_to = "quantile", values_to = "predicted") %>%
    mutate(quantile = c(q2.5=.025,q5=.05,q10=.1,q25=.25,q50=.5,q75=.75,q90=.9,q95=.95,q97.5=.975)[quantile])
}

score_nowcast <- function(all_steps, ev, lag = 0L, period_days = 1L, use_common = USE_COMMON_SET) {
  evs <- as.symbol(ev)
  all_steps <- all_steps %>% mutate(final = replace_na(final, 0))
  tgt <- all_steps %>%
    filter(round(as.numeric(date_run - !!evs)) == lag * period_days, !is.na(median))
  if (use_common) {
    nmod <- tgt %>% distinct(epidemic_label, nb_label, delay_label) %>% nrow()
    keep <- tgt %>% distinct(epidemic_label, nb_label, delay_label, date_run) %>%
      count(date_run) %>% filter(n == nmod) %>% pull(date_run)
    tgt <- tgt %>% filter(date_run %in% keep)
  }
  if (nrow(tgt) == 0) return(NULL)
  tgt %>% .to_long_quantile(ev) %>%
    as_forecast_quantile(observed = "final", predicted = "predicted", quantile_level = "quantile",
      forecast_unit = c("t", "date_run", "epidemic_label", "nb_label", "delay_label")) %>%
    score() %>%
    summarise_scores(by = c("epidemic_label", "nb_label", "delay_label"))
}

score_disease <- function(disease, period_days) {
  own_p <- file.path(OWN_DIR, paste0(disease, "_diseasenowcasting_steps.rds"))
  cmp_p <- file.path(CMP_DIR, paste0(disease, "_comparison_steps.rds"))
  if (!file.exists(own_p)) { cli_alert_warning("{disease}: no diseasenowcasting steps ({own_p}) — skip"); return(invisible()) }
  if (!file.exists(cmp_p)) { cli_alert_warning("{disease}: no comparison steps ({cmp_p}) — skip"); return(invisible()) }
  own <- read_rds(own_p); cmp <- read_rds(cmp_p)
  ev  <- own$event_date_col
  all_steps <- bind_rows(own$all_steps, cmp$all_steps)

  cli_h1(toupper(disease))
  sc <- score_nowcast(all_steps, ev, lag = 0L, period_days = period_days)
  if (is.null(sc)) { cli_alert_warning("{disease}: no common d*=0 targets"); return(invisible()) }
  tab <- sc %>%
    transmute(model = paste(epidemic_label, delay_label, sep = " / "),
              wis = round(wis, 1), over = round(overprediction, 1),
              under = round(underprediction, 1), disp = round(dispersion, 1),
              cov50 = round(interval_coverage_50, 2), cov90 = round(interval_coverage_90, 2),
              bias = round(bias, 2)) %>%
    arrange(wis)
  print(as.data.frame(tab), row.names = FALSE)

  # Verdict: best diseasenowcasting vs NobBS and best Epinowcast (WIS + coverage).
  is_cmp <- grepl("NobBS|Epinowcast", tab$model)
  best_d <- tab[!is_cmp, ][1, ]
  nobbs  <- tab[grepl("NobBS", tab$model), ][1, ]
  enw    <- tab[grepl("Epinowcast", tab$model), ] %>% arrange(wis) %>% slice(1)
  cli_alert_info(paste0(
    "best diseasenowcasting = {best_d$model}: WIS {best_d$wis} cov90 {best_d$cov90} | ",
    "NobBS WIS {nobbs$wis} cov90 {nobbs$cov90} | ",
    "best ENW {enw$model} WIS {enw$wis} cov90 {enw$cov90}"))
  beats_nobbs <- !is.na(best_d$wis) && !is.na(nobbs$wis) && best_d$wis <= nobbs$wis
  beats_enw   <- !is.na(best_d$wis) && nrow(enw) && best_d$wis <= enw$wis
  cli_alert_success("{disease}: diseasenowcasting beats NobBS = {beats_nobbs}; beats Epinowcast = {beats_enw} (WIS, lower=better)")
  invisible(tab)
}

period_of <- c(dengue = 7L, covid = 1L, mpox = 1L)
for (dz in c("dengue", "covid", "mpox")) score_disease(dz, period_of[[dz]])

if (length(LAGS) > 1 || LAGS[1] != 0L) {
  cli_h1("WIS by lag (d* = lag)")
  for (dz in c("dengue","covid","mpox")) {
    own_p <- file.path(OWN_DIR, paste0(dz,"_diseasenowcasting_steps.rds")); cmp_p <- file.path(CMP_DIR, paste0(dz,"_comparison_steps.rds"))
    if (!file.exists(own_p) || !file.exists(cmp_p)) next
    own <- read_rds(own_p); cmp <- read_rds(cmp_p); ev <- own$event_date_col
    as_ <- bind_rows(own$all_steps, cmp$all_steps); pd <- period_of[[dz]]
    cli_h2(dz)
    for (lg in LAGS) { s <- score_nowcast(as_, ev, lag = lg, period_days = pd)
      if (!is.null(s)) { best <- s %>% arrange(wis) %>% slice(1)
        cat(sprintf("  lag %d: best=%s/%s WIS=%.1f\n", lg, best$epidemic_label, best$delay_label, best$wis)) } }
  }
}
