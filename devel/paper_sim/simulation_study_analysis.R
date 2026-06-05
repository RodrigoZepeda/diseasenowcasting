# =============================================================================
# Simulation study -- ANALYSIS  (diseasenowcasting, RTMB package)
# =============================================================================
# Uses only the exported package API: tbl_now(), model(), nowcast(), predict(),
# summary(), and surprise().
#
# For each analysis day t and EVERY (epidemic x likelihood x delay) configuration
# we fit ONE nowcast and reuse it for two things:
#   (1) the nowcast quantiles vs the eventual truth  -> WIS + the paper figures;
#   (2) the surprise of the reporting delays that first become observable the
#       NEXT day (report == t + 1) under THAT fit -> a per-model anomaly score.
#       Because every configuration is scored, we get an ROC / AUC / sensitivity
#       / specificity for the delay-backlog detector FOR EACH MODEL, not just the
#       recommended one.  Ground truth is the `perturbed` backlog flag.
#
# The surprise MUST be computed inside the worker: the RTMB fit object holds
# external pointers that do not survive serialisation between processes, so a
# fitted nowcast cannot be shipped back and scored later.
#
# Outputs (devel/paper_sim/simulations/):
#   plot_final_sims_complete_with_wis.pdf  final nowcasts + WIS  (paper figure)
#   wis_pt.pdf                             WIS by lag             (paper figure)
#   wis.csv                                WIS summary table
#   roc_surprise.pdf                       ROC per model (faceted)
#   surprise_metrics.csv                   AUC / sensitivity / specificity per model
#   surprise_df.rds / .csv                 per-observation surprise scores (all models)
#   day_surprise.rds                       per-model, per-event-day surprise
#   all_steps.rds                          tidy nowcasts
#
# Run from the package root (slow):
#   WORKERS=8 Rscript devel/paper_sim/simulation_study_analysis.R
#   SMOKE=TRUE  WORKERS=4 Rscript devel/paper_sim/simulation_study_analysis.R   # quick check
# =============================================================================

library(diseasenowcasting)
library(tbl.now)
library(tidyverse)
library(scoringutils)
library(ggh4x)
library(pROC)

# ── Toggles ──────────────────────────────────────────────────────────────────
SMOKE   <- as.logical(Sys.getenv("SMOKE", "FALSE"))   # SMOKE=TRUE for a quick check
OUT_DIR <- "devel/paper_sim/simulations"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

N_DRAWS        <- if (SMOKE) 200L else 1000L
K_IMPUTE       <- if (SMOKE) 10L  else 25L   # two-stage delay imputations
SURPRISE_DRAWS <- if (SMOKE) 150L else 400L
SURPRISE_LEVEL <- 0.99                  # flag a delay when P(D >= d) < 1 - level
# Fit type is per delay family: TWO-STAGE everywhere (it re-injects delay
# uncertainty -> wider, better-calibrated intervals = decent coverage), EXCEPT
# the Dirichlet (non-parametric) delay, which uses one-stage (its two-stage
# simplex imputation is unstable here).
fit_type_for <- function(delay_label) if (delay_label == "Dirichlet") "one_stage" else "two_stage"
MAX_LAG        <- 15L                   # nowcast horizon scored for WIS-by-lag
TABLE_LAG      <- 7L                    # horizon averaged in the LaTeX WIS table

# ── Data ─────────────────────────────────────────────────────────────────────
# The simulation indexes time by integer day; the package derives the time grid
# from real Dates, so we map day d -> ORIGIN + d.
ORIGIN       <- as.Date("2020-01-01")
sim          <- readRDS("devel/paper_sim/SIR_sims.rds")
cases        <- sim$cases
perturb_days <- sim$perturb_days

tbl_full <- cases %>%
  transmute(t = ORIGIN + as.integer(floor_t), report = ORIGIN + as.integer(report),
            n = as.integer(n)) %>%
  tbl_now(event_date = t, report_date = report, case_count = n,
          data_type = "count-incidence", verbose = FALSE)

event_col  <- get_event_date(tbl_full)
report_col <- get_report_date(tbl_full)
truth_tbl  <- tbl_full %>% get_latest_reported_cases() %>%
  as_tibble() %>% transmute(.event_num, final = n)

# Per-(event, report) perturbation truth, for labelling newly-observable delays.
cell_truth <- cases %>%
  transmute(event_time = as.integer(floor_t), report = as.integer(report),
            n = as.integer(n), perturbed = perturbed)

# Analysis days.  The epidemic runs ~0..135; we nowcast from day 8 onward.
LAST_DAY <- as.integer(max(cell_truth$event_time))
DATES    <- if (SMOKE) seq(20L, 130L, by = 10L) else seq(8L, LAST_DAY)

# ── Configuration grid (the 18 paper models) ─────────────────────────────────
# N_pop matches the data-generating population (see simulation_study.R::N_POP).
SIR_N_POP <- sim$params$N_pop
EPI <- tribble(
  ~epidemic_label, ~epidemic,
  "HSGP", quote(hsgp_epidemic()),
  "AR1",  quote(ar1_epidemic()),
  "SIR",  bquote(sir_epidemic(N_pop = .(SIR_N_POP), use_beta_rw_trend = FALSE))
)
LIK <- tribble(
  ~nb_label,  ~likelihood,
  "NegBin",  quote(nb_likelihood()),
  "Poisson", quote(poisson_likelihood())
)
DLY <- tribble(
  ~delay_label,       ~delay,                          ~delay_color,
  "Lognormal",        quote(lognormal_delay()),         "#b75347",
  "GeneralizedGamma", quote(generalized_gamma_delay()), "#edc775",
  "Dirichlet",        quote(dirichlet_delay()),         "#94b594"
)
delay_colors <- setNames(DLY$delay_color, DLY$delay_label)

CONFIGS <- tidyr::crossing(EPI, LIK, DLY) %>%
  mutate(full_label = paste(epidemic_label, nb_label, delay_label, sep = "_"))

build_model <- function(cfg) {
  model(likelihood = eval(cfg$likelihood[[1]]),
        epidemic   = eval(cfg$epidemic[[1]]),
        delay      = eval(cfg$delay[[1]]))
}

# ── Delay tail-probability P(D >= d) for the surprise ROC ────────────────────
# Mean posterior tail probability P(D >= d) for a set of delays, via the exported
# surprise() (which now scores every delay family, including the Dirichlet
# non-parametric delay).  Returns one value per delay, in order, or NA when the
# score is unavailable.
delay_tail_prob <- function(nc, delays, n_draws, seed = 284675L) {
  delay_surprise <- tryCatch(
    diseasenowcasting::surprise(nc, new_data = data.frame(delay = delays),
      type = "delay", level = SURPRISE_LEVEL, n_draws = n_draws, seed = seed)$delay_surprise,
    error = function(e) NULL)
  if (!is.null(delay_surprise) && nrow(delay_surprise) == length(delays))
    delay_surprise$mean_tail_prob
  else
    rep(NA_real_, length(delays))
}

# ── Plot infrastructure ──────────────────────────────────────────────────────
theme_paper <- function(base_size = 10) {
  theme_linedraw(base_size = base_size) +
    theme(strip.background = element_rect(fill = "#224b5e", color = NA),
          strip.text       = element_text(color = "white", face = "bold", size = 10),
          legend.position  = "bottom", legend.title = element_text(face = "bold"),
          legend.key.height = unit(0.35, "cm"), legend.text = element_text(size = 10),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title    = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(color = "grey40", size = 12))
}
facet_labels <- labeller(
  delay_label    = c(Dirichlet = "Non-parametric", GeneralizedGamma = "Generalized-Gamma",
                     Lognormal = "Lognormal"),
  epidemic_label = c(SIR = "Epidemic: SIR", HSGP = "Epidemic: HSGP", AR1 = "Epidemic: AR(1)"),
  nb_label       = c(Poisson = "Likelihood: Poisson", NegBin = "Likelihood: Negative Binomial"))
strip_background <- strip_nested(
  text_x = elem_list_text(colour = "white", face = "bold"),
  background_y = elem_list_rect(fill = c("#224b5e", "#224b5e",
    rep(c(Dirichlet = delay_colors[["Dirichlet"]],
          GeneralizedGamma = delay_colors[["GeneralizedGamma"]],
          Lognormal = delay_colors[["Lognormal"]]), 2))))

QMAP <- c(q2.5 = 0.025, q5 = 0.05, q10 = 0.10, q25 = 0.25, q50 = 0.50,
          q75 = 0.75, q90 = 0.90, q95 = 0.95, q97.5 = 0.975)

# =============================================================================
# MAIN LOOP -- one worker per analysis day
# =============================================================================
suppressMessages({ library(future); library(doFuture); library(foreach) })
WORKERS <- as.integer(Sys.getenv("WORKERS", max(parallel::detectCores() - 1, 1)))
plan(multisession, workers = WORKERS)

run_one_date <- function(date_run) {
  suppressMessages({
    library(diseasenowcasting); library(tbl.now); library(dplyr); library(tidyr); library(methods)
  })
  now_date  <- ORIGIN + date_run
  model_tbl <- suppressWarnings(
    tbl_full %>% filter(.data[[event_col]] <= now_date, .data[[report_col]] <= now_date))
  obs_now   <- model_tbl %>% get_latest_reported_cases() %>%
    as_tibble() %>% transmute(.event_num, n)

  # Reports that FIRST become observable the next day -- scored for surprise by
  # every model's fit (which never saw them).  Same set for all configurations.
  new_cells <- cell_truth %>%
    filter(report == date_run + 1L, event_time <= date_run) %>%
    mutate(delay = report - event_time) %>%
    group_by(event_time, delay, perturbed) %>%
    summarise(n = sum(n), .groups = "drop")

  # -- fit each configuration once; reuse it for the nowcast AND the surprise ---
  per_cfg <- lapply(seq_len(nrow(CONFIGS)), function(i) {
    cfg <- CONFIGS[i, ]
    nc  <- tryCatch(
      nowcast(model_tbl, model = build_model(cfg), type = fit_type_for(cfg$delay_label),
              K = K_IMPUTE, now = now_date, n_draws = N_DRAWS, temporal_effects = "none", seed = 1),
      error = function(e) NULL)
    if (is.null(nc)) return(list(nowcast = NULL, surprise = NULL))

    nowc <- summary(predict(nc, seed = 2)) %>% as.data.frame() %>%
      transmute(.event_num, median, mean, q2.5, q5, q10, q25, q50, q75, q90, q95, q97.5) %>%
      mutate(epidemic_label = cfg$epidemic_label, nb_label = cfg$nb_label,
             delay_label = cfg$delay_label, date_run = date_run)

    surp <- NULL
    if (nrow(new_cells) > 0) {
      tp <- tryCatch(delay_tail_prob(nc, new_cells$delay, n_draws = SURPRISE_DRAWS),
                     error = function(e) NULL)
      if (!is.null(tp))
        surp <- new_cells %>%
          mutate(epidemic_label = cfg$epidemic_label, nb_label = cfg$nb_label,
                 delay_label = cfg$delay_label, fit_date = date_run,
                 report_date = date_run + 1L,
                 mean_tail_prob = tp,
                 is_long = tp < (1 - SURPRISE_LEVEL))
    }
    list(nowcast = nowc, surprise = surp)
  })

  nc_rows <- compact(lapply(per_cfg, `[[`, "nowcast"))
  nowcasts <- if (length(nc_rows) == 0L) NULL else
    bind_rows(nc_rows) %>%
      left_join(obs_now,   by = ".event_num") %>%
      left_join(truth_tbl, by = ".event_num") %>%
      mutate(n = replace_na(n, 0), final = replace_na(final, 0))

  list(nowcasts = nowcasts,
       surprise = bind_rows(compact(lapply(per_cfg, `[[`, "surprise"))))
}

results <- foreach(d = DATES, .options.future = list(seed = TRUE)) %dofuture% run_one_date(d)
plan(sequential)

final_nowcasts <- compact(lapply(results, `[[`, "nowcasts"))
surprise_rows  <- compact(lapply(results, `[[`, "surprise"))

# =============================================================================
# WIS + FINAL FIGURES
# =============================================================================
all_steps <- bind_rows(final_nowcasts) %>%
  # lag 0 = the censored nowcast edge (robust to the date origin offset).
  group_by(date_run, epidemic_label, nb_label, delay_label) %>%
  mutate(t = .event_num, lag = max(.event_num) - .event_num) %>%
  ungroup()
saveRDS(all_steps, file.path(OUT_DIR, "all_steps.rds"))

compute_wis <- function(steps, lag_val = 0L) {
  steps %>%
    filter(lag == lag_val) %>%
    pivot_longer(any_of(names(QMAP)), names_to = "qn", values_to = "predicted") %>%
    mutate(quantile_level = QMAP[qn]) %>% filter(!is.na(quantile_level)) %>%
    as_forecast_quantile(observed = "final", predicted = "predicted",
                         quantile_level = "quantile_level",
                         forecast_unit = c("t", "epidemic_label", "nb_label", "delay_label")) %>%
    scoringutils::score()
}

wis_scores <- compute_wis(all_steps, 0L)

wis_scores %>%
  summarise_scores(by = c("epidemic_label", "nb_label", "delay_label")) %>%
  ungroup() %>%
  arrange(wis) %>%
  mutate(across(c(wis, overprediction, underprediction, dispersion, bias,
                  interval_coverage_50, interval_coverage_90, ae_median),
                ~ scales::comma(.x, 0.01))) %>%
  write_excel_csv(file.path(OUT_DIR, "wis.csv"))

# ── Final nowcasts (lag 0) with the WIS overlaid -- the paper figure ─────────
plot_final <- all_steps %>%
  filter(lag == 0) %>%
  ggplot() +
  geom_ribbon(aes(t, ymin = q5, ymax = q95, fill = delay_label), alpha = 0.5, linewidth = 0.35) +
  geom_line(aes(t, median, color = delay_label, linetype = "Nowcast's median")) +
  geom_point(aes(t, final, shape = "Truth"), color = "gray15", fill = "white",
             stroke = 0.9, alpha = 0.5, size = 0.5) +
  geom_line(aes(t, wis, linetype = "WIS"), linewidth = 0.25, data = wis_scores) +
  facet_nested(cols = vars(epidemic_label), rows = vars(nb_label, delay_label),
               strip = strip_background, labeller = facet_labels) +
  scale_fill_manual("Delay model", values = delay_colors) +
  scale_color_manual("Delay model", values = delay_colors) +
  scale_shape_manual("Observations", values = c(Truth = 21)) +
  scale_linetype_manual("Information", values = c("WIS" = "solid", "Nowcast's median" = "dashed")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, NA)) +
  labs(title = "Nowcasts produced at each notification time with 95% credible interval",
       subtitle = "Simulation from a control-then-release SIR model with Weibull-distributed delays",
       x = "Notification time", y = "Cases",
       caption = "HSGP: Hilbert Space GP | SIR: discrete SIR | AR(1): Autoregressive of order 1") +
  theme_paper(base_size = 12)
ggsave(file.path(OUT_DIR, "plot_final_sims_complete_with_wis.pdf"),
       plot = plot_final, width = 12, height = 12)

# ── WIS by lag ───────────────────────────────────────────────────────────────
summary_wis <- map_dfr(0:MAX_LAG, function(lag_val) {
  compute_wis(all_steps, lag_val) %>%
    summarise_scores(by = c("epidemic_label", "nb_label", "delay_label")) %>%
    mutate(lag = lag_val)
})
wis_lag_plt <- ggplot(summary_wis) +
  geom_line(aes(lag, wis, color = nb_label)) + geom_point(aes(lag, wis, color = nb_label)) +
  facet_grid(rows = vars(epidemic_label), cols = vars(delay_label), labeller = facet_labels) +
  scale_color_manual("Likelihood", values = c(NegBin = "#224b5e", Poisson = "#b75347")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, NA)) +
  labs(title = "Mean weighted interval score (WIS) by distance from current date",
       subtitle = "Nowcast at each notification time for date = (notification time - lag)",
       x = "Lag", y = "WIS") +
  theme_paper()
ggsave(file.path(OUT_DIR, "wis_pt.pdf"), plot = wis_lag_plt, width = 8, height = 6)

# =============================================================================
# SURPRISE: per-model ROC, sensitivity, specificity
# =============================================================================
# Every model scored the same newly-observable delays, so we can compare their
# backlog-detection performance.  Detection is evaluated at the EVENT-DAY level
# (the natural unit for a reporting backlog): a day's score is its most
# surprising arriving report, min_t P(D >= d); the day is flagged when that
# minimum falls below 1 - level.  Ground truth = the day was perturbed.
surprise_df <- bind_rows(surprise_rows)
saveRDS(surprise_df, file.path(OUT_DIR, "surprise_df.rds"))
write_csv(surprise_df, file.path(OUT_DIR, "surprise_df.csv"))

CFG_KEYS <- c("epidemic_label", "nb_label", "delay_label")

day_surprise <- surprise_df %>%
  group_by(across(all_of(c(CFG_KEYS, "event_time")))) %>%
  summarise(min_tail_prob = min(mean_tail_prob),     # most surprising report
            perturbed     = any(perturbed),
            n_reports     = n(), .groups = "drop") %>%
  mutate(is_flagged = min_tail_prob < (1 - SURPRISE_LEVEL))
saveRDS(day_surprise, file.path(OUT_DIR, "day_surprise.rds"))

cat(sprintf("\nSurprise: ~%d delays scored by each of %d models over %d event-days.\n",
            as.integer(round(nrow(surprise_df) / nrow(CONFIGS))), nrow(CONFIGS),
            dplyr::n_distinct(day_surprise$event_time)))

# -- Per-model AUC / sensitivity / specificity + ROC curve points -------------
roc_one <- function(df) {
  # df: one model's per-day surprise. Returns metrics + the ROC path.
  if (dplyr::n_distinct(df$perturbed) < 2)
    return(list(metrics = NULL, roc = NULL))
  score   <- 1 - df$min_tail_prob                   # higher = more surprising
  roc_obj <- pROC::roc(response = df$perturbed, predictor = score,
                       direction = "<", quiet = TRUE)
  sens <- sum(df$is_flagged & df$perturbed)   / sum(df$perturbed)
  spec <- sum(!df$is_flagged & !df$perturbed) / sum(!df$perturbed)
  list(
    metrics = tibble(auc = as.numeric(pROC::auc(roc_obj)),
                     sensitivity = sens, specificity = spec,
                     n_days = nrow(df), n_perturbed = sum(df$perturbed)),
    roc = tibble(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities) %>%
            arrange(fpr, tpr))
}

surprise_metrics <- list(); roc_paths <- list()
for (i in seq_len(nrow(CONFIGS))) {
  cfg <- CONFIGS[i, ]
  df  <- day_surprise %>% semi_join(cfg, by = CFG_KEYS) %>%
         inner_join(cfg[CFG_KEYS], by = CFG_KEYS)
  r <- roc_one(df)
  if (!is.null(r$metrics)) {
    surprise_metrics[[i]] <- bind_cols(cfg[CFG_KEYS], r$metrics)
    roc_paths[[i]]        <- bind_cols(cfg[CFG_KEYS], r$roc)
  }
}
surprise_metrics <- bind_rows(surprise_metrics) %>% arrange(desc(auc))
roc_paths        <- bind_rows(roc_paths)

write_csv(surprise_metrics %>%
            mutate(across(c(auc, sensitivity, specificity), ~ round(.x, 3))),
          file.path(OUT_DIR, "surprise_metrics.csv"))

cat("\nDelay-backlog detection (top models by AUC):\n")
print(as.data.frame(head(surprise_metrics, 6)), row.names = FALSE)

# -- Faceted ROC: one panel per epidemic x likelihood, coloured by delay model -
if (nrow(roc_paths) > 0) {
  auc_lab <- surprise_metrics %>%
    mutate(lab = sprintf("%s: AUC %.2f", delay_label, auc))
  roc_plt <- ggplot(roc_paths, aes(fpr, tpr, color = delay_label)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
    geom_path(linewidth = 0.9) +
    facet_nested(cols = vars(epidemic_label), rows = vars(nb_label),
                 labeller = facet_labels) +
    scale_color_manual("Delay model", values = delay_colors) +
    coord_equal() +
    labs(title = "Detecting reporting-delay backlogs, by model",
         subtitle = "Event-day flagged when its most surprising report has P(D >= d) below the level",
         x = "False positive rate (1 - specificity)",
         y = "True positive rate (sensitivity)") +
    theme_paper(base_size = 12)
  ggsave(file.path(OUT_DIR, "roc_surprise.pdf"), plot = roc_plt, width = 11, height = 8)
}

# =============================================================================
# WIS SUMMARY TABLE (LaTeX) -- averaged over the nowcast horizon (lags 0..TABLE_LAG)
# =============================================================================
# Writes `wis_table.tex` (a complete table, ready to \input) and the underlying
# `wis_table_metrics.csv`.  Scores are averaged over the nowcast horizon because
# that is where the likelihood matters: at lag 0 alone, a misspecified epidemic
# model + NB can over-widen at the censored edge.  The table requires the colour
# definitions delayA / delayB / delayC and packages multirow, booktabs, xcolor.
wis_table <- all_steps %>%
  filter(lag <= TABLE_LAG) %>%
  pivot_longer(any_of(names(QMAP)), names_to = "qn", values_to = "predicted") %>%
  mutate(quantile_level = QMAP[qn]) %>% filter(!is.na(quantile_level)) %>%
  as_forecast_quantile(observed = "final", predicted = "predicted",
                       quantile_level = "quantile_level",
                       forecast_unit = c("t", "lag", "epidemic_label", "nb_label", "delay_label")) %>%
  scoringutils::score() %>%
  summarise_scores(by = c("epidemic_label", "nb_label", "delay_label")) %>%
  transmute(epidemic_label, nb_label, delay_label,
            WIS = wis, Over = overprediction, Under = underprediction,
            Disp = dispersion, Bias = bias,
            IC50 = interval_coverage_50, IC90 = interval_coverage_90, AE = ae_median)
write_csv(wis_table %>% arrange(WIS), file.path(OUT_DIR, "wis_table_metrics.csv"))

# -- assemble the LaTeX --------------------------------------------------------
epi_order   <- c("AR1", "HSGP", "SIR")
lik_order   <- c("NegBin", "Poisson")
delay_order <- c("Dirichlet", "GeneralizedGamma", "Lognormal")
epi_disp    <- c(AR1 = "AR(1)", HSGP = "HSGP", SIR = "SIR")
lik_disp    <- c(NegBin = "NB", Poisson = "Poisson")
delay_disp  <- c(Dirichlet = "Dirichlet", GeneralizedGamma = "Gen-Gamma", Lognormal = "Lognormal")
delay_cell  <- c(Dirichlet = "delayC", GeneralizedGamma = "delayB", Lognormal = "delayA")

wt <- wis_table %>%                                   # flag the best (lowest) WIS per block
  group_by(epidemic_label, nb_label) %>%
  mutate(is_best = WIS == min(WIS)) %>% ungroup()

fmt <- function(x) sprintf("%.2f", x)
latex_data_row <- function(r) {
  cc   <- delay_cell[[as.character(r$delay_label)]]
  wis  <- if (isTRUE(r$is_best)) paste0("\\bf ", fmt(r$WIS)) else fmt(r$WIS)
  vals <- c(wis, fmt(r$Over), fmt(r$Under), fmt(r$Disp), fmt(r$Bias),
            fmt(r$IC50), fmt(r$IC90), fmt(r$AE))
  paste0("\\cellcolor{", cc, "!25}", delay_disp[[as.character(r$delay_label)]], " & ",
         paste0("\\cellcolor{", cc, "!25}", vals, collapse = " & "), " \\\\")
}

body <- character(0)
for (ei in seq_along(epi_order)) {
  ep <- epi_order[ei]
  body <- c(body, sprintf("\\multirow{7}{*}{%s}", epi_disp[[ep]]))
  for (li in seq_along(lik_order)) {
    lk <- lik_order[li]
    for (di in seq_along(delay_order)) {
      r <- wt %>% filter(epidemic_label == ep, nb_label == lk, delay_label == delay_order[di])
      prefix <- if (di == 1L) sprintf("& \\multirow{3}{*}{%s}\n& ", lik_disp[[lk]]) else "& & "
      body <- c(body, paste0(prefix, latex_data_row(r)))
    }
    if (li == 1L) body <- c(body, "\\\\")             # blank spacer between NB and Poisson
  }
  body <- c(body, if (ei < length(epi_order)) "\\midrule" else "\\bottomrule")
}

# Caption built from the simulation parameters actually used (sim$params).
p   <- sim$params
mdelay <- p$weibull$scale * gamma(1 + 1 / p$weibull$shape)
caption_note <- sprintf(paste0(
  "Reported values include Weighted Interval Scoring (WIS), Overprediction (Over), ",
  "Underprediction (Under), Dispersion (Disp), Bias, Absolute Error (AE) and Interval ",
  "Coverage (IC) for 50 and 90\\%% credible intervals, averaged over the nowcast horizon ",
  "(lags 0--%d days). Models were fitted against a simulated SIR epidemic with a ",
  "control-then-release transmission rate (basic reproduction number $R_0 = %.1f$, recovery ",
  "rate $\\gamma = %.3f$; transmission reduced by %.0f\\%% over days %d--%d to induce a second ",
  "wave) in a population of $N = %s$ individuals. Daily incident cases were drawn with ",
  "negative-binomial observation noise (size $\\phi = %g$). Delays were simulated using a ",
  "Weibull$(%g, %g)$ distribution with an approximate mean of %.1f days."),
  TABLE_LAG, p$R0, p$gamma, 100 * p$control[["drop"]], p$control[["start"]], p$control[["end"]],
  formatC(p$N_pop, format = "d", big.mark = ","), p$phi,
  p$weibull$shape, p$weibull$scale, mdelay)

tex <- c(
  "\\begin{table}[!htb]", "\\centering",
  "\\caption{Nowcast comparison across different reporting-delay processes, epidemic-processes, and epidemic likelihood specifications for a simulated epidemic outbreak.}",
  "\\tiny", "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabular}{>{\\raggedleft\\arraybackslash}p{0.45in}|r | >{\\raggedleft\\arraybackslash}p{0.85in} | r r r r r r r r}",
  "\\toprule",
  "\\bf Epidemic process & \\bf Likelihood & \\bf Reporting-delay process  & \\bf WIS & \\bf Over & \\bf Under & \\bf Disp & \\bf Bias & \\bf IC 50\\% & \\bf IC 90\\% & \\bf AE \\\\",
  "\\midrule",
  body,
  "\\end{tabular}",
  paste0("\\caption*{\\tiny ", caption_note, "}"),
  "\\label{tab:wissim}", "\\end{table}")
writeLines(tex, file.path(OUT_DIR, "wis_table.tex"))
cat(sprintf("\nWrote wis_table.tex (lags 0-%d). Best model: %s.\n", TABLE_LAG,
            with(wis_table[which.min(wis_table$WIS), ],
                 paste(epidemic_label, nb_label, delay_label, sep = "/"))))

# =============================================================================
# SURPRISE METRICS BARPLOT (AUC / sensitivity / specificity per model)
# =============================================================================
# ggh4x facet_nested groups the columns by LIKELIHOOD then epidemic PROCESS;
# rows are the three detection metrics; bars are the delay model.
if (nrow(surprise_metrics) > 0) {
  metrics_long <- surprise_metrics %>%
    select(epidemic_label, nb_label, delay_label, auc, sensitivity, specificity) %>%
    pivot_longer(c(auc, sensitivity, specificity), names_to = "metric", values_to = "value") %>%
    mutate(
      metric         = factor(recode(metric, auc = "AUC", sensitivity = "Sensitivity",
                                      specificity = "Specificity"),
                              levels = c("AUC", "Sensitivity", "Specificity")),
      epidemic_label = factor(epidemic_label, levels = epi_order),
      nb_label       = factor(nb_label, levels = lik_order),
      delay_label    = factor(delay_label, levels = delay_order))

  delay_legend <- c(Dirichlet = "Non-parametric", GeneralizedGamma = "Generalized-Gamma",
                    Lognormal = "Lognormal")
  bar_plt <- ggplot(metrics_long, aes(delay_label, value, fill = delay_label)) +
    geom_col(width = 0.75, colour = "grey20", linewidth = 0.2) +
    geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.35, size = 2.5, colour = "grey15") +
    facet_nested(rows = vars(metric), cols = vars(nb_label, epidemic_label),
                 labeller = facet_labels, strip = strip_nested(
                   text_x = elem_list_text(colour = "white", face = "bold"),
                   background_x = elem_list_rect(fill = "#224b5e"))) +
    scale_fill_manual("Delay model", values = delay_colors, labels = delay_legend) +
    scale_y_continuous(limits = c(0, 1.1), breaks = c(0, 0.5, 1),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = "Reporting-delay backlog detection, by model",
         subtitle = "AUC, sensitivity and specificity of the delay-surprise detector (columns grouped by likelihood, then epidemic process)",
         x = NULL, y = NULL) +
    theme_paper(base_size = 12) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3))
  ggsave(file.path(OUT_DIR, "surprise_metrics_barplot.pdf"),
         plot = bar_plt, width = 11, height = 7.5)
  cat("Wrote surprise_metrics_barplot.pdf\n")
}

cat("\nDone. Outputs in", OUT_DIR, "\n")
