# =============================================================================
# Precompute prior-sensitivity data for vignettes/Understanding_Priors.Rmd
# =============================================================================
# For EVERY estimated prior we fit three nowcasts -- "tight", "default", "loose"
# -- on ONE disease (dengue) at dates and windows chosen to make THAT prior's
# effect as visible as possible.  For each fit we extract the THREE quantities
# a prior can move, so the vignette can show them side by side in one row:
#
#   1. Reporting-delay distribution   (fitted delay pmf)
#   2. Smoothed epidemic process      (latent incidence lambda_t, via quantile())
#   3. Nowcast                        (posterior-predictive counts, via predict())
#
# Key design choices:
#   * Each scenario carries its OWN (window_weeks, now_offset_weeks) so the
#     time frame is chosen to make the prior effect maximally visible:
#       - Long windows (80-150 weeks) reveal GP length-scale, basis-count, AR
#         persistence, covariate effects, and SIR population-size effects.
#       - Short windows (6-8 weeks) give the delay priors room to act.
#       - A past-peak window helps display SIR recovery rate.
#   * One-stage (joint) fits so delay priors visibly reshape the delay pmf.
#   * ps$observed is stored per-prior so each plot uses its own observed data.
#
# Output: inst/extdata/prior_sensitivity.rds  -- list(data, observed, specs, meta)
#
# Run once from the package root:  Rscript devel/precompute_prior_sensitivity.R
# =============================================================================
set.seed(23694)
devtools::load_all(".", quiet = TRUE)
library(tbl.now)
library(dplyr)
library(tidyr)

N_DRAWS   <- 500L
DELAY_MAX <- 18L          # weeks shown in the delay-distribution panel
`%||%`    <- function(a, b) if (is.null(a)) b else a

# ── Dengue data (full series) ─────────────────────────────────────────────────
data(denguedat)
agg <- denguedat %>% count(onset_week, report_week, name = "n")
tbl_full <- tbl_now(agg, event_date = onset_week, report_date = report_week,
                    case_count = n, data_type = "count-incidence", verbose = FALSE)

weekly <- agg %>% group_by(onset_week) %>% summarise(tot = sum(n), .groups = "drop") %>%
  arrange(onset_week)
peak_date <- weekly$onset_week[which.max(ifelse(weekly$onset_week < as.Date("1995-01-01"),
                                                weekly$tot, -Inf))]
event_col  <- get_event_date(tbl_full)
report_col <- get_report_date(tbl_full)

# Full-data version with 52-period seasonality (for covariate_prior scenarios).
tbl_full_te <- suppressMessages(
  tbl_full |>
    tbl.now::add_temporal_effects(tbl.now::temporal_effects(seasons = 52)) |>
    tbl.now::compute_temporal_effects()
)

# ── Scenario catalogue ────────────────────────────────────────────────────────
# Each entry specifies:
#   prior_name, section, tight/default/loose models, display labels,
#   window_weeks   -- how far back from NOW to include data
#   now_offset_weeks -- NOW = peak_date + 7 * now_offset_weeks
#                       (negative = before the peak; positive = after)
#   use_temporal_effects -- use tbl_full_te instead of tbl_full
scn <- function(prior_name, section, tight, default, loose,
                lab_tight, lab_default = "package default", lab_loose,
                window_weeks = 16L, now_offset_weeks = 2L,
                use_temporal_effects = FALSE) {
  list(prior_name = prior_name, section = section,
       models = list(tight = tight, default = default, loose = loose),
       labels = list(tight = lab_tight, default = lab_default, loose = lab_loose),
       window_weeks       = as.integer(window_weeks),
       now_offset_weeks   = as.integer(now_offset_weeks),
       use_temporal_effects = isTRUE(use_temporal_effects))
}
hs <- function(...) model(nb_likelihood(), hsgp_epidemic(...), lognormal_delay())
ar <- function(...) model(nb_likelihood(), ar1_epidemic(...),  lognormal_delay())
si <- function(...) model(nb_likelihood(), sir_epidemic(...),  lognormal_delay())

# Generic model builders that FIX one slot to a plain number (val = NULL keeps
# the package default).  A numeric in a constructor slot is treated by
# default_priors() as a hard-fixed (is_constant) value -- exactly the "set a
# parameter to a number" pedagogy the numbers panels rely on.
mk_epi <- function(epi_fn, slot = NULL, val = NULL) {
  args <- if (is.null(slot) || is.null(val)) list() else stats::setNames(list(val), slot)
  model(nb_likelihood(), do.call(epi_fn, args), lognormal_delay())
}
mk_delay <- function(delay_fn, slot = NULL, val = NULL) {
  args <- if (is.null(slot) || is.null(val)) list() else stats::setNames(list(val), slot)
  model(nb_likelihood(), hsgp_epidemic(), do.call(delay_fn, args))
}
mk_phi <- function(val = NULL) {
  lik <- if (is.null(val)) nb_likelihood() else nb_likelihood(phi = val)
  model(lik, hsgp_epidemic(), lognormal_delay())
}

# A "numbers" scenario: a SMALL fixed value, the package default, a LARGE fixed
# value.  `build(v)` returns the model with the slot fixed to v (build(NULL) =
# default).  Reuses scn()/plot_prior() with the small/default/large slots.
nums <- function(name, section, build, small, large, lab_small, lab_large,
                 window_weeks, now_offset_weeks = 2L, use_temporal_effects = FALSE) {
  scn(name, section, build(small), build(NULL), build(large),
      lab_small, "package default", lab_large,
      window_weeks = window_weeks, now_offset_weeks = now_offset_weeks,
      use_temporal_effects = use_temporal_effects)
}

# NOTE: each parameter is shown TWO ways -- a "(numbers)" scenario (small vs
# large FIXED value, the easy-to-imagine pedagogy) and, where a prior is the
# natural control, a "(prior)" scenario (tight vs loose).  tight/loose are
# *center-shifted* so the effect is visible.  Long windows reveal process-level
# effects; short windows let delay priors dominate.
scenarios <- list(

  # ======================= HSGP epidemic ====================================
  nums("gp_alpha (numbers)", "HSGP", function(v) mk_epi(hsgp_epidemic, "alpha", v),
       0.1, 8, "alpha = 0.1  (rigid, flat trend)", "alpha = 8  (very flexible)", 16L),
  scn("gp_alpha (prior)", "HSGP",
      hs(alpha = half_normal_prior(0, 0.1)), hs(), hs(alpha = half_normal_prior(0, 8)),
      "half_normal_prior(0, 0.1)  (rigid)", , "half_normal_prior(0, 8)  (very flexible)",
      window_weeks = 80L),

  nums("gp_ell (numbers)", "HSGP", function(v) mk_epi(hsgp_epidemic, "ell", v),
       2, 60, "ell = 2  (short, wiggly)", "ell = 60  (long, smooth)", 80L),
  scn("gp_ell (prior)", "HSGP",
      hs(ell = inv_gamma_prior(10, 1)), hs(), hs(ell = inv_gamma_prior(2, 8)),
      "inv_gamma_prior(10, 1)  (short, wiggly)", , "inv_gamma_prior(2, 8)  (long, smooth)",
      window_weeks = 80L),

  # num_basis is inherently a number -- the numbers panel IS the demonstration.
  nums("num_basis (numbers)", "HSGP", function(v) mk_epi(hsgp_epidemic, "num_basis", v),
       3, 50, "3 basis functions  (coarse)", "50 basis functions  (fine-grained)", 150L),

  # ======================= AR(1) epidemic ===================================
  nums("ar_phi (numbers)", "AR(1)", function(v) mk_epi(ar1_epidemic, "phi", v),
       0.1, 0.95, "phi = 0.1  (no memory)", "phi = 0.95  (near random walk)", 104L),
  scn("ar_phi (prior)", "AR(1)",
      ar(phi = normal_prior(0.95, 0.02)), ar(), ar(phi = normal_prior(-0.95, 0.02)),
      "normal_prior(0.95, 0.02)  (persistent)", , "normal_prior(-0.95, 0.02)  (alternating)",
      window_weeks = 104L),

  nums("ar_sigma (numbers)", "AR(1)", function(v) mk_epi(ar1_epidemic, "sigma", v),
       0.05, 1, "sigma = 0.05  (tiny jumps)", "sigma = 1  (large jumps)", 104L),
  scn("ar_sigma (prior)", "AR(1)",
      ar(sigma = exponential_prior(800)), ar(), ar(sigma = exponential_prior(3)),
      "exponential_prior(800)  (tiny jumps)", , "exponential_prior(3)  (large jumps)",
      window_weeks = 104L),

  # ======================= SIR epidemic =====================================
  nums("R0 (numbers)", "SIR", function(v) mk_epi(sir_epidemic, "R0", v),
       1.2, 3.5, "R0 = 1.2  (slow spread)", "R0 = 3.5  (explosive)", 24L, now_offset_weeks = 0L),
  scn("R0 (prior)", "SIR",
      si(R0 = lognormal_prior(log(1.3), 0.05)), si(), si(R0 = lognormal_prior(log(3.5), 0.7)),
      "lognormal_prior(log 1.3, 0.05)  (low, confident)", ,
      "lognormal_prior(log 3.5, 0.7)  (high, diffuse)",
      window_weeks = 24L, now_offset_weeks = 0L),

  nums("gamma_sir (numbers)", "SIR", function(v) mk_epi(sir_epidemic, "gamma", v),
       1/10, 1/2, "gamma = 0.1  (slow recovery)", "gamma = 0.5  (fast recovery)",
       30L, now_offset_weeks = 6L),
  scn("gamma_sir (prior)", "SIR",
      si(gamma = lognormal_prior(log(1/8), 0.05)), si(), si(gamma = lognormal_prior(log(1/2), 0.3)),
      "lognormal_prior(log 1/8, 0.05)  (slow recovery)", ,
      "lognormal_prior(log 1/2, 0.3)  (fast recovery)",
      window_weeks = 30L, now_offset_weeks = 6L),

  nums("N_eff (numbers)", "SIR", function(v) mk_epi(sir_epidemic, "N_eff", v),
       0.1, 0.9, "N_eff = 0.1  (small susceptible pool)", "N_eff = 0.9  (almost everyone)", 24L),
  scn("N_eff (prior)", "SIR",
      si(N_eff = beta_prior(40, 160)), si(), si(N_eff = beta_prior(1, 1)),
      "beta_prior(40, 160)  (~0.2, confident)", , "beta_prior(1, 1)  (uniform)",
      window_weeks = 24L),

  nums("N_pop (numbers)", "SIR", function(v) mk_epi(sir_epidemic, "N_pop", v),
       5000, 500000, "N_pop = 5000  (rapid saturation)", "N_pop = 500 000  (slow saturation)", 30L),

  # ======================= Delay: LogNormal =================================
  # Short windows (6-8 weeks) so few observed delays let the setting dominate.
  nums("delay_mu LN (numbers)", "Delay", function(v) mk_delay(lognormal_delay, "mu", v),
       log(0.6), log(8), "mu = log(0.6)  (~half-week)", "mu = log(8)  (~8 weeks)", 6L),
  scn("delay_mu LN (prior)", "Delay",
      model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(mu = normal_prior(log(0.6), 0.03))),
      hs(),
      model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(mu = normal_prior(log(5), 1.2))),
      "mu ~ normal(log 0.6, 0.03)  (confident short)", "package default (data-informed)",
      "mu ~ normal(log 5, 1.2)  (diffuse, longer)", window_weeks = 6L),

  nums("delay_sigma LN (numbers)", "Delay", function(v) mk_delay(lognormal_delay, "sigma", v),
       0.2, 2.5, "sigma = 0.2  (sharp, concentrated)", "sigma = 2.5  (very dispersed)", 6L),

  # ======================= Delay: Gamma =====================================
  # (slightly less extreme + a longer window so the one-stage joint fit converges)
  nums("delay_shape Gamma (numbers)", "Delay", function(v) mk_delay(gamma_delay, "shape", v),
       1, 8, "shape = 1  (exponential)", "shape = 8  (peaked)", 10L),
  nums("delay_rate Gamma (numbers)", "Delay", function(v) mk_delay(gamma_delay, "rate", v),
       0.4, 2, "rate = 0.4  (long delays)", "rate = 2  (short delays)", 10L),

  # ======================= Delay: Generalized-Gamma =========================
  nums("delay_mu GG (numbers)", "Delay", function(v) mk_delay(generalized_gamma_delay, "mu", v),
       log(0.6), log(8), "mu = log(0.6)  (short)", "mu = log(8)  (long)", 8L),
  nums("delay_sigma GG (numbers)", "Delay", function(v) mk_delay(generalized_gamma_delay, "sigma", v),
       0.3, 1.5, "sigma = 0.3  (concentrated)", "sigma = 1.5  (dispersed)", 8L),
  nums("delay_Q GG (numbers)", "Delay", function(v) mk_delay(generalized_gamma_delay, "Q", v),
       0.1, 1.5, "Q = 0.1  (light tail)", "Q = 1.5  (heavy tail)", 10L),
  scn("delay_Q GG (prior)", "Delay",
      model(nb_likelihood(), hsgp_epidemic(), generalized_gamma_delay(Q = normal_prior(0.1, 0.05))),
      model(nb_likelihood(), hsgp_epidemic(), generalized_gamma_delay()),
      model(nb_likelihood(), hsgp_epidemic(), generalized_gamma_delay(Q = normal_prior(1.5, 0.5))),
      "Q ~ normal(-1.5, 0.05)  (light tail)", , "Q ~ normal(1.5, 0.5)  (heavy tail)",
      window_weeks = 8L),

  # ======================= Delay: Dirichlet =================================
  nums("dirichlet_alpha (numbers)", "Delay", function(v) mk_delay(dirichlet_delay, "alpha", v),
       1.e-3, 100, "alpha = 0.1  (sparse, spiky)", "alpha = 8  (flat, uniform)", 2L),

  # ======================= Likelihood: NB overdispersion ====================
  nums("phi_nb (numbers)", "Likelihood", function(v) mk_phi(v),
       -1, log(30), "phi = 3  (very overdispersed)", "phi = 1000  (near-Poisson)", 16L),
  scn("phi_nb (prior)", "Likelihood",
      mk_phi(lognormal_prior(log(30), 0.1)), mk_phi(), mk_phi(lognormal_prior(-1, 0.6)),
      "phi ~ lognormal(log 30, 0.1)  (near-Poisson)", ,
      "phi ~ lognormal(-1, 0.6)  (very overdispersed)", window_weeks = 16L),

  # ======================= Covariate (seasonality) ==========================
  # 16-week window with 52-period seasonality attached so the covariate acts.
  scn("covariate_prior (prior)", "Covariates",
      model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(), covariate_prior = normal_prior(0, 0.01)),
      model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
      model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(), covariate_prior = normal_prior(0, 10)),
      "normal_prior(0, 0.01)  (ignores seasonality)", "package default  (std_normal_prior(0, 1))",
      "normal_prior(0, 10)  (large seasonal effects)",
      window_weeks = 16L, use_temporal_effects = TRUE)
)

# ── Extract the three panels from one fitted nowcast ─────────────────────────
extract_three <- function(nc) {
  # 3. Nowcast (posterior-predictive counts) -- exported predict()/summary().
  ncs <- as.data.frame(summary(predict(nc, seed = 2)))
  nowcast_df <- tibble(panel = "3. Nowcast", x = ncs$.event_num,
                       median = ncs$median, lower = ncs$q5, upper = ncs$q95)
  # 2. Smoothed epidemic (latent lambda_t) -- exported quantile() method.
  q <- quantile(nc, probs = c(0.05, 0.5, 0.95))
  epi_df <- tibble(panel = "2. Smoothed epidemic", x = seq_len(nrow(q)) - 1L,
                   median = q[, 2], lower = q[, 1], upper = q[, 3])
  # 1. Reporting-delay distribution (fitted pmf) -- via package internals.
  fit <- nc@fits[[1]]; data <- fit$data; priors <- fit$priors; rc <- fit$reconstruct
  fam <- data$delay_family; dseq <- seq(0, DELAY_MAX, by = 0.5)
  fns <- tryCatch({
    if (fam == 4L) {
      nb <- as.integer(data$np_model_length); pl <- fit$parList
      sp <- if (isTRUE(priors$delay_probs$is_constant == 1L)) priors$delay_probs$fixed
            else { el <- exp(pl$delay_logits); c(el, 1) / (sum(el) + 1) }
      diseasenowcasting:::.nonparametric_delay_functions(sp, nb)
    } else if (fam == 3L) {
      diseasenowcasting:::.delay_distribution_functions(
        3L, rc$delay_mu,
        diseasenowcasting:::.gengamma_shape_transform(fit$parList$delay_Q %||% -2)$shape_Q,
        rc$delay_sigma)
    } else {
      diseasenowcasting:::.delay_distribution_functions(fam, rc$delay_mu, rc$delay_sigma)
    }
  }, error = function(e) NULL)
  delay_df <- if (is.null(fns)) NULL else tibble(
    panel = "1. Reporting delay", x = dseq,
    median = pmax(0, as.numeric(fns$cdf(dseq + 0.5)) - as.numeric(fns$cdf(pmax(0, dseq - 0.5)))),
    lower = NA_real_, upper = NA_real_)
  bind_rows(delay_df, epi_df, nowcast_df)
}

# ── Run all scenarios ────────────────────────────────────────────────────────
cat("Fitting", length(scenarios) * 3, "nowcasts (", length(scenarios),
    "priors x 3 scenarios), dengue, peak =", as.character(peak_date), "\n")

all_rows  <- list()
spec_rows <- list()
obs_rows  <- list()   # per-prior observed data (for nowcast panel reference)

for (sc in scenarios) {

  # Per-scenario time window
  now_sc    <- peak_date + 7L * sc$now_offset_weeks
  window_sc <- 7L * sc$window_weeks
  base_tbl  <- if (isTRUE(sc$use_temporal_effects)) tbl_full_te else tbl_full

  data_sc <- base_tbl %>%
    filter(.data[[event_col]] <= now_sc, .data[[report_col]] <= now_sc,
           .data[[event_col]] >= now_sc - window_sc)

  # Observed-so-far and eventual truth for the "3. Nowcast" reference points.
  obs_now_sc <- data_sc %>% get_latest_reported_cases() %>% as_tibble() %>%
    transmute(x = .event_num, observed = n)
  truth_sc <- tbl_full %>%
    filter(.data[[event_col]] <= now_sc, .data[[event_col]] >= now_sc - window_sc) %>%
    get_latest_reported_cases() %>% as_tibble() %>%
    transmute(x = .event_num, final = n)
  obs_rows[[length(obs_rows) + 1L]] <- full_join(obs_now_sc, truth_sc, by = "x") %>%
    mutate(prior_name = sc$prior_name)

  cat(sprintf("%-28s  [%d-week window, offset %+d]\n",
              sc$prior_name, sc$window_weeks, sc$now_offset_weeks))

  for (scenario in c("tight", "default", "loose")) {
    spec_rows[[length(spec_rows) + 1L]] <- tibble(
      prior_name = sc$prior_name, section = sc$section,
      scenario = scenario, spec = sc$labels[[scenario]])
    cat(sprintf("  %-8s ...", scenario)); flush.console()
    res <- tryCatch({
      nc <- nowcast(data_sc, model = sc$models[[scenario]], type = "one_stage",
                    now = now_sc, n_draws = N_DRAWS, temporal_effects = "none", seed = 1)
      extract_three(nc)
    }, error = function(e) { cat(" SKIP:", conditionMessage(e), "\n"); NULL })
    if (!is.null(res)) {
      all_rows[[length(all_rows) + 1L]] <- res %>%
        mutate(prior_name = sc$prior_name, section = sc$section, scenario = scenario)
      cat(" OK\n")
    }
  }
}

out <- list(
  data     = bind_rows(all_rows),
  observed = bind_rows(obs_rows),   # data.frame with prior_name column
  specs    = bind_rows(spec_rows),
  meta     = list(disease = "dengue", peak_date = peak_date))

outdir  <- file.path("inst", "extdata")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
saveRDS(out, file.path(outdir, "prior_sensitivity.rds"))
cat("\nSaved", file.path(outdir, "prior_sensitivity.rds"),
    "(", format(object.size(out), units = "MB"), ")\n")
