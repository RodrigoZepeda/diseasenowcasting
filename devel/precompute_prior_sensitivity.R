# =============================================================================
# Precompute prior-sensitivity data for vignettes/prior_sensitivity.Rmd
# =============================================================================
# Runs ~120 nowcast fits (3 scenarios × ~40 prior × disease combinations).
# Each fit extracts the full summary table [T rows × quantile cols].
# Output: inst/extdata/prior_sensitivity.rds
#
# Run once from the package root:
#   Rscript devel/precompute_prior_sensitivity.R
# =============================================================================

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(tbl.now)
  library(dplyr)
})

# ── Helper: build tbl_now objects for each disease ─────────────────────────
build_tblnow <- function(disease, now) {
  switch(disease,
    dengue = {
      tbl_now(denguedat, event_date = onset_week, report_date = report_week,
              data_type = "linelist", verbose = FALSE)
    },
    mpox = {
      tbl_now(as.data.frame(mpoxdat), event_date = dx_date,
              report_date = dx_report_date, case_count = n,
              data_type = "count-incidence", verbose = FALSE) |>
        add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
        compute_temporal_effects()
    },
    covid = {
      tbl_now(covid_colombia, event_date = notification_date,
              report_date = diagnosis_date, strata = sex,
              case_count = n, data_type = "count-incidence", verbose = FALSE) |>
        add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
        compute_temporal_effects()
    }
  )
}

DISEASES <- list(
  dengue = list(now = as.Date("2010-06-14")),
  mpox   = list(now = as.Date("2022-09-15")),
  covid  = list(now = as.Date("2020-06-01"))
)

# ── Helper: run one nowcast and extract summary ─────────────────────────────
run_scenario <- function(disease, now, mdl, priors_override = NULL,
                         n_draws = 300, seed = 42) {
  tn <- build_tblnow(disease, now)
  tryCatch({
    prep  <- prepare_from_tbl_now(tn, mdl, now = now)
    eng   <- prep$data
    pris  <- default_priors(mdl, eng)
    if (!is.null(priors_override)) {
      for (nm in names(priors_override)) pris[[nm]] <- priors_override[[nm]]
    }
    fit_result <- fit(mdl, eng, priors = pris)
    draws_out  <- .nowcast_draws(fit_result, target = eng$max_time,
                                 n_draws = n_draws, seed = seed)
    smry <- draws_out$nowcast
    smry$disease <- disease
    smry
  }, error = function(e) { message("SKIP: ", disease, " - ", conditionMessage(e)); NULL })
}

# ── Helper: build a prior list entry (mirrors default_priors format) ────────
mk_prior <- function(dist_num, params) {
  list(dist = dist_num, params = .pad3(params), is_constant = 0L, fixed = numeric(0))
}

# ── Scenario grid ────────────────────────────────────────────────────────────
# Each entry: section, prior_name, scenario, prior_list_override, model_fn
# Scenarios: "tight", "default", "loose"

normal_id     <- normal_prior(0, 1)@num_id          # 1
lognormal_id  <- lognormal_prior(0, 1)@num_id        # 109
gamma_id      <- gamma_prior(2, 0.5)@num_id          # 105

scenarios <- list()

# ── Section 1: HSGP priors (gp_alpha, gp_ell) ──────────────────────────────
# gp_alpha: log-scale amplitude prior; tight=log-Normal(0,0.2), default=LogN(0,0.5), loose=LogN(0,1.5)
for (scen in list(
  list(scenario="tight",   params=c(log(1),0.2)),
  list(scenario="default", params=c(log(1),0.5)),
  list(scenario="loose",   params=c(log(1),1.5))
)) {
  override <- list(gp_alpha = mk_prior(lognormal_id, scen$params))
  scenarios[[length(scenarios)+1]] <- list(
    section="HSGP", subsection="GP amplitude (gp_alpha)",
    prior_name="gp_alpha", scenario=scen$scenario, prior_value=scen$params[2],
    override=override, model_fn=function() model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  )
}

# gp_ell: length-scale prior; tight=LogN(0,0.2), default=LogN(0,0.5), loose=LogN(0,1.5)
for (scen in list(
  list(scenario="tight",   params=c(log(1),0.2)),
  list(scenario="default", params=c(log(1),0.5)),
  list(scenario="loose",   params=c(log(1),1.5))
)) {
  override <- list(gp_ell = mk_prior(lognormal_id, scen$params))
  scenarios[[length(scenarios)+1]] <- list(
    section="HSGP", subsection="GP length-scale (gp_ell)",
    prior_name="gp_ell", scenario=scen$scenario, prior_value=scen$params[2],
    override=override, model_fn=function() model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  )
}

# ── Section 2: AR(1) priors ─────────────────────────────────────────────────
for (scen in list(
  list(scenario="tight",   ar_phi=c(0,0.1), ar_sig=c(0.1,0.05)),
  list(scenario="default", ar_phi=c(0,0.5), ar_sig=c(0.3,0.1)),
  list(scenario="loose",   ar_phi=c(0,1.0), ar_sig=c(0.5,0.3))
)) {
  override_phi <- list(ar_phi   = mk_prior(normal_id,    scen$ar_phi),
                       ar_sigma = mk_prior(lognormal_id, log(scen$ar_sig)))
  scenarios[[length(scenarios)+1]] <- list(
    section="AR1", subsection="AR1 autocorrelation (ar_phi)",
    prior_name="ar_phi/ar_sigma", scenario=scen$scenario, prior_value=scen$ar_phi[2],
    override=override_phi, model_fn=function() model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
  )
}

# ── Section 3: SIR priors (R0, recovery_rate) ───────────────────────────────
# R0 prior: log-scale ~ Normal(log(R0), sd); tight=0.1, default=0.5, loose=1.5
for (scen in list(
  list(scenario="tight",   r0=c(log(2),0.1)),
  list(scenario="default", r0=c(log(2),0.5)),
  list(scenario="loose",   r0=c(log(2),1.5))
)) {
  override <- list(R0 = mk_prior(lognormal_id, scen$r0))
  scenarios[[length(scenarios)+1]] <- list(
    section="SIR", subsection="Basic reproduction number (R0)",
    prior_name="R0", scenario=scen$scenario, prior_value=scen$r0[2],
    override=override, model_fn=function() model(nb_likelihood(), sir_epidemic(), lognormal_delay())
  )
}

# ── Section 4: LogNormal delay priors ────────────────────────────────────────
for (scen in list(
  list(scenario="tight",   mu_sd=0.1, sig_shape=5),
  list(scenario="default", mu_sd=0.5, sig_shape=2),
  list(scenario="loose",   mu_sd=1.5, sig_shape=0.5)
)) {
  override <- list(
    delay_mu    = mk_prior(normal_id,   c(log(5), scen$mu_sd)),
    delay_sigma = mk_prior(gamma_id,    c(scen$sig_shape, 1))
  )
  scenarios[[length(scenarios)+1]] <- list(
    section="Delay", subsection="Log-Normal delay",
    prior_name="delay_mu/sigma (LogNormal)", scenario=scen$scenario, prior_value=scen$mu_sd,
    override=override, model_fn=function() model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  )
}

# ── Section 5: Gamma delay priors ────────────────────────────────────────────
for (scen in list(
  list(scenario="tight",   mu_sd=0.1, sig_shape=5),
  list(scenario="default", mu_sd=0.5, sig_shape=2),
  list(scenario="loose",   mu_sd=1.5, sig_shape=0.5)
)) {
  override <- list(
    delay_mu    = mk_prior(normal_id, c(log(5), scen$mu_sd)),
    delay_sigma = mk_prior(gamma_id,  c(scen$sig_shape, 1))
  )
  scenarios[[length(scenarios)+1]] <- list(
    section="Delay", subsection="Gamma delay",
    prior_name="delay_mu/sigma (Gamma)", scenario=scen$scenario, prior_value=scen$mu_sd,
    override=override, model_fn=function() model(nb_likelihood(), hsgp_epidemic(), gamma_delay())
  )
}

# ── Section 6: GenGamma delay Q prior ─────────────────────────────────────────
for (scen in list(
  list(scenario="tight",   q_shape=10, q_rate=10),  # concentrated near Q=1
  list(scenario="default", q_shape=2,  q_rate=2),
  list(scenario="loose",   q_shape=0.5,q_rate=0.5)
)) {
  override <- list(delay_Q = mk_prior(gamma_id, c(scen$q_shape, scen$q_rate)))
  scenarios[[length(scenarios)+1]] <- list(
    section="Delay", subsection="GenGamma shape (Q)",
    prior_name="delay_Q", scenario=scen$scenario, prior_value=scen$q_shape/scen$q_rate,
    override=override, model_fn=function() model(nb_likelihood(), hsgp_epidemic(), generalized_gamma_delay())
  )
}

# ── Section 7: Dirichlet concentration ────────────────────────────────────────
for (scen in list(
  list(scenario="tight",   alpha=5.0),
  list(scenario="default", alpha=1.0),
  list(scenario="loose",   alpha=0.1)
)) {
  val <- scen$alpha
  scenarios[[length(scenarios)+1]] <- list(
    section="Delay", subsection="Dirichlet concentration",
    prior_name="dirichlet_alpha", scenario=scen$scenario, prior_value=val,
    override=NULL,   # alpha baked into the delay constructor
    model_fn=function(v=val) model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay(alpha=v))
  )
}

# ── Section 8: NB overdispersion (phi) ─────────────────────────────────────
for (scen in list(
  list(scenario="tight",   params=c(log(50), 0.2)),   # phi ~ 50, narrow
  list(scenario="default", params=c(log(20), 0.5)),   # phi ~ 20, default
  list(scenario="loose",   params=c(log(5),  1.0))    # phi ~ 5, very dispersed
)) {
  override <- list(phi_nb = mk_prior(lognormal_id, scen$params))
  scenarios[[length(scenarios)+1]] <- list(
    section="Likelihood", subsection="NB overdispersion (phi)",
    prior_name="phi_nb", scenario=scen$scenario, prior_value=exp(scen$params[1]),
    override=override, model_fn=function() model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  )
}

# ── Run all scenarios for each disease ────────────────────────────────────────
cat("Running", length(scenarios), "scenarios × 3 diseases =",
    length(scenarios)*3, "total fits\n")

all_results <- list()
for (dis in names(DISEASES)) {
  now <- DISEASES[[dis]]$now
  for (sc in scenarios) {
    mdl <- sc$model_fn()
    key <- paste(sc$section, sc$subsection, sc$scenario, dis, sep="|")
    cat(sprintf("  %-70s", key)); flush.console()
    t0 <- proc.time()[3]
    smry <- run_scenario(dis, now, mdl, priors_override = sc$override)
    dt   <- round(proc.time()[3] - t0, 1)
    if (!is.null(smry)) {
      smry$section    <- sc$section
      smry$subsection <- sc$subsection
      smry$prior_name <- sc$prior_name
      smry$scenario   <- sc$scenario
      smry$prior_value<- sc$prior_value
      all_results[[length(all_results)+1]] <- smry
      cat(sprintf(" OK (%.1fs)\n", dt))
    } else {
      cat(" SKIP\n")
    }
  }
}

combined <- do.call(rbind, all_results)
cat("\nTotal rows:", nrow(combined), "\n")

# Save
outdir <- file.path("inst", "extdata")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
outfile <- file.path(outdir, "prior_sensitivity.rds")
saveRDS(combined, outfile)
cat("Saved to:", outfile, "\n")
cat("Object size:", format(object.size(combined), units="MB"), "\n")
