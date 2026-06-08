# Validate strata on the real datasets: dengue (gender), mpox (race), covid (sex).
# For each: build a stratified tbl_now, run a one-stage nowcast at a mid date,
# confirm it converges and returns finite per-time totals; also confirm the
# unstratified pooled fit still runs.
suppressMessages(devtools::load_all("..", quiet = TRUE))
suppressMessages(library(tbl.now))

run_one <- function(tag, tn_strat, tn_pool, mdl, now) {
  cat("====", tag, "====\n")
  for (lab in c("stratified", "pooled")) {
    tn <- if (lab == "stratified") tn_strat else tn_pool
    res <- tryCatch({
      t0 <- Sys.time()
      nc <- nowcast(tn, mdl, type = "one_stage", n_draws = 200, now = now, seed = 1)
      s  <- summary(predict(nc, seed = 2))
      dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      sprintf("%-11s strata=%d target=%d median=[%s] finite=%s (%.1fs)",
              lab, nc@engine$num_strata, nc@target,
              paste(round(range(s$median)), collapse = ","), all(is.finite(s$median)), dt)
    }, error = function(e) sprintf("%-11s ERROR: %s", lab, conditionMessage(e)))
    cat(res, "\n")
  }
  cat("\n")
}

mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())

# ── Dengue: weekly linelist, strata = gender ─────────────────────────────────
dengue <- denguedat
dn_strat <- tbl_now(dengue, event_date = onset_week, report_date = report_week,
                    strata = gender, data_type = "linelist", verbose = FALSE)
dn_pool  <- tbl_now(dengue, event_date = onset_week, report_date = report_week,
                    data_type = "linelist", verbose = FALSE)
dn_now <- min(dengue$onset_week) + 200 * 7      # ~200 weeks in
run_one("DENGUE (gender)", dn_strat, dn_pool, mdl, dn_now)

# ── Mpox: daily counts, strata = race ────────────────────────────────────────
mpox <- as.data.frame(mpoxdat)
mp_strat <- tbl_now(mpox, event_date = dx_date, report_date = dx_report_date,
                    strata = race, case_count = n, data_type = "count-incidence", verbose = FALSE)
mp_pool  <- tbl_now(mpox, event_date = dx_date, report_date = dx_report_date,
                    case_count = n, data_type = "count-incidence", verbose = FALSE)
mp_now <- min(mpox$dx_date) + 90
run_one("MPOX (race)", mp_strat, mp_pool, mdl, mp_now)

# ── Covid: daily counts, strata = sex ────────────────────────────────────────
covid <- as.data.frame(covidat)
cv_strat <- tbl_now(covid, event_date = date_of_symptom_onset, report_date = date_of_registry,
                    strata = sex, case_count = n, data_type = "count-incidence", verbose = FALSE)
cv_pool  <- tbl_now(covid, event_date = date_of_symptom_onset, report_date = date_of_registry,
                    case_count = n, data_type = "count-incidence", verbose = FALSE)
cv_now <- min(covid$date_of_symptom_onset) + 120
run_one("COVID (sex)", cv_strat, cv_pool, mdl, cv_now)
