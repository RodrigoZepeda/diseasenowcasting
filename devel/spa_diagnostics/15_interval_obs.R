# =============================================================================
# The intervals are GIVEN by the publication cadence, not detected.
# =============================================================================
# For each event week the observable delays are exactly those at which a snapshot
# exists.  Consecutive observed delays d_prev < d_next define the interval
# (d_prev, d_next], and the observation is C_t(d_next) - C_t(d_prev) -- section 1
# of the plan.  Using the section 2-3 trajectory probabilities:
#
#   q_+(a,b) = sum_{r=a+1..b} g_D(r) [ p + (1-p) Gbar_C(b-r) ]
#   q_-(a,b) = (1-p) sum_{r=0..a} g_D(r) [ G_C(b-r) - G_C(a-r) ]
#
# Section 5's unit test (a = d-1, b = d must reproduce the point model) is checked
# first, because everything else is worthless if that fails.
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

q_plus <- function(a, b, gD, gC, p) {
  GC <- cumsum(gC); Gbar <- function(k) if (k < 0) 1 else 1 - GC[k + 1L]
  s <- 0
  for (r in max(a + 1L, 0L):b) if (r + 1L <= length(gD))
    s <- s + gD[r + 1L] * (p + (1 - p) * Gbar(b - r))
  s
}
q_minus <- function(a, b, gD, gC, p) {
  GC <- cumsum(gC); G <- function(k) if (k < 0) 0 else GC[min(k, length(GC) - 1L) + 1L]
  if (a < 0) return(0)
  s <- 0
  for (r in 0:a) if (r + 1L <= length(gD)) s <- s + gD[r + 1L] * (G(b - r) - G(a - r))
  (1 - p) * s
}

START <- as.Date("2023-09-23"); STATE <- "Texas"
raw <- tbl.now::flusight |>
  filter(location_name == STATE, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))
tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
              case_count = observation, data_type = "count-cumulative",
              verbose = FALSE) |> align_weeks(date_col = "report_date")

f <- suppressMessages(suppressWarnings(nowcast(
  tn, model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
            validation = validation_process(p = 0.9572)),
  type = "one_stage", n_draws = 20, temporal_effects = "none", seed = 1)))
fit <- f@fits[[1]]; e <- f@engine; pr <- f@priors
cD <- min(as.integer(e$max_conf_delay) - 1L, 15L); p <- pr$confirm_p$fixed
dfns <- diseasenowcasting:::.delay_distribution_functions(
  as.integer(e$delay_family), as.numeric(fit$delay_mu), as.numeric(fit$delay_sigma))
acdf <- as.numeric(dfns$cdf(seq_len(cD + 1L))); gD <- c(acdf[1], diff(acdf))
rmu <- as.numeric(fit$parList$retract_mu)
rsd <- 0.01 + exp(as.numeric(fit$parList$log_retract_sd_exc))
rfns <- diseasenowcasting:::.delay_distribution_functions(as.integer(pr$retract_family), rmu, rsd)
gC <- c(0, diff(c(0, as.numeric(rfns$cdf(seq_len(cD))))))
gW <- diseasenowcasting:::.convolve_delays(gD, gC)
mu <- as.numeric(fit$lambda) / p

cat("=== section 5 unit test: (d-1, d] must reproduce the point model ===\n")
ok <- TRUE
for (d in 1:cD) {
  qp <- q_plus(d - 1L, d, gD, gC, p); qm <- q_minus(d - 1L, d, gD, gC, p)
  ref_p <- gD[d + 1L]; ref_m <- (1 - p) * gW[d + 1L]
  if (abs(qp - ref_p) > 1e-12 || abs(qm - ref_m) > 1e-12) {
    ok <- FALSE
    cat(sprintf("  d=%2d  q+ %.10f vs %.10f   q- %.10f vs %.10f\n", d, qp, ref_p, qm, ref_m))
  }
}
cat(if (ok) "  PASS: interval model reduces exactly to the point model.\n"
    else    "  FAIL\n")

# How surprising are the extreme cells, point vs interval?
asof <- sort(unique(tn[[tbl.now::get_report_date(tn)]]))
ev   <- sort(unique(tn[[tbl.now::get_event_date(tn)]]))
cum  <- function(t, d) sum(e$increment_array[t, seq_len(d + 1L), 1])
zsc  <- function(z, a, w) (z - (a - w)) / sqrt(a + w + 1e-9)

cat("\n=== the extreme cells, read as points vs as cadence-given intervals ===\n")
cat(sprintf("%4s %4s %8s | %9s %9s %8s | %6s %9s %9s %8s\n",
            "t","d","z","alpha_pt","omega_pt","sd_pt","(a,b]","alpha_iv","omega_iv","sd_iv"))
for (tt in c(68, 70, 15, 16, 17, 75)) {
  h <- min(as.integer(e$d_star[tt, 1]), cD)
  obs_d <- (0:h)[(ev[1] + (tt - 1 + 0:h) * 7) %in% asof]
  if (!length(obs_d)) next
  d1 <- obs_d[1]
  z_pt <- e$increment_array[tt, d1 + 1L, 1]
  a_pt <- mu[tt] * gD[d1 + 1L]; w_pt <- (1 - p) * mu[tt] * gW[d1 + 1L]
  a <- -1L; b <- d1
  z_iv <- cum(tt, b)
  a_iv <- mu[tt] * q_plus(a, b, gD, gC, p); w_iv <- mu[tt] * q_minus(a, b, gD, gC, p)
  cat(sprintf("%4d %4d %8.0f | %9.1f %9.1f %8.1f | %6s %9.1f %9.1f %8.1f\n",
              tt, d1, z_pt, a_pt, w_pt, zsc(z_pt, a_pt, w_pt),
              sprintf("(%d,%d]", a, b), a_iv, w_iv, zsc(z_iv, a_iv, w_iv)))
}
cat("\n(sd = standard deviations from the model mean; the point reading of a first\n")
cat(" observation is compared with reading it as everything accumulated by then.)\n")
