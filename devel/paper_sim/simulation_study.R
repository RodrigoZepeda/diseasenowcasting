# =============================================================================
# Simulation study -- DATA GENERATION  (diseasenowcasting, RTMB package)
# =============================================================================
# A deterministic SIR epidemic with a TIME-VARYING transmission rate provides a
# smooth latent incidence curve lambda_t; daily incident cases are then drawn
# with NEGATIVE-BINOMIAL observation noise, and each case is assigned a (floored)
# Weibull reporting delay.
#
# The transmission rate follows a control-then-release profile: beta is high,
# SUPPRESSED during a control window [CTRL_START, CTRL_END] (an intervention),
# then released -- producing a distinct FIRST wave and a larger SECOND wave.
#
# Why this design (it makes the model comparison in the analysis informative):
#   * A genuine TWO-WAVE latent curve cannot be reproduced by a constant-beta
#     mechanistic SIR (one wave only), so the flexible HSGP fits the LogNormal /
#     Generalized-Gamma delay families far better than the mechanistic SIR -- and
#     better than the AR(1), which lags the turning points.  -> HSGP is the best
#     EPIDEMIC PROCESS for those delay families.
#   * The counts are HEAVY-TAILED: most days are negative-binomial with size PHI,
#     but a small fraction of days are drawn with a much smaller size PHI_SURGE
#     (occasional reporting surges / super-spreader days).  A Poisson likelihood
#     cannot represent these heavy-tailed spikes and is catastrophically
#     under-dispersed on the surge days, whereas the negative-binomial absorbs
#     them -- so NB is the best LIKELIHOOD within HSGP.  (Plain NB overdispersion
#     alone is NOT enough: the Weighted Interval Score rewards a sharp, slightly
#     under-covering Poisson over a wide NB unless the counts have genuine heavy
#     tails; the surge component is what supplies them.)
#   * A short epidemic (~110 days, two clear waves, no long flat start tail).
#
# REPORTING DELAYS ARE CLEAN: every case is reported with an unperturbed Weibull
# delay.  This serves two purposes: (a) the two-stage interval coverage stays
# decent (IC90 >= ~0.9), and (b) it keeps the backlog-DETECTION experiment clean
# -- because ordinary delays are short, the injected backlog stands out clearly.
# (An earlier design added a structural delay heavy-tail, but it was counter-
# productive: the COUNT surges above already make NB beat Poisson, and a genuine
# delay heavy-tail makes long delays "normal", flooding the surprise detector
# with false positives.)  No detection backlog is baked into the data: the
# experiment injects its perturbation at NOWCAST time (detection-only, never
# committed to the rolling model) -- see simulation_study_analysis.R.  We only
# record WHICH event-days are the designated perturbation points; the analysis
# inflates their reported delays purely inside surprise() to test detection.
#
# Output: devel/paper_sim/SIR_sims.rds -- list(cases, perturb_days, truth, params).
#
# Run from the package root:  Rscript devel/paper_sim/simulation_study.R
# =============================================================================
suppressMessages({
  library(deSolve)
  library(tidyverse)
})

OUT_RDS <- "devel/paper_sim/SIR_sims.rds"

# ── Epidemic parameters ──────────────────────────────────────────────────────
# Two distinct waves (peaks ~day 28 and ~day 73) with no long flat start tail.
# The population is moderate so the negative-binomial overdispersion stays strong
# relative to the (tight) two-stage delay imputation, letting NB beat Poisson.
N_POP   <- 40000      # total population
I0      <- 10         # initial infectious
R0      <- 2.4        # basic reproduction number before control
GAMMA   <- 1 / 7      # recovery rate (mean infectious period ~ 7 days)
TMAX    <- 110        # epidemic length in days (two clear waves)

# Control-then-release transmission: beta is multiplied by (1 - CTRL_DROP) during
# the window [CTRL_START, CTRL_END], with smooth (logistic) transitions of width
# CTRL_SW.  This caps the first wave then lets a larger second wave grow.
CTRL_DROP  <- 0.62    # fractional reduction in beta during the control window
CTRL_START <- 28      # control begins (day) -- just after the first peak
CTRL_END   <- 55      # control ends / is released (day)
CTRL_SW    <- 3       # smoothness (days) of the on/off transition

PHI       <- 12       # negative-binomial size on ordinary days
P_SURGE   <- 0.10     # fraction of days that are heavy-tailed reporting surges
PHI_SURGE <- 1.2      # NB size on surge days (small = heavy-tailed count spikes)

# ── Reporting delay (Weibull) -- CLEAN (no in-data backlog) ──────────────────
DELAY_SHAPE <- 2
DELAY_SCALE <- 2.5    # mean delay ~ 2.2 days

# ── Detection experiment: designated perturbation event-days ─────────────────
# These are NOT applied to the data.  The analysis inflates the reported delays
# of these event-days INSIDE surprise() (delay -> round(delay * PERTURB_FACTOR))
# to simulate a transient backlog and test per-model detection.  They are the
# positive class for the surprise ROC; every other event-day is a negative.
N_PERTURB      <- 20L   # number of perturbation event-days (the "20 points")
PERTURB_FACTOR <- 2.5   # delay inflation applied at detection time (analysis)

# ── 1. Latent incidence from a control-then-release SIR ODE ──────────────────
# The control factor dips to (1 - drop) between tc1 and tc2 (smooth logistic
# edges), so beta(t) = beta0 * control(t): high, suppressed, then released.
sir_ctrl <- function(t, y, p) with(as.list(c(y, p)), {
  control <- 1 - drop * (plogis((t - tc1) / sw) - plogis((t - tc2) / sw))
  inc <- beta0 * control * S * I / N
  list(c(S = -inc, I = inc - gamma * I, R = gamma * I, C = inc))
})
parms <- c(beta0 = R0 * GAMMA, gamma = GAMMA, N = N_POP,
           drop = CTRL_DROP, tc1 = CTRL_START, tc2 = CTRL_END, sw = CTRL_SW)
y0    <- c(S = N_POP - I0, I = I0, R = 0, C = 0)
ode_out <- as.data.frame(ode(y0, 0:TMAX, sir_ctrl, parms))

# Daily incident infections = increments of the cumulative incidence C.
# Attribute the incidence in interval (t, t+1] to event-day t (0-indexed).
lambda <- diff(ode_out$C)
cases_by_day <- tibble(floor_t = seq_along(lambda) - 1L, lambda = lambda)

# ── 2. Heavy-tailed negative-binomial observation noise ──────────────────────
# Ordinary days use size PHI; a fraction P_SURGE of days use the much smaller
# PHI_SURGE, producing occasional heavy-tailed count spikes that a Poisson
# likelihood cannot represent (this is what lets NB beat Poisson on the WIS).
set.seed(238759)
cases_by_day <- cases_by_day %>%
  mutate(
    surge = runif(n()) < P_SURGE,
    cases = rnbinom(n(), size = if_else(surge, PHI_SURGE, PHI), mu = lambda)
  )

# ── 3. Designate the perturbation event-days (detection positives) ───────────
# Spread across the timeline so both waves contribute; drawn from active days
# that stay observable during the rolling analysis (reports arrive within ~a week).
set.seed(7)
active_days  <- cases_by_day %>%
  filter(cases >= 5, floor_t >= 8L, floor_t <= TMAX - 6L) %>% pull(floor_t)
perturb_days <- sort(sample(active_days, min(N_PERTURB, length(active_days))))

# ── 4. Assign one CLEAN Weibull delay per case (no backlog in the data) ──────
set.seed(20240603)
cases <- cases_by_day %>%
  filter(cases > 0) %>%
  select(floor_t, cases) %>%
  uncount(cases) %>%                                   # one row per case
  mutate(
    delay  = floor(rweibull(n(), shape = DELAY_SHAPE, scale = DELAY_SCALE)),
    report = floor_t + delay
  )

# Aggregate to (event, report) cells.
cases_agg <- cases %>%
  count(floor_t, report, name = "n")

# Latent truth (the noiseless ODE incidence) -- handy for diagnostics/plots.
truth <- cases_by_day %>% transmute(floor_t, lambda, cases)

# ── Quick console diagnostic ─────────────────────────────────────────────────
local_max <- which(diff(sign(diff(lambda))) == -2) + 1L   # interior local maxima
cat(sprintf("SIR (N=%d, R0=%.1f, control %.0f%% over days %d-%d):\n",
            N_POP, R0, 100 * CTRL_DROP, CTRL_START, CTRL_END))
cat(sprintf("  latent peaks at days %s (lambda = %s), total = %.0f cases\n",
            paste(local_max, collapse = ", "),
            paste(round(lambda[local_max]), collapse = ", "), sum(lambda)))
cat(sprintf("  observed peak = %d cases | %d event-report cells | %d heavy-tailed surge days | clean delays\n",
            max(cases_by_day$cases), nrow(cases_agg), sum(cases_by_day$surge)))
cat(sprintf("  %d perturbation event-days (detection-only, injected in analysis): %s\n",
            length(perturb_days), paste(perturb_days, collapse = ", ")))

saveRDS(
  list(cases        = cases_agg,
       perturb_days = perturb_days,
       truth        = truth,
       params       = list(N_pop = N_POP, R0 = R0, gamma = GAMMA,
                           control = c(drop = CTRL_DROP, start = CTRL_START,
                                       end = CTRL_END, sw = CTRL_SW),
                           phi = PHI, phi_surge = PHI_SURGE, p_surge = P_SURGE,
                           weibull = list(shape = DELAY_SHAPE, scale = DELAY_SCALE),
                           perturb_factor = PERTURB_FACTOR)),
  OUT_RDS
)
cat(sprintf("Saved %s\n", OUT_RDS))
