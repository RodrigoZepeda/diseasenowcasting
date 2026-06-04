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
# then released -- producing a small FIRST wave and a larger SECOND wave.
#
# Why this design (it makes the model comparison in the analysis informative):
#   * A genuine TWO-WAVE latent curve cannot be reproduced by a constant-beta
#     mechanistic SIR (one wave only), so the flexible HSGP fits it far better;
#     the AR(1) lags the turning points.  -> HSGP wins on WIS.
#   * Negative-binomial observation noise (size = PHI) on LARGE counts makes the
#     overdispersion term mu^2/PHI dominate -> a Poisson likelihood is badly
#     under-dispersed (its intervals miss), so the NB likelihood wins on WIS.
#   * A short epidemic (~110 days, two waves) -- no need for hundreds of days.
#
# The paper's reporting-delay perturbation is applied to the UPPER bound of the
# delay only (a transient backlog): on a known set of event-days the Weibull
# scale is multiplied by PERTURB_FACTOR so those reports arrive much later.  Each
# case carries a `perturbed` flag = ground truth for the surprise-detection ROC.
#
# Output: devel/paper_sim/SIR_sims.rds  -- list(cases, perturb_days, params, truth).
#
# Run from the package root:  Rscript devel/paper_sim/simulation_study.R
# =============================================================================
suppressMessages({
  library(deSolve)
  library(tidyverse)
})

OUT_RDS <- "devel/paper_sim/SIR_sims.rds"

# ── Epidemic parameters ──────────────────────────────────────────────────────
# These were tuned so the recommended HSGP / NB model not only wins on WIS but
# also has DECENT interval coverage (90% coverage ~0.9 over the nowcast horizon
# with the two-stage fit): broader, slower waves reduce the censored-edge
# underprediction that otherwise sinks coverage, while the large population keeps
# the negative-binomial overdispersion strong enough that NB still beats Poisson.
N_POP   <- 45000      # total population (large -> NB overdispersion bites Poisson)
I0      <- 10         # initial infectious
R0      <- 1.85       # basic reproduction number before control (gentler growth)
GAMMA   <- 1 / 7      # recovery rate (mean infectious period ~ 7 days)
TMAX    <- 135        # epidemic length in days (two broad waves)

# Control-then-release transmission: beta is multiplied by (1 - CTRL_DROP) during
# the window [CTRL_START, CTRL_END], with smooth (logistic) transitions of width
# CTRL_SW.  This suppresses the first wave then lets a larger second wave grow.
CTRL_DROP  <- 0.55    # fractional reduction in beta during the control window
CTRL_START <- 26      # control begins (day)
CTRL_END   <- 62      # control ends / is released (day)
CTRL_SW    <- 3       # smoothness (days) of the on/off transition

PHI <- 18             # negative-binomial size (smaller = more overdispersed)

# ── Reporting delay (Weibull) + upper-bound backlog perturbation ─────────────
# A lighter backlog (fewer days, x2.5 not x3) keeps the surprise signal clearly
# detectable while limiting how many event-days are intrinsically un-coverable.
DELAY_SHAPE    <- 2
DELAY_SCALE    <- 2.2   # mean delay ~ 2 days
N_PERTURB      <- 8L    # number of perturbed (backlog) event-days
PERTURB_FACTOR <- 2.5   # multiply the Weibull scale on those days (~2.5x mean delay)

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

# ── 2. Negative-binomial observation noise ───────────────────────────────────
set.seed(238759)
cases_by_day <- cases_by_day %>%
  mutate(cases = rnbinom(n(), size = PHI, mu = lambda))

# ── 3. Pick the perturbed (backlog) event-days ───────────────────────────────
set.seed(7)
active_days  <- cases_by_day %>% filter(cases >= 5) %>% pull(floor_t)
perturb_days <- sort(sample(active_days, min(N_PERTURB, length(active_days))))

# ── 4. Assign one Weibull delay per case, tagging perturbed observations ─────
set.seed(20240603)
cases <- cases_by_day %>%
  filter(cases > 0) %>%
  select(floor_t, cases) %>%
  uncount(cases) %>%                                    # one row per case
  mutate(perturbed = floor_t %in% perturb_days) %>%
  rowwise() %>%
  mutate(
    scale_i = if (perturbed) DELAY_SCALE * PERTURB_FACTOR else DELAY_SCALE,
    delay   = floor(rweibull(1, shape = DELAY_SHAPE, scale = scale_i)),
    report  = floor_t + delay
  ) %>%
  ungroup() %>%
  select(floor_t, report, perturbed, delay)

# Aggregate to (event, report) cells, recording how many in each cell were
# perturbed (constant within an event-day, carried for convenience).
cases_agg <- cases %>%
  group_by(floor_t, report) %>%
  summarise(n = n(), n_perturbed = sum(perturbed), .groups = "drop") %>%
  mutate(perturbed = n_perturbed > 0)

# Latent truth (the noiseless ODE incidence) -- handy for diagnostics/plots.
truth <- cases_by_day %>% transmute(floor_t, lambda, cases)

# ── Quick console diagnostic ─────────────────────────────────────────────────
local_max <- which(diff(sign(diff(lambda))) == -2) + 1L   # interior local maxima
cat(sprintf("SIR (N=%d, R0=%.1f, control %.0f%% over days %d-%d):\n",
            N_POP, R0, 100 * CTRL_DROP, CTRL_START, CTRL_END))
cat(sprintf("  latent peaks at days %s (lambda = %s), total = %.0f cases\n",
            paste(local_max, collapse = ", "),
            paste(round(lambda[local_max]), collapse = ", "), sum(lambda)))
cat(sprintf("  observed peak = %d cases | %d event-report cells\n",
            max(cases_by_day$cases), nrow(cases_agg)))
cat(sprintf("  %d perturbed (backlog) event-days: %s\n",
            length(perturb_days), paste(perturb_days, collapse = ", ")))

saveRDS(
  list(cases        = cases_agg,
       perturb_days = perturb_days,
       truth        = truth,
       params       = list(N_pop = N_POP, R0 = R0, gamma = GAMMA,
                           control = c(drop = CTRL_DROP, start = CTRL_START,
                                       end = CTRL_END, sw = CTRL_SW),
                           phi = PHI,
                           weibull = list(shape = DELAY_SHAPE, scale = DELAY_SCALE),
                           perturb_factor = PERTURB_FACTOR)),
  OUT_RDS
)
cat(sprintf("Saved %s\n", OUT_RDS))
