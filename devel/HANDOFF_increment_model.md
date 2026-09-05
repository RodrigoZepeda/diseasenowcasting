# Handoff: count-cumulative nowcasting, session of 2026-09-02/03

Written to start a fresh conversation. Read this, then
`devel/spa_diagnostics/FINDINGS.md` sections J onward for detail.

---

## 1. Where the work started and where it ended

**Started:** the reviewer note
`devel/Reviewer corrections and recommendations for BRIEFING_count_cumulative.md`
had just landed and nothing had been done with it. It argued the low-`p`
pathology should be reframed as finite-horizon identification, and gave an
ordered list of experiments (its section 30).

**Ended:** the retraction/`p` model has been replaced by a signed-increment
model that forecasts ~10% better than a strong empirical baseline out of
sample. `p` is gone. Six real bugs were found along the way, three of which
changed conclusions.

---

## 2. The one result everything rests on

Finite-horizon cumulative data identify the **within-horizon retraction masses**

```
b_c = h_R(c) = (1 - p) g_C(c),      S_R(a) = r(a) = 1 - sum_{c<=a} b_c
```

and **not** `p`. Proven two ways in FINDINGS J (script `25_bc_reparam.R`):

* the `b`-form and the `(p, g_C)` form agree to 1.5e-14 on the real (a,b) pairs;
* holding `b_1..b_15` fixed and buying any `p <= r(H)` by parking the leftover
  mass past the horizon leaves the profile likelihood **identically constant** —
  0.00e+00 nats — where the lognormal-`g_C` profile spans 5,256 nats.

So every nat of curvature in `p` came from the parametric family extrapolating
into the unobserved tail. **This result does not depend on the profiler and was
re-verified after every bug fix.**

`main_identifiability_update.tex` is the write-up of exactly this: it takes
`h_R` as primitive and drops `p`. Its `alpha_t^d = mu_t g_D(d)`,
`omega_t^d = mu_t sum_{d1<d} g_D(d1) h_R(d-d1)` is the same object; for a
cadence interval the exact marginal is the `q_pairs` form in script 37.

---

## 3. Why `p` collapsed: the increment law, not the retention curve

FINDINGS K and N. The Poisson-Skellam increment law **cannot represent the
data**, and `p` was the only dial it had for dispersion:

| | observed | old model |
|---|---:|---:|
| increments exactly zero (of 805) | 620 (77%) | **118 (15%)** |
| gross retractions vs observed down-movement | — | **21.5x** |

Freeing the retraction curve completely does not help (it changes the fit but
lands on the same shape). What the data actually look like, across all 53
locations (script `28_generality.R`):

* **91% of increments are exactly zero** (median across states; Texas 77%);
* participation is driven by **cohort age** (58% move at age 1 -> 2.3% at 15),
  consistently across states — NOT by snapshot sparsity, which was a season
  artefact;
* **78% of non-zero moves are UP.** Post-publication movement is *not*
  retraction. `g_D` fits `P(delay 0) = 1.000` — reporting is complete at first
  publication, and the old model's 1.3-1.8 week delay existed only so retraction
  had something to cancel.

---

## 4. The model that works

Script `36_ztnb_magnitude.R`, sign and magnitude modelled **separately**:

```
pi(a, prev) = plogis(p0 + p1 log(1+a) + gamma 1{previous increment != 0})
theta(a)    = plogis(t0 + t1 log(1+a))                    P(the move is UP)
M           ~ zero-truncated NB(size, mu = kappa * level^beta)   on {1,2,...}

Delta = 0 w.p. 1-pi ;  +M w.p. pi*theta ;  -M w.p. pi*(1-theta)
```

first interval `~ Poisson(mu_t G_D(b))`, `mu_t` profiled per event week.

Why sign and magnitude must be separate (FINDINGS N.1): a Skellam ties them
together through one `lam`. The data need 78% up-moves *and* a range from 1 to
615; covering the tail forces a huge `lam`, and a huge `lam` forces symmetry.
Three Skellam variants (scripts 29, 30, 31) failed on exactly this.

---

## 5. Scoreboard

In-sample, all under the same guarded objective, Texas w15, 904 intervals:

| model | script | par | logL |
|---|---|---:|---:|
| lognormal `g_C` + `p` | 23 | 5 | −3154.84 |
| free retention (= the tex model, Poisson) | 26 | 17 | −3071.17 |
| sign x magnitude | 32 | 9 | −1510.05 |
| + Markov move state | 34 | 10 | −1505.75 |
| **+ ZTNB magnitude** | **36** | **10** | **−1496.34** |

Out of sample, 16 rolling origins, 794 (cohort, origin, horizon) triples,
refit at every origin, target = the value published at a future date:

| | WIS | skill vs empirical | 90% cov |
|---|---:|---:|---:|
| **ZTNB + Markov** | **4.513** | **+10.3%** | 0.940 |
| empirical ratio baseline | 5.033 | — | 0.826 |
| persistence | 5.770 | — | — |

**The `main_identifiability_update.tex` model** (free `h_R`, no `p`) was
implemented and forecast on a matched 12-origin / 578-triple set (FINDINGS T):

| model | WIS | skill vs empirical | 90% cov |
|---|---:|---:|---:|
| **h_R + ZTNB hurdle (script 38, BEST)** | **1.702** | **+22.4%** | 0.941 |
| ZTNB + Markov | 1.959 | +10.6% | 0.943 |
| empirical ratio baseline | 2.192 | — | 0.860 |
| persistence | 2.474 | — | — |
| tex model, free `h_R` | 2.838 | **−29.4%** | 0.979 |

The tex reduction is **right about identifiability** — it is section J's theorem,
and its Poisson version reproduces script 26 exactly (`S_R(15) = 0.474` vs
`r(15) = 0.467`). But it forecasts worse than a ratio lookup, because its
increment law is still Poisson-Skellam and so cannot put mass on the 77% of
increments that are exactly zero; its predictive is far too diffuse (90% coverage
0.979). **Identifiability and forecasting are orthogonal problems here.** A
production model wants `h_R`/`S_R` as the primitive retention object AND an
increment law that can be exactly zero.

**That synthesis is built: script `38_tex_ztnb.R` (FINDINGS U), and it is the
current best model.** Keep `alpha = mu_t q_+`, `omega = mu_t q_-` from the tex
model, then set sign `theta = alpha/(alpha+omega)` and magnitude mean
`(alpha+omega)/pi` with a hurdle `pi` and a zero-truncated NB magnitude. Because
`E[Delta] = pi E[M](2 theta - 1) = alpha - omega` for ANY `pi`, the tex mean
structure — and its whole identifiability argument — is preserved exactly while
`pi` absorbs the zeros. logL −1485.76, PIT p 0.25, WIS 1.702 (+22.4%).

**Its most important by-product:** `S_R(15)` fits at **0.998**, against 0.474 for
the same retention object under Poisson-Skellam increments. The "53% of reports
are eventually withdrawn" implied by the old fits is an artefact of the increment
law, not a property of the data (observed down-movement is 2,741 against 61,636
up). The long `g_C` tail the briefing spends two sections explaining largely
dissolves once the increment law is right — with no administrative-revision
component.

---

## 6. The bugs — three changed conclusions

| bug | where | effect |
|---|---|---|
| `lskel` sign error: Bessel order `\|z\|` but ratio exponent `z/2` | 29-31 | invalidated the first stage-1 pass; purged |
| `log_besselI` indexed a scalar `x` with a logical mask | 29-31 | `NA` in gate code only |
| **unguarded parabolic vertex refinement** | **18-26** | **reached back into the whole prior investigation** |
| `lmag` underflow: `log(1e-300)-log(1e-300)` = log-prob **0** | 32-33 | optimiser rode it to logL −110 on 736 obs |
| gate 2 conditioned on the observation being extreme | 29-32 | invalid test; numbers voided |
| coverage labelled 90% when the quantiles were 95% | 33 | made mild over-coverage look catastrophic |

**What the parabolic bug changed** (FINDINGS Q, S), after re-running everything:

* **FINDINGS J: unaffected**, by construction — verified.
* **K.1 restated**: freeing the retraction masses buys **83.67 nats**, not 9.8.
  The claim "removing every parametric restriction buys essentially nothing" is
  **retired**. K.2/K.3/K.4 stand.
* **Section I revised** (script 18 re-run, all 8 cells): recovery at
  `p_true=0.95` *improves* (0.917 -> **0.965**), so "the estimator is not broken"
  is stronger. But scenario C at `p_true=0.60` gives **0.334**, not 0.150 — the
  claim that the simulation "reproduced the exact real-data low-`p` value" is
  **not supported** and must not be repeated. And the "unstable `p_hat`, sd 0.215"
  story becomes a systematic bias (0.596, sd 0.065).

**Guard now in every script**, plus the assertion that would have caught all
three numerical failures at once: **a sum of log-probabilities cannot be
positive**, so the objective returns `1e12` if `total > 0`.

Log-likelihoods from before the guard are **not comparable** to those after.
`*_PREGUARD.rds` copies are kept.

---

## 7. Open, in priority order

1. **Over-coverage is stubborn**: 90% coverage 0.938 / 0.938 / 0.940 across
   three different magnitude and participation laws, nominal 0.90. Untried
   lever: magnitude autocorrelation (measured, `cor(log|z|, log|z_prev|) = 0.358`,
   never used).
2. **Persistence still beats every model at target ages 9-15** (1.042 vs 1.500).
3. **The two named stage-2 comparators were never run**: `baselinenowcast`
   (0.2.0 expects a reporting triangle of incremental reports, which does not map
   onto a revising cumulative series with negatives) and the production model via
   `backtest()`.
4. **Uncertainty is not calibrated**. Profile CIs from this objective have 0.00
   coverage in every recovery cell, widths 0.0002-0.0095. Stage 3 (generative
   bootstrap) is untouched.
5. **The estimand decision is deferred** — working assumption is the
   finite-horizon operational target (what gets published at a future date).
6. **The briefing still quotes retracted numbers** (`p_hat = 0.150`, the 9.8-nat
   claim). `BRIEFING_count_cumulative.md` needs a correction pass.
7. **Post-first arrivals are dropped** in the sign x magnitude model rather than
   convolved in. Fine for FluSight (`P(delay 0) = 1.000`); the package
   integration must restore them for data with genuine late reporting.

---

## 8. Files

* `devel/PLAN_increment_nowcasting.md` — the staged plan, stages 0-1 marked done,
  stage 2 partial.
* `devel/spa_diagnostics/FINDINGS.md` — sections J through S are this session.
* Scripts `25` (reparameterisation), `26` (free retention = tex model),
  `27` (why), `28` (generality), `29-31` (rejected Skellam variants),
  `32` (sign x magnitude, stage 1 passed), `33` (backtest harness),
  `34-35` (rejected refinements), `36` (**current best**), `37` (tex model
  backtest).
* Every long fit checkpoints to disk per start/origin. Two distinct process
  hazards bit this session:
  1. **`pgrep -f` self-matching.** A wait loop written as
     `until ! pgrep -f "exec/R.*18_param"; do sleep 60; done` never exits,
     because `pgrep -f` matches the waiter's OWN command line, which contains
     that pattern. Four such loops sat idle for up to two hours and made process
     state unreadable. Poll a **result file** instead
     (`until [ -f out.rds ]; do sleep 20; done`), or match on something absent
     from the waiter, e.g. `pgrep -f '[1]8_param'`.
  2. Long jobs launched with `nohup ... &` did get killed and restarted, costing
     real refits. Prefer a foreground call with an extended timeout, plus a
     per-start checkpoint so a kill costs one start rather than all of them.
