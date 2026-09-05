# Briefing: identifiability of the retention probability `p` in a count-cumulative nowcasting model

> ## CORRECTION NOTICE — 2026-09-03
>
> Six bugs were found after this briefing was written; three changed conclusions.
> The full record is `devel/spa_diagnostics/FINDINGS.md` sections Q, S and T, and
> the current state of the work is `devel/HANDOFF_increment_model.md`.
>
> **Retracted from this document:**
> * `p_true = 0.60 -> p_hat = 0.150` "reproduces the exact pathology" (§6.2). The
>   correct value is **0.334**; the match to the real-data low-`p` value was an
>   artefact of an unguarded parabolic refinement in the `mu_t` profile. The
>   *mechanism* is confirmed, the coincidence was not real.
> * "`p_hat` is unstable at high `p` (sd 0.215)". It is **0.065** — a systematic
>   bias, not wandering.
>
> **Superseded:** the framing throughout this document is that `p` is *confounded*
> with the retraction tail. FINDINGS section J later proved it is **exactly
> unidentified**: holding the within-horizon masses `b_c = (1-p) g_C(c)` fixed,
> the profile in `p` is identically constant (0.00e+00 nats) for any `p <= r(H)`.
> All curvature came from the parametric `g_C` extrapolating into the unobserved
> tail. `main_identifiability_update.tex` is the correct response — take `h_R`
> primitive, do not estimate `p`.
>
> **Not the whole story, though.** Removing `p` fixes identifiability and leaves
> forecasting *worse* than a ratio lookup (FINDINGS T), because the
> Poisson-Skellam increment law cannot put mass on the 77% of increments that are
> exactly zero. Sections 8-9 below attribute the pathology to administrative
> revisions forcing a long `g_C` tail; the increment law is the deeper cause, and
> once it is fixed the estimated retraction mass collapses from `S_R(15) = 0.47`
> to **0.998** — i.e. the long tail this document is trying to explain is itself
> mostly an artefact of the misspecified increment law.
>
> **Numbers in this document that were re-verified under the fixed profiler:**
> script 23 `p_hat` (w15) 0.518 -> 0.524, and the §6.2 recovery table (replaced
> in place). **Not re-verified:** every `0.417` full-follow-up figure (§5, §8.6),
> the `0.10` dense-grid optimum, and the "44.6 nats" profile depth in §5 — all
> were produced with the unguarded profiler and should be treated as indicative
> only.


**Purpose.** This is a self-contained briefing for an external reviewer. We want a
second opinion on a statistical identification problem and on three candidate
solutions. No prior context is assumed. Everything needed to form a view is here.

**Status.** Diagnosis is mature; the fix is not settled. We have run ~24 numbered
diagnostic experiments. Several of our own hypotheses were refuted by measurement
and are recorded as such.

---

## 1. What the software does

We maintain an R package for **nowcasting** epidemic time series: estimating what
the count for a recent time period will eventually settle at, given that reports
arrive with delay and that some already-counted reports are later withdrawn.

The package supports line-list data, count-incidence data, and **count-cumulative**
data. This briefing is only about the count-cumulative case, which is the hard one.

Inference is maximum-likelihood / Laplace via RTMB (an automatic-differentiation
framework). Nothing here depends on that choice.

---

## 2. The generative model

For event time `t` (a week when cases occurred):

    M_t ~ Poisson(mu_t)        (or Negative Binomial)

Each latent report independently draws:

* a **reporting delay** `D_rpt ~ g_D` — how long until it first appears;
* a **genuine indicator** `Y ~ Bernoulli(p)` — whether it is a real case;
* if not genuine, a **retraction lag** `D_C ~ g_C` — how long until it is removed.

A report is present in the cumulative count at delay `d` iff `D_rpt <= d` and
(`Y = 1` or `D_rpt + D_C > d`).

The settled (eventual) genuine mean is

    lambda_t = p * mu_t,     equivalently    mu_t = lambda_t / p.

`lambda_t` is the **nowcast target**: what the count will eventually be.

`p` is a **cure fraction** in survival terms: the proportion of reports that never
"fail" (are never retracted).

### Observation model, one delay

For the one-delay increment `Delta_t^d = C_t(d) - C_t(d-1)`, the Poisson marginal
is **Skellam** (difference of two independent Poissons):

    alpha_t^d = mu_t * g_D(d)                                     (additions)
    omega_t^d = mu_t * (1-p) * sum_{r=0}^{d-1} g_D(r) g_C(d-r)    (retractions)
    Delta_t^d ~ Skellam(alpha_t^d, omega_t^d)

The production likelihood is the product of these one-delay marginals — a
**composite likelihood**, not the exact joint across delays.

### Observation model, general interval (the corrected version)

Real data are not published every period. If consecutive **published** snapshots
for event time `t` sit at delays `a < b`, the correct observation is
`C_t(b) - C_t(a)`, with

    alpha_t(a,b) = mu_t * sum_{r=a+1}^{b} g_D(r) [ p + (1-p) Gbar_C(b-r) ]
    omega_t(a,b) = mu_t * (1-p) * sum_{r=0}^{a} g_D(r) [ G_C(b-r) - G_C(a-r) ]
    C_t(b) - C_t(a) ~ Skellam( alpha_t(a,b), omega_t(a,b) )

where `G_C` is the CDF of `g_C` and `Gbar_C = 1 - G_C`. Setting `a = d-1, b = d`
recovers the one-delay form exactly. Setting `a = -1` handles an event week whose
reporting delay is **left-censored** (first seen at delay `b`, so we know only that
reporting finished by `b`) — it marginalises over when in `[0,b]` reports arrived.

---

## 3. The data

**FluSight**: weekly US influenza hospitalisation counts, published as cumulative
snapshots. Each snapshot (`as_of` date) republishes the whole history, so a given
event week is observed repeatedly and its count is revised over time.

Texas is the primary debugging series. Key structure:

* event weeks: 2022-02-05 to 2025-11-08 (197 weeks)
* snapshots: 2023-09-23 to 2025-11-12 (**only 64**)
* snapshot spacing: 7 days for 53 of 63 intervals, plus gaps of
  14 (x4), 18, 21 (x2), 42, 49 and **203 days**
* **71** of 197 event weeks are first published by delay 1 (`round()` week
  index; 64 at delay 0 under the `floor()` index used from script 23 on), **29**
  are first published at delays 2-8, and **97** are left-censored (first observed
  more than 8 weeks after the event). 71 + 29 + 97 = 197.

Empirically, from mature cohorts with long follow-up:

* observed down-revision rate implies a **mature observed retained fraction
  `r_mature^obs` ~ 0.957**. Equating this with the model's latent `p` requires
  the assumption that administrative revisions (section 8.3) have been separated
  from genuine individual retractions -- it is strong external evidence about the
  publication process, not a direct measurement of `p`
* mature-cohort retained-fraction asymptote **~0.952**
* finite-follow-up bias on that estimate **~0.005** (so it is a good estimate)
* beyond ~43 weeks of follow-up, further net movement is **~ -2.0%**

---

## 4. THE PROBLEM

The likelihood drives `p` far below the empirical value.

| model variant | optimum `p` |
|---|---:|
| original: dense grid, missing snapshots entered as observed zeros | **0.10** |
| cadence-corrected interval likelihood, follow-up truncated at 15 weeks | **~0.42-0.52** |
| cadence-corrected, full follow-up (up to 196 weeks) | **0.417** [not re-verified] |
| empirical / mature-cohort value | **~0.95** |

At the original `p = 0.10` optimum the model posits **406,405 expected additions
and 357,051 expected retractions** to explain a series whose total absolute
movement is about **63,000**, with **99.4% of `g_C` mass on lag 1**. It predicts
**295,186 down-revisions against 2,741 observed** — a factor of 108. The `p ~ 0.95`
solution predicts 2,975, within 8.5%, with the delay profile approximately right.

The likelihood is **confidently wrong**, not uninformative: the profile is sharply
peaked (`p = 0.15` is 44.6 nats below the peak; the empirical value is ~14,600
nats below). *[Both depths were computed with the unguarded profiler and have not
been re-verified. The qualitative point — the likelihood is confidently wrong
rather than flat — survives, and FINDINGS J supersedes it anyway: the curvature
is entirely an artefact of the parametric `g_C`.]*

**Consequence.** The production default currently **fixes `p`** at the empirical
mature-cohort rate for count-cumulative data. Backtested over 6 as-of dates x 4
states this is catastrophe-avoidance rather than uniform improvement: WIS 14.1 vs
46.4 (Texas) and 9.8 vs 31.5 (California), but 9.2 vs 9.0 (New York) and 9.6 vs 9.2
(Florida) — and it costs interval coverage in every state. Both arms are badly
under-dispersed (nominal 50% intervals covering 0.17-0.50).

---

## 5. What we want your opinion on

Five questions, stated fully in section 9. In brief:

1. Is our identification diagnosis correct, and is the proposed fix the right one?
2. **Can we sidestep the whole problem by redefining the predictive target** from
   the settled count to a finite-horizon observable? We think yes and it seems
   under-appreciated; we want it stress-tested.
3. Is `g_C = g_D` a defensible identifying restriction?
4. We can show that *deletion* cannot separate administrative revision from genuine
   retraction in this dataset. Is there an identification strategy we have missed?
5. Does the composite (product-of-marginals) likelihood matter here?

---

## 6. ESTABLISHED: the core identification result

### 6.1 The fundamental observable

At follow-up age `a`, a report is still present with probability

    r(a) = p + (1-p) * Gbar_C(a)

Finite cumulative data observe `r(a)`, **not `p`**. If `Gbar_C(a) ~ 0` over the
observed ages then `r(a) ~ p` and `p` is identified. If substantial retraction
probability remains beyond the observed ages, many `(p, g_C)` pairs give the same
`r(a)` and `p` is not separately identified.

Note `P(D_C = infinity) = 0` by construction does **not** rescue this. A proper
distribution can place retractions arbitrarily late but finitely; for a finite
dataset `D_C = 10^6` is indistinguishable from `D_C = infinity`. What matters is
`Gbar_C(a)` over the **observed** follow-up ages.

### 6.2 Parameter recovery — the decisive experiment

We simulated from the model's own generative process, using the real FluSight
publication cadence and the corrected interval observation model, then refit `p`
with nuisance parameters re-optimised. 12 replicates per cell.

| retraction tail | `Gbar_C(horizon)` | `p_true` | `r(horizon)` | mean `p_hat` | bias | sd |
|---|---:|---:|---:|---:|---:|---:|
| A short | ~0 | 0.60 | 0.600 | **0.593** | -0.007 | 0.001 |
| A short | ~0 | 0.95 | 0.950 | **0.970** | +0.020 | 0.000 |
| B moderate | 0.035 | 0.40 | 0.421 | 0.379 | -0.021 | 0.001 |
| B moderate | 0.035 | 0.60 | 0.614 | 0.551 | -0.049 | 0.002 |
| B moderate | 0.035 | 0.80 | 0.807 | 0.714 | -0.086 | 0.013 |
| B moderate | 0.035 | 0.95 | 0.952 | **0.965** | +0.015 | 0.012 |
| C **long** | **0.799** | 0.60 | 0.920 | **0.334** | **-0.266** | 0.002 |
| C **long** | **0.799** | 0.95 | 0.990 | **0.596** | **-0.354** | 0.065 |

> **CORRECTED 2026-09-03.** The table above was re-run after fixing an unguarded
> parabolic vertex refinement in the `mu_t` profile, which silently added
> spurious, replicate-dependent amounts to the profile likelihood
> (`devel/spa_diagnostics/FINDINGS.md` section S). The **previous version of this
> table should not be cited**; in particular `p_true = 0.60 -> p_hat = 0.150` was
> **0.334**, and the apparent instability at C/0.95 (`sd = 0.215`) was
> **0.065** — a systematic bias, not wandering.

Three readings:

1. **The simulations strongly reject a gross implementation failure in the tested
   settings, more cleanly than before.** `p_true = 0.95` returns **0.965-0.970**
   whenever retraction lands within the horizon, and the bias at the decisive cell
   is now *positive* (+0.015). It never collapses to 0.1-0.4 on correctly
   generated high-`p` data. (R = 12 replicates per cell, saturated event-specific
   means and profiling machinery rather than the production AR1/RTMB pipeline, so
   this is not yet production validation.)
2. **A long tail biases `p` down hard and consistently.** `p_true = 0.60 ->
   p_hat = 0.334` and `p_true = 0.95 -> p_hat = 0.596`, from datasets whose
   retained fraction at horizon (0.920, 0.990) *looks like* real Texas.
   **RETRACTED:** the earlier claim that this "reproduces the exact pathology",
   resting on `p_hat = 0.150` matching the real-data value, was an artefact of the
   profiler bug and must not be repeated. The mechanism is confirmed; the
   numerical coincidence was not real.
3. A systematic downward bias persists when identified — up to 0.086 in scenario
   B, though the 0.95 cell is now slightly positive.

**Superseded in a deeper sense.** Section J of FINDINGS later proved
*analytically* that `p` is not merely confounded but **exactly unidentified**
from finite-horizon cumulative data: holding the within-horizon retraction masses
`b_c = (1-p) g_C(c)` fixed, the profile likelihood in `p` is identically
constant (0.00e+00 nats) for any `p <= r(H)`. The recovery study above is
consistent with that, but the analytic result is the primary evidence and does
not depend on any simulation or on the profiler.

### 6.3 Confirmation on real data

Profiling `p` on real Texas under a free versus a pinned-short retraction tail:

| retraction tail | `Gbar_C(horizon)` | optimum `p` |
|---|---:|---:|
| free (optimised) | large | **0.50** |
| pinned short | 0.0000 | 0.55 |
| pinned short | 0.0004 | **0.70** |

Removing the model's freedom to place retraction mass beyond the horizon lifts the
optimum from ~0.50 to ~0.70. **The confounding is active on the real data.** But it
stops well short of 0.95, so there is a residual.

### 6.4 The central statistical statement

> Finite-horizon cumulative observations identify `r(a) = p + (1-p) Gbar_C(a)`,
> not `p` independently of unconstrained retraction timing. When retraction timing
> is observed or externally constrained, `p` is recoverable. When it is not, the
> likelihood trades a smaller `p` against later retractions.

We deliberately avoid saying `p` is "structurally non-identifiable" — it is not,
in general. The precise claim is that it is not identifiable *separately from
sufficiently late retraction timing* at finite horizon.

---

## 7. RULED OUT (do not re-propose these)

| hypothesis | verdict and evidence |
|---|---|
| Saddlepoint approximation error | **Ruled out.** Verified against direct convolution `P(Delta=z) = sum_w Pois(w+z; alpha) Pois(w; omega)`. Net effect on the low-vs-high-`p` comparison is **-0.38 nats against a 14,275-nat gap** (0.003%), and in the direction that *disfavours* low `p`. |
| Weak / flat identification | **The fitted parametric likelihood is not flat, but finite-horizon `p`-tail confounding is fundamental.** The lognormal-`g_C` profile is sharply peaked (section 4); with the retraction tail beyond the horizon free it is *identically constant* in `p` (FINDINGS J.3). All the curvature is the parametric family extrapolating, not the data. |
| Composite vs joint information loss | **Not the leading explanation of the original collapse, but not fully quantified.** An exact adjacent-pair composite moved the optimum only 0.10 -> 0.20 -- under the *earlier* observation formulation, before the cadence-aware interval model, and with nuisance parameters not fully reoptimised. Its magnitude under the final model is untested, and it remains a live candidate for the interval undercoverage. |
| Poisson under-dispersion | **Ruled out.** Negative Binomial gives a virtually identical profile (`phi ~ 1.7`, active) and displaces the churn not at all. |
| Batch severity explaining state heterogeneity | **Ruled out** on revision *size* — the four states are indistinguishable on max/p99/mean revision size. (But see 8.3: simultaneity across cohorts *does* differ, and was not tested at the time.) |
| Fabricated zeros as *the* cause | **Ruled out as sole cause.** Real cells alone still prefer `p = 0.10` by 12,412 nats. |
| A weakly-informative prior on `logit(p)` | **Insufficient.** MAP with mean `logit(0.95)`: `s=1.0 -> 0.497`, `0.5 -> 0.499`, `0.25 -> 0.506`, `0.10 -> 0.558`, `0.05 -> 0.685`. Only a near-degenerate prior (equivalent to the hard fix) moves it. |
| Bounding beyond-horizon mass `Gbar_C(h) <= tau` | **Insufficient.** Gives `p = 0.550` regardless of `tau` in {0.30, 0.10, 0.02}, because the model relocates retraction to late **within-horizon** lags that young cohorts have not yet revealed. |

### A numerical lesson worth recording

Every summation over a latent count in this model failed the same way at least
once: a truncation window sized by mean-and-variance when the conditional mass sits
20-50 standard deviations out. On a real cell (`z = 2512, alpha = 1204,
omega = 974`) the conditional mode is at `n* = |z| + 2w*` with
`w* = (-|z| + sqrt(z^2 + 4*alpha*omega))/2`, i.e. 3317, against a marginal-quantile
cutoff of 2528 — **the sum was truncated before reaching its own maximum**,
understating `logP` by 182 nats. The pmf still sums to 1 and the moments are still
exact; only the far tail is wrong, and under the low-`p` solution that is exactly
where the likelihood lives. Use **feasibility/mode-driven** truncation, never
marginal quantiles.

---

## 8. NEW FINDINGS (this session)

### 8.1 Missing snapshots were entering as observed zeros

The original preprocessing built a dense (event-time x delay) array and zero-filled
it. For Texas, **2,649 of 5,671 cells inside the horizon (46.7%) correspond to
snapshots that were never published**, all entering as observed zeros. 34 event
times are told "exactly zero for up to 28 consecutive weeks, then the entire
count" — when the truth is only that reporting finished **by** then.

Correcting this (moving to the interval likelihood of section 2) moves the optimum
from `p = 0.10` to `p ~ 0.42-0.52`. **This was the single largest correction so
far.** It is not yet in the production code path, only in prototypes.

### 8.2 Follow-up truncation at 15 weeks was hiding structural infeasibility

All prior diagnostics truncated follow-up at `cD = 15` weeks. Removing that
truncation makes the likelihood **structurally infeasible** — it assigns rate zero
to movements that actually occurred:

| configuration | infeasible negatives | infeasible positives | event weeks affected |
|---|---:|---:|---:|
| `cD = 15` (all prior work) | **0** | **0** | 0 of 99 |
| full follow-up, windowed (`cD = 111`) | 27 | 39 | 44 of 112 |
| full follow-up, unwindowed (`cD = 196`) | 113 | 65 | 117 of 197 |

Two distinct failures:

* **Late negatives** (interval starts at ages 16-169 weeks) require `g_C` mass at
  correspondingly long lags. A long `g_C` tail makes them feasible — but a long tail
  is exactly what confounds `p`. **So the model can only represent late
  administrative down-revisions by adopting the tail that destroys `p`.**
* **Late positives** (65 cells, totalling **+3,467**) cannot be fixed by any `g_C`,
  because `alpha_t(a,b)/mu_t = sum_{r=a+1}^{b} g_D(r)[...]` requires **`g_D`** mass
  at ages up to 38+ weeks, which the 60 exact-delay cohorts flatly refute. There is
  no administrative up-revision channel in the model at all.

`cD = 15` had **zero** such cells. The truncation everyone treated as a convenience
was load-bearing: it excluded precisely the observations that prove the observation
model is incomplete.

### 8.3 The down-revisions are snapshot-clustered and multiplicative

Earlier work indexed revisions by **event time** and concluded they were "diffuse".
Indexed by **calendar snapshot date** they are the opposite:

* top 1 of 61 Texas snapshots carries **36.1%** of all down-revision mass
* top 3 carry **63.1%**; top 5 carry **82.1%**
* New York: **1** snapshot carries 80% of its down-revision mass

The signature is **multiplicative**, and tight. On snapshot 2024-01-06, nine cohorts
(ages 1 through 9) were cut simultaneously by a decaying proportional gradient
(-0.20, -0.13, -0.13, -0.17, -0.10, -0.09, -0.09, -0.07, -0.05); the **sd of the
relative change is 0.048 against an sd of 118 for the absolute change**.

Separately, three consecutive snapshots each cut the newest (age-1) cohort by about
20% (-386, -385, -382 on event weeks 2023-12-30, 2024-01-06, 2024-01-13). Note this
matches the fitted pathology: at the low-`p` optimum, **99.4% of `g_C` mass sits on
lag 1**. A recurring ~20% cut at age 1 is exactly what a model with `g_C(1) ~ 1` and
small `p` produces.

Plausible real-world causes: retrospective reconciliation, deduplication,
reclassification, methodology change, redistribution across event weeks, provider
corrections. None is "an individual false report being retracted".

### 8.4 The unifying mechanism

`mu_t = lambda_t / p` with `omega ~ mu_t (1-p)`. Lowering `p` is the **only** dial
that inflates gross report flow while holding the settled level `lambda_t` fixed,
and it simultaneously opens the retraction channel. So *any* unexplained churn —
whatever its origin — is absorbed by pushing `p` down. **`p` is the model's churn
dial**, and every data-representation defect turns it down. This explains the
406,405-additions-vs-63,000-movement figure directly.

### 8.5 Long follow-up does not resolve `p` under the current revision model

| arm (Texas) | intervals used | `cD` | `p_hat` |
|---|---:|---:|---:|
| `cD = 15`, no deletions | 904 | 15 | 0.518 |
| full follow-up, windowed, drop structurally-impossible cells | 3,092 | 111 | 0.417 |
| **unwindowed** (all 197 event weeks), same | 8,474 | 196 | **0.417** |
| matched-volume placebo x 3 (random snapshots deleted) | ~2,370 | 111 | 0.362 / 0.447 / 0.424 |
| drop administratively-flagged snapshots, windowed | 2,421 | 111 | **0.804** |
| drop administratively-flagged snapshots, unwindowed | 6,892 | 196 | **0.970** |

Adding 85 mature pre-2023 cohorts and extending follow-up from 111 to 196 weeks
moved `p_hat` by **nothing** (0.417 -> 0.417). The mature-cohort tail information
gets absorbed into the long `g_C` tail rather than pinning it, because the
administrative negatives are still present forcing that tail.

Only deleting the administratively-flagged snapshots moves `p`, and the
matched-volume placebo (same number of intervals removed at random snapshots)
does **not** reproduce it — so it is *which* snapshots, not how many.

### 8.6 But deletion cannot settle the question — an important negative result

The administrative-snapshot rule removes **63.9% of all negative revision mass**
(2,494 of 3,905) but only **15.0% of positive mass**. Negatives are the only thing
informing `1-p`, so stripping 64% of them mechanically inflates `p`. **Therefore
`p_hat = 0.970` must NOT be read as a recovery of `p`.**

Worse, the natural control is impossible to construct: the 11 flagged snapshots
hold 2,494 of the 3,905 total negative mass, while **every other snapshot combined
holds only 1,411**. One cannot remove a comparable quantity of negative revision
without hitting the administrative snapshots, because in this dataset they are very
nearly the same set.

**Conclusion: no deletion experiment can separate "removed the negatives" from
"removed the administrative events."** Only a model that *explains* the negatives —
keeping the data and letting the likelihood arbitrate between an administrative
mechanism and a retraction mechanism — can settle it.

---

## 9. THE QUESTIONS

### Q1. Is the diagnosis right, and is the proposed fix the right one?

Our current causal account: administrative revisions (not representable by the
model) force a long `g_C` tail -> the long tail confounds `p` via
`r(a) = p + (1-p)Gbar_C(a)` -> `p` collapses. Cadence/zero-fill errors amplified
this substantially (0.10 -> 0.42) but were not the whole story.

Proposed fix: an explicit **snapshot-indexed multiplicative administrative
component**, conceptually

    Delta_obs(t,s) = Delta_report(t,s) + A(t,s)

with `A` shared across event times `t` at the same snapshot date `s`, regularised
toward "no adjustment". Given the evidence in 8.3 (tight relative changes, wide
absolute changes) we favour a multiplicative `kappa_s` scaling the cumulative.

**Is this the right structure? Is there a standard treatment of administrative
revision in the surveillance/nowcasting literature we should be using instead?**

### Q2. Can we sidestep identification entirely by changing the predictive target?

This is the question we most want stress-tested.

Our current predictive target is the **settled count** `lambda_t = p * mu_t`. In
code it is computed as `observed_cumulative + future_additions -
standing_retractions`, where `standing_retractions` is Binomial with probability
`retraction_mean / observed_mean`. That target depends on `p` **directly**, not
through `r(a)`. Two models the likelihood cannot distinguish:

| | `p` | `Gbar_C(a)` | `r(a)` | predicted settled / observed |
|---|---:|---:|---:|---:|
| A | 0.95 | 0.00 | 0.95 | **1.00** |
| B | 0.15 | 0.941 | 0.95 | **0.16** |

Identical fit; an 84% difference in the nowcast. This is confirmed empirically by
the WIS backtest in section 4.

**But**: if the target were instead the **finite-horizon observable**
`E[C_t(a+h)]` — "what will the cumulative read `h` snapshots from now" — that is a
function of `r(a+h)`, which the likelihood **does** identify for `h` inside or near
the observed age range. And that is how FluSight is actually scored: against a
later `as_of` snapshot, never against a true settled value.

**Questions.** Is this reasoning correct? For which `h` does it hold, and how
should we characterise the extrapolation error as `a+h` moves beyond well-observed
ages? Is "the eventual settled value" ever the right target for a nowcast that will
be scored against future snapshots? Are we fooling ourselves — e.g. does the
composite likelihood's known under-dispersion re-enter here in a way that makes the
finite-horizon predictive intervals wrong even when the point prediction is fine?

### Q3. Is `g_C = g_D` a defensible identifying restriction?

`g_D` is well identified (reporting is essentially complete by week 1, from the 60
exact-delay cohorts). Tying `g_C = g_D` forces `Gbar_C(a) ~ 0` for `a >= 2`, hence
`r(a) = p`, and `p` becomes immediately identified — internally, with no external
data.

Our reservations: (a) it buys identification by **assumption, not information** —
it relocates the untestable assumption from "`p` = 0.952" to "retraction is as fast
as reporting"; (b) retraction is an administrative review process with no obvious
reason to share a timescale with initial reporting, and if it is slower then `p`
absorbs the difference and is biased low; (c) evidence suggests it will land around
`p ~ 0.65-0.75`, not 0.95. Its one clear advantage over the current hard fix is
that `p` gets a posterior with propagated uncertainty rather than a point mass.

**Is there a principled weaker version** — e.g. a shared shape family with a free
scale, or a hierarchical link between `g_D` and `g_C` — that identifies `p` without
asserting equality?

### Q4. Is there an identification strategy we have missed?

Given 8.6 — that deletion provably cannot separate administrative revision from
genuine retraction in this dataset — what else could?

Candidates we have considered but not resolved:

* **Cross-state pooling.** Administrative recalibrations are jurisdiction-specific
  and land on specific snapshot dates; genuine retraction should share a `g_C`
  across states. Does the fact that 2024-11-16 appears in all four states (Texas
  +26%, California +3.3%, New York ~0, Florida -0.6%) help or hurt?
* **The delay profile of down-revisions.** 56.5% of observed down-revisions land by
  delay 1. Does the *shape* of the down-revision delay distribution identify `p`
  even when the totals do not?
* **Mature-cohort survival directly.** Beyond ~43 weeks of follow-up, net movement
  is ~ -2.0%. Can this be used as a direct plug-in estimate of `Gbar_C` with
  propagated uncertainty, rather than being fitted?

### Q5. Does the composite likelihood matter here?

The production likelihood is a product of one-delay marginals; the true joint
across delays is available in principle but has never been computed. The adjacent-
pair covariance is known exactly:

    Cov(Delta_t^d, Delta_t^e) = -mu_t (1-p) g_D(d) g_C(e-d),   d < e

An adjacent-pair composite moved the optimum only 0.10 -> 0.20. We treat this as
"real but minor" and have **not** computed the full joint. Our expectation — that
the joint also prefers low `p`, since no rearrangement of the same cells obviously
rescues a `+2512` the model cannot generate — is an expectation, **not a result**.

**Is that a safe thing to leave uncomputed?** And separately: the intervals are
badly under-dispersed (nominal 50% covering 0.17-0.50). Is that the composite
likelihood's known variance understatement, or a separate problem?

---

## 10. Things we would ask you not to do

These are recorded because we or a previous reviewer proposed them and measurement
refuted them:

1. Do not treat missing snapshots as observed zeros.
2. Do not assume every negative revision is an individual false-report retraction.
3. Do not infer that `P(D_C = infinity) = 0` makes `p` identifiable at finite horizon.
4. Do not use marginal-quantile or mean +/- SD truncation for latent-count sums.
5. Do not propose a weak prior on `p`; it is overwhelmed (evidence in section 7).
6. Do not constrain only total beyond-horizon retraction mass; timing across
   observed ages is what matters.
7. Do not attribute the pathology to the saddlepoint approximation; it is exonerated.
8. Do not attribute it to Poisson dispersion; NB behaves identically.
9. Do not revert to "batch revisions are THE cause"; that framing was superseded by
   the parameter-recovery result, which makes confounding the primary driver and
   batches a residual.
10. Please keep **identification**, **model misspecification**, **numerical
    approximation**, and **uncertainty calibration** explicitly distinct. Conflating
    them has cost us the most time.
