# Plan: a working increment nowcaster for count-cumulative data

**2026-09-02.** Written after FINDINGS sections J and K. Supersedes the ordering
in *Reviewer corrections and recommendations* section 30, for the reason given in
K.4: the marginal increment law is rejected by a count of zeros, so every item
downstream of it is currently measuring a model that does not fit.

---

## 0. What the data are

Texas, `w15` arm, 805 increments after first observation:

| feature | value |
|---|---:|
| mean | 0.56 |
| variance | 1635 |
| exactly zero | 620 (77%) |
| `\|z\| > 50` | 27 (3%) |
| trajectory `C_t(a)/C_t(last)` | 1.012 at age 0 -> 1.000 by age 9 |

Movement is concentrated in snapshots (top 3 of 62 hold 36% of the mass, top 6
hold 56%, and 14 snapshots move nothing at all), but **within** an active
snapshot only a subset of cohorts move (9, 4, 11, 5, 4, 1 of 15 in the six
largest). Sparsity is needed in both dimensions. A single shared multiplier
`kappa_s` across all cohorts of a snapshot (reviewer section 16) is too rigid.

So the process to model is: *reporting is essentially complete at first
publication; thereafter nothing happens, except occasionally, in bursts, to some
cohorts, roughly proportionally.*

---

## 1. Fix the estimand first (reviewer section 30.3)

**Recommendation: the finite-horizon operational target.**

```
lambda_{t,H}^obs = E[ C_t(H) | data now ]
```

the count that will actually be published at a stated future age (or, better,
future publication date `s*`, giving age `a* = s* - t`, so the target is
invariant to skipped snapshots -- reviewer section 10).

Reasons: it is what FluSight scores; it is what section J proves the data
identify (`r(a)` for observable `a`); and it removes `p` from the model rather
than pinning it. The settled/latent target stays available as a *second*,
clearly-labelled estimand requiring an explicit tail assumption (section 29).

**Consequences.** Parameterise the epidemic scale by the gross report rate
`mu_t`, never `lambda_t = p mu_t` (reviewer section 7). Report `r(a)`, not `p`.
The production hard fix (`p` pinned near 0.95) becomes unnecessary for this data
type.

---

## 2. The model

Two components, matching the two things the data do.

### 2a. Arrivals -- already validated, keep as is

For the first interval and any genuine late reporting, the section J `b`-form:

```
alpha_t(a,b) = mu_t sum_{r=a+1}^{b} gD(r) [1 - Bcum(b-r)]
```

`p`-free, verified to machine precision against the old parameterisation
(J.2), and it handles left-censored first observations without a separate
mechanism.

### 2b. Revisions -- a hurdle, sparse in both dimensions

For a post-completion interval on cohort `t` at snapshot `s`:

```
M_{t,s} ~ Bernoulli(pi_{s,a})            does this cohort move at this snapshot?
Delta | M = 0  =  arrivals only          (exactly 0 whenever arrivals are done)
Delta | M = 1  ~  arrivals - Revision
```

with the revision on a **relative** scale, since the empirical spread of
proportional changes is far tighter than of absolute ones (briefing 8.3):

```
Revision_{t,s} ~ law with location kappa_s * L_t(a) and dispersion phi
logit pi_{s,a} = pi_0 + nu_s + w(a)          nu_s strongly shrunk; 23% of
                                             snapshots are exactly inactive
log kappa_s    = u_s                         shared across cohorts, heavy-tailed
```

`L_t(a)` is the cohort's current level. **Use the model's expected level**
`mu_t * (published fraction at age a)` rather than the observed `C_t(a)`, so the
model stays generative and `simulate`/`backtest` keep working. The conditional
variant (plug in observed `C_t(a)`) is the fallback if the generative one fits
badly -- it is legitimate for forecasting but weakens simulation.

### 2c. Why this fixes the pathology

`M = 0` gives an exact zero, so the 77% costs nothing. Jump dispersion is bought
by `kappa_s` on the 3% of cells that move, not by a global churn level applied to
every interval. `p` stops being the dispersion dial (K.4), which is the whole
mechanism behind the low-`p` collapse.

### 2d. Estimation

Marginalise `M_{t,s}` analytically -- two points per cell, a `logsumexp`, exact
and cheap, and it keeps the discrete part out of the Laplace approximation. Only
`nu_s` and `u_s` (62 snapshots each) go to Laplace as random effects, which is
what the existing RTMB pipeline already does well. No new quadrature.

---

## 3. Stages, each with the gate it must pass

### Stage 0 -- generality  [DONE, script 28, FINDINGS L]
**Gate passed**, with three corrections to the specification above:
1. participation is driven by cohort **age** (58% at age 1 -> 2.3% at age 15,
   consistent across all 53 locations), not by snapshot sparsity -- the snapshot
   concentration of movement mass was mostly a season effect;
2. the shared release calendar is real but second-order (cross-state correlation
   of participation rate 0.213 vs a 0.000 null, z = 62), so a national snapshot
   effect on `pi` is a stage-2 refinement, not the primary structure;
3. small moves are additive and only large ones multiplicative, so the revision
   scale is `sigma0 + sigma1 * level`, not a pure `kappa_s`.

Consequently the retention curve `b_c` is dropped from the model entirely: down
moves belong to the revision component (FINDINGS L.4). See L.5 for the revised
specification, which is what script 29 fits.

### Stage 1 -- prototype the increment law  [DONE, scripts 29-32, FINDINGS M+N]
**All four gates passed**, by script `32_sign_magnitude.R`, after three rejected
iterations that are worth keeping in the record:
* 29 (one revision scale) -- zeros 558/620, PIT p 5e-4;
* 30 (two scales) -- zeros 609/620, PIT p 1.6e-4, +90 nats;
* 31 (age-dependent asymmetry) -- 0.40 nats, `psi` = 1.00 at every age, which
  proved the obstruction was structural: inside the Skellam family the same
  `lam` sets both the spread and the sign balance, and the data need those
  decoupled (144 up vs 41 down, |move| median 5 max 615);
* 32 -- sign and magnitude modelled separately, logL −1500.94, zeros 620/620,
  PIT KS p 0.096, tail exceedances within 8-39% of expected, and ~50x cheaper
  because it needs no Bessel.

Note gate 2 as originally written was invalid (it conditioned on the observation
being extreme); FINDINGS N.4 redefines it as an unconditional predictive check
and voids the earlier numbers for it.

The original stage-1 text follows for reference.

### Stage 1 (original) -- prototype the increment law
Fit 2a + 2b on Texas `w15`, `mu_t` profiled as in scripts 23/26.
**Gates, in order:**
1. **zeros**: predicted exact-zero count within ~10% of 620 (current model: 85);
2. **jumps**: the 27 cells with `|z| > 50` inside their 95% predictive interval
   at roughly nominal rate;
3. **randomised PIT** on all 805 increments approximately uniform;
4. logL materially above the -3055 of script 26 (it should be, and if it is not,
   the hurdle is not what is missing).

### Stage 2 -- the forecast  [PARTIAL, script 33, FINDINGS O]
Rolling origin, 16 origins, 794 triples: **WIS 4.568 vs 5.033 empirical (+9.2%)
and 5.770 persistence (+20.8%)**, skill rising with horizon (+1.7% at h=1 to
+14.8% at h=4). Two defects and one gap:
* **over-dispersed**: 90% coverage 0.977 against nominal 0.90 (O.3);
* at target ages 9-15 plain persistence still wins (1.042 vs 1.333) (O.4);
* the gate named `baselinenowcast` and the production model as comparators and
  neither has been run yet (O.5).
Two likelihood bugs were found and fixed here (O.1), one of which -- an underflow
that let `lmag` return log-probability 0 -- was gameable by the optimiser.

### Stage 2 (original) -- the forecast, on the stage 1 estimand
Predictive distribution for `C_t(a+h)`: a mixture over which future snapshots
activate, so it has a point mass at "no change" plus a tail -- the shape the
current model cannot produce and the likely cause of the interval undercoverage.
**Gate:** WIS against `baselinenowcast` and against the current production model,
reported **by target age** as well as by horizon (reviewer section 9 -- support
falls from 761 intervals at lag 1 to 49 at lag 15, so a horizon-only table hides
where the model is extrapolating).

### Stage 3 -- uncertainty
Do **not** use inverse-Hessian standard errors: the objective is a composite
likelihood and dependence runs both within event time and across cohorts sharing
a snapshot (reviewer section 25). Calibrate by a generative bootstrap that
reproduces epidemic variation, reporting, revision events and the real snapshot
cadence (section 30.9).
**Gate:** nominal coverage on held-out snapshots, by target age.

### Stage 4 -- package integration
Wire into `model()` / `nowcast()` as a revision component alongside the existing
validation machinery (`R/31_retraction_likelihood.R`, `R/28_confirmation_likelihood.R`).
Expose `r(a)` and the horizon target; do not expose `p` for this data type.
Retire the pinned-`p` hard fix for count-cumulative input.
**Gate:** existing tests green; `Validation_processes.Rmd` updated.

### Stage 5 -- only now, revisit the deferred questions
Mature-cohort refit with the revision component in place (does the tail become
informative? reviewer section 30.5), cross-state pooling (30.6), expanded
recovery on the production AR1/RTMB pipeline (30.7), composite-vs-joint (30.8).
Each of these is worth doing *after* stage 1, and worth nothing before it.

---

## 4. Dropped, with reasons

| dropped | reason |
|---|---|
| estimating `p` for the operational target | exactly unidentified (J.3); the target does not need it |
| the pinned-`p` hard fix | an operational calibration constraint compensating for a misspecified increment law |
| `g_C = g_D` | identification by assumption, and it addresses timing, not dispersion (reviewer section 18) |
| exact joint likelihood | cannot repair a rejected marginal law (K.4); revisit at stage 5 |
| more flexible retraction curves | 9.8 nats for 12 parameters says this is not the binding constraint (K.1) |

---

## 5. Risks

1. **The burst structure may be partly deterministic** (data-source release
   policy), in which case `pi_s` is predictable from the calendar and should use
   a covariate rather than a random effect. Check at stage 0.
2. **62 snapshots is not many** for snapshot-level effects. Hierarchical
   shrinkage is essential; if `nu_s` and `u_s` are jointly weak, collapse to a
   single `pi` and a heavy-tailed `kappa`.
3. **Generative vs conditional level** (2b). If the generative form fits badly,
   the conditional fallback changes what `simulate()` can honestly do, and that
   needs to be said in the docs rather than papered over.
4. **The linelist retraction path is a different model** and is not touched by
   this plan; keep the two data types clearly separated in the interface.
