# Reviewer corrections and recommendations for `BRIEFING_count_cumulative.md`

## Purpose of this note

Read `BRIEFING_count_cumulative.md` as the factual record of the investigation, but apply the corrections and reframings below before drawing conclusions.

The investigation has made substantial progress. The remaining problem is now much more sharply characterized than the original “low-\(p\)” pathology suggested.

The central issues should be kept separate:

1. finite-horizon identification of \(p\) versus retraction timing;
2. misspecification of administrative revisions;
3. observation-cadence errors;
4. composite-likelihood efficiency/uncertainty;
5. numerical approximation;
6. choice of predictive estimand.

Several earlier hypotheses have been convincingly rejected, but a few statements in the current briefing are stronger than the evidence supports.

---

# 1. Most important mathematical correction: use the directly identified retraction masses

The briefing correctly introduces

\[
r(a)
=
p+(1-p)\bar G_C(a).
\]

There is an even cleaner finite-horizon parameterization.

Define

\[
\boxed{
b_c=(1-p)g_C(c).
}
\]

Then

\[
\sum_{c=1}^a b_c
=
(1-p)G_C(a),
\]

and therefore

\[
\boxed{
r(a)
=
1-\sum_{c=1}^a b_c.
}
\]

Also,

\[
\boxed{
p
=
1-\sum_{c=1}^{\infty}b_c.
}
\]

This exposes the identification problem directly.

Finite-horizon cumulative data can learn the retraction masses

\[
b_1,b_2,\ldots,b_H
\]

to the extent that the corresponding ages are observed.

But eventual \(p\) requires

\[
1-\sum_{c=1}^{\infty}b_c,
\]

which additionally requires knowing the unobserved mass

\[
\sum_{c>H}b_c.
\]

Thus, absent extrapolating restrictions,

\[
\boxed{
\text{finite-horizon data identify within-horizon retraction mass, not its eventual complement \(p\).}
}
\]

This is more precise than saying simply that the data observe \(r(a)\).

---

# 2. The interval likelihood can also be written without \(p\) separately

This reparameterization is useful because it shows exactly where the information lies.

For an observed cadence interval

\[
(a,b],
\]

the positive probability becomes

\[
\begin{aligned}
q_+(a,b)
&=
\sum_{r=a+1}^{b}
g_D(r)
\left[
p+(1-p)\bar G_C(b-r)
\right]\\
&=
\boxed{
\sum_{r=a+1}^{b}
g_D(r)
\left[
1-\sum_{c=1}^{b-r}b_c
\right].
}
\end{aligned}
\]

The negative probability becomes

\[
\begin{aligned}
q_-(a,b)
&=
(1-p)
\sum_{r=0}^{a}
g_D(r)
[
G_C(b-r)-G_C(a-r)
]\\
&=
\boxed{
\sum_{r=0}^{a}
g_D(r)
\sum_{c=a-r+1}^{b-r}b_c.
}
\end{aligned}
\]

Therefore, if the epidemic scale is parameterized directly by the gross report rate

\[
\mu_t,
\]

the complete finite-horizon interval likelihood can be written in terms of

\[
\boxed{
\mu_t,\quad g_D,\quad b_1,b_2,\ldots
}
\]

without \(p\) appearing separately.

This should be derived and checked explicitly.

It gives a very clear interpretation:

> \(p\) is an asymptotic quantity derived from the total retraction mass. Recent cumulative data directly identify only the part of that mass that has had time to occur.

This is arguably the cleanest mathematical statement produced by the investigation so far.

---

# 3. This also sharpens the distinction between statistical identification and parametric extrapolation

The briefing currently says that \(p\) becomes identifiable when retractions occur within the horizon.

That is reasonable operationally, but should be phrased carefully.

If \(g_C\) belongs to a restrictive parametric family, finite data may produce a sharply peaked estimate of \(p\) because the fitted family extrapolates the observed early retraction behavior into the unobserved tail.

That is not the same thing as nonparametric identification by the data.

A better distinction is:

### Data identification

The observations identify quantities such as

\[
b_c=(1-p)g_C(c)
\]

for observable lags \(c\).

### Parametric identification

A model for \(g_C\) extrapolates those quantities beyond the observed ages and thereby determines

\[
p=1-\sum_{c=1}^{\infty}b_c.
\]

When nearly all retraction mass occurs in the observed region, the extrapolation is negligible and \(p\) is effectively data-identified.

When substantial mass can occur later, the inferred \(p\) depends materially on the tail model.

Use the term

> **finite-horizon \(p\)–retraction-timing confounding**

rather than claiming universal structural non-identifiability.

---

# 4. Do not summarize the problem using only \(\bar G_C(H)\)

One correction is important in sections 6 and 9.

The relevant object is **not one scalar**

\[
\bar G_C(H)
\]

at the maximum observation horizon.

The relevant object is the survival curve

\[
\boxed{
\{
\bar G_C(a):a\in\mathcal A_{\mathrm{observed}}
\}
}
\]

over the entire distribution of cohort follow-up ages.

This is already implied by the later experiment showing that constraining only

\[
\bar G_C(H)
\]

does not solve the problem: the model simply moves retractions to late **within-horizon** ages that young cohorts have not yet reached.

Therefore replace statements like

> "`Gbar(horizon)` is the whole story"

with

> "The survival curve \(\bar G_C(a)\) over the observed age distribution is the relevant object."

Tables comparing retraction models should report at least

\[
\bar G_C(1),
\quad
\bar G_C(2),
\quad
\bar G_C(4),
\quad
\bar G_C(8),
\quad
\bar G_C(15),
\]

plus median and upper quantiles.

A single tail probability at the largest horizon can hide the actual confounding mechanism.

---

# 5. The mature-cohort value near 0.95 should NOT be called ground-truth \(p\)

This is one of the main conceptual corrections needed in the briefing.

The document currently describes quantities near

\[
0.952-0.957
\]

as an empirical estimate of \(p\).

But the later analysis also establishes that observed negative revisions include snapshot-clustered administrative recalibrations that may not correspond to individual false reports retracting.

If so,

\[
1-
\frac{\text{observed down revisions}}
{\text{observed appearances}}
\]

is not automatically an estimator of the model's latent genuine-report probability \(p\).

Similarly, the mature published retained fraction is an observable property of the publication process, not necessarily the probability that an individual original report was genuinely correct.

Rename this quantity something like

\[
\boxed{
p_{\mathrm{mature}}^{\mathrm{obs}}
}
\]

or

\[
\boxed{
r_{\mathrm{mature}}^{\mathrm{obs}}.
}
\]

Describe it as:

> the mature observed retained fraction of the published series.

It is strong external evidence about the operational process.

It equals the model's \(p\) only if administrative corrections have been separated from individual report/retraction dynamics.

This distinction is especially important because the current production hard fix uses this quantity.

The hard fix is therefore currently best viewed as an **operational calibration constraint**, not a clean direct measurement of latent \(p\).

---

# 6. Reconsider whether latent \(p\) is even the estimand the application needs

This leads directly to Question 2, which I think is more important than the briefing currently suggests.

There are at least three distinct possible targets.

## Scientific latent target

\[
\lambda_t=p\mu_t.
\]

Interpretation:

> eventual number of genuinely valid events under the individual report/retraction model.

This is scientifically meaningful only if the genuine/false-report interpretation is itself valid.

## Eventual published-data target

The count after a long operational maturation period.

This may include administrative definitions and corrections not represented by \(Y\).

## Finite-horizon operational target

\[
C_t(a+h),
\]

the count that will actually appear in the data source at a specified future publication date.

These should not be treated as interchangeable estimands.

If FluSight evaluation is against a future `as_of` snapshot, then the finite-horizon operational target may be the most directly relevant estimand for forecast scoring.

Changing the target is not merely a statistical workaround.

It is a substantive decision about what the nowcast is intended to predict.

---

# 7. Important correction to the proposed finite-horizon solution

The briefing currently argues that predicting

\[
E[C_t(a+h)]
\]

may avoid the \(p\)-identification problem because it depends on \(r(a+h)\).

That is close, but one more change is required.

The current model parameterizes

\[
\mu_t=\frac{\lambda_t}{p}.
\]

So if the model continues to parameterize the epidemic trajectory through \(\lambda_t\), then \(p\) still enters the gross report intensity.

To **truly sidestep \(p\)** for finite-horizon prediction, parameterize the observed process directly in terms of

\[
\boxed{
\mu_t
}
\]

rather than

\[
\lambda_t=p\mu_t,
\]

along with the identifiable retraction masses

\[
b_c=(1-p)g_C(c).
\]

Then for finite horizon \(h\),

\[
E[C_t(h)]
=
\mu_t
\sum_{r=0}^{h}
g_D(r)
\left[
1-\sum_{c=1}^{h-r}b_c
\right].
\]

No eventual \(p\) is required.

This is a major candidate model reformulation.

---

# 8. A horizon-specific estimand may be preferable

For an operational forecast, define

\[
\boxed{
\lambda_{t,H}^{\mathrm{obs}}
=
E[C_t(H)].
}
\]

Or equivalently model a horizon-specific retention probability

\[
\boxed{
p_H
=
r(H)
=
1-\sum_{c=1}^{H}b_c.
}
\]

Unlike eventual \(p\),

\[
p_H
\]

is potentially identifiable from data containing sufficient observations around age \(H\).

For multiple forecast horizons, model the complete finite-age retention curve

\[
r(1),r(2),\ldots,r(H)
\]

or equivalently

\[
b_1,\ldots,b_H.
\]

This may be statistically preferable to estimating an unobserved infinite-horizon cure fraction solely to forecast short-horizon published counts.

---

# 9. Finite-horizon prediction is safe only inside the empirically supported age range

The finite-horizon idea does not make extrapolation disappear.

For a case currently at age \(a\), predicting age

\[
a+h
\]

requires information about the retention/reporting process through that age.

Confidence should therefore depend on the density of historical cohorts observed at comparable ages.

Define an empirical support measure such as

\[
n_{\mathrm{risk}}(u)
=
\#\{\text{historical cohorts observed through age }u\}.
\]

The further \(a+h\) lies beyond ages with substantial historical support, the more the prediction again depends on tail assumptions.

Thus report or validate performance as a function of target age.

Do not simply classify horizons as "inside the maximum observed horizon."

---

# 10. Forecast calendar time, not “\(h\) snapshots,” where possible

Because FluSight publication cadence is irregular, "three snapshots ahead" does not correspond to a fixed amount of time.

For operational forecasting define the target by future publication date

\[
s^\star
\]

or elapsed calendar time.

For event week \(t\), the relevant age is then

\[
a^\star=s^\star-t.
\]

This keeps the target invariant to skipped publications.

---

# 11. The cadence correction is unquestionably real, but fix one wording issue

The briefing says that when the first snapshot occurs at delay \(b\), the truth is that "reporting finished by then."

That is too strong.

The correct statement is:

> At the first available snapshot at delay \(b\), we know the cumulative number present by \(b\); reports contributing to that snapshot may have arrived at any delay in \([0,b]\), and additional reports may still arrive later.

Thus

\[
C_t(b)-C_t(-1)=C_t(b)
\]

is an interval observation.

It does not imply the reporting process has completed.

---

# 12. Resolve an arithmetic/data-description ambiguity

The briefing says:

- 197 event weeks;
- 71 have an exact reporting delay;
- 97 are left-censored.

But

\[
71+97=168,
\]

not 197.

Explain the remaining 29 event weeks.

They may be outside the usable snapshot range, never observed, right-censored, or otherwise excluded, but the briefing should say so explicitly.

A new reviewer will notice this immediately.

---

# 13. Be careful with the phrase “structurally infeasible”

The full-follow-up analysis reports cells for which the fitted model assigns zero rate.

Before calling these observations structurally impossible, distinguish:

### True model-support impossibility

The mathematical model assigns exactly zero probability.

### Artificial computational/support truncation

The implementation has truncated \(g_D\) or \(g_C\) to finite tabulated support.

### Scientific near-impossibility

The parametric model technically assigns nonzero probability, but it is astronomically small.

For example, a lognormal delay model technically has positive mass at very large finite delays unless explicitly truncated.

Therefore Claude should verify exactly why those rates become zero.

Do not conflate a finite discretization table with the mathematical support of the delay distribution.

---

# 14. Dropping “structurally impossible” cells is only a diagnostic

The full-follow-up results around

\[
\hat p=0.417
\]

are based partly on dropping cells that the current observation model cannot represent.

That is useful evidence of misspecification.

It is not a valid final likelihood analysis.

Make this explicit in the summary table.

For example label the row:

> full follow-up, **unsupported cells removed for diagnostic purposes**

rather than simply:

> full follow-up.

The eventual production model must assign those observations a probability through an expanded revision mechanism rather than delete them.

---

# 15. The snapshot-clustering evidence for administrative revisions is strong, but not absolute proof

The pattern in snapshot time is highly informative:

- a small number of snapshots contain a large fraction of downward revision mass;
- many cohorts move simultaneously;
- proportional changes are much tighter than absolute changes.

This is inconsistent with the simplest model of independent homogeneous individual retractions and strongly supports a common snapshot-level process.

But replace language such as

> "None is an individual false report being retracted"

with something more defensible:

> "The joint pattern is difficult to reconcile with independent individual retractions alone and strongly suggests a shared administrative revision mechanism."

Some administrative audit event could, in principle, trigger many legitimate retractions simultaneously.

The important modeling fact is the **shared snapshot dependence**, not the semantic label assigned to each removed record.

---

# 16. A multiplicative administrative component is plausible, but a single \(\kappa_s\) may be too restrictive

A shared snapshot multiplier is a good starting model because the empirical evidence is much tighter on relative than absolute changes.

However, the example given in the briefing has percentage changes varying systematically with cohort age.

Therefore

\[
C^{\mathrm{obs}}_{t,s}
=
\kappa_s C^{\mathrm{latent}}_{t,s}
\]

may be too rigid.

Consider instead something like

\[
\boxed{
\log \kappa_{s,a}
=
u_s+w(a)v_s,
}
\]

where

- \(u_s\) is a snapshot-level administrative shock;
- \(w(a)\) is an age-loading function;
- \(v_s\) controls how strongly the shock varies with cohort age.

A simpler first prototype could use

\[
\kappa_{s,a}
=
1+\delta_s w(a).
\]

Strong shrinkage should keep

\[
\delta_s\approx0
\]

for ordinary snapshots.

The exact form should be selected from the empirical revision geometry rather than assumed in advance.

---

# 17. Consider a sparse contamination model for administrative snapshots

An alternative useful formulation is a mixture:

\[
Z_s
\sim
Bernoulli(\pi_{\mathrm{admin}}),
\]

where

\[
Z_s=0
\]

means the ordinary report/retraction process applies and

\[
Z_s=1
\]

activates an administrative revision mechanism shared across cohorts at snapshot \(s\).

This has several attractions:

1. most snapshots can remain ordinary;
2. administrative corrections are explicitly sparse;
3. the likelihood, rather than deletion, allocates evidence between mechanisms;
4. \(p\) no longer has to explain every negative revision.

This is conceptually preferable to deleting flagged snapshots.

---

# 18. Do not expect `g_C = g_D` to provide identification “for free”

There is no generic statistical reason that reporting delay and retraction delay should be equal.

They correspond to different mechanisms.

Therefore

\[
g_C=g_D
\]

is an identifying assumption, not information supplied by the data.

A weaker shared family with separate scale,

\[
D_{\mathrm{rpt}}\sim F(\theta_D),
\qquad
D_C\sim F(\theta_C),
\]

does not by itself solve the problem if \(\theta_C\) can still move retractions into poorly observed ages.

Similarly, a hierarchical relationship between the two distributions only helps to the extent that the hierarchical prior actually constrains \(g_C\).

The preferred source of identification is external or mature data that directly inform retraction timing.

---

# 19. A better weaker alternative is an age-specific survival envelope

If substantive knowledge can provide bounds such as

\[
\bar G_C(a)\le U(a),
\]

for selected ages, this is a more transparent identifying restriction than

\[
g_C=g_D.
\]

For one age,

\[
r(a)
=
p+(1-p)\bar G_C(a)
\]

implies

\[
p
=
\frac{r(a)-\bar G_C(a)}
{1-\bar G_C(a)}.
\]

If only

\[
0\le\bar G_C(a)\le U(a)
\]

is known, then \(p\) is partially identified.

For example,

\[
\boxed{
\frac{r(a)-U(a)}
{1-U(a)}
\le
p
\le
r(a),
}
\]

subject to valid probability bounds.

Using several ages gives intersecting constraints.

This could provide scientifically interpretable sensitivity intervals instead of forcing equality between two unrelated delay processes.

---

# 20. The observed down-revision delay *shape* does not identify \(p\) by itself

The briefing asks whether the fact that 56.5% of observed down-revisions occur by delay 1 helps identify \(p\).

It helps identify the **conditional timing of observed retractions**.

But under

\[
b_c=(1-p)g_C(c),
\]

normalizing within the observed horizon gives approximately

\[
\frac{b_c}
{\sum_{j\le H}b_j}
=
\frac{g_C(c)}
{G_C(H)}.
\]

The common scale factor

\[
1-p
\]

cancels.

Therefore the relative delay shape of observed retractions does not by itself determine \(p\).

It becomes informative about \(p\) only when paired with information about the total retraction mass or the unobserved tail.

This is an important answer to Question 4.

---

# 21. Mature follow-up can help, but only after administrative revisions are modeled

The current result that extending follow-up from 111 to 196 weeks leaves

\[
\hat p=0.417
\]

should be phrased as:

> Under the current misspecified revision model, additional mature follow-up does not resolve \(p\).

It does **not** establish that mature follow-up contains no identification information.

The current model is using late administrative negatives as evidence for a long \(g_C\).

Once those revisions have their own observation mechanism, mature cohorts may become precisely the data needed to constrain the true retraction tail.

This should be retested after adding the administrative component.

---

# 22. Cross-state pooling is promising, but only with an explicit decomposition

Pooling states could help because genuine retraction timing and administrative revisions may have different dependence structures.

A possible hierarchy is

\[
g_{C,s}
\sim
\text{shared population distribution},
\]

combined with jurisdiction-specific snapshot effects

\[
A_{j,s}.
\]

However, administrative corrections may also have national/shared causes, so do not assume independence across states.

The 2024-11-16 example is useful precisely because states respond differently on the same date.

A future model could contain:

- national snapshot effect;
- state-specific snapshot effect;
- common or hierarchically pooled retraction timing.

This is potentially a powerful source of separation.

---

# 23. The current statement that composite-likelihood information loss is “minor” is too strong

The evidence for this is not fully current.

The adjacent-pair experiment:

- was conducted under an earlier observation formulation;
- did not use the final cadence-aware interval model;
- and, in earlier diagnostics, nuisance parameters were not fully reoptimized under the pairwise objective.

Therefore replace:

> "Composite vs joint information loss — real but minor."

with:

> "Cross-delay dependence is not the leading explanation of the original collapse based on preliminary pairwise diagnostics, but its magnitude has not been fully reassessed under the corrected interval model."

This distinction matters.

---

# 24. The full joint likelihood cannot solve the fundamental unseen-tail problem

Even if the exact joint likelihood were implemented perfectly, it cannot observe retractions that occur after the dataset ends.

Therefore the fundamental finite-horizon equivalence between

\[
p
\]

and unobserved late retraction mass remains.

The full joint likelihood may:

- improve efficiency;
- exploit trajectory compatibility;
- alter finite-sample point estimates;
- improve estimation of within-horizon \(b_c\);
- change uncertainty.

But it cannot create information about events that have not yet occurred.

So computing the full joint likelihood is **not required to establish the main \(p\)-tail identification result**.

It remains important for validating the composite approximation and uncertainty.

---

# 25. Do not attribute the interval undercoverage to composite likelihood yet

The severe undercoverage is real, but several mechanisms remain possible:

- naive Hessian uncertainty under a composite likelihood;
- ignored cross-delay dependence;
- ignored cross-event dependence from administrative snapshots;
- model misspecification;
- fixed-\(p\) uncertainty omission;
- random-effect/Laplace approximation;
- finite-sample bias;
- forecast-target mismatch.

The composite likelihood is therefore one candidate, not an established cause.

For a composite likelihood, ordinary inverse-Hessian standard errors are generally not the appropriate uncertainty measure.

Use either:

\[
\boxed{
\text{Godambe/sandwich information}
}
\]

or, preferably here,

\[
\boxed{
\text{full-generative parametric/bootstrap calibration}.
}
\]

Because dependence exists both within event time and potentially across event times sharing a snapshot, a simple one-way clustering scheme may also be insufficient.

---

# 26. The recovery study is decisive qualitatively but not yet production validation

The simulation results strongly reject a gross implementation failure.

However,

\[
R=12
\]

replicates per scenario is too small for final validation.

Also, the recovery study uses saturated event-specific epidemic means and approximate profiling machinery rather than the complete production AR1/RTMB pipeline.

Therefore replace:

> "The estimator is sound."

with:

> "The recovery experiment provides strong evidence against a gross implementation failure and demonstrates the expected finite-horizon confounding mechanism."

For production validation, repeat selected scenarios using:

- the actual AR1 model;
- the actual RTMB objective;
- the production optimizer;
- substantially more replicates;
- several random initializations.

At least the critical high-\(p\), short/moderate/long-tail scenarios should be repeated with tens to hundreds of datasets.

---

# 27. Do not blame the small recovery bias on the composite likelihood without testing it

The observed bias of roughly

\[
0.02-0.08
\]

is worth investigating.

But under correctly specified marginals, composite likelihoods are typically expected to lose efficiency more directly than to create a large systematic asymptotic bias.

Before attributing this bias to composite-likelihood information loss, check:

1. grid/profile approximation;
2. nuisance optimization accuracy;
3. warm-start dependence;
4. finite-sample bias;
5. boundary behavior;
6. discretization of \(g_C\);
7. exact versus approximate Skellam likelihood;
8. saturated-\(\lambda_t\) profiling versus production AR1 fitting.

The previously measured SPA error appears much too small to be an obvious explanation, but this should be tested directly on the simulation settings.

---

# 28. Recommended reparameterization experiment

A particularly valuable next experiment is to fit the finite-horizon model directly using

\[
\boxed{
\mu_t
}
\]

and

\[
\boxed{
b_c=(1-p)g_C(c)
}
\]

instead of

\[
\lambda_t,\;p,\;g_C.
\]

For a chosen maximum operational horizon \(H\), estimate

\[
b_1,\ldots,b_H
\]

with constraints

\[
b_c\ge0,
\]

and

\[
\sum_{c=1}^{H}b_c\le1.
\]

Then

\[
r(H)
=
1-\sum_{c=1}^{H}b_c
\]

is directly available.

This model asks the data only for quantities that they can observe.

If an eventual settled target is required, append a separate model for the unobserved tail

\[
\sum_{c>H}b_c
\]

using external information.

This cleanly separates:

\[
\boxed{
\text{finite-horizon estimation}
}
\]

from

\[
\boxed{
\text{infinite-horizon extrapolation}.
}
\]

---

# 29. Suggested hierarchy of estimands

The package may ultimately benefit from explicitly exposing two targets.

## Operational nowcast

"What will this published count be at future age \(H\)?"

Estimate directly from finite-horizon quantities.

## Settled/latent nowcast

"What would the eventual genuine count be under the report/retraction model?"

Requires extrapolation to

\[
p
=
1-\sum_{c=1}^{\infty}b_c.
\]

This second target should report sensitivity to assumptions about late retractions.

Do not silently use one when scoring against the other.

---

# 30. Recommended next experiments

Proceed in roughly this order.

### 1. Validate the \(b_c\) reparameterization algebraically

Show that the cadence interval likelihood can be written entirely in terms of

\[
\mu_t,\quad g_D,\quad b_c
\]

for all observed finite horizons.

Check numerically against the existing \(p,g_C\) parameterization.

### 2. Fit the operational finite-horizon model

Estimate

\[
\mu_t
\]

and within-horizon \(b_c\) directly.

Compare fit, stability and forecasts with the current \(p,g_C\) model.

### 3. Reassess the scientific target

Determine whether the actual use case requires:

- future published count;
- count at a standardized maturity age;
- or latent eventual genuine count.

### 4. Model snapshot-level administrative revisions

Prototype a sparse shared multiplicative/age-loaded component rather than deleting snapshots.

### 5. Refit mature cohorts with the administrative component

Ask whether mature data now constrain the late retraction tail.

### 6. Re-evaluate cross-state pooling

Separate shared retraction timing from national/state administrative effects.

### 7. Expand simulation recovery

Use the production AR1/RTMB implementation and many more replicates.

### 8. Reassess composite likelihood

Only after the corrected interval and administrative models are in place, compare marginal, pairwise and — on manageable subsets — exact joint likelihoods.

### 9. Calibrate uncertainty

Use a generative bootstrap that reproduces:

- epidemic variation;
- reporting;
- retraction;
- actual snapshot cadence;
- administrative revision events.

---

# 31. Revised answers to the five questions in the briefing

## Q1. Is the identification diagnosis correct?

**Mostly yes, but sharpen it.**

Finite-horizon data identify within-horizon retraction masses

\[
b_c=(1-p)g_C(c)
\]

and retention probabilities

\[
r(a),
\]

not eventual \(p\) separately from unobserved late retractions.

Administrative revision misspecification then contaminates those same retraction signals.

The proposed snapshot-level administrative component is a sensible next model, although a single common multiplier may need age-dependent loadings.

---

## Q2. Can the problem be sidestepped by changing the target?

**Potentially yes, and this is a serious modeling option.**

But to remove \(p\) from the problem, also reparameterize the epidemic scale using

\[
\mu_t
\]

rather than

\[
\lambda_t/p.
\]

Then finite-horizon published counts can be modeled through directly estimable retention quantities.

This works best for forecast ages well represented historically.

It does not identify the eventual settled latent count.

---

## Q3. Is \(g_C=g_D\) defensible?

**Not without substantive mechanism-specific evidence.**

It buys identification by assumption.

A more defensible strategy is external information on \(g_C\), age-specific survival constraints, partial identification, or hierarchical transfer from data where retraction timing is genuinely observed.

---

## Q4. Is there another identification strategy?

Several are promising:

1. direct \(b_c\) / retention-curve parameterization;
2. mature cohorts after administrative revisions are modeled;
3. cross-state hierarchical decomposition;
4. external retraction-timing data;
5. age-specific bounds on \(\bar G_C(a)\);
6. partial-identification intervals for eventual \(p\).

The observed within-horizon delay *shape* of retractions alone cannot identify \(p\), because the factor \(1-p\) cancels when that shape is normalized.

---

## Q5. Does the composite likelihood matter?

**Probably for efficiency and uncertainty; its effect on the final corrected point estimate remains incompletely quantified.**

It cannot solve the fundamental post-horizon identification problem.

The old adjacent-pair result should not be used as definitive evidence that the effect is minor under the final cadence-aware model.

---

# 32. Revised central statement for the new session

The cleanest way to summarize the current state is:

\[
\boxed{
\text{The finite-horizon data identify the observed retraction masses }
b_c=(1-p)g_C(c)
\text{ over observable lags.}
}
\]

\[
\boxed{
\text{Eventual }p
=
1-\sum_{c=1}^{\infty}b_c
\text{ additionally requires information about unobserved late retractions.}
}
\]

Administrative snapshot-level revisions contaminate the observed \(b_c\) because the current model interprets every downward correction as an individual false-report retraction.

Therefore there are two distinct modeling tasks:

1. model the finite-horizon report/retraction process using identifiable quantities;
2. separately model administrative revisions and any extrapolation from finite-horizon retention to eventual \(p\).

That framing is more precise than treating the entire problem as either “bad estimation of \(p\)” or “batch revisions.”

---

# 33. Statements in the briefing that should be softened or changed

Replace:

> "The estimator is sound."

with:

> "The simulations strongly reject a gross implementation failure in the tested settings."

Replace:

> "Weak / flat identification — No."

with:

> "The fitted parametric likelihood is not flat, but finite-horizon \(p\)-tail confounding remains fundamental."

Replace:

> "`p_empirical` is ~0.957."

with:

> "The mature observed retention fraction is ~0.95; equating it with latent \(p\) requires the assumption that administrative revisions have been separated from genuine retractions."

Replace:

> "`Gbar(horizon)` is the whole story."

with:

> "The survival curve over the complete distribution of observed follow-up ages is the relevant object."

Replace:

> "Administrative revisions are not individual false-report retractions."

with:

> "Their shared snapshot-level pattern is difficult to reconcile with independent retractions alone and motivates a separate administrative component."

Replace:

> "Composite vs joint information loss is minor."

with:

> "Preliminary pairwise diagnostics suggest it is not the leading cause, but its effect under the final interval model has not yet been fully quantified."

Replace:

> "Long follow-up does not help."

with:

> "Long follow-up does not resolve the problem under the current revision-misspecified model."

---

# 34. Final recommendation to the new reviewer

Do not start by tuning \(p\), imposing \(g_C=g_D\), or implementing the exact joint likelihood.

First verify the finite-horizon reparameterization

\[
b_c=(1-p)g_C(c)
\]

and determine whether the package can model its operational forecasting target directly in terms of

\[
\mu_t,\quad g_D,\quad b_c.
\]

In parallel, characterize and model the snapshot-level administrative process.

Only after those two pieces are separated should eventual \(p\), full-joint dependence, and uncertainty calibration be revisited.

The most useful question for the next session is no longer:

> "Why does the optimizer estimate the wrong \(p\)?"

It is:

> **"Which quantities are actually identified by the finite publication history, which quantities require extrapolation, and which observed revisions do not belong to the report/retraction mechanism at all?"**