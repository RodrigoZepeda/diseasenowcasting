# Nowcasting under confirmation / retraction (up-and-down revisions)

A mathematical derivation + implementation plan for extending the
censored-regression nowcast (article `article_v1`, eqs. 2–10) to
surveillance streams whose cumulative incidence **moves up *and* down**
as cases get confirmed and un-confirmed.

Canonical dataset:
[`tbl.now::flusight`](https://rodrigozepeda.github.io/tbl.now/reference/flusight.html)
— for each `(target_end_date = t, location = s)` the count `observation`
revises across `as_of = r`, occasionally **downward** (empirically ≈1.2
% of weekly increments are negative, range −54…+437).

> Status: math derived and **numerically validated** (see §7). No
> package code changed. This is a design document.

------------------------------------------------------------------------

## 1. The phenomenon and why the naïve fix fails

A “confirmed influenza” admission for week `t` is reported with a delay;
later, some reports are **retracted** (reclassified, corrected) —
producing a *negative* increment. The series for week `t` therefore
first over-shoots, then settles to its final value.

The tempting fix — *multiply the reported count by a confirmed fraction
`p`* — is wrong for three independent reasons, all of which the
derivation below makes precise:

1.  **Timing.** Retractions lag additions by a *convolved* delay
    `G_W = G_D * G_C`. Early on you have seen the additions but **not
    yet** the retractions that will cancel some of them, so the correct
    correction factor is **time-dependent**, not a flat `p`.
2.  **Variance.** Pending retractions inject extra uncertainty: the
    observation variance is `additions + removals`, strictly larger than
    a thinned-Poisson variance. Flat-`p` scaling is over-confident.
3.  **Information.** The negative increments are *data* about the
    retraction process; a flat `p` throws them away and biases the delay
    fit.

------------------------------------------------------------------------

## 2. Framework: a marked Poisson point process with a transient (retraction) stream

I model the reports as a **marked Poisson point process** — the same
object that already underlies the paper (eq. 2 is a marked Poisson
process whose marks are reporting delays). The extension is to give some
marks a **second, cancelling event**.

Equivalently and most cleanly, the observed signal is the
**superposition of two independent marked Poisson streams**:

| Stream | Rate (per event-week `t`) | Marks (per report) | Net effect |
|----|----|----|----|
| **Genuine** | `λ_t` (← the nowcast target) | `+1` at delay `D ~ G_D` | permanent `+1` |
| **Erroneous / transient** | `η_t` | `+1` at `D ~ G_D`, then `−1` at `D + C`, `C ~ G_C` | self-cancels to `0` |

- The **genuine stream alone is exactly the current model** (eq. 2 with
  mean `λ_t`).
- The **erroneous stream** is what creates the down-revisions.
- `G_W := G_D * G_C` is the *withdrawal-delay* cdf (onset → retraction);
  `*` is discrete convolution. `g_C(0)=0` (a retraction happens strictly
  after its report).

**Per-case (semi-Markov) reading.** Each report is a tiny Markov-renewal
/ competing-risks process:
`not-yet-reported → reported → {permanent | retracted}`, with sojourn
`G_D` to “reported” and sojourn `G_C` to “retracted”. The
sojourn-to-absorption being a **sum** `D + C` is exactly why the
withdrawal delay is the convolution `G_W = G_D * G_C`. So the
semi-Markov view supplies the *delay algebra*; the Poisson-marking view
supplies the *likelihood independence* (§3). I lead with the point
process because it is what makes the likelihood tractable and connects
directly to the existing code.

**Parameter map** (so the epidemic model keeps targeting the quantity of
interest):

    λ_t  = E[final genuine count for week t]      ← epidemic process f(t): HSGP / AR(1) / SIR (unchanged)
    p    = P(a report is genuine) ∈ (0,1]         ← new (e.g. constant, logit-scale)
    η_t  = λ_t (1−p)/p = E[erroneous reports]      ← nuisance rate of the transient stream
    μ_t  = λ_t + η_t = λ_t / p = E[gross reports]

------------------------------------------------------------------------

## 3. Key results (with proofs) — all validated in §7

Write `g_D(d)`, `G_D(d)` for the report-delay pmf/cdf,
`g_W = g_D * g_C`, `G_W = cumsum g_W`. Fix event-week `t`; let
`d* = r − t` be the elapsed weeks observed.

**Lemma 1 (Poisson marking / colouring).** If the number of points is
`Poisson(μ)` and each point is independently assigned a type `τ` with
probability `q(τ)`, then the type-counts `{N_τ}` are **independent**
`Poisson(μ q(τ))`. *(Standard.)*

**Lemma 2 (renewal convolution).** A report’s retraction occurs at delay
`D + C`; hence the withdrawal-delay law is `g_W = g_D * g_C`,
`G_W = G_D * g_C`. *(Sum of independent sojourns.)*

**Proposition 1 (gross additions and removals).** Conditional on
`λ_t, η_t`,

    A_d  := # reports appearing  at delay d  ~ Poisson( μ_t · g_D(d) ),   independent across d
    W_d  := # reports retracted  at delay d  ~ Poisson( η_t · g_W(d) ),   independent across d

*Proof.* “Appears at `d`” and “retracted at `d`” are each disjoint types
over `d`; apply Lemma 1 to each stream and superpose (additions come
from both streams at rate `(λ_t+η_t)g_D = μ_t g_D`; removals from the
erroneous stream at rate `η_t g_W`). ∎

**Proposition 2 (the observed increment is Skellam).** The net weekly
increment `m_t^d = A_d − W_d` satisfies

    m_t^d  ~  Skellam( α_t^d , β_t^d ),     α_t^d = μ_t g_D(d),   β_t^d = η_t g_W(d),
    E[m_t^d] = α − β,   Var[m_t^d] = α + β.

independently across `d` **under the independent-thinning idealisation**
(treat the erroneous `+1` and its later `−1` as two independent
processes). The exact cross-delay dependence is negligible: simulated
`max|corr| ≈ 0.06` (§7). This is the model where the likelihood “**sums
over all integers**”: the Skellam pmf is the convolution

    P(m_t^d = j) = Σ_{w ≥ max(0,−j)}  Poisson(j+w ; α) · Poisson(w ; β),      j ∈ ℤ,

`w` = number of (latent) removals, additions `= j+w`. The observed net
`j` ranges over all of `ℤ`; the latent removal count `w` is summed out.

**Proposition 3 (the current cumulative is a thinned Poisson — no
signedness).**

    C_t(d*) = Σ_{d≤d*} m_t^d  ~  Poisson( λ_t G_D(d*) + η_t [G_D(d*) − G_W(d*)] )
                              =  Poisson( μ_t [ G_D(d*) − (1−p) G_W(d*) ] ).

*Proof.* `C_t(d*) =` (everything appeared by `d*`) − (everything
retracted by `d*`) = Σ`A_d` − Σ`W_d`, a sum of independent Poissons ⇒
Poisson with the summed rate `μ_t G_D(d*) − η_t G_W(d*)`; substitute
`η_t=(1−p)μ_t`. The two equivalent forms separate the **genuine** part
`λ_t G_D(d*)` (→`λ_t`) from the **still-standing erroneous** part
`η_t[G_D−G_W]` (→`0`). ∎ The cumulative is always `≥ 0`: *signedness
lives only in the increments.*

**Proposition 4 (consistency / target recovery).** As `d*→∞`,
`G_D,G_W→1`, so `E[C_t(∞)] = λ_t`. The model converges to the final
genuine count — the nowcast target.

**Proposition 5 (exact reduction).** If `p=1` (`η_t=0`): Prop. 2 gives
`m_t^d ~ Poisson(λ_t g_D(d))` and Prop. 3 gives
`C_t(d*) ~ Poisson(λ_t G_D(d*))` — i.e. the **current model (eqs. 2–10)
verbatim**. The extension is a strict generalisation. ∎

**Effective detection fraction (the right “correction”).** Define
`Q_1(d*) = G_D(d*) − (1−p) G_W(d*)`, so `E[C_t(d*)] = μ_t Q_1(d*)`.
Since the final mean is `λ_t = p μ_t`, the **fraction of the final count
already realised** at horizon `d*` is

    ρ(d*) = E[C_t(d*)] / λ_t = [ G_D(d*) − (1−p) G_W(d*) ] / p .

`ρ` is **non-monotone**: limits are `ρ(∞) = (1 − (1−p))/p = 1`
(converges to the final), but early on
`G_W ≈ 0 ⇒ ρ(d*) ≈ G_D(d*)/p > G_D(d*)` — an **over-count**, because the
additions have arrived but the retractions that will cancel some of them
have not. It then decays back toward `1` as retractions catch up. A flat
factor `p` (constant in `d*`) cannot reproduce this
overshoot-then-settle shape — that is the precise sense in which
“multiply by the confirmed fraction” is wrong.

------------------------------------------------------------------------

## 4. Likelihood (what to actually code)

Per `(t, s)`, conditional on the latent log-mean (which carries
over-dispersion):

    ℓ_t = Σ_{d=0}^{d*}  ln Skellam( m_t^d ; μ_t g_D(d) , η_t g_W(d) )            (Regime B, primary)

- **Future bins (`d>d*`) are simply omitted** — Poisson independence
  means no separate survival term is needed for the increments (the
  `S_k` of eq. 6 is replaced by bin independence). The dependence on
  `λ_t` enters through `α_t^d = μ_t g_D(d)`.
- **Over-dispersion (NB).** Keep `λ_t` (equivalently `log μ_t`)
  **latent**, supplied by the existing HSGP / AR(1) / SIR process;
  condition on it (Poisson-Skellam). The latent process *is* the
  over-dispersion — no closed-form NB-Skellam needed. (Matches how the
  package already Laplace-marginalises the latent epidemic.)
- **Right-censored / missing `as_of`** is handled exactly as today (a
  delay known only to be `≤ j` contributes the cdf term; here it
  constrains the *addition* stream).

**Skellam log-pmf, two AD-safe implementations** (both validated to
machine precision, §7):

1.  **`besselI` (closed form).** RTMB exports a differentiable
    `besselI`:

        lS(j;α,β) = −(α+β) + (j/2) log(α/β) + log besselI(2√(αβ), |j|, expon.scaled=TRUE) + 2√(αβ)

2.  **Bessel-free truncated log-sum-exp** (no special function,
    fixed-length loop ⇒ trivially AD-safe; preferable for robustness):

        lS(j;α,β) = logsumexp_{w=max(0,−j)}^{K} [ dpois(j+w; α, log) + dpois(w; β, log) ],
        K ≈ 2(α+β)+40

**Reduced “cumulative-only” likelihood (Regime A, fallback).** If only
the latest cumulative is modelled, use Prop. 3: replace today’s expected
observed count `λ_t G_D(d*)` by `μ_t[G_D(d*) − (1−p)G_W(d*)]` and keep
the existing Poisson/NB count likelihood on the non-negative cumulative.
~2-line change to the mean; **but** it cannot identify `p`/`G_C` from
non-negative data alone — you need the signed increments (Regime B) for
that.

------------------------------------------------------------------------

## 5. Identifiability

- With **only the final cumulative**, `p` and `G_C` enter solely through
  the standing over-count `η_t[G_D−G_W]`; they are weakly identified.
- The **signed increment history** (FluSight `as_of` ×
  `target_end_date`) identifies them: negative increments pin down the
  *rate* (`η_t`, hence `p`) and *timing* (`G_C`, via `G_W`) of
  retractions. This is the empirical reason to use Regime B.
- Start with `p` **constant** (one logit parameter, shared across
  `t,s`); optionally let it vary by stratum or be a small
  logit-linear/seasonal process later.

------------------------------------------------------------------------

## 6. Implementation plan (mapping to the package — for a later coding PR)

> Conservative, additive, mirrors the existing component pattern. No
> existing path changes when `p=1`.

1.  **New component object — a retraction process.** Reuse the
    delay-family machinery for `G_C` (lognormal / gamma / geometric),
    plus a scalar `p` (logit). Constructor e.g.
    `confirmation_process(retract_delay = ..., p_prior = ...)`. Default
    `p=1` ⇒ off ⇒ exact current model.
2.  **`prepare_data`.** From the `as_of × target_end_date` cumulative
    build the signed increment array `m[t, d, s] ∈ ℤ` (difference over
    consecutive `as_of`); record `d*` and any censoring. Keep the
    existing aggregation for the genuine/addition part.
3.  **Delay algebra (inside the objective, AD-safe).** Compute `g_D`,
    `g_C`, then `g_W = conv(g_D, g_C)` and `G_W = cumsum(g_W)` with a
    **fixed-length** discrete convolution (RTMB-traceable).
4.  **New likelihood branch.** `α = μ_t g_D(d)`, `β = η_t g_W(d)` with
    `μ_t = λ_t/p`, `η_t = λ_t(1−p)/p`; accumulate `Σ_d lS(m_t^d; α, β)`
    using the truncated-logsumexp Skellam (or `besselI`). Future bins
    omitted; censored bins use the cdf term.
5.  **Epidemic process unchanged** — it keeps modelling `λ_t` (final
    genuine). Only the observation layer changes.
6.  **Reduction test (must pass).** `p=1` ⇒ identical fit to the current
    Poisson/NB model on a no-retraction dataset (regression test).
7.  **Validation on `flusight`.** Recover sane `p`, retraction delay,
    and a final-count nowcast whose intervals widen appropriately where
    down-revisions are active.
8.  **Downstream.** `reconstruct` / predict report `λ_t` (final) and, if
    wanted, the standing cumulative `μ_t Q_1(d*)`; diagnostics can show
    the addition vs. removal split.

------------------------------------------------------------------------

## 7. Numerical validation (already run; `set.seed(1)`)

Marked-process simulation (`μ=200`, `p=0.85`, NB-ish `g_D`, `g_C` on
`1..12`, `d*=8`):

| Claim | Theory | Simulation |
|----|----|----|
| Net cumulative `C_t(d*)` mean = var (Poisson) | `μ Q_1 = 169.7` | mean `169.7`, var `169.5` |
| `Q_1 = G_D − (1−p)G_W` | `0.848` | (matches) |
| Increment at `d=4` mean `α−β`, var `α+β` | `20.70`, `29.77` | `20.72`, `29.61` |
| Cross-delay increment dependence | `0` (idealised) | `max|corr| = 0.058` |
| Reduction `p=1`: `Q_1 = G_D(d*)` | `0.977` | `0.977` |
| Skellam lpmf: truncated-logsumexp vs `besselI` | equal | `diff ≤ 3e−15` |
| RTMB provides AD `besselI` | — | yes |

------------------------------------------------------------------------

## 8. Open questions for Rodrigo

1.  **Target.** Confirm the nowcast target is the *final genuine count*
    `λ_t` (the value the cumulative converges to). The epidemic process
    keeps modelling `λ_t`.
2.  **Data granularity.** Confirm FluSight gives only the **net**
    cumulative per `as_of` (so additions/removals are not separated ⇒
    Skellam). If gross add/remove were ever separate, the model becomes
    two plain Poisson regressions (no Skellam needed).
3.  **Regime.** Model the **signed increment history** (Regime B,
    recommended — uses the up/down info, identifies `p` and `G_C`)
    vs. only the latest cumulative with a corrected mean (Regime A,
    simpler, weak identifiability)? The “sum over all integers” you
    described ⇒ Regime B.
4.  **`p` structure.** Constant to start, or stratum/seasonally varying?
5.  **Retraction delay `G_C`.** Its own family (recommended: geometric
    or lognormal) vs. tie to the report delay `G_D`? Same *appearance*
    delay `G_D` for genuine and erroneous reports (my assumption) — OK?
6.  **Over-dispersion.** Conditional Poisson-Skellam with the latent
    epidemic process supplying over-dispersion (recommended), or a
    closed-form NB-Skellam?
