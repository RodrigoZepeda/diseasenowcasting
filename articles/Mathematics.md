# Mathematical Foundations of diseasenowcasting

## Overview

This vignette derives the statistical model underlying
`diseasenowcasting` from first principles. It is intended for
mathematically oriented readers who want to understand the likelihood
construction, the epidemic process specifications, and the inference
algorithm.

## 1. The two-process framework

Let \\N_t\\\_{t \geq 0} denote the **latent epidemic process**: the
(unobserved) total number of cases occurring at event time t. At each
time t there is a **reporting-delay process** \\D_t\\\_{t \geq 0} that
governs when those cases will eventually appear in surveillance data.

Formally, if N_t = n cases occur at time t, then each case i is
associated to a delay d_i \sim G_D(\cdot \mid \theta_t), where G_D is
the delay’s cummulative distribution function (CDF) with parameter
vector \theta_t. At calendar time \tau \geq t, we observe only those
cases whose delay satisfies d_i \leq \tau - t. This creates
right-censoring: for recent event times, cases have yet to be reported.

**The nowcasting problem** is to estimate the posterior distribution of
N_t for the most recent event times, given all reported delays up to the
current calendar time.

## 2. The censored likelihood

### 2.1 Setting

Consider a single event time t. At calendar time \tau \geq t, the
maximum observable delay is d^\*\_t = \tau - t. Suppose we have observed
k cases with delays d_1, \ldots, d_k \leq d^\*\_t, and we know there are
n - k cases that will be reported in the future (with delays \>
d^\*\_t). Since n is unobserved, we marginalise over all n \geq k.

### 2.2 Derivation

The joint probability of the observed delays, conditional on N_t = n,
is:

P(d_1, \ldots, d_k \mid \theta_t,\\ N_t = n) = \binom{n}{k}
\prod\_{i=1}^k \Delta G_D(d_i \mid \theta_t) \cdot \bigl\[1 -
G_D(d^\*\_t \mid \theta_t)\bigr\]^{n-k},

where

\Delta G_D(d \mid \theta_t) = \begin{cases} G_D(d \mid \theta_t) -
G_D(d-1 \mid \theta_t) & \text{(continuous } G_D\text{)}, \\ g_D(d \mid
\theta_t) & \text{(discrete } G_D \text{ with pmf } g_D\text{)}.
\end{cases}

Summing over all n \geq k and incorporating a prior \pi(\theta_t), the
likelihood at time t is:

\text{Likelihood}\_t(\theta_t \mid d_1, \ldots, d_k) \\\propto\\
\pi(\theta_t) \cdot \prod\_{l=0}^{L} \bigl\[\Delta G_D(l \mid
\theta_t)\bigr\]^{m_l} \cdot S_k(\theta_t),

where m_l = \\\\i : d_i = l\\ is the count of observed delays equal to
l, and the **latent process term** is

S_k(\theta_t) = \sum\_{n \geq k} \binom{n}{k} \bigl\[1 - G_D(d^\*\_t
\mid \theta_t)\bigr\]^{n-k} P(N_t = n \mid \theta_t).

### 2.3 Log-likelihood

Taking logarithms, the per-time contribution is

\ell_t(\theta_t \mid m_0, m_1, \ldots, m_L) = \ln \pi(\theta_t) +
\sum\_{l=0}^{L} m_l \ln \Delta G_D(l \mid \theta_t) + \ln S_k(\theta_t).

The **full log-likelihood** sums over all event times: \ell(\theta) =
\sum\_{t} \ell_t(\theta_t \mid m_0^{(t)}, \ldots, m_L^{(t)}).

### 2.4 Closed-form S_k for standard epidemic models

**Poisson** (N_t \sim \mathrm{Poisson}(\lambda_t)):

\ln S_k(\theta_t) = k \ln \lambda_t - \ln k! - G_D(d^\*\_t \mid
\theta_t)\\ \lambda_t.

**Negative Binomial** (N_t \sim \mathrm{NB}(r_t, p_t), mean
r_t(1-p_t)/p_t):

\ln S_k(\theta_t) = k\ln(1-p_t) + r_t \ln p_t + \ln\binom{k + r_t -
1}{k} - (k + r_t)\ln\bigl\[p_t + G_D(d^\*\_t \mid
\theta_t)(1-p_t)\bigr\].

Both expressions arise from recognising that S_k is the
probability-generating function of N_t evaluated at \[1 - G_D(d^\*\_t)\]

------------------------------------------------------------------------

## 3. The epidemic process

The latent mean incidence at time t (across strata s \in \\1,\ldots,S\\)
is

\mu_t^{(s)} = \exp\\\bigl(\gamma_0^{(s)} + f^{(s)}(t) +
\mathbf{X}\_t^{(s)\top} \boldsymbol{\gamma}^{(s)}\bigr),

where \exp(\cdot) ensures positivity, \gamma_0^{(s)} is a
stratum-specific intercept, \mathbf{X}\_t^{(s)} is a covariate vector
(e.g. day-of-week dummies), and f^{(s)}(t) is a stratum-specific
temporal trend. Three specifications of f are available.

### 3.1 Hilbert-Space Gaussian Process (HSGP)

A Gaussian process f \sim GP(0, k(t,t')) is approximated via M
eigenfunctions \\\phi_j\\ of the Laplacian on the domain \[-L, L\]:

f(t) \approx \sum\_{j=1}^{M} \beta_j\\ \phi_j(t)\\ \sqrt{S(\lambda_j)},

where S(\lambda) is the spectral density of the kernel evaluated at
frequency \lambda_j = j\pi/(2L), and \beta_j \overset{\text{iid}}{\sim}
N(0,1).

The `diseasenowcasting` package uses M chosen automatically as \lceil
1.5\sqrt{T}\rceil, capped at 20 for daily series longer than 400 days to
avoid ill-conditioned Hessians.

### 3.2 Autoregressive trend AR(1)

f(t) = m_t, \quad m_t = \phi\\ m\_{t-1} + \epsilon_t, \quad \epsilon_t
\sim N(0, \sigma^2),

with stationary initialisation m_1 \sim N(0, \sigma^2 / (1 - \phi^2)).

### 3.3 Discrete-time SIR

The mean incidence can instead be derived from a compartmental model.
The familiar continuous-time **SIR** model splits a population of size N
into susceptible S, infectious I, and recovered R compartments,

\frac{dS}{dt} = -\beta\\\frac{S\\I}{N}, \qquad \frac{dI}{dt} =
\beta\\\frac{S\\I}{N} - \gamma\\ I, \qquad \frac{dR}{dt} = \gamma\\ I,

with transmission rate \beta and recovery rate \gamma. We use a
**discrete-time** version of this model, advancing one observation step
at a time. Two standard adjustments make it well behaved for inference:

1.  We track the compartments as *fractions* of an **effective
    population** N\_{\text{eff}} = (S_0/N)\\N, writing s_t =
    S_t/N\_{\text{eff}} and i_t = I_t/N\_{\text{eff}}. The estimated
    susceptible fraction S_0/N lets the model use an effective
    population smaller than the census N.
2.  The mass-action infection term \beta\\S\\I/N is replaced by its
    **chain-binomial** (Reed–Frost) survival form, so that the number of
    new infections can never exceed the susceptible pool:

\underbrace{\Delta_t}\_{\text{new infections (fraction)}} = s_t\left(1 -
e^{-\beta_t\\ i_t}\right).

The compartments then update exactly like the SIR equations above —
susceptibles lose \Delta_t, infectious gain \Delta_t and lose a fraction
\gamma to recovery:

s\_{t+1} = s_t - \Delta_t, \qquad i\_{t+1} = \Delta_t + (1-\gamma)\\
i_t,

and the **mean reported incidence** is \mu_t =
N\_{\text{eff}}\\\Delta_t. The transmission rate \beta_t =
R_0\\\gamma\\e^{u_t} is written in terms of the basic reproduction
number R_0 (since R_0 = \beta/\gamma), and u_t follows an AR(1) trend so
that the effective reproduction number R_t = R_0\\e^{u_t} drifts over
time. The recovery rate \gamma\in(0,1), the basic reproduction number
R_0, and the susceptible fraction S_0/N are estimated parameters.

**Stratified (coupled) SIR:** For several strata, the force of infection
in stratum s depends on the *total* infectious pool \sum\_{s'}
i_t^{(s')}, so \Delta_t^{(s)} = s_t^{(s)}\bigl(1 - e^{-\beta_t^{(s)}
\sum\_{s'} i_t^{(s')}}\bigr). This captures cross-group transmission
while allowing stratum-specific transmission rates.

## 4. The delay distribution

### 4.1 Parametric families

The delay CDF G_D(\cdot \mid \theta) can be any of:

| Family | Parameters | Notes |
|----|----|----|
| **LogNormal** | (\log\mu,\\ \sigma) | \mu is the mean; \sigma is natural-scale SD |
| **Gamma** | (\mu, \sigma) | Parameterised by mean and SD |
| **Generalised Gamma** | (\log\mu, \sigma, Q) | Parametrization from \[@prentice1974log\] |

The delay distribution is **constant over time**: a single set of
parameters \theta governs G_D(\cdot \mid \theta) for every event-time t.

### 4.2 Non-parametric (Dirichlet) delay

Let L be the maximum explicitly modelled delay. A probability simplex
over \\0, 1, \ldots, L\\ receives a Dirichlet prior: \bigl(g_D(0),
\ldots, g_D(L)\bigr) \sim \mathrm{Dir}(\alpha_0, \ldots, \alpha_L).

To handle right-censoring (delays \> L), an (L+1)-th category captures
the tail probability. Conditional on falling in the tail, delays follow
an \mathrm{Exponential}(1) distribution, yielding: \tilde{G}\_D(x) =
\begin{cases} \sum\_{k=0}^{\lfloor x\rfloor} \tilde{g}\_D(k), & x \leq
L, \\ \sum\_{k=0}^{L} \tilde{g}\_D(k) + \tilde{g}\_D(L+1)\bigl(1 -
e^{-(x-(L+1))}\bigr), & x \> L. \end{cases}

### 4.3 Missing report dates

When a report date is unavailable, the corresponding delay is
right-censored at the analysis date \tau. The contribution to the
log-likelihood becomes \ln G_D(d^\*\_j \mid \theta_t) instead of \ln
\Delta G_D(d_j \mid \theta_t), yielding:

\ell_t(\theta_t) = \ln\pi(\theta_t) + \sum\_{\text{observed}} m_l \ln
\Delta G_D(l \mid \theta_t) + \sum\_{\text{censored}} m_j^\* \ln G_D(j
\mid \theta_t) + \ln S_k(\theta_t).

## 5. Stratification

With S strata, the log-likelihood decomposes additively: \ell(\theta) =
\sum\_{t} \sum\_{s=1}^{S} \ell\_{t,s}(\theta), where each stratum
contributes its own m_l^{(s)}, k^{(s)}, and S\_{k^{(s)}}(\theta) term.
The delay distribution G_D and the NB overdispersion \phi are **shared**
across strata; the epidemic mean \mu_t^{(s)} and the GP/AR1/SIR
trajectory parameters are **per-stratum**.

------------------------------------------------------------------------

## 6. Bayesian inference via Laplace approximation

`diseasenowcasting` performs inference via the **joint-mode Laplace
approximation**:

**Step 1 – MAP estimation.** Minimise the negative log-posterior
-\ell(\theta) \text{ w.r.t. } \theta using `nlminb` (L-BFGS-B with
analytical gradients via RTMB/CppAD).

**Step 2 – Hessian.** Evaluate H = -\nabla^2 \ell(\hat\theta) (the
precision matrix of the Laplace approximation) using RTMB’s automatic
differentiation.

**Step 3 – Posterior draws.** Sample \theta^{(i)} \sim N(\hat\theta,\\
H^{-1}), \quad i = 1, \ldots, B, via a sparse Cholesky factorisation of
H.

**Step 4 – Predictive draws.** For each \theta^{(i)}: \lambda_t^{(i)} =
\exp\\\bigl(\text{cap}\bigl(\mu_t^{(i)}\bigr)\bigr), \quad
G^\*{}^{(i)}\_t = G_D\\\bigl(d^\*\_t + 1 \mid \theta_t^{(i)}\bigr),
\text{Nowcast}\_t^{(i)} = k_t + \mathrm{NB}\\\bigl(\lambda_t^{(i)}(1 -
G^\*{}^{(i)}\_t),\\ \phi^{(i)}\bigr), where \mathrm{cap}(\cdot) is a
smooth upper-bound function preventing overflow: \text{cap}(x) = U -
\log\\\bigl(1 + e^{U - x}\bigr), \quad U = \min(\max(6,\\ \log(1 +
k\_{\max})),\\ 16).

The posterior-predictive distribution of the nowcast at event time t is
then summarised from the draws.

## 7. One-stage and two-stage inference

With `type = "one_stage"`, the reporting-delay, epidemic, likelihood,
and any revision parameters are optimized in one joint objective. This
retains all posterior dependence, but it can be more sensitive to the
joint geometry between recent incidence and incomplete reporting.

With `type = "two_stage"`, the reporting delay and epidemic process are
separated by **multiple imputation**:

1.  **Stage 1.** Fit a delay-only model to a recent window of the series
    (default: 120 events), obtaining a posterior (\hat\theta_D,
    \hat\Sigma_D) over delay parameters.

2.  **Stage 2.** Draw K delay parameter vectors \theta_D^{(1)}, \ldots,
    \theta_D^{(K)} from a spread around the Stage-1 estimate, fix each
    as a known constant, and fit a joint epidemic model for each.

3.  **Pooling.** Posterior-predictive draws are pooled across
    imputations: \text{Pooled}\_{t}^{(i)} =
    \text{Nowcast}\_{t,\\k(i)}\\\left(\theta_D^{(k(i))}\right), where
    k(i) cycles over imputations.

------------------------------------------------------------------------

## 8. Resolution processes: confirmation and retraction

Sections 2–7 assume a report is a case, full stop. Registers rarely work
that way: a report is provisional, and is later **resolved** one way or
the other. Two conventions dominate, and they are mirror images of one
another.

A report is **resolved exactly once**: a test comes back, and it is
either positive (the case is *confirmed*) or negative (the case is
*retracted*). There is no chain — nothing is confirmed and then later
retracted. What differs between surveillance systems is only **which
resolutions get a date column**:

|  | **retraction only** | **confirmation only** | **both** |
|----|----|----|----|
| dates recorded | the **negatives** | the **positives** | both signs |
| a missing date means | not retracted **yet** | not confirmed **yet** | not resolved **yet** |
| nowcast target | reports never retracted | reports eventually confirmed | reports that resolve positive |
| lag support | \\1, 2, \ldots\\ | \\0, 1, \ldots\\ | \\0, 1, \ldots\\ |
| `revision_type` records | only `"retracted"` | only `"confirmed"` | both |

The lag support differs only because a *retraction* recorded in the same
period as its report describes a case that was never visible in any data
vintage, whereas a test coming back the day it was ordered is ordinary.
All three are the same generative object seen through different windows,
so they share one likelihood; §8.2 gives the single parameter that
switches between them.

The software interface mirrors the three-axis data object. `tbl.now`
supplies the event, report and revision date columns, the
`revision_type` column, and the optional `is_censored_revision` column
through attributes on the `tbl_now`.
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
reads those attributes directly; it has no parallel column-name
arguments. In
`tbl_now(..., is_censored_revision = result_is_upper_bound)`, the named
logical column marks revision dates that are upper bounds rather than
exact dates. The model is configured as

``` r

model(
  nb_likelihood(),
  ar1_epidemic(),
  lognormal_delay(),                 # event to report
  revision = revision_process(
    dirichlet_revision(),          # report to revision
    mode = "auto")
)
```

There is one revision-delay law in this first prototype. In either
single-sign mode it is the lag for the sign that is recorded. In
`mode = "both"` the same law is used for confirmations and retractions.
Consequently, with both signs observed, the age of a pending report
informs when it will resolve but not which sign it will have. Separate
competing-risk lag laws are not fitted by this prototype.

### 8.1 What a missing resolution date means

For case i write t_i for the event date, r_i for the report date, y_i
for the resolution date (retraction or confirmation), and

D_i = r_i - t_i \quad (\text{appearance delay}), \qquad R_i = y_i - r_i
\quad (\text{resolution lag}).

A row with y_i missing is **not** a resolved negative. Under retraction
it is a case *not retracted yet*; under confirmation, one *not confirmed
yet*. Either way the resolution lag is right-censored at the age of its
report, \tau - r_i, where \tau is the analysis date. Every result below
follows from taking that censoring seriously; reading “missing” as a
settled answer is what produces the naive, over-confident correction.

The two readings differ in an instructive way. Under retraction,
evidence *accumulates in favour* of a standing report: the longer it
survives unretracted, the more likely it is genuine. Under confirmation
it accumulates *against*: the longer a report sits unconfirmed, the more
likely it never will be. That sign flip is the whole difference on the
prediction side (§8.7).

### 8.2 The generative model

The reports form the same marked Poisson process as §2, with two extra
marks per case: a label L_i \in \\+, -\\ with P(L = +) = p, and a
resolution lag R_i \sim g_R (any delay family of §4 may serve as g_R).
A + report is one that belongs in the target — genuine under retraction,
confirmable under confirmation. Writing \lambda_t for the mean
**settled** count, which is what the epidemic process of §3 models, the
gross report intensity is

\mu_t \\=\\ \frac{\lambda_t}{p} \qquad\text{in both modes.}

The single switch is **which label’s resolution you see**. Let

\pi \\=\\ P(\text{the resolution is observed}) \\=\\ \begin{cases} 1 - p
& \text{retraction only (you see the negatives)},\\ p &
\text{confirmation only (you see the positives)},\\ 1 & \text{both
(every resolution is visible)}.\end{cases}

Everything from here is written in \pi and is mode-free. The resolved
term R\ln\pi of §8.4 is really N\_+\ln p + N\_-\ln(1-p); the two
single-sign modes put the whole count on one side and leave the other
empty, which is why the compact form works for all three.

### 8.3 Observable trajectory types

At horizon d^\*\_t = \tau - t a report is in exactly one of three
states, and Poisson colouring makes their counts **independent
Poisson**:

| Type | Visible as | Count | Mean |
|----|----|----|----|
| not yet reported (D \> d^\*) | row **absent** | n\_\varnothing | \mu_t\\\bar G_D(d^\*) |
| unresolved (D = a \le d^\*, no resolution by \tau) | row, y_i missing | A^\circ\_{t,a} | \mu_t\\g_D(a)\\h(d^\*-a) |
| resolved (D = a, D + R = b \le d^\*) | row, y_i present | B\_{t,a,b} | \mu_t\\\pi\\g_D(a)\\g_R(b-a) |

where

h(j) \\=\\ (1-\pi) + \pi\\\bar G_R(j)

is the probability that a report made j periods ago is still unresolved:
either it belongs to a silent class, or it belongs to an observed class
but its resolution has not landed. h(\infty) = 1 - \pi. Under retraction
h(0) = 1 — a report filed today carries no evidence, since nothing could
have been retracted yet — but under confirmation h(0) = (1-p) + p\\\bar
G_K(0) \< 1: a case *could* have been confirmed the same period and was
not, which is already information.

**When both signs are recorded, \pi = 1 and h(j) = \bar G_R(j), free of
p.** An unresolved row then says only that its test has not come back;
because the lag law is shared between the two signs, its age carries no
information about *which way* it will go. Two consequences, both checked
in the tests: p is a plain binomial on the resolved rows — the MLE is
N\_+/(N\_+ + N\_-), with no censoring correction — and the survival part
is a plain right-censored fit of g_R using every row. The two blocks are
orthogonal.

### 8.4 The likelihood

n\_\varnothing is unobserved, but the counts are *independent*, so
marginalising it contributes nothing. Because \sum_a \kappa_a +
\sum\_{a\<b}\nu\_{ab} = \mu_t G_D(d^\*) — the retraction structure
cancels out of the exposure term — the per-event-time log-likelihood
splits into three blocks:

\ell_t = \underbrace{\ln S\_{k_t}(\mu_t)}\_{\text{(i) count}} +
\underbrace{\sum\_{i:\\t_i = t} \ln g_D(D_i)}\_{\text{(ii) appearance
delay}} + \underbrace{\sum\_{\text{unresolved}} \ln h(\tau - r_i) +
\sum\_{\text{resolved}} \bigl\[\ln \pi + \ln
g_R(R_i)\bigr\]}\_{\text{(iii) resolution}} .

Three things make this cheap to implement:

- **(i) is the S_k of §2.4 verbatim**, with the single substitution
  \lambda_t \mapsto \mu_t = \lambda_t / p, and with k_t counting
  **every** row — standing and already-retracted alike.
- **(ii) is the delay block of §2.3 verbatim**: every reported case
  appeared with delay g_D regardless of its label.
- **(iii) collapses to two one-dimensional tables.** A retracted row’s
  factor separates, and a standing row’s factor h(d^\*\_t - a) depends
  on t and a only through \tau - r_i, the *age of the report*. Pooled
  over event times and strata, \ell^{\mathrm{res}} = R\ln\pi + \sum\_{c}
  r_c \ln g_R(c) + \sum\_{j} u_j \ln h(j), with R the number of resolved
  rows, r_c those with lag c, and u_j the unresolved rows whose report
  is j periods old.

At p = 1 block (iii) vanishes, h \equiv 1 and \mu_t = \lambda_t: the
model is the one of §2 exactly, not approximately.

### 8.5 Over-dispersion

Give each event time a gamma frailty \Lambda_t \sim \mathrm{Gamma}(r,r),
as in §2.4. It multiplies **every** trajectory-type mean by the same
factor, so it cancels from the multinomial split of the rows: blocks
(ii) and (iii) are unchanged, and block (i) is the ordinary
negative-binomial S_k of §2.4 at mean \mu_t. **No quadrature is
needed.**

### 8.6 Identifiability: a mixture-cure model

Block (iii) is exactly the **Berkson–Gage mixture-cure** likelihood,

\prod\_{\text{retracted}} (1-p)\\g_C(C_i) \\\times\\
\prod\_{\text{standing}} \bigl\[p + (1-p)\bar G_C(\tau - r_i)\bigr\],

for right-censored lags with cure fraction p. Two consequences:

1.  p is identified whenever there is **sufficient follow-up** — reports
    whose age \tau - r_i runs well past the bulk of g_R. Those pin
    h(\infty) = 1-\pi directly.
2.  In block (i), \ln\mu_t = \gamma_0 - \ln p + f(t) +
    \mathbf{X}\_t^\top\boldsymbol{\gamma}, so p is **exactly aliased
    with the epidemic intercept**. The count block therefore contributes
    no information about p, and the over-dispersion-knob pathology that
    forces a strong prior in §9.4 cannot occur. `diseasenowcasting` uses
    a *weak* data-informed Beta on p here, centred on the retraction
    rate among reports with enough follow-up (the raw rate R/k
    under-estimates 1 - p, because recent reports have not had time to
    be retracted).

Blocks (ii) and (iii) also involve neither \lambda_t nor the epidemic
process, so the stepwise boundary of §7 carries over without introducing
a second revision-imputation stage. In `type = "two_stage"`, Stage 1
estimates only the event-to-report law g_D. Each imputed \theta_D^{(k)}
is fixed in Stage 2, while the revision-delay parameters and p remain
free and are estimated jointly with the epidemic process. Posterior
sampling within each Stage-2 fit therefore propagates revision
uncertainty; pooling across k additionally propagates reporting-delay
uncertainty. In `type = "one_stage"`, g_D, g_R, p, and the epidemic
process are all estimated together.

### 8.7 Reconstruction

The settled count splits into the standing rows that turn out to be
genuine and the genuine cases not yet reported:

\widehat N_t = \underbrace{w\\B_t}\_{\text{resolved rows}} \\+\\
\sum\_{a=0}^{d^\*\_t} \mathrm{Binomial}\bigl(A^\circ\_{t,a},\\
\rho(d^\*\_t - a)\bigr) \\+\\
\mathrm{Poisson}\bigl(\Lambda_t\\\lambda_t\\\bar G_D(d^\*\_t)\bigr),

where B_t is the number of resolved rows and, per mode,

\begin{array}{lll} \text{retraction:} & w = 0, & \rho(j) = \dfrac{p}{p +
(1-p)\bar G_C(j)} \\ \\ (\text{increasing in } j),\\\[2ex\]
\text{confirmation:} & w = 1, & \rho(j) = \dfrac{p\\\bar G_K(j)}{(1-p) +
p\\\bar G_K(j)} \\ \\ (\text{decreasing in } j). \end{array}

A retracted row is gone, so it contributes nothing; a confirmed row is
already in the target, so it contributes with certainty. Between them,
**each unresolved row enters the nowcast independently with probability
\rho(\tau - r_i)**, a function of its report age alone. The binomial
split is free of the frailty, so over-dispersion enters only through the
future term. Both means come to \lambda_t — unbiased — and at p = 1 the
retraction form is the Step-4 formula of §6.

### 8.8 Partially observed rows

Censoring **coarsens the observable partition**: instead of knowing
which trajectory type a row is, we know only that it is one of a set.
Poisson colouring still applies — a count over a union of types is
Poisson with the summed intensity — so such a row contributes the log of
a *sum*, and the exposure term is untouched (summed over all types it is
\mu_t G_D(d^\*) however they are grouped).

With the appearance delay known only to lie in \[a\_{\text{lo}},
a\_{\text{hi}}\], and writing b for the **withdrawal delay** measured
from the event (b = q_i - t_i — the natural coordinate when the report
date is itself uncertain):

| Row | Contribution |
|----|----|
| standing | \ln \sum\_{a} g_D(a)\\ h(d^\*\_t - a) |
| retracted **at** b | \ln(1-p) + \ln \sum\_{a \le \min(a\_{\text{hi}},\\ b-1)} g_D(a)\\ g_C(b - a) |
| retracted **by** B | \ln(1-p) + \ln \sum\_{a \le \min(a\_{\text{hi}},\\ B-1)} g_D(a)\\ G_C(B - a) |

The three cases the package supports are instances of these two kernels:

1.  **Censored report, exact retraction.** The retraction *itself*
    bounds the report — a case cannot be withdrawn before it is filed,
    so a \le b - 1, and the effective upper bound is
    \min(a\_{\text{hi}}, b - 1).
2.  **Exact report, censored retraction.** A single term, with the lag
    entering as G_C(B - a) rather than g_C(b-a): we know the retraction
    happened by B, not when.
3.  **Both censored.** The full double sum.

Each collapses to the exact-row term when its interval is a single
point, so the exactly observed rows of §8.4 are the degenerate case, not
a separate branch.

**A discretisation trap.** The two delays do *not* share a convention:
the appearance delay is binned as g_D(a) = F(a+1) - F(a) (delay a falls
in bin a+1) while the retraction lag is binned as g_C(c) = F_C(c) -
F_C(c-1) with F_C(0) := 0. Applying the first to g_C shifts the lag pmf
by one bin, which biases the retraction timing without ever looking
wrong.

### 8.9 Stratification

As in §5, the log-likelihood is a sum over (t,s) cells, and g_D, g_C and
\phi are shared while \lambda_t^{(s)} is per stratum. The confirmation
probability may be either: shared (the default) or estimated per stratum
via `revision_process(stratified_p = TRUE)`. The cure block is already a
sum over strata, so a per-stratum p^{(s)} changes nothing structural —
each stratum contributes its own R^{(s)}, r_c^{(s)}, u_j^{(s)} under the
same prior, and the count block uses \mu_t^{(s)} =
\lambda_t^{(s)}/p^{(s)}. g_C stays shared because the retraction *lag*
is usually a property of the verification workflow, whereas p reflects
how often a given group is misclassified. With sparse strata the shared
p is the safer choice.

### 8.10 Conventions and edge cases

- **As-of masking.** A retraction dated **after** \tau has not happened
  yet: the row is standing, and the date is masked rather than the row
  dropped. Skipping this leaks the future into the fit.
- **Same-period retractions.** g_C lives on \\1, 2, \ldots\\ (matching
  §9), so a case retracted in the same event-unit period as its report
  was never visible in any data vintage. Such rows are dropped from the
  data entirely.
- **No retractions observed.** The cure block would sit on the p = 1
  boundary with only its prior for support; that boundary *is* the
  ordinary count model, so the package fits that instead.
- **Choosing g_C.** \rho(j) is applied to *every* standing case, so at
  high counts a **shape** error in g_C biases the nowcast by more than
  its Monte-Carlo noise. On a COVID series of \approx 8000 cases/day, a
  lognormal g_C fitted to a 1 + \mathrm{Poisson}(2) lag left a 0.9\\
  bias and lost nominal coverage, while a Dirichlet g_C recovered \rho
  to four decimals. Prefer
  [`dirichlet_revision()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_delay.md)
  when counts are large.

### 8.11 The three data types

Everything above is written per **row**, but a row need not be a case.

**Linelist.** One row per case, carrying (t_i, r_i, y_i). Each row has
weight 1.

**Count-incidence.** One row per distinct (t, r, y) combination with a
case count n — the aggregated form of exactly the same information,
since y is part of the key and `NA` marks the unresolved cases. Every
quantity in §8.4 and §8.8 is a *weighted* tally, so aggregating changes
nothing: the sufficient statistics m_a, R, r_c, u_j, the censoring
patterns and the per-cell resolved and unresolved counts are identical,
and the two log-likelihoods agree exactly. The package implements this
by weighting each tally by n instead of by 1.

**Count-cumulative.** A stream of repeatedly published levels is not an
aggregation of the row-level revision likelihood. It does not reveal
which records remain pending, and therefore cannot identify the revision
probability p separately from a conditional revision-delay distribution.
Downward revisions are handled by the dedicated collapsed retraction
kernel of §9, not by
[`revision_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_process.md).

### 8.12 Relationship to the count-cumulative model

The row-level model identifies p because it observes resolved and
standing records. A cumulative database publishes only their net total.
Its identifiable primitive is instead the unconditional finite-age
withdrawal probability h_R(\ell) in §9. Equating terminal database
retention with biological truth is an additional scientific assumption,
not a consequence of the cumulative data.

------------------------------------------------------------------------

## 9. Count-cumulative data: finite-horizon retention

Some surveillance systems publish, for each event-time t, a running
level C_t(d) known at age d. The level may increase as reports arrive
and decrease when records are withdrawn. The estimand is the retained
database count at a configurable finite settlement horizon H, C_t(H).
The default is H=26 model steps. This is biological truth only under the
extra assumption that a record is true if and only if it is never
withdrawn.

### 9.1 The identifiable retraction object

The cumulative stream identifies an unconditional, possibly defective
retraction kernel

h_R(\ell)=\Pr(R=\ell),\quad \ell=1,\ldots,H,
S_R(a)=1-\sum\_{\ell=1}^{a}h_R(\ell),\quad a=0,\ldots,H.

The package uses the parsimonious finite-horizon factorisation

h_R(\ell)=m_R g_R(\ell), \qquad 0\leq m_R\leq1,

where g_R is a lognormal, gamma, or generalized-gamma mass normalised
over 1{:}H. Here m_R is the retraction mass within H and S_R(H) is
terminal retention. This factorisation must not be interpreted as
separate identification of a biological truth probability and a
conditional revision law.

### 9.2 Cumulative-level composite likelihood

Let g_D(r) be the appearance-delay mass. The probability that a record
has appeared by age d and remains in the database is

q_C(d)=\sum\_{r=0}^{d}g_D(r)S_R(d-r),

so

\mathbb E\[C_t(d)\]=\mu_t q_C(d).

With
[`poisson_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
each marginal is Poisson with this mean; with
[`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
it is negative binomial in the package’s mean/size parameterisation.
Levels at different ages for one event-time are dependent. Their product
is therefore a **composite likelihood**, not an exact joint likelihood.

### 9.3 Signed hurdle update composite

Define

\Delta_t(0)=C_t(0),\qquad \Delta_t(d)=C_t(d)-C_t(d-1),
\alpha_t(d)=\mu_tg_D(d),\qquad
\omega_t(d)=\mu_t\sum\_{r=0}^{d-1}g_D(r)h_R(d-r).

The hurdle probability is

\pi_t(d)=\\1-\exp\[-(\alpha_t(d)+\omega_t(d))\]\\
\operatorname{logit}^{-1}(\eta_t(d)),

where \eta_t(d) may contain age and previous-nonzero effects. Thus
0\<\pi_t(d)\leq\min\\1,\alpha_t(d)+\omega_t(d)\\. Given movement, the
sign is positive with probability \alpha/(\alpha+\omega) and the
unsigned magnitude has its own mean (\alpha+\omega)/\pi.

For `observation = "hurdle_ztnb"`, the magnitude follows a
zero-truncated negative binomial. If z is its requested own mean and s
its size, the parent NB mean m is obtained on the
automatic-differentiation tape by solving

z=\Psi_s(m)=\frac{m}{1-\Pr\\\mathrm{NB}(m,s)=0\\}.

Using z directly as the parent NB mean is wrong. For
`observation = "hurdle_ztpoisson"`, the same construction uses a
zero-truncated Poisson indexed by own mean (\alpha+\omega)/\pi and has
no magnitude-dispersion parameter.

Both versions preserve

\mathbb E\[\Delta_t(d)\] =\pi\frac{\alpha-\omega}{\alpha+\omega}
\frac{\alpha+\omega}{\pi} =\alpha_t(d)-\omega_t(d).

#### Skellam check and the role of confirmation

Before adding the hurdle, the Poisson-process construction gives a
useful exact one-age marginal. At a fixed age d, the reports entering
the provisional count and the earlier reports withdrawn at that age are
disjoint marked-Poisson classes:

A_t(d)\sim\operatorname{Poisson}\\\alpha_t(d)\\,\qquad
W_t(d)\sim\operatorname{Poisson}\\\omega_t(d)\\,\qquad A_t(d)\perp
W_t(d).

Therefore

\Delta_t(d)=A_t(d)-W_t(d)
\sim\operatorname{Skellam}\\\alpha_t(d),\omega_t(d)\\,

with mean \alpha_t(d)-\omega_t(d) and variance \alpha_t(d)+\omega_t(d).
This checks the signs and rates used by the hurdle construction. Under
shared gamma frailty the corresponding marginal is the gamma-mixed
Skellam (the package’s SkNB calculation); additions and withdrawals are
conditionally independent given that frailty.

There is no third “confirmation update” in a count-cumulative
provisional register. A report enters through A_t(d) whether it will
later be confirmed or retracted. Confirmation leaves that report in the
level, while a negative revision produces the withdrawal counted by
W_t(d). Adding a separate confirmation intensity would count the same
positive report twice. If a source publishes only confirmed totals, its
confirmation date is the observation/report date and the ordinary
event-to-observation delay model applies. When row-level reports and
their positive/negative revision outcomes are both available, §8’s
marked revision likelihood is used instead of a Skellam likelihood.

The product over ages is again a composite likelihood because updates
from one event-time are dependent.

### 9.4 Operational reconstruction and uncertainty

At an analysis origin, let d^\* be the newest observed age. Prediction
is anchored to the level actually known then:

C_t(H)=C_t(d^\*)+\sum\_{d=d^\*+1}^{H}\Delta_t(d).

The hurdle models simulate those future updates sequentially and carry
the previous-nonzero state. If a simulated signed path ends below zero,
the public count is projected to zero and the number of projections is
exposed in the prediction diagnostics. The cumulative-level model uses
an explicitly labelled anchored independent-update approximation; it is
not an exact conditional law.

Finally, curvature from either composite likelihood is pseudo-posterior
curvature. The current intervals do not include a sandwich/Godambe or
cluster-bootstrap calibration, so nominal coverage is not guaranteed.
