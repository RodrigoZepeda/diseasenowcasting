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

## 7. Two-stage cascade

We fit the model in two steps due to the non-identifiability between
delay and epidemic processess. The two-stage cascade addresses this via
**multiple imputation**:

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
