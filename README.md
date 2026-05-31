# dcast3

A **cmdstanr-free** reimplementation of the
[diseasenowcast2](../diseasenowcast2) epidemic-nowcasting model using
[RTMB](https://github.com/kaskr/RTMB) (CppAD autodiff + a built-in Laplace
approximation for the latent epidemic process). No Stan, no CSV round-trip,
no compile step per fit.

The reporting delay is modelled directly as a stochastic process, jointly with
the epidemic dynamics, via the Günther et al. (2021) censored likelihood — the
same mathematics as the Stan reference, transcribed into AD-friendly R.

## Same model menu, RTMB engine

The user-facing API is identical to diseasenowcast2:

```r
library(dcast3)

mdl  <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
dat  <- prepare_data(mdl, m, X = X, d_star = d_star)        # mirrors data_to_stan()
pr   <- default_priors(mdl, dat, phi = lognormal_prior(log(20), 0.5))
fit_ <- fit(mdl, dat, priors = pr)                          # RTMB Laplace
nc   <- nowcast(fit_, n_draws = 2000)                       # posterior-predictive nowcast

# Two-stage multiple-imputation nowcast (the COVID-winning cascade):
nc2  <- nowcast_twostage(mdl, m, X = X, K = 25)
```

`m` is the observation matrix `[event_time, count, delay, strata...]` (delays
1-indexed), exactly as in diseasenowcast2.

## Status — full model menu implemented

**Delay families** (`lognormal_delay`, `gamma_delay`, `generalized_gamma_delay`,
`dirichlet_delay`) × **epidemic processes** (`hsgp_epidemic`, `ar1_epidemic`,
`sir_epidemic`) × **likelihoods** (`poisson_likelihood`, `nb_likelihood`), with
the joint Laplace fit, posterior nowcast draws, and the two-stage
multiple-imputation cascade.

| Component | Validation |
|---|---|
| Delay-only **LogNormal** (per-time censoring) | Matches fixed Stan MAP to ~1e-5 on COVID windows |
| Delay-only **Gamma** | Matches Stan MAP to ~1e-4 |
| Delay-only **GenGamma** | Recovers synthetic delays; numerically fragile on heavy COVID tails (mirrors the Stan reference's GG fragility) |
| **Dirichlet** (default) delay, HSGP joint | Fits + nowcast end-to-end; simplex via softmax-with-reference + Dirichlet prior |
| **AR(1)** / **HSGP** / **SIR** joint fits | Recover synthetic epidemics; joint-mode Laplace (default) or marginal `random=` (opt-in) |
| **HSGP** joint nowcast | Matches Stan GQ to ~2% across all quantiles on COVID where both converge; more robust than Stan |
| Nowcast draws (joint-precision Laplace) + two-stage multisample cascade | End-to-end on synthetic + COVID |

Engine notes:

- **Two Laplace regimes.** By default `fit()` uses `use_random = FALSE`:
  the latent epidemic coefficients (`ar_innov` / `basis_coefs`) stay fixed
  effects and the whole parameter vector is optimised jointly, with the
  posterior covariance taken from the joint Hessian at the mode — i.e. exactly
  what cmdstanr `$laplace()` does, and 20–400× faster than the alternative.
  Set `options(dcast3.use_random = TRUE)` (or call the builder with
  `use_random = TRUE`) to instead declare those coefficients `random` so RTMB
  does the marginal (nested) Laplace and integrates them out — statistically
  the more accurate regime, but far slower on long series (it adds an inner
  Newton solve at every outer step). See `devel/RTMB_PORT_PLAN.md`.
- The discretised delay log-PMF is evaluated via the survival tail for large
  delays so heavy-tailed reporting delays do not saturate to `-Inf`.
- `fit()` runs a small init ladder (epidemic-level offsets) for robustness on
  long series; it converges on COVID dates where the Stan reference returns
  `NULL` or a degenerate GQ.

Delay families: Gamma uses RTMBdist `pgamma2` (mean/SD). Generalized Gamma is a
heavy-tail-robust port of `generalized_gamma.stan` — exact gamma body via the
AD-stable basic `pgamma` plus a Wilson–Hilferty normal survival tail, with the
shape `Q` bounded to `(0.05, 3)` to tame the `1/Q²` gradient near zero. This
replaces RTMBdist `pgengamma`, whose gradient NaNs near `Q→0`, large `|Q|`, or
small `sigma`. GG now converges on the full 721-day COVID series (delays to 331)
in ~2 s. The discretised PMF is survival-tail-robust for all families.

## Validation scripts

```sh
# from ~/Documents/dcast3, with diseasenowcast2 + tbl.now installed
N_DATES=5 Rscript devel/validate_phase1.R   # delay-only vs Stan MAP (gate <= 2%)
N_DATES=3 Rscript devel/validate_joint.R    # joint HSGP nowcast vs Stan GQ
```

## Dependencies

RTMB, RTMBdist, S7, Matrix, cli (Imports); tbl.now (Suggests, for the COVID
validation scripts).

## Benchmark vs NobBS + Epinowcast

```sh
# 1. (Re)install so parallel workers can library(dcast3) — re-run after code changes
R CMD INSTALL ~/Documents/dcast3

# 2. Run the parallel two-stage multisample benchmark (detached, ~1.5-3 h full)
bash devel/run_own_models_dcast3.sh
#    scaled smoke: N_DATES=5 RUN_COVID=FALSE RUN_MPOX=FALSE TUNE_WORKERS=4 bash devel/run_own_models_dcast3.sh

# 3. Score WIS + 50/90% coverage at d*=0 vs NobBS + Epinowcast (common date set)
Rscript devel/score_dcast3.R           # add LAGS="0 1 2 3" for a by-lag table
```

The scorer reuses diseasenowcast2/devel/results/*_comparison_steps.rds (NobBS +
Epinowcast, already computed on the same evaluation dates / seeds) and the same
scoringutils methodology as plots_wis.R::score_nowcast().
