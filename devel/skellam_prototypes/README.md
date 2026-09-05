# Count-cumulative RTMB prototypes

Everything in this directory is deliberately outside `R/`; none of it is part
of the package API.

The current prototype implements the two proposals in
`main_identifiability_update.tex`:

1. **Cumulative composite likelihood**

   ```text
   C_t(d) ~ Poisson(mu_t q_C(d))
   ```

   An NB marginal with the same mean is also available. Products over `d` are
   explicitly treated as composite likelihoods because cumulative levels for a
   cohort are dependent.

2. **Signed hurdle--ZTNB updates**

   ```text
   Delta_t(d) = C_t(d) - C_t(d-1)
   alpha_t(d) = mu_t g_D(d)
   omega_t(d) = mu_t sum_{r<d} g_D(r) h_R(d-r)
   ```

   The probability of a non-null update is constrained to be no greater than
   `min(1, alpha + omega)`. Conditional on movement, direction is
   `alpha/(alpha+omega)` and magnitude is zero-truncated NB with its **own**
   mean `(alpha+omega)/Pr(movement)`. The parent NB mean is obtained by
   inverting `Psi` on the RTMB tape. Consequently,

   ```text
   E[Delta_t(d)] = alpha_t(d) - omega_t(d)
   ```

   exactly. This is the manuscript hurdle model, not a conventional ZINB with
   a second source of zeroes.

Both likelihoods use the package's AR, HSGP, and SIR epidemic mechanisms and
the package's lognormal, gamma, and generalized-gamma delay functions.

For cumulative Poisson/NB fits, the latent epidemic coefficients are integrated
as RTMB random effects using Laplace. This avoids placing the full AR/HSGP path
in the outer optimizer. The hurdle fit retains the faster MAP path because its
all-location gradients were already stable. `OPTIMIZATION_FINDINGS.md` records
the before/after 1,060-fit optimization gate.

## Collapsed retraction description

The likelihood receives only the primitive finite-age kernel `h_R` and
survival `S_R`. For a parsimonious prototype using the package's existing delay
families, it is constructed as

```text
h_R(l) = retract_mass * g_R(l), l = 1,...,H
S_R(a) = 1 - sum_{l<=a} h_R(l).
```

This factorization is a finite-horizon tail restriction. Results report
`h_R`, `S_R`, `retract_mass`, and `q_C`; they should not be interpreted as
nonparametric identification of a separate biological `p` and validation-delay
law. The terminal target is `C_t(H)`, where `H` is configurable (52 model weeks
by default, or 26 for a six-month sensitivity analysis).

## As-of construction and no leakage

`flusight_asof_data.R` builds two `tbl.now` count-cumulative arms and calls
`tbl.now::complete_zeroes()`:

- `calendar`: retain every real calendar week, including surveillance gaps;
- `compressed`: number observed publication weeks consecutively, so long gaps
  do not create enormous delays. Event weeks not on that publication clock are
  outside this arm.

The complete clock is constructed first. At an origin such as `2025-01-02`,
`asof_panel()` then keeps only rows satisfying both
`event_date_actual <= now` and `report_date_actual <= now`. For that example,
the latest usable Texas release is `2024-12-28`.

The empirical baseline is also origin-safe. At target age `a`, it estimates
settled/current multipliers only from older cohorts for which both age `a` and
age `H` were observable by that origin. It never reads the retrospective truth
table during calibration.

## Reconstruction

The operational nowcast is anchored at the value actually observed at the
origin:

```text
C_t(d*) + sum_{d=d*+1}^H Delta_t(d).
```

The hurdle model simulates future hurdle--ZTNB updates. The Poisson cumulative
model can also use the underlying Skellam marginals for this approximation. For
comparison, the cumulative model exposes a `direct` draw from its terminal
marginal, but this is unconditioned on the current cumulative value and is not
the preferred operational reconstruction. Negative reconstructed totals are
clamped at zero and should be audited in a fuller calibration study.

Final available FluSight values for event years 2024 and 2025 are treated as
settled retrospective truth, as requested. They are used only for scoring.

## Run

From the package root:

```sh
OMP_NUM_THREADS=1 Rscript devel/skellam_prototypes/smoke_identifiability.R

OMP_NUM_THREADS=1 NOWS=2025-01-02 SETTLEMENT_WEEKS=52 \
  Rscript devel/skellam_prototypes/run_asof_backtest.R

OMP_NUM_THREADS=1 STATE=California EPIDEMIC=hsgp \
  SETTLEMENT_WEEKS=26 N_ORIGINS=4 \
  Rscript devel/skellam_prototypes/run_asof_backtest.R

OMP_NUM_THREADS=1 SETTLEMENT_WEEKS=26 \
  Rscript devel/skellam_prototypes/run_all_locations_stability.R
```

Options are `STATE`, `EPIDEMIC=ar|hsgp|sir`,
`DELAY=lognormal|gamma|generalized_gamma`, `SETTLEMENT_WEEKS`,
`TARGET_MAX_AGE`, `N_ORIGINS`, `N_DRAWS`, `NOWS`, `CLOCKS`, and `SEED`.

The older `rtmb_count_cumulative.R`, `run_flusight.R`, and
`run_settled_flusight.R` files are retained as legacy experiments. They use a
fixed empirical `p`, a separate retraction law, and a conventional ZINB, so
they should not be used as evidence for the revised model.

This remains a prototype: predictive draws condition on fitted parameters and
the naive composite-likelihood Hessian is not used for uncertainty. A cluster
bootstrap or sandwich/Godambe correction is needed before package integration.
The completed 53-location stability results are summarized in
`ALL_LOCATIONS_FINDINGS.md`; the cumulative optimization work is summarized in
`OPTIMIZATION_FINDINGS.md`.
