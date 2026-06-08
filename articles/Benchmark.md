# Benchmark (diseasenowcasting vs NobBS and epinowcast)

## NOTE

> The numbers here are from a previous iteration of the package we are
> working on updating.

### TL; DR

We compared different datasets across distinct `diseasenowcasting`
models as well as NobBS and epinowcast. The `HSGP/LogNormal` and
`HSGP/GeneralizedGamma` are the suggested models as they are
consistently the ranking among the top performers.

### Overview

This vignette presents a systematic benchmark comparison of
`diseasenowcasting` against two widely used nowcasting methods:
[**NobBS**](https://cran.r-project.org/web/packages/NobBS/index.html)
and [**Epinowcast**](https://github.com/epinowcast/epinowcast). We
compare across three real-world disease surveillance datasets:

| Dataset | Disease | Temporal unit | Source |
|----|----|----|----|
| `denguedat` (tbl.now) | Dengue fever | Weekly | (McGough et al. 2020) |
| `mpoxdat` (tbl.now) | Mpox | Daily | (Rohrer et al. 2025) |
| `covid_colombia` (diseasenowcasting) | COVID-19 | Daily | (Instituto Nacional de Salud (INS) 2024) |

The comparison uses **50 evaluation dates** per disease, selected at
random. Forecasts are scored for a nowcast at a 0 delay, that is, the
newest event time at each evaluation date.

### Scoring methodology

All methods are scored using the **Weighted Interval Score (WIS)**. WIS
decomposes into:

- **Overprediction** — penalty for predicted intervals that are too high
  relative to the truth.
- **Underprediction** — penalty for predicted intervals that are too
  low.
- **Dispersion** — penalty for uncertainty intervals that are too wide.

Coverage at 50% and 90% credible intervals is also reported. A good
model should have empirical coverage close to the corresponding level.

All comparisons are performed on a **common date set** — dates where
both `diseasenowcasting` and the comparison method have produced a valid
nowcast — ensuring fairness.

### Results

You can access the results from inside the package:

``` r

library(diseasenowcasting)
library(dplyr)

# Pre-computed benchmark results (50 evaluation dates per disease), shipped in
# inst/extdata.  Look in the installed package first and fall back to the source
# tree, so the vignette also knits before the package is installed (e.g. pkgdown
# or R CMD build).
candidates <- c(
  system.file("extdata", "comparison_scores.rds", package = "diseasenowcasting"),
  "../inst/extdata/comparison_scores.rds",
  file.path("inst", "extdata", "comparison_scores.rds")
)
scoring_file <- candidates[nzchar(candidates) & file.exists(candidates)][1]
have_scores  <- !is.na(scoring_file)

if (have_scores) {
  scores_df <- readRDS(scoring_file) |>
    rename(Model = model, WIS = wis, Overprediction = over, Underprediction = under,
           Dispersion = disp, Bias = bias, Cov50 = cov50, Cov90 = cov90, `N dates` = n_dates)
}
```

> *The pre-computed benchmark scores
> (`inst/extdata/comparison_scores.rds`) were not found in this build,
> so the score tables below are omitted.*

#### Dengue fever (weekly, Colombia)

**Key findings (dengue):**

- All `diseasenowcasting` HSGP variants beat NobBS and all Epinowcast
  variants on WIS.
- Best `diseasenowcasting`: **HSGP / GenGamma**, WIS ≈ 7.0, cov90 ≈
  0.94.

#### Mpox (daily, USA)

**Key findings (mpox):**

- `diseasenowcasting` AR1 variants achieve the lowest WIS, beating NobBS
  by a wide margin (~7×).
- Best `diseasenowcasting`: **AR1 / Gamma**,

#### COVID-19 (daily, Colombia)

**Key findings (COVID-19):**

- `diseasenowcasting` beats NobBS on WIS for COVID as well, but the
  margin depends on the delay family.
- Best `diseasenowcasting`: **HSGP / GenGamma**.

### Conclusion

`HSGP/LogNormal` and `HSGP/GeneralizedGamma` are the suggested models as
they are consistently the ranking among the top performers.

### How to reproduce

The full benchmark — the **complete own-model grid** (HSGP / AR(1) / SIR
x {LogNormal, Gamma, Generalized-Gamma, Dirichlet}) fit with
[`nowcast_twostage()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast_twostage.md)
and the seasonal / day-of-week covariates, plus **NobBS** and **three
Epinowcast variants** (point effect / random effect, default, random
walk), scored on a common evaluation-date set — lives in a single
self-contained script:

    devel/benchmark_full.R

Run it (after `R CMD INSTALL` of this package, so the parallel workers
can load it):

``` sh
Rscript devel/benchmark_full.R                                  # all 3 diseases, 50 dates
N_DATES=50  Rscript devel/benchmark_full.R  
```

It prints a ranked WIS table per disease and saves the tidy scores to
`devel/results/benchmark_scores.rds`.

> Requirements: `NobBS` and `epinowcast` (the latter needs `cmdstanr` +
> a working CmdStan). The COVID series uses the aggregated Colombia data
> at the path in the `COVID_RDS` environment variable.

> **Note.** Earlier this section inlined a *simplified* reimplementation
> that fit only three own models, a single Epinowcast variant, and ran
> the own models through
> [`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)
> **without** the `X` covariates — which understated the
> diseasenowcasting models and inflated NobBS. `devel/benchmark_full.R`
> is the faithful, covariate-aware pipeline that reproduces the tables
> above.

### References

Instituto Nacional de Salud (INS). 2024. *Casos Positivos de COVID-19 En
Colombia*. Datos Abiertos Colombia.
<https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia-/gt2j-8ykr>.

McGough, Sarah F, Michael A Johansson, Marc Lipsitch, and Nicolas A
Menzies. 2020. “Nowcasting by Bayesian Smoothing: A Flexible,
Generalizable Model for Real-Time Epidemic Tracking.” *PLoS
Computational Biology* 16 (4): e1007735.

Rohrer, Rebecca, Allegra Wilson, Jennifer Baumgartner, et al. 2025.
“Nowcasting to Monitor Real-Time Mpox Trends During the 2022 Outbreak in
New York City: Evaluation Using Reportable Disease Data Stratified by
Race or Ethnicity.” *Online Journal of Public Health Informatics* 17
(1): e56495.
