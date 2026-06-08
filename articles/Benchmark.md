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

#### Dengue fever (weekly, Colombia)

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | N dates |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| HSGP/GeneralizedGamma | 7.04 | 1.18 | 1.64 | 4.21 | 0.01 | 0.70 | 0.94 | 50 |
| HSGP/LogNormal | 7.28 | 1.11 | 1.58 | 4.59 | 0.02 | 0.70 | 0.94 | 50 |
| HSGP/Dirichlet | 7.40 | 1.71 | 1.60 | 4.10 | 0.04 | 0.68 | 0.90 | 50 |
| HSGP/Gamma | 7.41 | 0.91 | 2.04 | 4.45 | -0.12 | 0.70 | 0.93 | 50 |
| NobBS | 8.13 | 1.39 | 3.09 | 3.66 | 0.03 | 0.58 | 0.92 | 50 |
| AR1/LogNormal | 11.32 | 1.19 | 4.20 | 5.93 | 0.04 | 0.66 | 0.98 | 50 |
| AR1/GeneralizedGamma | 12.53 | 0.83 | 6.88 | 4.82 | -0.13 | 0.60 | 0.96 | 50 |
| AR1/Dirichlet | 13.65 | 1.53 | 6.41 | 5.70 | 0.08 | 0.70 | 0.96 | 50 |
| SIR/Dirichlet | 14.61 | 0.78 | 9.29 | 4.53 | -0.30 | 0.48 | 0.86 | 50 |
| AR1/Gamma | 15.48 | 0.96 | 8.98 | 5.54 | -0.02 | 0.67 | 0.91 | 50 |
| SIR/LogNormal | 16.81 | 0.63 | 11.91 | 4.27 | -0.48 | 0.40 | 0.80 | 50 |
| SIR/GeneralizedGamma | 17.40 | 0.62 | 13.34 | 3.44 | -0.54 | 0.34 | 0.66 | 50 |
| SIR/Gamma | 18.18 | 0.14 | 14.05 | 3.99 | -0.67 | 0.30 | 0.70 | 50 |
| Epinowcast (rw) | 22.13 | 12.75 | 8.00 | 1.38 | -0.05 | 0.15 | 0.24 | 50 |
| Epinowcast (default) | 24.47 | 9.05 | 14.85 | 0.57 | -0.20 | 0.06 | 0.14 | 50 |
| Epinowcast (weekly RE) | 25.91 | 8.82 | 16.68 | 0.41 | -0.38 | 0.05 | 0.11 | 50 |

Dengue nowcast scores at d\*=0 (50 common evaluation dates, weekly).
{.table}

**Key findings (dengue):**

- All `diseasenowcasting` HSGP variants beat NobBS and all Epinowcast
  variants on WIS.
- Best `diseasenowcasting`: **HSGP / GenGamma**, WIS ≈ 7.0, cov90 ≈
  0.94.

#### Mpox (daily, USA)

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | N dates |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| AR1/Gamma | 4.13 | 0.91 | 0.58 | 2.65 | 0.23 | 0.53 | 0.94 | 49 |
| AR1/GeneralizedGamma | 4.23 | 1.02 | 0.56 | 2.65 | 0.24 | 0.49 | 0.94 | 49 |
| AR1/LogNormal | 4.29 | 1.00 | 0.54 | 2.74 | 0.25 | 0.49 | 0.94 | 49 |
| HSGP/GeneralizedGamma | 4.87 | 0.73 | 0.51 | 3.63 | 0.26 | 0.53 | 1.00 | 49 |
| HSGP/Gamma | 5.33 | 0.67 | 0.52 | 4.14 | 0.22 | 0.67 | 1.00 | 49 |
| HSGP/LogNormal | 5.78 | 0.83 | 0.48 | 4.48 | 0.29 | 0.57 | 1.00 | 49 |
| SIR/GeneralizedGamma | 6.56 | 0.37 | 0.25 | 5.94 | 0.04 | 0.96 | 1.00 | 49 |
| SIR/Gamma | 6.84 | 0.33 | 0.26 | 6.26 | 0.05 | 0.96 | 1.00 | 49 |
| SIR/LogNormal | 7.29 | 0.39 | 0.23 | 6.67 | 0.08 | 0.94 | 1.00 | 49 |
| AR1/Dirichlet | 9.86 | 1.72 | 0.46 | 7.69 | 0.30 | 0.51 | 0.94 | 49 |
| HSGP/Dirichlet | 12.22 | 2.44 | 0.41 | 9.37 | 0.42 | 0.33 | 0.92 | 49 |
| Epinowcast (point effect) | 17.34 | 11.93 | 0.61 | 4.80 | 0.69 | 0.15 | 0.52 | 49 |
| SIR/Dirichlet | 20.00 | 1.90 | 0.12 | 17.98 | 0.42 | 0.82 | 1.00 | 49 |
| NobBS | 27.66 | 1.06 | 25.21 | 1.40 | -0.18 | 0.18 | 0.45 | 49 |
| Epinowcast (default) | 30.67 | 22.69 | 3.82 | 4.15 | 0.37 | 0.17 | 0.38 | 49 |
| Epinowcast (rw) | 35.56 | 21.38 | 2.65 | 11.53 | 0.45 | 0.25 | 0.38 | 49 |

Mpox nowcast scores at d\*=0 (49 common evaluation dates, daily).
{.table style="width:100%;"}

**Key findings (mpox):**

- `diseasenowcasting` AR1 variants achieve the lowest WIS, beating NobBS
  by a wide margin (~7×).
- Best `diseasenowcasting`: **AR1 / Gamma**,

#### COVID-19 (daily, Colombia)

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | N dates |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| HSGP/GeneralizedGamma | 675.17 | 194.66 | 148.23 | 332.28 | 0.07 | 0.38 | 0.88 | 50 |
| HSGP/LogNormal | 771.91 | 208.55 | 114.67 | 448.69 | 0.02 | 0.44 | 0.92 | 50 |
| NobBS | 952.04 | 382.26 | 50.54 | 519.24 | 0.24 | 0.58 | 0.84 | 50 |
| SIR/GeneralizedGamma | 1043.75 | 487.97 | 229.12 | 326.66 | 0.10 | 0.46 | 0.76 | 50 |
| SIR/Dirichlet | 1051.04 | 511.65 | 221.79 | 317.59 | 0.24 | 0.40 | 0.74 | 50 |
| HSGP/Dirichlet | 1119.08 | 744.91 | 96.93 | 277.24 | 0.52 | 0.14 | 0.40 | 50 |
| SIR/LogNormal | 1495.60 | 849.20 | 210.75 | 435.65 | 0.25 | 0.34 | 0.72 | 50 |
| AR1/GeneralizedGamma | 1605.05 | 75.54 | 756.19 | 773.32 | -0.32 | 0.38 | 0.98 | 50 |
| AR1/LogNormal | 1615.45 | 80.90 | 659.69 | 874.86 | -0.28 | 0.48 | 1.00 | 50 |
| AR1/Dirichlet | 1656.90 | 101.19 | 698.94 | 856.77 | -0.26 | 0.32 | 0.98 | 50 |
| Epinowcast (rw) | 8886.32 | 8201.68 | 177.22 | 507.42 | 0.62 | 0.04 | 0.09 | 50 |
| Epinowcast (point effect) | 18666.43 | 13143.37 | 1.05 | 5522.01 | 0.92 | 0.09 | 0.11 | 50 |
| Epinowcast (default) | 45515.66 | 41745.39 | 549.03 | 3221.25 | 0.41 | 0.06 | 0.14 | 50 |

COVID-19 nowcast scores at d\*=0 (50 common evaluation dates, daily).
{.table style="width:100%;"}

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
