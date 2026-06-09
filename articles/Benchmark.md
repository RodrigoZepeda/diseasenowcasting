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
  system.file("extdata", "benchmark_scores.rds", package = "diseasenowcasting"),
  "../inst/extdata/benchmark_scores.rds",
  file.path("inst", "extdata", "benchmark_scores.rds")
)
scoring_file <- candidates[nzchar(candidates) & file.exists(candidates)][1]

# Not found locally (e.g. on the pkgdown website)? Download from the public repo.
if (is.na(scoring_file)) {
  base <- "https://raw.githubusercontent.com/RodrigoZepeda/diseasenowcasting"
  for (ref in c("master", "main")) {                       # whichever is the default branch
    url <- paste0(base, "/", ref, "/inst/extdata/benchmark_scores.rds")
    tmp <- tempfile(fileext = ".rds")
    ok  <- tryCatch({
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
      file.exists(tmp) && file.size(tmp) > 0
    }, error = function(e) FALSE)
    if (isTRUE(ok)) { scoring_file <- tmp; break }
  }
}
have_scores <- !is.na(scoring_file) && file.exists(scoring_file)

if (have_scores) {
  scores_df <- readRDS(scoring_file) |>
    rename(Model = model, WIS = wis, Overprediction = over, Underprediction = under,
           Dispersion = disp, Bias = bias, Cov50 = cov50, Cov90 = cov90, `N dates` = n_dates)
}
```

#### Dengue fever (weekly, Colombia)

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | N dates |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| own HSGP/nb/GeneralizedGamma | 7.5 | 1.5 | 1.7 | 4.3 | -0.11 | 0.65 | 0.91 | 43 |
| own HSGP/nb/LogNormal | 7.5 | 1.1 | 1.6 | 4.8 | -0.03 | 0.65 | 0.98 | 43 |
| own HSGP/nb/Dirichlet | 7.8 | 2.0 | 1.6 | 4.1 | -0.01 | 0.58 | 0.93 | 43 |
| own HSGP/nb/Gamma | 7.8 | 1.1 | 2.0 | 4.8 | -0.11 | 0.63 | 0.98 | 43 |
| NobBS | 8.3 | 1.2 | 3.5 | 3.5 | -0.03 | 0.60 | 0.91 | 43 |
| own AR1/nb/GeneralizedGamma | 13.7 | 1.1 | 6.5 | 6.1 | -0.06 | 0.74 | 0.95 | 43 |
| own AR1/nb/Dirichlet | 13.8 | 2.0 | 5.3 | 6.5 | 0.13 | 0.67 | 0.95 | 43 |
| own AR1/nb/LogNormal | 13.8 | 1.4 | 6.3 | 6.1 | 0.04 | 0.74 | 0.95 | 43 |
| own AR1/nb/Gamma | 15.3 | 1.2 | 8.0 | 6.1 | -0.02 | 0.67 | 0.93 | 43 |
| own SIR/nb/Dirichlet | 15.7 | 1.6 | 10.3 | 3.8 | -0.32 | 0.37 | 0.84 | 43 |
| own SIR/nb/GeneralizedGamma | 16.7 | 0.7 | 12.0 | 4.0 | -0.54 | 0.35 | 0.70 | 43 |
| own SIR/nb/LogNormal | 17.0 | 0.7 | 11.5 | 4.8 | -0.45 | 0.37 | 0.86 | 43 |
| Epinowcast (default) | 18.6 | 3.8 | 14.4 | 0.4 | -0.29 | 0.07 | 0.12 | 43 |
| own SIR/nb/Gamma | 19.4 | 0.1 | 15.1 | 4.2 | -0.65 | 0.33 | 0.72 | 43 |
| Epinowcast (weekly RE) | 22.0 | 3.5 | 17.9 | 0.6 | -0.41 | 0.10 | 0.20 | 43 |
| Epinowcast (rw) | 22.9 | 10.2 | 11.9 | 0.8 | -0.13 | 0.12 | 0.19 | 43 |

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
| own AR1/nb/GeneralizedGamma | 16.1 | 1.4 | 2.8 | 11.8 | 0.03 | 0.64 | 1.00 | 28 |
| own AR1/nb/Gamma | 16.2 | 1.4 | 2.9 | 11.8 | 0.01 | 0.68 | 1.00 | 28 |
| own AR1/nb/LogNormal | 16.5 | 1.4 | 2.7 | 12.4 | 0.07 | 0.68 | 1.00 | 28 |
| Epinowcast (point effect) | 18.0 | 10.6 | 1.3 | 6.0 | 0.45 | 0.25 | 0.64 | 28 |
| own HSGP/nb/Gamma | 19.8 | 1.5 | 3.4 | 14.9 | -0.02 | 0.68 | 1.00 | 28 |
| own AR1/nb/Dirichlet | 20.2 | 2.1 | 2.6 | 15.5 | 0.07 | 0.61 | 1.00 | 28 |
| own HSGP/nb/GeneralizedGamma | 20.2 | 1.9 | 3.2 | 15.1 | -0.04 | 0.68 | 1.00 | 28 |
| own HSGP/nb/Dirichlet | 21.0 | 2.5 | 3.1 | 15.4 | 0.03 | 0.57 | 1.00 | 28 |
| own HSGP/nb/LogNormal | 23.9 | 2.2 | 2.9 | 18.7 | -0.06 | 0.68 | 1.00 | 28 |
| own SIR/nb/GeneralizedGamma | 24.9 | 2.2 | 1.4 | 21.4 | 0.11 | 0.79 | 1.00 | 28 |
| own SIR/nb/Gamma | 25.1 | 2.1 | 1.5 | 21.5 | 0.15 | 0.79 | 1.00 | 28 |
| own SIR/nb/LogNormal | 27.1 | 2.1 | 1.4 | 23.6 | 0.13 | 0.79 | 1.00 | 28 |
| Epinowcast (rw) | 29.3 | 15.9 | 5.6 | 7.7 | 0.16 | 0.25 | 0.61 | 28 |
| own SIR/nb/Dirichlet | 30.8 | 3.2 | 1.1 | 26.5 | 0.20 | 0.82 | 1.00 | 28 |
| NobBS | 45.0 | 0.2 | 44.1 | 0.7 | -0.81 | 0.11 | 0.14 | 28 |
| Epinowcast (default) | 46.2 | 28.8 | 5.5 | 11.9 | 0.47 | 0.18 | 0.32 | 28 |

Mpox nowcast scores at d\*=0 (49 common evaluation dates, daily).
{.table}

**Key findings (mpox):**

- `diseasenowcasting` AR1 variants achieve the lowest WIS, beating NobBS
  by a wide margin (~7×).
- Best `diseasenowcasting`: **AR1 / Gamma**,

#### COVID-19 (daily, Colombia)

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | N dates |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| own HSGP/nb/GeneralizedGamma | 715.9 | 170.7 | 169.9 | 375.2 | 0.01 | 0.34 | 0.86 | 50 |
| own HSGP/nb/LogNormal | 852.6 | 194.7 | 144.0 | 513.9 | -0.03 | 0.52 | 0.94 | 50 |
| own SIR/nb/Dirichlet | 899.7 | 409.3 | 211.6 | 278.8 | 0.19 | 0.36 | 0.74 | 50 |
| NobBS | 952.0 | 382.3 | 50.5 | 519.2 | 0.24 | 0.58 | 0.84 | 50 |
| own HSGP/nb/Dirichlet | 981.0 | 537.0 | 98.6 | 345.4 | 0.43 | 0.16 | 0.62 | 50 |
| own SIR/nb/GeneralizedGamma | 1017.8 | 418.3 | 263.2 | 336.2 | 0.08 | 0.42 | 0.74 | 50 |
| own SIR/nb/LogNormal | 1416.1 | 765.8 | 217.0 | 433.2 | 0.22 | 0.40 | 0.74 | 50 |
| own AR1/nb/LogNormal | 1736.8 | 79.9 | 807.7 | 849.1 | -0.31 | 0.36 | 0.98 | 50 |
| own AR1/nb/Dirichlet | 1738.4 | 103.0 | 771.2 | 864.3 | -0.32 | 0.32 | 1.00 | 50 |
| own AR1/nb/GeneralizedGamma | 1747.1 | 76.4 | 897.1 | 773.6 | -0.33 | 0.32 | 0.96 | 50 |
| Epinowcast (default) | 24184.6 | 22202.3 | 548.6 | 1433.7 | 0.52 | 0.10 | 0.16 | 50 |
| Epinowcast (point effect) | 43789.0 | 23295.3 | 0.0 | 20493.7 | 0.97 | 0.00 | 0.16 | 50 |
| Epinowcast (rw) | 44079.0 | 40659.0 | 94.8 | 3325.2 | 0.66 | 0.09 | 0.13 | 50 |

COVID-19 nowcast scores at d\*=0 (50 common evaluation dates, daily).
{.table}

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
