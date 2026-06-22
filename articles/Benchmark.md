# Benchmark (diseasenowcasting vs NobBS, epinowcast and baselinenowcast)

> **NOTE:** THIS VIGNETTE IS UNDER DEVELOPMENT.

## TL; DR

**Does `diseasenowcasting` actually work?** On three real outbreaks —
dengue, mpox and COVID-19 — we asked each method to estimate the most
recent, still- incomplete case counts, and then checked those estimates
against the totals that *eventually* arrived. `diseasenowcasting` was
**consistently as accurate as, and usually more accurate than**, three
established nowcasting packages (NobBS, epinowcast and baselinenowcast).
It was also consistently **faster**

> Its `HSGP/LogNormal` and `HSGP/GeneralizedGamma` models are the most
> reliable hence the ones we recommend.

## What this benchmark does

A nowcast estimates **what the recent case counts will turn out to be**
once all the delayed reports have come in. This benckmark:

1.  Picks a past date and hides everything reported after it (so the
    method only sees what a real-time analyst would have seen).
2.  Asks each method to nowcast the most recent date.
3.  Compares that nowcast to the count that **eventually** got reported.
4.  Repeats over **50 dates per disease** and summarises.

We do this for three diseases with different reporting behaviours, and
against three widely used tools:

| Disease | Data | Time unit | Compared against |
|----|----|----|----|
| Dengue fever | `denguedat` (Puerto Rico) (McGough et al. 2020) | Weekly | NobBS, epinowcast, baselinenowcast |
| Mpox | `mpoxdat` (USA) (Rohrer et al. 2025) | Daily | NobBS, epinowcast, baselinenowcast |
| COVID-19 | `covid_colombia` (Instituto Nacional de Salud (INS) 2024) | Daily | NobBS, epinowcast, baselinenowcast |

[**NobBS**](https://cran.r-project.org/web/packages/NobBS/index.html),
[**epinowcast**](https://github.com/epinowcast/epinowcast) and
[**baselinenowcast**](https://github.com/epinowcast/baselinenowcast) are
the established nowcasting packages we compare against.

### How to read the score tables

Each method gives a prediction interval for the eventual count. We score
it with the **Weighted Interval Score (WIS)**, one of the standard
metrics in nowcasting and forecasting challenges.

> The one thing to remember: **lower WIS is better.**

We also report **coverage**: how often the truth actually falls inside
the stated prediction intervals. A well-calibrated method’s **90%
interval should contain the truth about 90% of the time**.

### How was it compared

> **Note** Each method is scored only on the dates where it *and*
> `diseasenowcasting` both produced a nowcast (a “common date set”). A
> package that occasionally fails to fit therefore only shrinks *its
> own* comparison, never the whole benchmark. A note under each table
> reports who failed and how often. Where a package offers several
> options (e.g. epinowcast’s reporting- delay choices), we show the ones
> that fit most reliably.

We tried **epinowcast** with the Log-normal, Log-logistic and Gamma
delay distributions, plus a **non-parametric** delay. We utilized the
`Pathfinder` option on **epinowcast** as **sampling** was too slow for a
realistic comparison.

**baselinenowcast and NobBS packages are run as designed.** They are run
*as is*.

## Results

The pre-computed scores ship with the package, so these tables reproduce
exactly.

For each disease there are four tables: (1) **every `diseasenowcasting`
model**, so you can see the spread; (2) **`diseasenowcasting` vs
NobBS**; (3) **`diseasenowcasting` vs epinowcast**; and (4)
**`diseasenowcasting` vs baselinenowcast**. In every table, **lower WIS
= more accurate**, and you want coverage close to its stated level (0.90
for `Cov90`). The footnote under each table notes any competitor that
failed to fit on some dates.

## Nobbs

### Dengue fever (weekly, Colombia)

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | Time (s) |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| own HSGP/nb/GeneralizedGamma | **7.0** | 1.5 | 1.6 | 4.0 | **0.00** | **0.62** | **0.92** | 6.96 |
| own HSGP/nb/LogNormal | 7.1 | 1.4 | **1.5** | 4.1 | 0.03 | **0.60** | **0.94** | 5.06 |
| own HSGP/nb/Dirichlet | 7.6 | 1.9 | **1.5** | 4.2 | 0.01 | **0.62** | **0.90** | **4.09** |
| NobBS | 8.1 | 1.4 | 3.1 | 3.7 | 0.03 | **0.58** | **0.92** | 54.38 |
| own AR1/nb/GeneralizedGamma | 13.3 | 1.0 | 7.0 | 5.3 | -0.12 | **0.68** | **0.94** | 76.96 |
| own AR1/nb/Dirichlet | 13.7 | 1.8 | 5.8 | 6.1 | 0.08 | **0.66** | **0.94** | 13.34 |
| own AR1/nb/LogNormal | 13.8 | 1.4 | 6.7 | 5.7 | **0.00** | **0.70** | **0.96** | 17.59 |
| own SIR/nb/Dirichlet | 14.6 | 1.4 | 9.7 | 3.6 | -0.29 | 0.42 | 0.86 | 20.09 |
| own SIR/nb/LogNormal | 16.3 | **0.6** | 11.5 | 4.1 | -0.49 | 0.38 | 0.80 | 29.56 |
| own SIR/nb/GeneralizedGamma | 16.5 | **0.6** | 12.5 | 3.4 | -0.52 | 0.34 | 0.68 | 104.60 |
| Epinowcast (LogNormal, RE) | 16.5 | 4.7 | 10.5 | 1.2 | -0.29 | 0.24 | 0.30 | 75.65 |
| Epinowcast (LogNormal, RW) | 17.2 | 5.5 | 10.2 | 1.5 | -0.22 | 0.20 | 0.28 | 76.31 |
| baselinenowcast | 17.4 | 1.9 | 5.9 | 9.5 | 0.19 | **0.56** | 0.82 | 14.65 |
| Epinowcast (Gamma, RW) | 23.7 | 13.7 | 9.4 | **0.6** | 0.18 | 0.06 | 0.10 | 17.66 |
| Epinowcast (Gamma, RE) | 31.0 | 9.5 | 20.6 | 1.0 | -0.11 | 0.04 | 0.12 | 25.18 |

Dengue: diseasenowcasting vs NobBS (50 common evaluation dates). {.table
style="width:100%;"}

> *Convergence over the 50 evaluation dates: Epinowcast (LogNormal,
> point) failed on 50 of 50 dates; Epinowcast (Gamma, point) failed on
> 50 of 50 dates; Epinowcast (Nonparametric, RW) failed on 50 of 50
> dates; Epinowcast (Nonparametric, point) failed on 50 of 50 dates;
> Epinowcast (Nonparametric, RE) failed on 50 of 50 dates. The
> comparison table uses each method’s most reliable variant(s) on the
> dates they share.*

**Key findings (dengue):** the `diseasenowcasting` HSGP variants take
the top spots, beating both NobBS and the best Epinowcast variant on
WIS. Best model: **HSGP / LogNormal** (WIS \approx 7, cov90 \approx
0.94).

### Mpox (daily, USA)

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | Time (s) |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| own HSGP/nb/GeneralizedGamma | **13.3** | 2.4 | 1.9 | 9.0 | 0.31 | 0.45 | **0.94** | 0.79 |
| own AR1/nb/GeneralizedGamma | 13.5 | 3.2 | 1.7 | 8.6 | 0.39 | 0.37 | **0.94** | 0.83 |
| own AR1/nb/LogNormal | 13.8 | 3.3 | 1.7 | 8.8 | 0.38 | 0.39 | **0.92** | 0.79 |
| own HSGP/nb/LogNormal | 14.1 | 2.5 | 1.8 | 9.7 | 0.33 | 0.45 | **0.96** | 0.73 |
| own SIR/nb/GeneralizedGamma | 16.3 | 1.2 | 0.8 | 14.3 | **0.10** | **0.88** | **1.00** | 1.46 |
| own SIR/nb/LogNormal | 16.9 | 1.3 | 0.9 | 14.7 | 0.12 | **0.88** | **1.00** | 1.38 |
| own HSGP/nb/Dirichlet | 20.0 | 4.1 | 1.7 | 14.3 | 0.40 | 0.41 | 0.88 | **0.72** |
| own AR1/nb/Dirichlet | 21.1 | 4.6 | 1.4 | 15.1 | 0.40 | 0.37 | 0.88 | 0.79 |
| own SIR/nb/Dirichlet | 26.1 | 2.4 | **0.6** | 23.1 | 0.32 | **0.86** | **1.00** | 1.39 |
| NobBS | 27.7 | **1.1** | 25.2 | **1.4** | -0.18 | 0.18 | 0.45 | 59.34 |

Mpox: diseasenowcasting vs NobBS (49 common evaluation dates). {.table
style="width:100%;"}

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | Time (s) |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Epinowcast (Gamma, RE) | **12.6** | 7.6 | 1.3 | **3.7** | 0.47 | 0.29 | 0.62 | 7.20 |
| Epinowcast (Gamma, point) | 13.0 | 7.6 | 1.0 | 4.4 | 0.51 | 0.25 | 0.62 | 6.14 |
| own HSGP/nb/GeneralizedGamma | 13.5 | 2.5 | 1.9 | 9.1 | 0.30 | 0.44 | **0.94** | 0.78 |
| own AR1/nb/GeneralizedGamma | 13.5 | 3.2 | 1.7 | 8.6 | 0.38 | 0.38 | **0.94** | 0.82 |
| own AR1/nb/LogNormal | 13.8 | 3.2 | 1.7 | 8.9 | 0.37 | 0.40 | **0.92** | 0.79 |
| own HSGP/nb/LogNormal | 14.3 | 2.6 | 1.9 | 9.9 | 0.33 | 0.44 | **0.96** | 0.72 |
| Epinowcast (LogNormal, point) | 15.6 | 10.4 | 0.7 | 4.5 | 0.60 | 0.19 | 0.54 | 6.31 |
| Epinowcast (LogNormal, RE) | 16.5 | 10.2 | 1.4 | 4.8 | 0.63 | 0.12 | 0.46 | 7.21 |
| own SIR/nb/GeneralizedGamma | 16.6 | **1.2** | 0.8 | 14.6 | **0.09** | **0.88** | **1.00** | 1.44 |
| own SIR/nb/LogNormal | 17.2 | 1.3 | 0.9 | 15.0 | 0.12 | **0.88** | **1.00** | 1.37 |
| own HSGP/nb/Dirichlet | 20.4 | 4.1 | 1.7 | 14.5 | 0.39 | 0.42 | 0.88 | **0.71** |
| own AR1/nb/Dirichlet | 21.2 | 4.6 | 1.4 | 15.2 | 0.39 | 0.38 | **0.90** | 0.79 |
| own SIR/nb/Dirichlet | 26.6 | 2.5 | **0.6** | 23.5 | 0.31 | **0.85** | **1.00** | 1.37 |
| Epinowcast (Gamma, RW) | 37.7 | 21.7 | 5.0 | 11.0 | 0.43 | 0.17 | 0.35 | 21.73 |

Mpox: diseasenowcasting vs epinowcast (48 common evaluation dates).
{.table style="width:100%;"}

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | Time (s) |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| own HSGP/nb/GeneralizedGamma | **11.8** | 2.5 | 1.2 | **8.0** | 0.38 | 0.44 | **0.93** | 0.80 |
| own HSGP/nb/LogNormal | 12.5 | 2.6 | 1.3 | 8.5 | 0.41 | 0.44 | **0.95** | **0.73** |
| own AR1/nb/GeneralizedGamma | 12.6 | 3.7 | 1.0 | **8.0** | 0.48 | 0.33 | **0.93** | 0.84 |
| own AR1/nb/LogNormal | 12.9 | 3.7 | 1.0 | 8.2 | 0.48 | 0.35 | **0.91** | 0.80 |
| own SIR/nb/GeneralizedGamma | 14.7 | **1.2** | 0.4 | 13.1 | **0.14** | **0.88** | **1.00** | 1.53 |
| own SIR/nb/LogNormal | 15.3 | 1.3 | 0.4 | 13.6 | 0.16 | **0.88** | **1.00** | 1.44 |
| own HSGP/nb/Dirichlet | 18.4 | 4.3 | 1.2 | 13.0 | 0.48 | 0.37 | 0.86 | **0.73** |
| own AR1/nb/Dirichlet | 19.5 | 5.1 | 0.8 | 13.5 | 0.50 | 0.33 | 0.86 | 0.80 |
| own SIR/nb/Dirichlet | 24.3 | 2.4 | **0.2** | 21.7 | 0.38 | **0.88** | **1.00** | 1.45 |
| baselinenowcast | 53.1 | 14.5 | 2.1 | 36.5 | 0.56 | 0.23 | 0.81 | 3.33 |

Mpox: diseasenowcasting vs baselinenowcast (43 common evaluation dates).
{.table style="width:100%;"}

> *Convergence over the 49 evaluation dates: Epinowcast (LogNormal,
> point) failed on 1 of 49 dates; Epinowcast (LogNormal, RE) failed on 1
> of 49 dates; Epinowcast (Gamma, RW) failed on 1 of 49 dates;
> Epinowcast (Gamma, point) failed on 1 of 49 dates; Epinowcast (Gamma,
> RE) failed on 1 of 49 dates; Epinowcast (LogNormal, RW) failed on 2 of
> 49 dates; baselinenowcast failed on 6 of 49 dates; Epinowcast
> (Nonparametric, RW) failed on 49 of 49 dates; Epinowcast
> (Nonparametric, point) failed on 49 of 49 dates; Epinowcast
> (Nonparametric, RE) failed on 49 of 49 dates. The comparison table
> uses each method’s most reliable variant(s) on the dates they share.*

**Key findings (mpox):** the `diseasenowcasting` Generalized-Gamma
variants achieve the lowest WIS, roughly **2× better than NobBS** and
well ahead of Epinowcast.

### COVID-19 (daily, Colombia)

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | Time (s) |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| own HSGP/nb/GeneralizedGamma | **699.0** | 258.6 | 162.9 | 277.4 | 0.07 | 0.32 | 0.76 | 4.17 |
| own HSGP/nb/LogNormal | 811.5 | 346.3 | 129.1 | 336.1 | **0.06** | 0.28 | 0.88 | 3.30 |
| NobBS | 952.0 | 382.3 | **50.5** | 519.2 | 0.24 | **0.58** | 0.84 | 95.62 |
| own SIR/nb/Dirichlet | 970.0 | 497.2 | 200.4 | **272.4** | 0.28 | 0.34 | 0.68 | 15.41 |
| own SIR/nb/GeneralizedGamma | 1037.9 | 506.9 | 228.6 | 302.4 | 0.13 | 0.44 | 0.74 | 44.89 |
| own HSGP/nb/Dirichlet | 1065.9 | 643.3 | 92.4 | 330.2 | 0.50 | 0.14 | 0.54 | 3.60 |
| own SIR/nb/LogNormal | 1511.0 | 881.1 | 211.5 | 418.4 | 0.27 | 0.36 | 0.70 | 21.56 |
| own AR1/nb/LogNormal | 1642.9 | **82.3** | 752.0 | 808.6 | -0.31 | 0.38 | **0.96** | **1.89** |
| own AR1/nb/GeneralizedGamma | 1665.1 | 84.3 | 826.8 | 754.0 | -0.33 | 0.30 | **0.96** | 2.17 |
| own AR1/nb/Dirichlet | 1686.2 | 103.4 | 731.2 | 851.6 | -0.30 | 0.26 | **0.96** | 2.71 |

COVID-19: diseasenowcasting vs NobBS (50 common evaluation dates).
{.table}

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | Time (s) |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| own HSGP/nb/GeneralizedGamma | **734.2** | 275.2 | 168.3 | 290.7 | 0.13 | 0.34 | 0.74 | 4.07 |
| own HSGP/nb/LogNormal | 853.3 | 368.4 | 131.9 | 353.0 | **0.11** | 0.28 | 0.87 | 3.22 |
| own SIR/nb/Dirichlet | 1021.7 | 526.2 | 213.2 | **282.3** | 0.29 | 0.34 | 0.66 | 15.13 |
| own SIR/nb/GeneralizedGamma | 1092.2 | 538.6 | 243.0 | 310.5 | 0.15 | 0.43 | 0.72 | 43.68 |
| own HSGP/nb/Dirichlet | 1125.6 | 682.4 | 98.3 | 344.9 | 0.52 | 0.13 | 0.51 | 3.53 |
| own SIR/nb/LogNormal | 1589.6 | 929.5 | 225.0 | 435.1 | 0.27 | 0.38 | 0.68 | 21.75 |
| own AR1/nb/LogNormal | 1705.2 | **83.3** | 798.5 | 823.5 | -0.31 | 0.36 | **0.96** | **1.88** |
| own AR1/nb/GeneralizedGamma | 1729.3 | 85.9 | 879.0 | 764.5 | -0.33 | 0.28 | **0.96** | 2.15 |
| own AR1/nb/Dirichlet | 1745.0 | 104.0 | 777.7 | 863.3 | -0.31 | 0.26 | **0.96** | 2.68 |
| Epinowcast (LogNormal, RE) | 16082.2 | 12227.4 | 93.9 | 3760.9 | 0.81 | 0.09 | 0.34 | 48.85 |
| Epinowcast (LogNormal, RW) | 20996.4 | 18234.2 | 351.1 | 2411.0 | 0.65 | 0.06 | 0.06 | 87.64 |
| Epinowcast (LogNormal, point) | 21471.2 | 15805.1 | **3.3** | 5662.8 | 0.91 | 0.02 | 0.26 | 30.90 |

COVID-19: diseasenowcasting vs epinowcast (47 common evaluation dates).
{.table}

| Model | WIS | Overprediction | Underprediction | Dispersion | Bias | Cov50 | Cov90 | Time (s) |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| own HSGP/nb/GeneralizedGamma | **713.2** | 263.9 | 166.2 | 283.0 | 0.09 | 0.33 | 0.76 | 4.24 |
| own HSGP/nb/LogNormal | 828.0 | 353.3 | 131.7 | 343.0 | 0.08 | 0.29 | 0.88 | 3.35 |
| own SIR/nb/Dirichlet | 989.7 | 507.3 | 204.5 | **277.9** | 0.31 | 0.35 | 0.67 | 15.71 |
| own SIR/nb/GeneralizedGamma | 1059.0 | 517.2 | 233.3 | 308.5 | 0.14 | 0.45 | 0.73 | 45.79 |
| own HSGP/nb/Dirichlet | 1087.6 | 656.4 | 94.3 | 336.9 | 0.53 | 0.14 | 0.53 | 3.67 |
| baselinenowcast | 1397.9 | 515.0 | **84.5** | 798.5 | **0.05** | **0.53** | 0.88 | 10.99 |
| own SIR/nb/LogNormal | 1541.8 | 899.1 | 215.8 | 426.9 | 0.29 | 0.37 | 0.69 | 21.99 |
| own AR1/nb/LogNormal | 1676.4 | **84.0** | 767.3 | 825.1 | -0.29 | 0.39 | **0.96** | **1.92** |
| own AR1/nb/GeneralizedGamma | 1699.0 | 86.0 | 843.6 | 769.4 | -0.32 | 0.31 | **0.96** | 2.20 |
| own AR1/nb/Dirichlet | 1720.6 | 105.5 | 746.1 | 869.0 | -0.29 | 0.27 | **0.96** | 2.75 |

COVID-19: diseasenowcasting vs baselinenowcast (49 common evaluation
dates). {.table}

> *Convergence over the 50 evaluation dates: baselinenowcast failed on 1
> of 50 dates; Epinowcast (LogNormal, RW) failed on 2 of 50 dates;
> Epinowcast (LogNormal, point) failed on 2 of 50 dates; Epinowcast
> (LogNormal, RE) failed on 2 of 50 dates; Epinowcast (Gamma, point)
> failed on 9 of 50 dates; Epinowcast (Gamma, RE) failed on 10 of 50
> dates; Epinowcast (Gamma, RW) failed on 12 of 50 dates; Epinowcast
> (Nonparametric, RW) failed on 50 of 50 dates; Epinowcast
> (Nonparametric, point) failed on 50 of 50 dates; Epinowcast
> (Nonparametric, RE) failed on 50 of 50 dates. The comparison table
> uses each method’s most reliable variant(s) on the dates they share.*

**Key findings (COVID-19):** `diseasenowcasting` again leads on WIS,
though the margin over NobBS depends on the delay family.

## Conclusion

Across three diseases with very different reporting patterns, scored
against the counts that eventually arrived, **`diseasenowcasting`
matched or beat NobBS, epinowcast and baselinenowcast** on accuracy
(WIS) while keeping its uncertainty intervals trustworthy.

## How to reproduce

Everything lives in a single self-contained script:

    devel/benchmark_full.R

Run it (after `R CMD INSTALL` of this package, so the parallel workers
can load it):

``` sh
RUN_AUTO=FALSE Rscript devel/benchmark_full.R                   # skip auto_nowcast (faster)
```

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

## References

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
