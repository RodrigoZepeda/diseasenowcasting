# COVID-19 Notifications – Colombia 2020-2023

Daily case counts of COVID-19 from Colombia's national epidemiological
surveillance system (INS), aggregated by notification date, diagnosis
date, and sex. Each row is a unique combination of these three variables
together with the number of cases `n`.

## Usage

``` r
covid_colombia
```

## Format

A data frame with 35,501 rows and 4 variables:

- notification_date:

  `Date`. The event date – when the case was notified / symptom onset
  was recorded.

- diagnosis_date:

  `Date`. The report date – when the laboratory diagnosis was registered
  in the national system.

- sex:

  `character`. Biological sex of the case: `"Female"` or `"Male"`.

- n:

  `integer`. Number of cases with this (notification_date,
  diagnosis_date, sex) combination.

## Source

Instituto Nacional de Salud (INS), Colombia. Data accessed via the
SIVIGILA open-data platform and pre-processed for the diseasenowcasting
benchmarking study.

## Details

In the nowcasting context the **event date** is `notification_date`
(when the case symptom onset was recorded in the system) and the
**report date** is `diagnosis_date` (when the laboratory result was
entered). The delay between the two reflects the time from symptom onset
to laboratory confirmation and data entry – the quantity the nowcasting
model estimates and corrects for.

The dataset covers the full first three years of the Colombian epidemic
(2020-03-02 to 2023-03-03) and was used in the NobBS comparison study to
benchmark the diseasenowcasting / diseasenowcast2 engine.

## Examples

``` r
# Build a stratified tbl_now (event = notification, report = diagnosis,
# strata = sex).  Pipe through temporal effects for day-of-week covariates.
if (requireNamespace("tbl.now", quietly = TRUE)) {
  tn <- tbl.now::tbl_now(
    covid_colombia,
    event_date  = notification_date,
    report_date = diagnosis_date,
    strata      = sex,
    case_count  = n,
    data_type   = "count-incidence",
    verbose     = FALSE
  )
  print(tn)
}
#> # A tibble:  35,501 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    notification_date diagnosis_date sex          n .event_num .report_num .delay
#>    <date>            <date>         <chr>    <int>      <dbl>       <dbl>  <dbl>
#>    [event_date]      [report_date]  [strata] [cas…      [...]       [...]  [...]
#>  1 2020-03-02        2020-03-06     Female       1          0           4      4
#>  2 2020-03-03        2020-03-14     Female       1          1          12     11
#>  3 2020-03-06        2020-03-09     Male         1          4           7      3
#>  4 2020-03-07        2020-03-09     Female       1          5           7      2
#>  5 2020-03-08        2020-03-11     Female       2          6           9      3
#>  6 2020-03-09        2020-03-11     Female       1          7           9      2
#>  7 2020-03-09        2020-03-11     Male         2          7           9      2
#>  8 2020-03-10        2020-03-11     Female       1          8           9      1
#>  9 2020-03-10        2020-03-12     Female       2          8          10      2
#> 10 2020-03-10        2020-03-13     Male         1          8          11      3
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-05-26 | Event date: "notification_date" | Report date:
#> # "diagnosis_date"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 35,491 more rows
```
