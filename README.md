
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diseasenowcasting

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/diseasenowcasting)](https://CRAN.R-project.org/package=diseasenowcasting)
[![Codecov test
coverage](https://codecov.io/gh/RodrigoZepeda/diseasenowcasting/graph/badge.svg)](https://app.codecov.io/gh/RodrigoZepeda/diseasenowcasting)
[![R-CMD-check](https://github.com/RodrigoZepeda/diseasenowcasting/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RodrigoZepeda/diseasenowcasting/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Example

``` r
library(diseasenowcasting)
library(dplyr)

set.seed(32658235)

# Create a fake disease process
num_steps  <- 50
num_strata <- 2
num_delays <- 10
sims       <- simulate_disease(num_steps = num_steps, num_strata = num_strata,
                               num_delays = num_delays) |> 
  rename(strata = .strata)

# Now use model to predict disease process. If no strata is required omit the strata option
ncast <- nowcast(sims, "onset_date", "report_date", strata = "strata", cores = 4)

#Get the predicted values in a nice format
predicted_values <- summary_nowcast(ncast)
predicted_values
#> # A tibble: 102 × 6
#>    onset_date strata  Mean    SD   q05   q95
#>    <date>     <chr>  <dbl> <dbl> <dbl> <dbl>
#>  1 2024-08-09 s1         6     0     6     6
#>  2 2024-08-10 s1         6     0     6     6
#>  3 2024-08-11 s1         6     0     6     6
#>  4 2024-08-12 s1         5     0     5     5
#>  5 2024-08-13 s1         6     0     6     6
#>  6 2024-08-14 s1         6     0     6     6
#>  7 2024-08-15 s1         5     0     5     5
#>  8 2024-08-16 s1         6     0     6     6
#>  9 2024-08-17 s1         5     0     5     5
#> 10 2024-08-18 s1         3     0     3     3
#> # ℹ 92 more rows

#You can also create a plot of the nowcast
plot_nowcast(ncast)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
