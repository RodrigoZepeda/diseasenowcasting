
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diseasenowcasting

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/diseasenowcasting)](https://CRAN.R-project.org/package=diseasenowcasting)
[![Codecov test
coverage](https://codecov.io/gh/RodrigoZepeda/diseasenowcasting/graph/badge.svg)](https://app.codecov.io/gh/RodrigoZepeda/diseasenowcasting)
[![R-CMD-check](https://github.com/RodrigoZepeda/diseasenowcasting/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RodrigoZepeda/diseasenowcasting/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Example

``` r
library(diseasenowcasting)

# Create a fake disease process
set.seed(265824)
sims <- simulate_process_for_testing()

# Run a nowcast with very few iterations
predictions <- nowcast(sims, "onset_date", "report_date", cores = 4)
#> ℹ Computing a nowcast for 2024-09-11 per "days"
#> ℹ Assuming data is count-data where counts are in column `n`. To change this set `data_type = "linelist"`

#Get the predicted values
preds <- predictions |> 
  posterior::as_draws() |> 
  posterior::subset_draws("N_predict") |> 
  posterior::summarise_draws()

preds
#> # A tibble: 10 × 10
#>    variable          mean median    sd   mad    q5   q95  rhat ess_bulk ess_tail
#>    <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 N_predict[1,1]   7485    7485  0     0     7485  7485 NA         NA       NA 
#>  2 N_predict[2,1]   7178    7178  0     0     7178  7178 NA         NA       NA 
#>  3 N_predict[3,1]  16581   16581  0     0    16581 16581 NA         NA       NA 
#>  4 N_predict[4,1]  28006.  28005  3.03  2.97 28003 28012  1.00    4071.    3860.
#>  5 N_predict[5,1]  16233.  16232  4.51  4.45 16228 16242  1.00    3983.    3990.
#>  6 N_predict[6,1]  14445.  14444  5.56  4.45 14438 14456  1.00    4214.    3982.
#>  7 N_predict[7,1]   6076.   6075  6.31  5.93  6068  6088  1.00    3934.    3805.
#>  8 N_predict[8,1]   2219.   2218  7.10  7.41  2209  2232  1.00    3404.    3965.
#>  9 N_predict[9,1]   3486.   3485  7.90  7.41  3476  3501  1.00    4076.    3876.
#> 10 N_predict[10,1] 56870.  56869  8.46  8.90 56859 56885  1.00    3726.    3897.
```
