
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

data(denguedat)

# Change to more iterations
predictions <- nowcast(denguedat, "onset_week", "report_week",
  now = as.Date("1990-10-01"),
  dist = "NegativeBinomial", cores = 4, iter = 1000
)
#> ℹ Computing a nowcast for 1990-10-01 per weeks
#> ℹ Assuming data is linelist-data where each observation is a test. If you are working with count-data set `data_type = "count"`
#> Warning in .local(object, ...): some chains had errors; consider specifying
#> chains = 1 to debug
#> here are whatever error messages were returned
#> [[1]]
#> Stan model 'nowcast' does not contain samples.
#> 
#> [[2]]
#> Stan model 'nowcast' does not contain samples.
#> 
#> [[3]]
#> Stan model 'nowcast' does not contain samples.
#> Warning: The largest R-hat is NA, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

#Get the predicted values
preds <- predictions |> 
  posterior::as_draws() |> 
  posterior::subset_draws("N_predict") |> 
  posterior::summarise_draws()
```
