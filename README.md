
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diseasenowcasting

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/diseasenowcasting)](https://CRAN.R-project.org/package=diseasenowcasting)
[![Codecov test
coverage](https://codecov.io/gh/RodrigoZepeda/diseasenowcasting/graph/badge.svg)](https://app.codecov.io/gh/RodrigoZepeda/diseasenowcasting)
<!-- badges: end -->

## Example

``` r
library(diseasenowcasting)

data(denguedat)

#Might need to run again if it gives an unserialized error. Working on it. 
predictions <- nowcast(denguedat, "onset_week", "report_week", 
                       now = as.Date("1990-10-01"), 
                       dist = "NegativeBinomial", cores = 4, iter = 1000)
#> ℹ Computing a nowcast for 1990-10-01 per weeks
#> ℹ Assuming data is linelist-data where each observation is a test. If you are working with count-data set `data_type = "count"`
```
