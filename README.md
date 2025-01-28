
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diseasenowcasting <a href="https://rodrigozepeda.github.io/diseasenowcasting/"><img src="man/figures/logo.png" align="right" height="139" alt="diseasenowcasting website" /></a>

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

#Load example dataset
data(denguedat)

#Run the nowcast model stratified by gender
predictions <- nowcast(denguedat, true_date = "onset_week", report_date = "report_week", 
                       method = "variational", now = as.Date("1990-10-01"), strata = "gender", 
                       seed = 398477, refresh = 0)

#Create a nice plot for the predictions
plot(predictions, datesbrakes = "1 month")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
