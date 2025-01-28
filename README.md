
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

`diseasenowcasting` is an R package for nowcasting time series of
epidemiological cases. Epidemiologic surveillance tools usually have an
intrinsic delay between the **true date** of an event and the **report
date** for that event. Some examples include the **true date** being
symptom onset or testing time and the **report date** corresponds to
when the case was registered in the system. `diseasenowcasting` uses
Bayesian structural time series models (via the probabilistic
programming language [Stan](https://mc-stan.org/)) to infer the cases
that have not yet been reported thus providing a prediction of the final
number of cases.

> :warning: `diseasenowcasting` is currently under active development
> and some interface might change from the final version.

## Installing

To install `diseasenowcasting` you need to use the `remotes` package.

``` r
#install.packages("remotes") # <- Uncomment if you have not installed the `remotes` package
remotes::install_github("RodrigoZepeda/diseasenowcasting", dependencies = c("Imports","Suggests"))
```

## Example

``` r
set.seed(6728)
library(diseasenowcasting)
library(dplyr)
```

For this example we will use the `denguedat` dataset that comes with the
package.

``` r
#Load example dataset
data(denguedat)
```

The \[nowcast()\] function performs the `nowcast`. The things required
are the `true_date` and `report_date` and `strata` (if exists).

``` r
#Use just a subsample of the data for the example
denguedat <- denguedat |> 
  filter(report_week <= as.Date("1990/08/01", format = "%Y/%m/%d"))

#Run the nowcast model stratified by gender. Refresh = 0 makes it quiet
ncast <- nowcast(denguedat, true_date = "onset_week", report_date = "report_week", 
                       strata = "gender", refresh = 0)
```

The `summary` and `plot` options allow you to use the predictions:

``` r
#Create a nice plot for the predictions
plot(ncast, datesbrakes = "1 month")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## Tutorials

You can read the following articles for a more in-depth introduction to
the package:

- [An introduction to the
  package:](https://rodrigozepeda.github.io/diseasenowcasting/articles/Introduction.html)
  provides an overview of the main functions.
- [Advanced nowcast
  options:](https://rodrigozepeda.github.io/diseasenowcasting/articles/Advanced-nowcast-options.html)
  provides an in-depth look at the `nowcast()` function and its options.
