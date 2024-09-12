
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
num_strata <- 2
num_delays <- 8
sims       <- simulate_process_for_testing(num_strata = num_strata, num_delays = num_delays)

# Now use model to predict disease process. If no strata is required omit the strata option
predictions <- nowcast(sims, "onset_date", "report_date", cores = 4, strata = ".strata")
#> ℹ Computing a nowcast for 2024-09-11 per "days"
#> ℹ Assuming data is count-data where counts are in column `n`. To change this set `data_type = "linelist"`

#Get the predicted values in a nice format
predicted_values <- predictions |>
  posterior::as_draws() |>
  posterior::subset_draws("N_predict") |>
  posterior::summarise_draws() |>
  dplyr::mutate(.strata = as.numeric(stringr::str_remove_all(variable,".*\\[.*,|\\]"))) |>
  dplyr::mutate(.tval = as.numeric(stringr::str_remove_all(variable,".*\\[|,.*\\]")))

# Plot everything
library(ggplot2)

# Sum over all delays
data_delays <- sims |>
  dplyr::group_by(.tval, .strata) |>
  dplyr::summarise(n = sum(n)) 
#> `summarise()` has grouped output by '.tval'. You can override using the
#> `.groups` argument.

# Create plot
ggplot(data_delays) +
  geom_ribbon(aes(x = .tval, ymin = q5, ymax = q95, fill = as.character(.strata)), 
              data = predicted_values, linetype = "dotted", alpha = 0.5) +
  geom_line(aes(x = .tval, y = n, color = as.character(.strata))) +
  geom_line(aes(x = .tval, y = mean, color = as.character(.strata)), 
            data = predicted_values, linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
