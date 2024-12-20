
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
library(ggplot2)
library(dplyr)
library(posterior)

set.seed(32658235)

# Create a fake disease process
num_steps  <- 15
num_strata <- 2
num_delays <- 10
sims       <- simulate_disease(num_steps = num_steps, num_strata = num_strata,
                               num_delays = num_delays)

# Now use model to predict disease process. If no strata is required omit the strata option
predictions <- nowcast(sims, "onset_date", "report_date", strata = ".strata", 
                       chains = 4, cores = 4)

#Get the predicted values in a nice format
predicted_values <- predictions$generated_quantities |>
  as_draws() |>
  subset_draws("N_predict") |>
  summarise_draws() |>
  mutate(.strata = as.numeric(stringr::str_remove_all(variable,".*\\[.*,|\\]"))) |>
  mutate(.tval = as.numeric(stringr::str_remove_all(variable,".*\\[|,.*\\]"))) |> 
  left_join(
    predictions$data$preprocessed_data |> distinct(.tval, onset_date)
  ) |>
  left_join(
    predictions$data$strata_dict
  ) |> 
  mutate(.strata = .strata_unified)
  
# Sum over all delays
data_delays <- sims |>
  group_by(onset_date, .strata) |>
  summarise(n = sum(n)) 

# Create plot
ggplot(data_delays) +
  geom_ribbon(aes(x = onset_date, ymin = q5, ymax = q95, fill = as.character(.strata)), 
              data = predicted_values, linetype = "dotted", alpha = 0.5) +
  geom_line(aes(x = onset_date, y = n, color = as.character(.strata))) +
  geom_line(aes(x = onset_date, y = mean, color = as.character(.strata)), 
            data = predicted_values, linetype = "dotted") +
  theme_bw() +
  scale_color_manual("Strata", values = c("tomato3", "forestgreen")) +
  scale_fill_manual("Strata", values = c("tomato3", "forestgreen")) +
  labs(
    x = "Time",
    y = "Cases"
  )
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
