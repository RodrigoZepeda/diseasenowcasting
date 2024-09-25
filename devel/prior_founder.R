library(ggplot2)
library(posterior)
library(dplyr)

#rstantools::rstan_config(); devtools::load_all()

num_strata <- 1 #doesn't work with more RN
num_delays <- 5

sims       <- simulate_disease(num_steps = 100,
                               num_strata = num_strata,
                               num_delays = num_delays,
                               dist = "Poisson",
                               priors = set_priors(mu_0_mean_param_1 = "mean", mu_0_sd_param_1 = "sd"))



ggplot() +
  geom_point(aes(x = onset_date, y = n, color = .strata),
            data = sims$simulations) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma_format())
#
# predictions$model |>
#   as_draws() |>
#   subset_draws("mu_0_sd") |>
#   summarise_draws()
#



