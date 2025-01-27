library(ggplot2)
library(posterior)
library(dplyr)
library(diseasenowcasting)

#rstantools::rstan_config(); devtools::load_all()

#FIXME: Fix the simulation priors
num_strata <- 3
num_delays <- 5
sims       <- simulate_disease(num_steps = 100,
                               num_strata = num_strata,
                               num_delays = num_delays, dist = "Normal")


ggplot(sims) +
  geom_line(aes(x = true_date, y = n, color = .strata),
            data = sims) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma_format())
