library(ggplot2)
library(posterior)
library(dplyr)

#rstantools::rstan_config(); devtools::load_all()
clipr::write_clip(rstan::get_stancode(stan_fit))
clipr::write_clip(rstan::get_stancode(gq_stan))
#FIXME: Fix the simulation priors
num_strata <- 3
num_delays <- 5
sims       <- simulate_disease(num_steps = 30,
                               num_strata = num_strata,
                               num_delays = num_delays,
                               dist = "Normal")


ggplot(sims) +
  geom_line(aes(x = onset_date, y = n, color = .strata),
            data = sims) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma_format())
