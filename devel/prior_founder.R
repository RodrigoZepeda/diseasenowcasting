library(ggplot2)
library(posterior)
library(dplyr)

rstantools::rstan_config(); devtools::load_all()
rstantools::rstan_config(); devtools::load_all()

num_strata <- 1 #doesn't work with more RN
num_delays <- 5

for (k in 1:10){
sims       <- simulate_disease(num_steps = 100,
                               num_strata = num_strata,
                               num_delays = num_delays,
                               warmup_steps = 0,
                               dist = "Normal",
                               priors = set_priors(
                                 mu_degree = 1,
                                 nu_degree = 0,
                                 mu_is_constant = TRUE,
                                 mu_0_mean_param_1 = 1000,
                                 xi_sd_param_2 = 0,
                                 p = 0,
                                 q = 0),
                               init = list(r = 50)
                               )
}

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

sims$stan_fit$model |> as_draws() |> subset_draws("r") |> summarise_draws()

lambdavals <- sims$stan_fit$model |>
  as_draws() |>
  subset_draws("lambda") |>
  summarise_draws() |>
  dplyr::mutate(!!as.symbol(".tval")  := as.numeric(stringr::str_remove_all(!!as.symbol("variable"),".*\\[.*,|\\]"))) |>
  dplyr::mutate(!!as.symbol("delay") := as.numeric(stringr::str_remove_all(!!as.symbol("variable"),".*\\[|,.*\\]"))) |>
  dplyr::select(!!as.symbol(".tval"), !!as.symbol("delay"), !!as.symbol("median")) |>
  dplyr::left_join(
    sims$stan_fit$data$preprocessed_data |> dplyr::distinct(onset_date, .tval)
  )

ggplot() +
  geom_line(aes(x = onset_date, y = median, color = as.character(delay)),
            data = lambdavals |> filter(onset_date < max(onset_date))) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma_format())


