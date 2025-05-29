library(tidyverse)
library(ggridges)
data("denguedat")

#Consider the data used for a nowcast
mdel <- preprocess_for_nowcast(denguedat, true_date = "onset_week",
                               report_date = "report_week", units = "week",
                               now = ymd("1990/05/01")) |>
  mutate(epiweek = epiweek(onset_week)) |>
  group_by(.delay) |>
  summarise(n = sum(n)) |>
  ungroup()

#And the data for the update 1 week later
mdel2 <- preprocess_for_nowcast(denguedat, true_date = "onset_week",
                               report_date = "report_week", units = "week",
                               now = ymd("1990/05/01") + days(7)) |>
  mutate(epiweek = epiweek(onset_week)) |>
  group_by(.delay) |>
  summarise(n = sum(n)) |>
  ungroup()

interval_mat <- mdel |>
  mutate(.tval = 1:n()) |>
  select(.tval, n, .delay) |>
  as.matrix()

interval_mat2 <- mdel2 |>
  mutate(.tval = 1:n()) |>
  select(.tval, n, .delay) |>
  as.matrix()


#Now create a model
delay <- cmdstanr::cmdstan_model("devel/delay_only.stan", cpp_options = list(stan_threads = TRUE))
fit <- delay$sample(
  data = list(
    delay_distribution = 1,
    N_interval = nrow(interval_mat),
    N_new_observations_interval = nrow(interval_mat2),
    N_right = 0,
    time_delay_interval_censored = interval_mat,
    new_time_delay_interval_censored = interval_mat2,
    time_delay_right_censored = matrix(NA, nrow = 0, ncol = 3)
  ),
  init = function(chain_id){list(delay_alpha = 1, delay_beta = 1)},
  chains = 4,
  threads_per_chain = 4,
  parallel_chains = 4
)
true_delays_model <- fit$draws("true_simulated_delays", format = "df")
observed_delays_model <- fit$draws("observed_simulated_delays", format = "df")
fit$summary("multiobs")
probability <- fit$summary("theta_new_obs_interval")
vals <- probability[1:10,c("median","q5","q95")] |> bind_cols(interval_mat2)
vals <- vals |>
  mutate(logprob = n*log(median))
delay_prob <- fit$summary(variables = "probability_of_delay",
                          mean = mean, q = ~posterior::quantile2(., probs = c(0.025, 0.975))) |>
  mutate(delay = as.numeric(str_remove_all(variable, ".*\\[|\\]")) - 1)

#fit$summary("theta_new_obs_interval")


probs_delaydat <- delaydat |>
  group_by(delay, epiweek) |>
  tally() |>
  left_join(
    delaydat |>
    group_by(epiweek) |>
    summarise(total = n()) |>
    ungroup(),
    by = join_by(epiweek)
  ) |>
  mutate(prob = n/total) |>
  arrange(epiweek, delay)

ggplot()  +
  geom_point(aes(x = delay, y = prob, group = as.factor(epiweek), color = "Observed"), data = probs_delaydat,
             position = position_jitter(width = 0.25, height = 0), alpha = 0.5) +
  geom_errorbar(aes(x = delay, ymin = `q2.5`, ymax = `q97.5`, color = "Inference"), data = delay_prob, width = 0.3) +
  #geom_line(aes(x = delay, y = mean, color = "Inference"), data = delay_prob) +
  geom_point(aes(x = delay, y = mean, color = "Inference"), data = delay_prob, size = 2) +
  #geom_density(aes(x = true_simulated_delays), data = true_delays_model) +
  theme_ridges() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "Delay",
    y = "Probability"
  ) +
  scale_x_continuous(breaks = 0:15) +
  scale_color_manual("Data", values = wesanderson::wes_palette("Zissou1")[c(1,4)])

