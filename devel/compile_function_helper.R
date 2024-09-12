set.seed(265824)

data("denguedat")
# Run a nowcast with very few iterations
#sims <- simulate_process_for_testing()
num_strata <- 2
num_delays <- 8
sims <- simulate_process_for_testing(num_strata = num_strata, num_delays = num_delays,
  mu_0_param_1 = log(4.2), nu_0_param_1 = log(5.4))
predictions <- nowcast(sims, "onset_date", "report_date", cores = 4, strata = ".strata",
                       mu_0_param_1 = log(4.2), nu_0_param_1 = log(5.4), r_param_2 = 100)


#Get the predicted values
divifs <- predictions |>
  posterior::as_draws() |>
  posterior::subset_draws("N_predict") |>
  posterior::summarise_draws() |>
  dplyr::mutate(.strata = as.numeric(stringr::str_remove_all(variable,".*\\[.*,|\\]"))) |>
  dplyr::mutate(.tval = as.numeric(stringr::str_remove_all(variable,".*\\[|,.*\\]")))

library(ggplot2)
sims |>
  dplyr::group_by(.tval, .strata) |>
  dplyr::summarise(n = sum(n)) |>
  ggplot() +
  geom_ribbon(aes(x = .tval, ymin = q5, ymax = q95, fill = as.character(.strata)), data = divifs, linetype = "dotted", alpha = 0.5) +
  geom_line(aes(x = .tval, y = n, color = as.character(.strata))) +
  geom_line(aes(x = .tval, y = mean, color = as.character(.strata)), data = divifs, linetype = "dotted") +
  theme_bw()



# # Change to more iterations
# predictions <- nowcast(denguedat, "onset_week", "report_week",
#                        now = as.Date("1990-10-01"),
#                        dist = "NegativeBinomial", cores = 4)
#
# generated_qs <- rstan::stan_model("inst/stan/generated_quantities.stan")
# generated_quantities <- rstan::gqs(generated_qs, data = predictions$stan_data, draws = as.matrix(predictions$model))
