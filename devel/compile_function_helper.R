set.seed(425245)
library(ggplot2)

data("denguedat")
# Run a nowcast with very few iterations
#sims <- simulate_process_for_testing()
num_steps  <- 50
num_strata <- 3
num_delays <- 8
sims <- simulate_process_for_testing(num_steps = num_steps,
                                     num_strata = num_strata, num_delays = num_delays)

# Sum over all delays
data_delays <- sims |>
  dplyr::group_by(.tval, .strata) |>
  dplyr::summarise(n = sum(n))

ggplot(data_delays) +
  geom_line(aes(x = .tval, y = n, color = as.character(.strata))) +
  theme_bw()

#Check the data
predictions <- nowcast(sims, "onset_date", "report_date", method = "optimization", verbose = TRUE,
                       priors = set_priors(mu_degree = 1))


#Get the predicted values in a nice format
predicted_values <- predictions$generated_quantities |>
  posterior::as_draws() |>
  posterior::subset_draws("N_predict") |>
  posterior::summarise_draws() |>
  dplyr::mutate(.strata = as.numeric(stringr::str_remove_all(variable,".*\\[.*,|\\]"))) |>
  dplyr::mutate(.tval = as.numeric(stringr::str_remove_all(variable,".*\\[|,.*\\]")))

# # Sum over all delays
# data_delays <- denguedat |>
#   dplyr::mutate(.tval = 1 + as.numeric(onset_week - min(onset_week))/7) |>
#   dplyr::count(.tval, gender) |>
#   dplyr::rename(.strata = gender)

data_delays <- sims |>
  dplyr::group_by(.tval) |>
  dplyr::summarise(n = sum(n))

# Create plot
ggplot(data_delays) +
  geom_ribbon(aes(x = .tval, ymin = q5, ymax = q95, fill = as.character(.strata)),
              data = predicted_values, linetype = "dotted", alpha = 0.5) +
  geom_line(aes(x = .tval, y = n)) +
  geom_line(aes(x = .tval, y = mean, color = as.character(.strata)),
            data = predicted_values, linetype = "dotted") +
  theme_bw()

