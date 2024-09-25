set.seed(425)
library(tidyverse)
now <- as.Date("1990-10-01")
predictions <- nowcast(denguedat, "onset_week", "report_week",
                       dist = "Normal", method = "variational",
                       now = now)

#Get the predicted values in a nice format
predicted_values <- predictions$generated_quantities |>
  posterior::as_draws() |>
  posterior::subset_draws("N_predict") |>
  posterior::summarise_draws() |>
  dplyr::mutate(.strata = as.numeric(stringr::str_remove_all(variable,".*\\[.*,|\\]"))) |>
  dplyr::mutate(.tval = as.numeric(stringr::str_remove_all(variable,".*\\[|,.*\\]"))) |>
  dplyr::left_join(
    predictions$data$preprocessed_data |>
      dplyr::distinct(.strata, .tval, onset_week, .strata_unified)
  ) |>
  dplyr::mutate(.strata = .strata_unified)

obs <- denguedat |>
  count(onset_week) |>
  filter(onset_week <= now)

# Create plot
ggplot() +
  geom_ribbon(aes(x = onset_week, ymin = q5, ymax = q95, fill = "diseasenowcasting"),
              data = predicted_values, alpha = 0.25) +
  geom_line(aes(x = onset_week, y = n, color = "Train data", fill = "Train data"), data = obs) +
  geom_line(aes(x = onset_week, y = mean, color = "diseasenowcasting"),
            data = predicted_values, linetype = "dotted") +
  theme_bw()







