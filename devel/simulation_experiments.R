set.seed(425)
library(tidyverse)
library(diseasenowcasting)
t1 <- Sys.time()
predictions <- nowcast(denguedat, "onset_week", "report_week",
                       dist = "Normal", method = "variational",
                       strata = "gender",
                       priors = set_priors(mu_p = 1, mu_q = 1, nu_p = 1))
t2 <- Sys.time()
print(t2 - t1)

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
  dplyr::mutate(.strata = .strata_unified) |>
  dplyr::rename(gender = .strata)

obs <- denguedat |>
  count(onset_week, gender)

# Create plot
ggplot() +
  geom_ribbon(aes(x = onset_week, ymin = q5, ymax = q95,
                  fill = gender),
              data = predicted_values, alpha = 0.25) +
  geom_line(aes(x = onset_week, y = n, color = gender), data = obs) +
  geom_line(aes(x = onset_week, y = median, color = gender),
            data = predicted_values, linetype = "dotted") +
  theme_bw() +
  facet_wrap(~gender) +
  xlim(c(ymd("2010/01/01"), ymd("2010/12/31")))







