set.seed(425)
library(ggplot2)
library(NobBS)

data("denguedat")
now <- as.Date("1990-10-01")

test_nowcast <- NobBS(data=denguedat, units="1 week", now = now,
                      onset_date="onset_week", report_date="report_week")

#Check the data
#FIXME: Method variational doesn't generate error when creating
predictions <- nowcast(denguedat, "onset_week", "report_week",
                       method = "variational",
                       now = now,
                       priors = set_priors(p = 4))


#Get the predicted values in a nice format
predicted_values <- predictions$generated_quantities |>
  posterior::as_draws() |>
  posterior::subset_draws("N_predict") |>
  posterior::summarise_draws() |>
  dplyr::mutate(.strata = as.numeric(stringr::str_remove_all(variable,".*\\[.*,|\\]"))) |>
  dplyr::mutate(.tval = as.numeric(stringr::str_remove_all(variable,".*\\[|,.*\\]"))) |>
  dplyr::left_join(
    predictions$data$preprocessed_data |>
      dplyr::distinct(.tval, onset_week)
  )

obs <- predictions$data$preprocessed_data |>
  dplyr::group_by(onset_week) |>
  dplyr::summarise(n = sum(n))

# Create plot
ggplot() +
  geom_ribbon(aes(x = onset_week, ymin = q5, ymax = q95, fill = "diseasenowcasting"),
              data = predicted_values, alpha = 0.25) +
  geom_ribbon(aes(x = onset_date, ymin = lower, ymax = upper, fill = "NobBS"),
              data = test_nowcast$estimates, alpha = 0.25) +
  geom_line(aes(x = onset_week, y = n, color = "Train data", fill = "Train data"), data = obs) +
  geom_line(aes(x = onset_week, y = mean, color = "diseasenowcasting"),
            data = predicted_values, linetype = "dotted") +
  geom_line(aes(x = onset_date, y = estimate, color = "NobBS"), data = test_nowcast$estimates) +
  geom_point(aes(x = onset_week, y = n, color = "All data", fill = "All data"), data =
              denguedat |>
               dplyr::group_by(onset_week) |>
               dplyr::count() |>
               dplyr::filter(onset_week <= !!now)) +
  theme_bw() +
  scale_color_manual("Package",
        values = c("NobBS" = "#3B9AB2", "diseasenowcasting" = "#E1AF00",
                   "All data" = "#F21A00",
                   "Train data" = "black")) +
  scale_fill_manual("Package",
        values = c("NobBS" = "#3B9AB2", "diseasenowcasting" = "#E1AF00",
                   "All data" = "#F21A00",
                   "Train data" = "black"))

