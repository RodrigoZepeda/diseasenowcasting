data(denguedat)


denguedat_red=denguedat[denguedat$report_week<=as.Date('1990-12-22'),]
backncast= nowcast(.disease_data=denguedat_red,
                   true_date="onset_week",
                   report_date="report_week",
                   method = "optimization", dist = "Normal",
                   refresh=0, strata = "gender")



backtest_summary1 <- backtest(backncast,
                              true_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1990-01-22'),
                              end_date = as.Date('1990-05-22'),
                              stride = 4,
                              min_horizon = -3,
                              method = "optimization", dist = "Normal",
                              refresh=0,
                              model_name='model_Normal')

backtest_summary2 <- backtest(backncast,
                              true_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1990-01-22'),
                              end_date = as.Date('1990-05-22'),
                              stride = 4,
                              min_horizon = -3,
                              method = "optimization", dist = "Poisson",
                              refresh=0,
                              model_name='model_Poisson')


#plot backtest: basically the same as regualar plot + the observed as a line.
# as default plot horizon 0 but make the user choose.






mtr <- backtest_metrics(backtest_summary1,backtest_summary2, horizons = c(0,-1))




library(ggplot2)
library(tidyr)
library(dplyr)
# plot_backtest_metrics
#something like this
#https://rodrigozepeda.github.io/diseasenowcasting/articles/Comparison-to-other-methods.html
#strata in row and metrics in the column of a facet_wrap. decide the default metrics


#need to selct the horizon. default is 0, can only select one.
sel_hoz=0
sel_mtr=c("wis", "ae_median", "bias")
datesbrakes="2 weeks" #copy from plot to get a date per each point ########## TO DO ##############
########## TO DO ##############



# Ensure 'now' is of Date type
  mtr$now <- as.Date(mtr$now)

# Convert data to long format for ggplot2
long_mtr <- filter(mtr, horizon == sel_hoz) %>%
  pivot_longer(cols = any_of(sel_mtr), names_to = "metric", values_to = "value")

# Calculate averages for each metric and model
metric_averages <- long_mtr %>%
  group_by(metric, model, Strata_unified) %>%
  summarize(avg_value = mean(value, na.rm = TRUE), .groups = "drop")


# Plot the data
ggplot(long_mtr, aes(x = now, y = value, color = model)) +
  geom_point() +
  facet_wrap(Strata_unified ~ metric, scales = "free_y") +
  theme_minimal() +
  labs(x = "Date", y = NULL) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    strip.text.y = element_text(angle = 0),  # Rotate facet labels to be vertical
    legend.position = "bottom"  # Place the legend at the bottom
  ) +
  scale_x_date(
    date_labels = "%Y-%b-%d",  # Date format
    minor_breaks = NULL,  # Remove minor breaks
    date_breaks = datesbrakes  # Adjust date breaks as necessary
  ) +
  geom_hline(data = metric_averages, aes(yintercept = avg_value, color = model), linetype = "dashed")




##add to the vignettes > articles > diseasenowcasting.Rmd > 3. Evaluating the model











