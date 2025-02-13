library(ggrepel)
data(denguedat)


denguedat_red=denguedat[denguedat$report_week<=as.Date('1991-12-22'),]
backncast= nowcast(.disease_data=denguedat_red,
                   true_date="onset_week",
                   report_date="report_week",
                   method = "optimization", dist = "Normal",
                   refresh=0, strata = "gender")



backtest_summary1 <- backtest(backncast,
                              true_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1991-01-22'),
                              end_date = as.Date('1991-07-22'),
                              stride = 4,
                              min_horizon = -3,
                              method = "optimization", dist = "Normal",
                              refresh=0,
                              model_name='model_Normal')

backtest_summary2 <- backtest(backncast,
                              true_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1991-01-22'),
                              end_date = as.Date('1991-07-22'),
                              stride = 4,
                              min_horizon = -3,
                              method = "optimization", dist = "Poisson",
                              refresh=0,
                              model_name='model_Poisson')

backtest_summary3 <- backtest(backncast,
                              true_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1991-01-22'),
                              end_date = as.Date('1991-07-22'),
                              stride = 4,
                              min_horizon = -3,
                              method = "optimization", dist = "NegativeBinomial",
                              refresh=0,
                              model_name="Negative Binomial")

backtest_summary4 <- backtest(backncast,
                              true_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1991-01-22'),
                              end_date = as.Date('1991-07-22'),
                              stride = 4,
                              min_horizon = -3,
                              method = "optimization", dist = "Student",
                              refresh=0,
                              model_name="Stu/.dent$")


#plot backtest: basically the same as regualar plot + the observed as a line.
# as default plot horizon 0 but make the user choose.






mtr <- backtest_metrics(backtest_summary1,backtest_summary2,backtest_summary3,backtest_summary4, horizons = c(0,-1,-2))


# plot_backtest_metrics
#something like this
#https://rodrigozepeda.github.io/diseasenowcasting/articles/Comparison-to-other-methods.html
#strata in row and metrics in the column of a facet_wrap. decide the default metrics


#need to selct the horizon. default is 0, can only select one.
horizons=unique(mtr$horizon)
sel_mtr="ae_median" #metric to consider
#datesbrakes="2 weeks" #copy this from plot.nowcaster

# Ensure 'now' is of Date type
  mtr$now <- as.Date(mtr$now)

# Convert data to long format for ggplot2
long_mtr <- mtr %>%
  pivot_longer(cols = sel_mtr, names_to = "metric", values_to = "value")

# Calculate averages for each metric and model
metric_averages <- long_mtr %>%
  group_by(horizon,  Strata_unified, metric, model) %>%
  summarize(avg_value = mean(value, na.rm = TRUE), .groups = "drop")


##add to the vignettes > articles > diseasenowcasting.Rmd > 3. Evaluating the model
ggplot(long_mtr, aes(x = now, y = value, color = model)) +
  geom_point() +
  geom_line() +
  facet_grid(Strata_unified ~ horizon, scales = "free_y", labeller = labeller(horizon = function(h) paste("Horizon =", h))) +
  theme_minimal() +
  labs(x = NULL, y = sel_mtr, color = NULL) +  # Remove legend title by setting color to NULL
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom",  # Place the legend at the bottom
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  ggplot2::scale_x_date(
    date_labels = "%Y-%b-%d",  # Date format
    minor_breaks = NULL,
    breaks = sort(unique(dplyr::pull(mtr, now)))
  ) +
  geom_hline(data = metric_averages, aes(yintercept = avg_value, color = model), linetype = "dashed")+
  geom_text_repel(data = metric_averages, aes(x = as.Date(Inf), y = avg_value, label = paste("avg.", round(avg_value, 2)), color = model),
                  hjust = 1, size = 3, inherit.aes = FALSE, show.legend = FALSE)

