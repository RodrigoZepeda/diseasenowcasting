

library(ggrepel)
library(tidyr)
library(dplyr)
data(denguedat)

#try with not optimization
#remeber to add ggrepel to optional


denguedat_red=denguedat[denguedat$report_week<=as.Date('1992-3-22'),]
backncast= nowcast(.disease_data=denguedat_red,
                   true_date="onset_week",
                   report_date="report_week",
                   method = "variational", dist = "Normal",
                   refresh=0, strata = "gender")



backtest_summary1 <- backtest(backncast,
                              true_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1991-01-22'),
                              end_date = as.Date('1991-05-22'),
                              stride = 4,
                              min_horizon = -3,
#                              method = "optimization",
                              dist = "Normal",
                              refresh=0,
                              model_name='model_Normal')

backtest_summary2 <- backtest(backncast,
                              true_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1991-01-22'),
                              end_date = as.Date('1991-05-22'),
                              stride = 4,
                              min_horizon = -3,
#                              method = "optimization",
                              dist = "Poisson",
                              refresh=0,
                              model_name='model_Poisson')

backtest_summary3 <- backtest(backncast,
                              true_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1991-01-22'),
                              end_date = as.Date('1991-07-22'),
                              stride = 4,
                              min_horizon = -3,
#                              method = "optimization",
                              dist = "NegativeBinomial",
                              refresh=0,
                              model_name="Negative Binomial")

backtest_summary4 <- backtest(backncast,
                              true_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1991-01-22'),
                              end_date = as.Date('1991-07-22'),
                              stride = 4,
                              min_horizon = -3,
#                              method = "optimization",
                              dist = "Student",
                              refresh=0,
                              model_name="Stu/.dent$")


#plot backtest: basically the same as regualar plot + the observed as a line.
# as default plot horizon 0 but make the user choose.



ciccio <- backtest_metrics(backtest_summary1,backtest_summary2,backtest_summary3,backtest_summary4, horizons = c(0,-1,-2))

x <- backtest_metrics(backtest_summary1,backtest_summary2,backtest_summary3,backtest_summary4, horizons = c(0,-1,-2))
#x<- backtest_metrics(backtest_summary3,backtest_summary4, horizons = c(0,-1))


# plot_backtest_metrics
#something like this
#https://rodrigozepeda.github.io/diseasenowcasting/articles/Comparison-to-other-methods.html



horizons=unique(x$horizon)
metric="dispersion" #metric to consider
#datesbrakes="2 weeks" #copy this from plot.nowcaster



# Ensure 'now' is of Date type
  x$now <- as.Date(x$now)

# Convert data to long format for ggplot2
long_mtr <- x %>%
  pivot_longer(cols = metric, names_to = "metric", values_to = "value")

# Calculate averages for each metric and model
metric_averages <- long_mtr %>%
  group_by(horizon,  Strata_unified, metric, model) %>%
  summarize(avg_value = mean(value, na.rm = TRUE), .groups = "drop")


##add to the vignettes > articles > diseasenowcasting.Rmd > 3. Evaluating the model
ggplot(long_mtr, aes(x = now, y = value, color = model)) +
  geom_jitter(width = 5, alpha = 0.6) +
  facet_grid(Strata_unified ~ horizon, scales = "free_y", labeller = labeller(horizon = function(h) paste("Horizon =", h))) +
  theme_minimal() +
  labs(x = NULL, y = metric, color = NULL) +  # Remove legend title by setting color to NULL
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom",  # Place the legend at the bottom
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  ggplot2::scale_x_date(
    date_labels = "%Y-%b-%d",  # Date format
    minor_breaks = NULL,
    breaks = sort(unique(dplyr::pull(x, now)))
  ) +
  geom_hline(data = metric_averages, aes(yintercept = avg_value, color = model), linetype = "dashed")+
  geom_text_repel(data = metric_averages, aes(x = as.Date(Inf), y = avg_value, label = paste("avg.", round(avg_value, 2)), color = model),
                  hjust = 1, size = 3, inherit.aes = FALSE, show.legend = FALSE)

