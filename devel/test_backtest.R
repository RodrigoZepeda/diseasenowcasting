data(denguedat)
backtest_summary1 <- backtest(.disease_data=denguedat,
                              onset_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1990-01-22'),
                              end_date = as.Date('1990-05-22'),
                              stride = 4,
                              min_horizon = -3,
                              method = "optimization", dist = "Normal",
                              priors = set_priors(),
                              refresh=0,
                              model_name='model_Normal')

backtest_summary2 <- backtest(.disease_data=denguedat,
                              onset_date="onset_week",
                              report_date="report_week",
                              start_date = as.Date('1990-01-22'),
                              end_date = as.Date('1990-05-22'),
                              stride = 4,
                              min_horizon = -3,
                              method = "optimization", dist = "Poisson",
                              priors = set_priors(),
                              refresh=0,
                              model_name='model_Poisson')


metrics1 <- backtest_metrics(backtest_summary1, c('MAE','RMSE','WIS'), c(0,-1,-2,-3))
metrics2 <- backtest_metrics(backtest_summary2, c('MAE','RMSE','WIS'), c(0,-1,-2,-3))
metrics <- backtest_metrics(list(backtest_summary1,backtest_summary2),
                             c('MAE','RMSE','WIS'), c(0,-1,-2,-3))

print(metrics)
