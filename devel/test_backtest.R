data(denguedat)




denguedat_red=denguedat[denguedat$report_week<=as.Date('1990-05-22'),]
backncast= nowcast(.disease_data=denguedat_red,
                   true_date="onset_week",
                   report_date="report_week",
                   method = "optimization", dist = "Normal",
                   refresh=0)



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


metrics12 <- backtest_metrics(backtest_summary1,backtest_summary2)













