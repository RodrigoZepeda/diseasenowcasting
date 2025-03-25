library(dplyr)

data(denguedat)

denguedat <- denguedat |>
  filter(report_week <= as.Date("1990/08/01", format = "%Y/%m/%d"))

ncast1 <- nowcast(denguedat, true_date = "onset_week", report_date = "report_week",
                  method = "optimization", dist = "Normal", iter = 10)
