set.seed(425)
library(ggplot2)

data("denguedat")
denguedat_sel=denguedat[denguedat$onset_week >= as.Date("1991-09-01") & denguedat$onset_week <= as.Date("1991-10-01"),]

predictions_largelist <-
  nowcast(denguedat_sel, "onset_week", "report_week", method="variational",
          strata = "gender")
