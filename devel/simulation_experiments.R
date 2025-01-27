set.seed(425)
library(tidyverse)
library(diseasenowcasting)
now <- as.Date("1990-10-01")
t1 <- Sys.time()
predictions <- nowcast(denguedat, "onset_week", "report_week",
                       method = "variational",
                       strata = "gender", now = now,
                       temporal_effects_delay = temporal_effects(week_of_year = TRUE),
                       #normalize_data = F,
                       priors = set_priors(has_cycle = F))
t2 <- Sys.time()
print(t2 - t1)

plot_nowcast(predictions, datesbrakes = "1 month")

