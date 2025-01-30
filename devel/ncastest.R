#rm(list = ls())
library(diseasenowcasting)
library(NobBS)
library(dplyr)
library(lubridate)

data(denguedat)

ncast <- nowcast(denguedat, true_date = "onset_week", now = as.Date("1990-10-15"),
                 report_date = "report_week", refresh = 0,
                 method = "sampling", iter = 1000,
                 seed = 27549, cores = 4, chains = 4)

set.seed(645287)
dates_possible <- seq(min(denguedat$onset_week), max(denguedat$onset_week), by = "1 week")
dates_to_test  <- sample(dates_possible, size = 10) |> sort()

#We will consider only periods of at most 3 years else it makes it really slow
mindate <- min(denguedat$onset_week)

ncast_wis_table <- NULL

#The diseasenowcasting version
for (now in dates_to_test){

  cli::cli_alert_info("Processing date = {.val {which(dates_to_test == now)}}")
  now <- as.Date(now)

  #Get new data
  new_data <- denguedat |> filter(report_week <= now & report_week >= max(now - years(3), mindate))

  #Update the model
  ncast_update  <- update(ncast, new_data = new_data, now = now)

  ncast_summary <- summary(ncast_update, quantiles = c(0.025, 0.05, 0.25, 0.50, 0.75, 0.95, 0.975))

  ncast_summary <- tibble(now = now, ncast_summary)

  ncast_summary <- ncast_summary |>
    mutate(horizon = as.numeric(difftime(onset_week, !!now, units="weeks"))) |>
    filter(horizon > -3)

  ncast_wis_table <- rbind(ncast_wis_table, ncast_summary)

}
ncast_wis_table <- ncast_wis_table |>
  select(-sd, -median, -`50%`, -Strata_unified)

#The NoBBS version
set.seed(2507284)
nobbs_wis_table <- NULL
for (now in dates_to_test){

  cli::cli_alert_info("Processing date = {.val {which(dates_to_test == now)}}")
  now <- as.Date(now)

  #Get new data
  new_data <- denguedat |> filter(report_week <= now & report_week >= max(now - years(3), mindate))

  #Update the model calculating the quantiles at 95, 90 and 50%.
  #The new Nobbs version doesn't require you to estimate three times to get quantiles
  #but I am assuming the user has the CRAN version
  nbbs_cast_95 <- NobBS(data = new_data, units = "1 week", now = now,
                     onset_date = "onset_week", report_date = "report_week",
                     specs = list(conf = 0.95))

  nbbs_cast_90 <- NobBS(data = new_data, units = "1 week", now = now,
                        onset_date = "onset_week", report_date = "report_week",
                        specs = list(conf = 0.9))

  nbbs_cast_50 <- NobBS(data = new_data, units = "1 week", now = now,
                        onset_date = "onset_week", report_date = "report_week",
                        specs = list(conf = 0.5))

  #Bind all into a single table
  nbbs_cast <- nbbs_cast_95[["estimates"]] |>
    select(estimate, lower, upper, onset_date, n.reported) |>
    rename(`97.5%` = upper, `2.5%` = lower, mean = estimate, observed = n.reported) |>
    left_join(
      nbbs_cast_50[["estimates"]] |>
      select(lower, upper, onset_date) |>
      rename(`75%` = upper, `25%` = lower), by = "onset_date"
    ) |>
    left_join(
      nbbs_cast_90[["estimates"]] |>
        select(lower, upper, onset_date) |>
        rename(`95%` = upper, `5%` = lower), by = "onset_date"
    ) |>
    mutate(horizon = as.numeric(difftime(onset_date, !!now, units="weeks"))) |>
    filter(horizon > -3) |>
    mutate(now = !!now) |>
    rename(onset_week = onset_date)

  nobbs_wis_table <- rbind(nobbs_wis_table, nbbs_cast)

}
