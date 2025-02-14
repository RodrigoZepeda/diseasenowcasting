## code to prepare `DATASET` dataset goes here
library(lubridate)
library(dplyr)
library(readr)
set.seed(28654)
mpox <- readr::read_csv("https://raw.githubusercontent.com/nychealth/mpox_nowcast_eval/refs/heads/main/evaluation_df_public.csv")
mpox <- mpox |>
  mutate(across(everything(), mdy))
mpox <- mpox |>
  rowwise() |>
  mutate(race =
           sample(c("Non-Hispanic White","Hispanic","Black","Asian","Other"), size = 1,
                  prob = c(0.309, 0.283, 0.202, 0.156, 0.05)))

mpoxdat <- mpox |>
  count(dx_date, dx_report_date, race)


usethis::use_data(mpoxdat, overwrite = TRUE)
