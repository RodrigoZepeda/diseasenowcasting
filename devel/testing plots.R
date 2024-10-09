library(tidyverse)
#library(diseasenowcasting)
set.seed(237589629)
dis <- simulate_disease(num_steps = 25, num_delays = 5, num_strata = 3) |>
  rename(strata_1 = .strata) |>
  tidyr::uncount(n) |>
  rowwise() |>
  mutate(strata_2 = sample(c("M","F"), 1, replace = T)) |>
  ungroup()


ncast <- nowcast(dis, onset_date = "onset_date", report_date = "report_date",
                 method = "optimization", strata = c("strata_1", "strata_2"))

summary_nowcast(ncast)
