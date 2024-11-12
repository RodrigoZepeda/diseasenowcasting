
#### check with no strata, check with . in the strata name


### ".strata_unified" and ".strata" can't be used as

#rstantools::rstan_config()
devtools::load_all()


# Create a fake disease process
#num_strata <- 5
#num_delays <- 8
#num_steps=15
sims=simulate_disease(num_steps = 30, num_delays = 5, num_strata = 3)
colnames(sims)[2] <- "imieistata"
pred_sims = nowcast(sims, "onset_date", "report_date", method="variational",strata = "imieistata")
plot_nowcast_bar3(pred_sims)

denguedat_sel=denguedat[denguedat$onset_week >= as.Date("1993-9-01") & denguedat$onset_week <= as.Date("1993-12-01"),]
set.seed(122)  # Set seed for reproducibility
n <- nrow(denguedat_sel)  # Total number of rows
indices <- sample(1:n, size = ceiling(n / 4))  # Randomly select a third of the rows
# Replace the selected indices in the gender column with "other"
denguedat_sel$gender[indices] <- "Other"
set.seed(211)  # Set seed for reproducibility
n <- nrow(denguedat_sel)  # Total number of rowsß
set.seed(123)  # Setting seed for reproducibility

# Define age groups
age_groups <- c("Children", "Adult", "Elderly")

# Assign random age groups
denguedat_sel$age_group <- sample(age_groups, nrow(denguedat_sel), replace = TRUE)


#indices <- sample(1:n, size = ceiling(n / 4))  # Randomly select a third of the rows
## Replace the selected indices in the gender column with "other"
#denguedat_sel$gender[indices] <- ".uncertain"


predictions_dengue<-
  nowcast(denguedat_sel, "onset_week", "report_week", method="variational",
          strata = c("gender","age_group"))

#colnames(denguedat_sel)[3] = ".g"
#predictions_dengue<-
#  nowcast(denguedat_sel, "onset_week", "report_week", method="variational",
#          strata = ".g")


#denguedat_sel[3] = NULL
#predictions_dengue<-
#  nowcast(denguedat_sel, "onset_week", "report_week", method="variational")


set.seed(237589629)

dis <- simulate_disease(num_steps = 100, num_delays = 5, num_strata = 3) |>
  dplyr::rename(strata_1 = .strata) |>
  tidyr::uncount(n) |>
  dplyr::rowwise() |>
  dplyr::mutate(strata_2 = sample(c("M","F"), 1, replace = T)) |>
  dplyr::ungroup()




