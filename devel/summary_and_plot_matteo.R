
#### check with no strata, check with . in the strata name


### ".strata_unified" and ".strata" can't be used as

#rstantools::rstan_config()
devtools::load_all()


# Create a fake disease process
#num_strata <- 5
#num_delays <- 8
#num_steps=15
sims=simulate_disease(num_steps = 20, num_delays = 5, num_strata = 9)
colnames(sims)[2] <- "imieistata"
pred_sims = nowcast(sims, "onset_date", "report_date", method="variational",strata = "imieistata")
plot_nowcast(pred_sims)

denguedat_sel=denguedat[denguedat$onset_week >= as.Date("1991-09-01") & denguedat$onset_week <= as.Date("1991-12-01"),]
set.seed(122)  # Set seed for reproducibility
n <- nrow(denguedat_sel)  # Total number of rows
indices <- sample(1:n, size = ceiling(n / 4))  # Randomly select a third of the rows
# Replace the selected indices in the gender column with "other"
denguedat_sel$gender[indices] <- "Other"
set.seed(211)  # Set seed for reproducibility
n <- nrow(denguedat_sel)  # Total number of rowsß
indices <- sample(1:n, size = ceiling(n / 4))  # Randomly select a third of the rows
# Replace the selected indices in the gender column with "other"
denguedat_sel$gender[indices] <- ".uncertain"


predictions_dengue<-
  nowcast(denguedat_sel, "onset_week", "report_week", method="variational",
          strata = "gender")

#colnames(denguedat_sel)[3] = ".g"
#predictions_dengue<-
#  nowcast(denguedat_sel, "onset_week", "report_week", method="variational",
#          strata = ".g")


#denguedat_sel[3] = NULL
#predictions_dengue<-
#  nowcast(denguedat_sel, "onset_week", "report_week", method="variational")




# Define child class that inherits from ParentClass
#setClass(
#  "nowcast_stanfit",
#  contains = "stanfit"
#)
#class(predictions_largelist) <- 'nowcast_stanfit'



#'
#'























