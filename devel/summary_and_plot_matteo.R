library(diseasenowcasting)

# Create a fake disease process
#num_strata <- 3
#num_delays <- 8
#num_steps=12
#sims       <- simulate_process_for_testing(num_steps=num_steps,num_strata = num_strata, num_delays = num_delays)
# Now use model to predict disease process. If no strata is required omit the strata option
#predictions <- nowcast(sims, "onset_date", "report_date", cores = 4, strata = ".strata")

load("~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/7_Nowcast/repos/diseasenowcasting/diseasenowcasting/data/denguedat.rda")
denguedat_sel=denguedat[denguedat$onset_week >= as.Date("1991-09-01") & denguedat$onset_week <= as.Date("1991-10-01"),]

predictions_largelist <-
  nowcast(denguedat_sel, "onset_week", "report_week", method="variational",
          strata = "gender")


predictions <- predictions_largelist$generated_quantities


# Define child class that inherits from ParentClass
#setClass(
#  "nowcast_stanfit",
#  contains = "stanfit"
#)
#class(predictions) <- 'nowcast_stanfit'

summary_nowcast <- function(x, ...) {
  #Get names from input data
  onset_date_name <- predictions_largelist$data$call_parameters$onset_date
  strata_name <- predictions_largelist$data$call_parameters$strata
  date_dic <- predictions_largelist$data$preprocessed_data |>
    dplyr::select(.tval, onset_week) |>
    dplyr::distinct()

  predictions_summary <- x |>
    posterior::as_draws() |>
    posterior::subset_draws("N_predict") |>
    posterior::summarise_draws() |>
    # Extract strata and time values
    dplyr::mutate(.strata = as.numeric(stringr::str_remove_all(variable, ".*\\[.*,|\\]")),
                  .tval = as.numeric(stringr::str_remove_all(variable, ".*\\[|,.*\\]"))) |>
    # assign the input strata names
    dplyr::left_join(predictions_largelist$dict$strata_dict, by = ".strata") |>
    # assign the onset dates
    dplyr::left_join(date_dic, by = ".tval") |>
    # Select and reorder the columns
     dplyr::select(all_of(onset_date_name), .strata_unified, mean, sd, q5, q95)|>
    # Rename the columns
    dplyr::rename(
                  !!strata_name := .strata_unified,
                  Mean = mean,
                  SD = sd,
                  q05 = q5,
                  q95 = q95)

  return(predictions_summary)
}


plot.summary <- function(predictions, ...) {
  # Make summary
  prediction_summary <-summary_nowcast(predictions)

  # Get input data
  data <- predictions_largelist$data$preprocessed_data

  # Sum over all delays
  data_delays <- data |>
    dplyr::group_by(.tval,  !!dplyr::sym(strata_name)) |>
    dplyr::summarise(n = sum(n), .groups = "drop") |>
    # assign the onset dates
    dplyr::left_join(date_dic, by = ".tval")

  # Create plot with facets
  ggplot2::ggplot(data_delays) +
    ggplot2::geom_ribbon(ggplot2::aes(x = !!dplyr::sym(onset_date_name), ymin = q05, ymax = q95, fill =  !!dplyr::sym(strata_name)),
                data = prediction_summary, linetype = "dotted", alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = n, color = !!dplyr::sym(strata_name))) +
    ggplot2::geom_line(ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = Mean, color = !!dplyr::sym(strata_name)),
              data = prediction_summary, linetype = "dotted") +
    ggplot2::facet_wrap(~ gender) + # Separate plot for each strata.   ## it needs to take the var strata_name!!!!!!!########
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none"
    )
  }

plot.summary(predictions)













##########################################
#######
#######. OOOOOLLLLLDDDDD



plot.summary1 <- function(data, predictions, ...) {
  # Make summary
  prediction_summary <- summary(predictions)

  # Sum over all delays
  data_delays <- data |>
    dplyr::rename(Time = .tval,
                  !!as.symbol("Strata") := !!as.symbol(".strata"),
                  n = n) |>
    dplyr::group_by(Time, Strata) |>
    dplyr::summarise(n = sum(n), .groups = "drop")

  # Create plot with facets
  ggplot2::ggplot(data_delays) +
    ggplot2::geom_ribbon(ggplot2::aes_string(x = Time, ymin = q05, ymax = q95, fill = as.character(Strata)),
                         data = prediction_summary, linetype = "dotted", alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = n, color = as.character(Strata))) +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Mean, color = as.character(Strata)),
                       data = prediction_summary, linetype = "dotted") +
    ggplot2::facet_wrap(~ Strata) +  # Separate plot for each strata
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none"
    )
}
