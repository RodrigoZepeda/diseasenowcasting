#' Makes a summary of nowcast() output
#'
#' Makes a tidy summary dataframe of the results of the function nowcast()
#'
#' @param nowcast_output the output of the nowcast() function
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#' # Run a nowcast with very few iterations
#' # change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
#' now <- as.Date("1990-10-01")
#'
#' # perform nowcasting
#' predictions <- nowcast(denguedat, "onset_week", "report_week", now = now, method = "optimization", seed = 2495624, iter = 10)
#'
#' # create summary dataframe
#' summary_nowcast(predictions)
#'
#'
#' @export
summary_nowcast <- function(nowcast_output, ...) {
  #Get names from input data
  onset_date_name <- nowcast_output$data$call_parameters$onset_date
  strata_name <- nowcast_output$data$call_parameters$strata
  date_dic <- nowcast_output$data$preprocessed_data |>
    dplyr::select(.tval, onset_week) |>
    dplyr::distinct()

  predictions_summary <- nowcast_output$generated_quantities |>
    posterior::as_draws() |>
    posterior::subset_draws("N_predict") |>
    posterior::summarise_draws() |>
    # Extract strata and time values
    dplyr::mutate(.strata = as.numeric(stringr::str_remove_all(variable, ".*\\[.*,|\\]")),
                  .tval = as.numeric(stringr::str_remove_all(variable, ".*\\[|,.*\\]"))) |>
    # assign the input strata names
    dplyr::left_join(nowcast_output$data$strata_dict, by = ".strata") |>
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



#' Create plots form the nowcast() output
#'
#' Plots the prediction of the function nowcast() of the observed value
#'
#' @param nowcast_output the output of the nowcast() function
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#' # Run a nowcast with very few iterations
#' # change to method = "sampling" when working and remove the iter = 10 (or set to iter = 2000)
#' now <- as.Date("1990-10-01")
#'
#' # perform nowcasting
#' predictions <- nowcast(denguedat, "onset_week", "report_week", now = now, method = "optimization", seed = 2495624, iter = 10)
#'
#' # create summary dataframe
#' plot_nowcast(predictions)
#'
#'
#' @export
plot_nowcast <- function(nowcast_output, ...) {
  # Get names from input data
  onset_date_name <- nowcast_output$data$call_parameters$onset_date
  strata_name <- nowcast_output$data$call_parameters$strata

  # Create dictionaries for onset week and strata
  date_dic <- nowcast_output$data$preprocessed_data |>
    dplyr::select(.tval, onset_week) |>
    dplyr::distinct()
  stata_dic <- nowcast_output$data$strata_dict

  # Make summary
  prediction_summary <- summary_nowcast(nowcast_output)

  # Get input data and sum over all delays
  data_delays <- nowcast_output$data$preprocessed_data |>
    dplyr::group_by(.tval, .strata) |>
    dplyr::summarise(n = sum(n), .groups = "drop") |>
    # Assign the onset dates
    dplyr::left_join(date_dic, by = ".tval") |>
    # Assign the strata names
    dplyr::left_join(stata_dic, by = ".strata") |>
    # Rename the strata column
    dplyr::rename(!!strata_name := .strata_unified)

  # Create plot with facets
    ggplot2::ggplot(data_delays) +
    # Add prediction ribbon for 95% CI
    ggplot2::geom_ribbon(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), ymin = q05, ymax = q95, fill =  !!dplyr::sym(strata_name)),
                         data = prediction_summary, alpha = 0.3
      ) +
    # Add real cases line
    ggplot2::geom_line(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = n, color = !!dplyr::sym(strata_name), linetype="Observed")
      ) +
    # Add predicted mean line
    ggplot2::geom_line(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = Mean, color = !!dplyr::sym(strata_name), linetype = "Predicted (Mean and 95% CI)"),
                       data = prediction_summary
      ) +
    # Facet by strata
    ggplot2::facet_wrap(as.formula(paste("~", strata_name)), scales = "free_y") +
      ggplot2::scale_linetype_manual("", values = c("Observed" = "solid", "Predicted (Mean and 95% CI)" = "dotted"))+

      #Fixed legend for ribbon, mean, and real cases
     #ggplot2::scale_color_discrete(name = NULL) +  # Keep colors mapped by strata
     #ggplot2::scale_fill_discrete(name = NULL) #+   # Keep fills mapped by strata

      # Manually control the legend
      ggplot2::guides(
        fill = "none",
        color = "none"
      ) +

      # Labels and theme
      ggplot2::labs(
        y = "Cases"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "top",
        legend.direction = "horizontal"
      )
}
