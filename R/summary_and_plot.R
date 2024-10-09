#' Makes a summary of nowcast() output
#'
#' Makes a tidy summary dataframe of the results of the function nowcast()
#'
#' @param nowcast_output the output of the nowcast() function
#'
#' @return A summary tibble of the nowcast results,
#' it specifies: onset time, Strata, Mean value, standard deviation, quantiles
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
#' predictions <- nowcast(denguedat, "onset_week", "report_week", now = now, method = "optimization",
#' seed = 2495624, iter = 10)
#'
#' # create summary dataframe
#' summary_nowcast(predictions)
#'
#'
#' @export
summary_nowcast <- function(nowcast_output) {
  #set column names for summary
  strata_unified_col <- ".strata_unified"
  mean_col <- "mean"
  sd_col <- "sd"
  q05_col <- "q5"
  q95_col <- "q95"
  mean_col_new <- "Mean"
  sd_col_new <- "SD"
  q05_col_new <- "q05"
  q95_col_new <- "q95"
  #Get names from input data
  onset_date_name <- nowcast_output$data$call_parameters$onset_date
  #check if strata column in present
  strata_name <- strata_name <- if (!is.null(nowcast_output$data$call_parameters$strata)) nowcast_output$data$call_parameters$strata else "Strata"
  date_dic <- nowcast_output$data$preprocessed_data |>
    dplyr::select(!!as.symbol(".tval"), dplyr::all_of(onset_date_name)) |>
    dplyr::distinct()

  predictions_summary <- nowcast_output$generated_quantities |>
    posterior::as_draws() |>
    posterior::subset_draws("N_predict") |>
    posterior::summarise_draws() |>
    # Extract strata and time values
    dplyr::mutate(.strata = as.numeric(stringr::str_remove_all(!!as.symbol("variable"), ".*\\[.*,|\\]")),
                  .tval = as.numeric(stringr::str_remove_all(!!as.symbol("variable"), ".*\\[|,.*\\]"))) |>
    # assign the input strata names
    dplyr::left_join(nowcast_output$data$strata_dict, by = ".strata") |>
    # assign the onset dates
    dplyr::left_join(date_dic, by = ".tval") |>
    # Select and reorder the columns
    dplyr::select(dplyr::all_of(onset_date_name), !!as.symbol(".strata_unified"),
                  !!as.symbol("mean"), !!as.symbol("sd"), !!as.symbol("q5"), !!as.symbol("q95")) |>
    # strata_name is a variable
    dplyr::rename_with(~ strata_name, .cols = !!rlang::sym(strata_unified_col)) |>
    # Specify a string for each remaining column
    dplyr::rename(
      !!rlang::sym(mean_col_new) := !!rlang::sym(mean_col),
      !!rlang::sym(sd_col_new) := !!rlang::sym(sd_col),
      !!rlang::sym(q05_col_new) := !!rlang::sym(q05_col),
      !!rlang::sym(q95_col_new) := !!rlang::sym(q95_col)
    )

  return(predictions_summary)
}






#' Create plots form the nowcast() output
#'
#' Plots the predictions of the function nowcast() over the input observed values
#'
#' @param nowcast_output the output of the nowcast() function
#'
#' @return A a ggplot2 object that plots the evolution of the nowcast over the observed values,
#' it creates a subplot for each strata
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
#' predictions <- nowcast(denguedat, "onset_week", "report_week", now = now, method = "optimization",
#' seed = 2495624, iter = 10)
#'
#' # create summary dataframe
#' plot_nowcast(predictions)
#'
#'
#' @export
plot_nowcast <- function(nowcast_output) {
  # Get names from input data
  onset_date_name <- nowcast_output$data$call_parameters$onset_date
  strata_name <- nowcast_output$data$call_parameters$strata


  # Create dictionaries for onset week and strata
  date_dic <- nowcast_output$data$preprocessed_data |>
    dplyr::select(!!as.symbol(".tval"), !!dplyr::sym(onset_date_name)) |>
    dplyr::distinct()
  stata_dic <- nowcast_output$data$strata_dict

  # Make summary
  prediction_summary <- summary_nowcast(nowcast_output)

  strata_name <- names(prediction_summary)[2]
  #fixed colnames from summary
  strata_unified_col <- ".strata_unified"
  mean_col_new <- names(prediction_summary)[3]
  sd_col_new <- names(prediction_summary)[4]
  q05_col_new <- names(prediction_summary)[5]
  q95_col_new <- names(prediction_summary)[6]

  # Get input data and sum over all delays
  data_delays <- nowcast_output$data$preprocessed_data |>
    dplyr::group_by(!!as.symbol(".tval"), !!as.symbol(".strata")) |>
    dplyr::summarise(n = sum(!!as.symbol("n")), .groups = "drop") |>
    # Assign the onset dates
    dplyr::left_join(date_dic, by = ".tval") |>
    # Assign the strata names
    dplyr::left_join(stata_dic, by = ".strata") |>
    # Rename the strata column
    dplyr::rename_with(~ strata_name, .cols = !!rlang::sym(strata_unified_col))


  # Create plot with facets
  plotnow <- ggplot2::ggplot(data_delays) +
    # Add prediction ribbon for 95% CI
    ggplot2::geom_ribbon(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), ymin = !!as.symbol("q05"), ymax = !!as.symbol("q95"), fill =  !!dplyr::sym(strata_name)),
      data = prediction_summary, alpha = 0.3
    ) +
    # Add real cases line
    ggplot2::geom_line(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = !!as.symbol("n"), color = !!dplyr::sym(strata_name), linetype="Observed")
    ) +
    # Add predicted mean line
    ggplot2::geom_line(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = !!as.symbol("Mean"), color = !!dplyr::sym(strata_name),
                   linetype = "Predicted (Mean and 95% CI)"),
      data = prediction_summary
    ) +
    # Facet by strata
    ggplot2::facet_wrap(stats::as.formula(paste("~", strata_name)), scales = "free_y") +
    ggplot2::scale_linetype_manual("", values = c("Observed" = "solid", "Predicted (Mean and 95% CI)" = "dotted"))+

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

    # if all data as the same strata, it does not plot the title
    if (length(unique(data_delays$.strata)) == 1) {
      plotnow <- plotnow + ggplot2::theme(strip.text = ggplot2::element_blank())
    }
  return(plotnow)
}

