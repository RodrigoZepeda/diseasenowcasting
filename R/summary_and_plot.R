#' Makes a summary of nowcast() output
#'
#' Makes a tidy summary dataframe of the results of the function nowcast()
#'
#' @param nowcast_output the output of the nowcast() function
#' @param quantiles a vector of to specify the quantiles to display.
#' Default is c(0.05, 0.95).
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
#' predictions <- nowcast(denguedat, "onset_week", "report_week",
#'   now = now, method = "optimization",
#'   seed = 2495624, iter = 10
#' )
#'
#' # create summary dataframe
#' summary_nowcast(predictions)
#'
#' @export

summary_nowcast <- function(nowcast_output, quantiles=NULL) {

  quants <- if (is.null(quantiles)) c(0.05, 0.95) else quantiles
  # Check if quants is a numeric vector with values between 0 and 1
  if (!is.numeric(quants) || any(quants < 0 | quants > 1)) {
    stop("quants must be a numeric vector with values between 0 and 1.")
  }

  # Get names from input data
  onset_date_name <- nowcast_output$data$call_parameters$onset_date

  date_dic <- nowcast_output$data$preprocessed_data |>
    dplyr::select(!!as.symbol(".tval"), dplyr::all_of(onset_date_name)) |>
    dplyr::distinct()

  predictions_summary <- nowcast_output$generated_quantities |>
    posterior::as_draws() |>
    posterior::subset_draws("N_predict") |>
    posterior::summarise_draws("mean","sd",~quantile(.x, probs = quants)) |>
    # Extract strata and time values
    dplyr::mutate(
      .strata = as.numeric(stringr::str_remove_all(!!as.symbol("variable"), ".*\\[.*,|\\]")),
      .tval = as.numeric(stringr::str_remove_all(!!as.symbol("variable"), ".*\\[|,.*\\]"))
    ) |>
    # assign the input strata names
    dplyr::left_join(nowcast_output$data$strata_dict, by = ".strata") |>
    # assign the onset dates
    dplyr::left_join(date_dic, by = ".tval") |>
    # Select and reorder the columns
    dplyr::select(
      !!rlang::sym(onset_date_name), dplyr::everything(), -!!as.symbol("variable"),  -!!as.symbol(".tval"), -!!as.symbol(".strata"),
    ) |>
    dplyr::rename(!!as.symbol("Strata_unified") := !!as.symbol(".strata_unified"))

  return(predictions_summary)
}






#' Create plots form the nowcast() output
#'
#' Plots the predictions of the function nowcast() over the input observed values
#'
#' @param nowcast_output the output of the nowcast() function
#'
#' # @return A a ggplot2 object that plots the evolution of the nowcast over the observed values,
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
#' predictions <- nowcast(denguedat, "onset_week", "report_week",
#'   now = now, method = "optimization",
#'   seed = 2495624, iter = 10
#' )
#'
#' # create summary dataframe
#' plot_nowcast(predictions)
#'
#' @export
plot_nowcast <- function(nowcast_output) {

  # Get names from input data
  onset_date_name <- nowcast_output$data$call_parameters$onset_date
  Strata_unified = "Strata_unified"
  dot_strata_unified = ".strata_unified"

  # Create dictionaries for onset week and strata
  date_dic <- nowcast_output$data$preprocessed_data |>
    dplyr::select(!!as.symbol(".tval"), !!dplyr::sym(onset_date_name)) |>
    dplyr::distinct()
  stata_dic <- nowcast_output$data$strata_dict

  # Make summary
  prediction_summary <- summary_nowcast(nowcast_output)

  # Get input data and sum over all delays
  data_delays <- nowcast_output$data$preprocessed_data |>
    dplyr::group_by(!!as.symbol(".tval"), !!as.symbol(".strata")) |>
    dplyr::summarise(n = sum(!!as.symbol("n")), .groups = "drop") |>
    # Assign the onset dates
    dplyr::left_join(date_dic, by = ".tval") |>
    # Assign the strata names
    dplyr::left_join(stata_dic, by = ".strata") |>
    # Rename the strata column
    dplyr::rename_with(~Strata_unified, .cols = !!rlang::sym(dot_strata_unified))

  # Create plot with facets
  plotnow <- ggplot2::ggplot(data_delays) +
    # Add prediction ribbon for 95% CI
    ggplot2::geom_ribbon(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), ymin = !!as.symbol("5%"), ymax = !!as.symbol("95%"), fill = !!as.symbol("Strata_unified")),
      data = prediction_summary, alpha = 0.3
    ) +
    # Add real cases line
    ggplot2::geom_line(
      ggplot2::aes(
        x = !!dplyr::sym(onset_date_name), y = !!as.symbol("n"), color = !!as.symbol("Strata_unified"),
        linetype = "Observed")
    ) +
    # Add predicted mean line
    ggplot2::geom_line(
      ggplot2::aes(
        x = !!dplyr::sym(onset_date_name), y = !!as.symbol("mean"), color = !!as.symbol("Strata_unified"),
        linetype = "Predicted (Mean and 95% CI)"
      ),data = prediction_summary
    ) +
    # Facet by strata

    ggplot2::facet_wrap(stats::as.formula(paste0("~", Strata_unified)), scales = "free_y") +
    ggplot2::scale_linetype_manual("", values = c("Observed" = "solid", "Predicted (Mean and 95% CI)" = "dotted")) +

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
