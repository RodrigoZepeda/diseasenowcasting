#' Makes a summary of your nowcast
#'
#' Makes a tidy summary dataframe of the results of the function diseasenowcasting::nowcast()
#'
#' @param nowcast_output the output of the diseasenowcasting::nowcast() function
#' @param quantiles a vector to specify the quantiles to display.
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

summary_nowcast <- function(nowcast_output,
                            quantiles=c(0.025, 0.975)) {

  if (!is.numeric(quantiles) || any(quantiles < 0 | quantiles > 1)) {
    stop("quantiles must be a number or a numeric vector with values between 0 and 1.")
  }

  # Get names from input data
  onset_date_name <- nowcast_output$data$call_parameters$onset_date

  date_dic <- nowcast_output$data$preprocessed_data |>
    dplyr::select(!!as.symbol(".tval"), dplyr::all_of(onset_date_name)) |>
    dplyr::distinct()

  predictions_summary <- nowcast_output$generated_quantities |>
    posterior::as_draws() |>
    posterior::subset_draws("N_predict") |>
    posterior::summarise_draws("mean","sd",~quantile(.x, probs = quantiles)) |>
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













#' Plot your nowcast
#'
#' Create barplots to show real cases and the predictions of the function diseasenowcasting::nowcast()
#'
#' @param nowcast_output The output of the diseasenowcasting::nowcast() function
#'
#' @param color A string indicating the color for the barplots,
#' Works with the default R colors of grDevices::colors(). Hex color codes works as well. Best results with dark colors
#'
#' @param datesbrakes A string giving the distance between x-axis breaks
#' if `NULL`, one label per bar
#' Other valid examples are:
#' "2 weeks", or "10 years", 'sec', 'min', 'hour', 'day', 'week', 'month', 'year',
#' optionally followed by 's'.
#'
#' @param casesbrakes Desired number of y-axis breaks. You may get slightly more or fewer breaks that requested.
#'
#' @param rowsfacet Number of rows for arranging facets when using ggplot2::facet_wrap().
#' It allows manual control over the layout of multiple strata in the plot.
#' If `NULL`, the number of rows is automatically determined by ggplot2.
#'
#' @param colsfacet Number of columns for arranging facets when using ggplot2::facet_wrap().
#' It allows manual control over the layout of multiple strata in the plot.
#' If `NULL`, the number of columns is automatically determined by ggplot2.
#'
#' @param quantiles a vector of two values to specify the quantiles for the error bars.
#'
#' @return ggplot2 barplots to show real cases and the predictions of the function diseasenowcasting::nowcast().
#' It creates a subplot for each strata
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
#' predictions <- nowcast(denguedat, "onset_week", "report_week",strata = "gender",
#'                       now = now, method = "optimization",
#'                       seed = 2495624, iter = 10
#'                       )
#'
#'
#' # plot results
#' plot_nowcast(predictions)
#'
#' # other plotting options
#' plot_nowcast(predictions, rowsfacet = 2,
#'              datesbrakes = "2 weeks", casesbrakes = 15,
#'              color = "firebrick2")
#'
#' @importFrom grDevices col2rgb colors
#' @export
plot_nowcast <- function(nowcast_output,
                         color = "deepskyblue4",
                         datesbrakes = NULL,
                         casesbrakes = 10,
                         rowsfacet = NULL,
                         colsfacet = NULL,
                         quantiles=c(0.025, 0.975)
                         ) {
  # Check if the color is in the built-in colors or is a valid hex code
  if (!(color %in% colors() || grepl("^#([0-9A-Fa-f]{3}){1,2}$", color))) {
    # If it's not valid, try to convert to RGB and catch errors
    if (tryCatch({
      col2rgb(color)
      TRUE  # If successful, return TRUE
    }, error = function(e) {
      FALSE  # If an error occurs, return FALSE
    })) {
      # color is valid as RGB
    } else {
      stop("color is not a valid color string in R.")
    }
  }

  # Check if datesbrakes is either NULL or a valid specification
  if (!is.null(datesbrakes) &&
      !grepl("^\\d+\\s*(day|week|month|year|days|weeks|months|years)$|^(day|week|month|year|days|weeks|months|years)$", datesbrakes)) {
    stop('datesbrakes is not a valid string for date breaks.
         Examples are: "2 weeks", "10 days", "day", "months", "year"...')
  }

  # Check if casesbrakes is a positive integer
  if (!is.null(casesbrakes) && (!is.numeric(casesbrakes) || casesbrakes <= 0 || floor(casesbrakes) != casesbrakes)) {
    stop("casesbrakes must be a positive integer.")
  }

  # Check if rowfacet is NULL or a positive integer
  if (!is.null(rowsfacet) && (!is.numeric(rowsfacet) || rowsfacet <= 0 || floor(rowsfacet) != rowsfacet)) {
    stop("rowsfacet must be NULL or a positive integer.")
  }

  # Check if colsfacet is NULL or a positive integer
  if (!is.null(colsfacet) && (!is.numeric(colsfacet) || colsfacet <= 0 || floor(colsfacet) != colsfacet)) {
    stop("colsfacet must be NULL or a positive integer.")
  }

  # Check if quantiles is valid, and only specify two numbers
  if (!is.numeric(quantiles) || any(quantiles < 0 | quantiles > 1)) {
    stop("quantiles must be a number or a numeric vector with values between 0 and 1.")
  }
  if (length(quantiles)!=2) {
    stop("must specify 2 quantiles for error bars")
  }

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
  prediction_summary <- summary_nowcast(nowcast_output, quantiles = quantiles)
  #get quantile labels for legend
  quantile_labels <- paste0(
    colnames(prediction_summary)[4], " - ", colnames(prediction_summary)[5], " interval"
  )
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

  plotnow <- ggplot2::ggplot(data_delays) +
    # Add real cases as solid bars (observed)
    ggplot2::geom_bar(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = !!as.symbol("n"), fill = "Reported"),
      stat = "identity", position = "dodge", color = NA, alpha = 0.8
    ) +

    # Add predicted median as transparent bars (predicted)
    ggplot2::geom_bar(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = !!as.symbol("mean"), fill = "Estimated, not yet reported"),
      stat = "identity", position = "dodge", alpha = 0.45, color = NA, data = prediction_summary
    ) +

    # Add error bars for 95% CI
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = !!dplyr::sym(onset_date_name),
        ymin = !!dplyr::pull(prediction_summary, 4),
        ymax = !!dplyr::pull(prediction_summary, 5),
        group = !!as.symbol("Strata_unified")
      ),
      width = 0,
      color = "gray40",
      data = prediction_summary
    ) +

    # Facet by strata
    ggplot2::facet_wrap(stats::as.formula(paste0("~", Strata_unified)), nrow = rowsfacet, ncol=colsfacet)

  if (!is.null(datesbrakes)) {
    plotnow <- plotnow + ggplot2::scale_x_date(
      date_labels = "%Y-%b-%d",  # Date format
      minor_breaks = NULL,  # Remove minor breaks
      date_breaks = datesbrakes  # datesbrakes specified by user
    )
  } else {
    plotnow <- plotnow + ggplot2::scale_x_date(
      date_labels = "%Y-%b-%d",  # Date format
      minor_breaks = NULL,
      breaks = sort(unique(dplyr::pull(prediction_summary, 1)))  # Ensure each bar has a corresponding date label
    )
  }

  plotnow <- plotnow +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = casesbrakes),  # Set the number of breaks
      labels = scales::label_number(accuracy = 1),  # Ensure integer labels
      sec.axis = ggplot2::dup_axis()  # Duplicate axis on the right
    ) +

    # Custom legend for fill colors
    ggplot2::scale_fill_manual(
      name = NULL,  # Remove legend title
      values = c("Reported" = color, "Estimated, not yet reported" = color),
      labels = c("Reported", paste("Predicted, not yet reported\nError bars: ", quantile_labels, sep = ""))
    ) +

    # Manually control the legend to show different alpha levels
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(
          alpha = c(0.8, 0.45),  # Full opacity for "Reported", transparency for "Estimated"
          color = NA  # Make sure the legend matches the bar outlines
        )
      )
    ) +

    # Labels and theme
    ggplot2::labs(
      y = "Cases"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "top",
      legend.justification='right',
      legend.direction = "horizontal"
    )

  # If all data have the same strata, do not plot the facet title
  if (length(unique(data_delays$.strata)) == 1) {
    plotnow <- plotnow + ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  return(plotnow)
}
