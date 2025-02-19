#' Compare Model scores
#'
#' Create a plot to compare the metrics calculated with diseasenowcasting::backtest_metrics()
#'
#' @param x The output of the [backtest_metrics()] function
#'
#'
#' @inheritParams backtest_metrics
#'
#' @examples
#' # Load the data
#' data(denguedat)
#'
#'
#' @importFrom grDevices col2rgb colors
#' @export
plot.backtest_metrics <- function(x,  metric =  c("mae", "rmse", "wis"), horizons = 0,
                                  datesbrakes = NULL, ...){

  if (!requireNamespace("ggplot2", quietly = TRUE)){
    cli::cli_alert_warning(
      "To produce plots please install the `ggplot2` package."
    )
    return(invisible())
  }

  if (!requireNamespace("ggrepel", quietly = TRUE)){
    cli::cli_alert_warning(
      "To produce plots please install the `ggrepel` package."
    )
    return(invisible())
  }


  # Check if datesbrakes is either NULL or a valid specification
  if (!is.null(datesbrakes) &&
      !grepl("^\\d+\\s*(day|week|month|year|days|weeks|months|years)$|^(day|week|month|year|days|weeks|months|years)$", datesbrakes)) {
    stop('datesbrakes is not a valid string for date breaks.
         Examples are: "2 weeks", "10 days", "day", "months", "year"...')
  }
  print(horizons)
  # Check if horizons provided exists in x
  if (any(!horizons %in% unique(x$horizon))) {
    stop('At least one of the selcted horizons was not evaluated in the backtest_metrics object provided')
  }

  # Check if only one available metric was given
  if (length(metric) != 1){
    stop('Specify a single metric. This function plots only one metric at a time')
  }
  # Check the metric is available
  available_metrics <- colnames(x)[-c(1:4)]
  if (any(!metric %in% available_metrics)){
    stop('The metric you selected does not exists or was not evaluated in the backtest_metrics object provided')
  }


  # Ensure 'now' is of Date type
  x$now <- as.Date(x$now)


  # Convert data to long format for ggplot2
  long_mtr <- x |>
    tidyr::pivot_longer(
      cols = !!rlang::sym("metric"),
      names_to = "metric",
      values_to = "value"
    )

  # Calculate averages for each metric and model
  metric_averages <- long_mtr |>
    dplyr::group_by(
      !!rlang::sym("horizon"),
      !!rlang::sym("Strata_unified"),
      !!rlang::sym("metric"),
      !!rlang::sym("model")
    ) |>
    dplyr::summarize(
      avg_value = mean(!!rlang::sym("value"), na.rm = TRUE),
      .groups = "drop"
    )

  ## Add to the vignettes > articles > diseasenowcasting.Rmd > 3. Evaluating the model
  plotbktest <- ggplot2::ggplot(long_mtr, ggplot2::aes(x = !!rlang::sym("now"), y = !!rlang::sym("value"), color = !!rlang::sym("model"))) +
    ggplot2::geom_jitter(width = 5, alpha = 0.6) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(!!rlang::sym("Strata_unified")),
      cols = ggplot2::vars(!!rlang::sym("horizon")),
      scales = "free_y",
      labeller = ggplot2::labeller(horizon = function(h) paste("Horizon =", h))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = NULL,
      y = metric,
      color = NULL
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      axis.title.y = ggplot2::element_text(size = 15),
      legend.position = "bottom",      # Place the legend at the bottom
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_x_date(
      date_labels = "%Y-%b-%d",         # Date format
      minor_breaks = NULL,
      breaks = sort(unique(dplyr::pull(x, !!rlang::sym("now"))))
    ) +
    ggplot2::geom_hline(
      data = metric_averages,
      ggplot2::aes(
        yintercept = !!rlang::sym("avg_value"),
        color = !!rlang::sym("model")
      ),
      linetype = "dashed"
    ) +
    ggrepel::geom_text_repel(
      data = metric_averages,
      ggplot2::aes(
        x = as.Date(Inf),
        y = !!rlang::sym("avg_value"),
        label = paste("avg.", round(!!rlang::sym("avg_value"), 2)),
        color = !!rlang::sym("model")
      ),
      hjust = 1, size = 3, inherit.aes = FALSE, show.legend = FALSE
    )
  return(plotbktest)
}

