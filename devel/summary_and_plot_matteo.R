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
n <- nrow(denguedat_sel)  # Total number of rowsß
indices <- sample(1:n, size = ceiling(n / 4))  # Randomly select a third of the rows
# Replace the selected indices in the gender column with "other"
denguedat_sel$gender[indices] <- "Other"
set.seed(211)  # Set seed for reproducibility
n <- nrow(denguedat_sel)  # Total number of rowsß
indices <- sample(1:n, size = ceiling(n / 4))  # Randomly select a third of the rows
# Replace the selected indices in the gender column with "other"
denguedat_sel$gender[indices] <- "uncertain"



predictions_dengue<-
  nowcast(denguedat_sel, "onset_week", "report_week", method="variational",
          strata = "gender")


# Define child class that inherits from ParentClass
#setClass(
#  "nowcast_stanfit",
#  contains = "stanfit"
#)
#class(predictions_largelist) <- 'nowcast_stanfit'


### add at the top eventually, something makes it not work, find out why
#' @return A summary tibble of the nowcast results,
#' it specifies: onset time, Strata, Mean value, standard deviation, quantiles
#'
#'
#' # @return A a ggplot2 object that plots the evolution of the nowcast over the observed values,
#' it creates a subplot for each strata

summary_nowcast_dev <- function(nowcast_output) {
  # set column names for summary                   #### avoid renaming the unnecessary. a bunch of quantiles and stratas.
  strata_unified_col <- ".strata_unified"
  mean_col <- "mean"
  sd_col <- "sd"       ## need to go, must be handled by posterior::summarise_draw
  q05_col <- "q5"
  q95_col <- "q95"
  mean_col_new <- "Mean"
  sd_col_new <- "SD"
  q05_col_new <- "q05"
  q95_col_new <- "q95"
  # Get names from input data
  onset_date_name <- nowcast_output$data$call_parameters$onset_date
  # check if strata column in present
  # Get strata names if present
  if (is.null(nowcast_output$data$call_parameters$strata)) {
    strata_names <- "Strata"
  } else if (length(nowcast_output$data$call_parameters$strata)==1) {
    strata_names <- nowcast_output$data$call_parameters$strata
  } else {
    strata_names <- nowcast_output$data$call_parameters$strata
  }

  date_dic <- nowcast_output$data$preprocessed_data |>
    dplyr::select(!!as.symbol(".tval"), dplyr::all_of(onset_date_name)) |>
    dplyr::distinct()

  predictions_summary <- nowcast_output$generated_quantities |>
    posterior::as_draws() |>
    posterior::subset_draws("N_predict") |>
    posterior::summarise_draws("mean","sd",~quantile(.x, probs = c(0.4, 0.6))) |> #just need to parametrize this
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
      dplyr::all_of(onset_date_name), !!as.symbol(".strata_unified"),
      !!as.symbol("mean"), !!as.symbol("sd"), !!as.symbol("q5"), !!as.symbol("q95")
    ) |>
    # strata_name is a variable
    dplyr::rename_with(~strata_name, .cols = !!rlang::sym(strata_unified_col)) |>  ### this needs to go
    # Specify a string for each remaining column                            ### this needs to go
    dplyr::rename(
      !!rlang::sym(mean_col_new) := !!rlang::sym(mean_col),
      !!rlang::sym(sd_col_new) := !!rlang::sym(sd_col),
      !!rlang::sym(q05_col_new) := !!rlang::sym(q05_col),
      !!rlang::sym(q95_col_new) := !!rlang::sym(q95_col)
    )

  return(predictions_summary)
}


plot_nowcast_dev <- function(nowcast_output) {
  # Get names from input data
  onset_date_name <- nowcast_output$data$call_parameters$onset_date
  strata_name <- nowcast_output$data$call_parameters$strata


  # Create dictionaries for onset week and strata
  date_dic <- nowcast_output$data$preprocessed_data |>
    dplyr::select(!!as.symbol(".tval"), !!dplyr::sym(onset_date_name)) |>
    dplyr::distinct()
  stata_dic <- nowcast_output$data$strata_dict

  # Make summary
  prediction_summary_dev <- summary_nowcast_dev(nowcast_output)

  strata_name <- names(prediction_summary)[2]
  # fixed colnames from summary
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
    dplyr::rename_with(~strata_name, .cols = !!rlang::sym(strata_unified_col))


  # Create plot with facets
  plotnow <- ggplot2::ggplot(data_delays) +
    # Add prediction ribbon for 95% CI
    ggplot2::geom_ribbon(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), ymin = !!as.symbol("q05"), ymax = !!as.symbol("q95"), fill = !!dplyr::sym(strata_name)),
      data = prediction_summary, alpha = 0.3
    ) +
    # Add real cases line
    ggplot2::geom_line(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = !!as.symbol("n"), color = !!dplyr::sym(strata_name), linetype = "Observed")
    ) +
    # Add predicted mean line
    ggplot2::geom_line(
      ggplot2::aes(
        x = !!dplyr::sym(onset_date_name), y = !!as.symbol("Mean"), color = !!dplyr::sym(strata_name),
        linetype = "Predicted (Mean and 95% CI)"
      ),
      data = prediction_summary
    ) +
    # Facet by strata
    ggplot2::facet_wrap(stats::as.formula(paste("~", strata_name)), scales = "free_y") +
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







###########PLOT

