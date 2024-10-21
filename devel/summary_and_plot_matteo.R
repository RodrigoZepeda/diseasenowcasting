
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

dis <- simulate_disease(num_steps = 35, num_delays = 5, num_strata = 2) |>
  dplyr::rename(strata_1 = .strata) |>
  tidyr::uncount(n) |>
  dplyr::rowwise() |>
  dplyr::mutate(strata_2 = sample(c("M","F"), 1, replace = T)) |>
  dplyr::ungroup()





plot_nowcast_bar3 <- function(nowcast_output) {

  maincolor = "deepskyblue4"
  maincolor = "red"
  rowfacet = NULL
  colfacet = NULL
  datebrakes = NULL
  casesbrakes = 10

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

  plotnow <- ggplot2::ggplot(data_delays) +
    # Add real cases as solid bars (observed)
    ggplot2::geom_bar(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = !!as.symbol("n"), fill = "Reported"),
      stat = "identity", position = "dodge", color = "white", alpha = 0.8
    ) +

    # Add predicted median as transparent bars (predicted)
    ggplot2::geom_bar(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = !!as.symbol("mean"), fill = "Estimated, not yet reported"),
      stat = "identity", position = "dodge", alpha = 0.45, color = "white", data = prediction_summary
    ) +

    # Add error bars for 95% CI
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = !!dplyr::sym(onset_date_name),
        ymin = !!as.symbol("5%"),
        ymax = !!as.symbol("95%"),
        group = !!as.symbol("Strata_unified")
      ),
      width = 0,
      color = "gray40",
      data = prediction_summary
    ) +

    # Facet by strata
    ggplot2::facet_wrap(stats::as.formula(paste0("~", Strata_unified)), nrow = rowfacet, ncol=colfacet)

    if (!is.null(datebrakes)) {
      plotnow <- plotnow + ggplot2::scale_x_date(
        date_labels = "%Y-%b-%d",  # Date format
        minor_breaks = NULL,  # Remove minor breaks
        date_breaks = datebrakes  # datebrakes specified by user
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
      breaks = scales::pretty_breaks(n = casesbrakes),  # Set the number of breaks
      labels = scales::label_number(accuracy = 1),  # Ensure integer labels
      sec.axis = ggplot2::dup_axis()  # Duplicate axis on the right
    ) +

    # Custom legend for fill colors
    ggplot2::scale_fill_manual(
      name = NULL,  # Remove legend title
      values = c("Reported" = maincolor, "Estimated, not yet reported" = maincolor),
      labels = c("Reported", "Estimated, not yet reported")
    ) +

    # Manually control the legend to show different alpha levels
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(
          alpha = c(0.8, 0.45),  # Full opacity for "Reported", transparency for "Estimated"
          color = "white"  # Make sure the legend matches the bar outlines
        )
      )
    ) +

    # Labels and theme
    ggplot2::labs(
      y = "Cases"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90),
      legend.position = "top",
      legend.direction = "horizontal"
    )

  # If all data have the same strata, do not plot the facet title
  if (length(unique(data_delays$.strata)) == 1) {
    plotnow <- plotnow + ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  return(plotnow)
}



















###########.  OLD.  ###########


# Define child class that inherits from ParentClass
#setClass(
#  "nowcast_stanfit",
#  contains = "stanfit"
#)
#class(predictions_largelist) <- 'nowcast_stanfit'





plot_nowcast_bar <- function(nowcast_output) {

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

  # Convert dates to numeric to use in scale_x_continuous
  data_delays$onset_week_numeric <- as.numeric(as.Date(data_delays[[onset_date_name]]))
  prediction_summary$onset_week_numeric <- as.numeric(as.Date(dplyr::pull(prediction_summary, 1)))



  plotnow <- ggplot2::ggplot(data_delays) +
    # Add real cases as solid bars (observed)
    ggplot2::geom_bar(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = !!as.symbol("n"), fill = !!as.symbol("Strata_unified")),
      stat = "identity", position = "dodge", color = "white", alpha = 0.8
    ) +

    # Add predicted median as transparent bars (predicted)
    ggplot2::geom_bar(
      ggplot2::aes(x = !!dplyr::sym(onset_date_name), y = !!as.symbol("mean"), fill = !!as.symbol("Strata_unified")),
      stat = "identity", position = "dodge", alpha = 0.35, color = "white", data = prediction_summary
    ) +

    # Add error bars for 95% CI
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = !!dplyr::sym(onset_date_name),
        ymin = !!as.symbol("5%"),
        ymax = !!as.symbol("95%"),
        group = !!as.symbol("Strata_unified")
      ),
      width = 3,
      color = "gray40",
      data = prediction_summary
    ) +

    # Facet by strata
    ggplot2::facet_wrap(stats::as.formula(paste0("~", Strata_unified))) +

    # Manually control the legend
    ggplot2::guides(
      fill = "none",
      color = "none"
    ) +

    # Labels and theme
    ggplot2::labs(
      y = "Cases",
      x = onset_date_name
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 60)
    ) +

    #ggplot2::scale_x_date(
    #  date_labels = "%Y-%b-%d",
    #  minor_breaks = NULL,
      #date_breaks = "week",
      #date_breaks = "day",
    #) +

    # Ensure one label per bar
    ggplot2::scale_x_continuous(
      breaks = sort(unique(data_delays$onset_week_numeric)),  # Set breaks for each unique date
      labels = format(sort(unique(data_delays[[onset_date_name]])), "%Y-%b-%d")  # Format date labels
    )

    # Adjust y-axis ticks
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(),
                       n.breaks = 15,
                       minor_breaks = NULL
                       )

  # If all data have the same strata, do not plot the facet title
  if (length(unique(data_delays$.strata)) == 1) {
    plotnow <- plotnow + ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  return(plotnow)
}
