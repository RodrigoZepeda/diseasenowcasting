

#' Plot simulated process
#'
plot_simulation <- function(simulation, value = c("n","lambda")){

  simulation |>
    dplyr::mutate(across(c(".delay"), as.character)) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes_string(
      x = "onset_date", y = value[1], color = ".delay")) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(".strata", scales = "free") +
    ggplot2::scale_color_viridis_d("Delay") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))

}


sims <- simulate_process_for_testing(
  mu_0_param_1 = log(100),
  nu_0_param_1 = log(6),
  mu_0_param_2 = 0.01,
  nu_0_param_2 = 0.01,
  nu_param_2 = 0.01,
  mu_param_2 = 0.01,
  r_param_2 = 0.01,
  is_negative_binomial = T)
plot_simulation(sims, value = "lambda")
infer_now
out <- nowcast(sims, onset_date = "onset_date", report_date = "report_date", cores = 4)

