#' Infer `max_time` for custom epidemic processes
#'
#' Infers `max_time` from a model and a `tbl_now`. `max_time` is the number of
#' event-time points the model spans: the epidemic process runs from `t = 0` to
#' `t = max_time - 1`.
#'
#' @details
#' This function is useful for building custom epidemic processes whose
#' [intensity function][custom_epidemic] loops over time and therefore needs to
#' know the number of event-times *before* it is written (e.g. an SIR recursion
#' or a random walk with one innovation per time point).
#'
#' @param data A `tbl_now` object.
#' @param model A [model()] object (default `model()`).  Only its dimensions
#'   matter here, so any model works as a placeholder.
#' @return The number of event-time points in the model (an integer).
#'
#' @examples
#' library(tbl.now)
#' library(dplyr)
#' mpox_pooled <- mpoxdat |>
#'   ungroup() |>
#'   summarise(n = sum(n), .by = c(dx_date, dx_report_date))
#' mpox_tn <- tbl_now(mpox_pooled,
#'                    event_date  = dx_date,
#'                    report_date = dx_report_date,
#'                    case_count  = n,
#'                    data_type   = "count-incidence",
#'                    verbose     = FALSE)
#'
#' # The result is 316: the model spans from time t = 0 (min event_num)
#' # to time t = 315 (max report_num).
#' infer_max_time(mpox_tn)
#'
#' @export
#' @md
infer_max_time <- function(data, model = diseasenowcasting::model()){
  prepare_from_tbl_now(data, model)$max_time
}
