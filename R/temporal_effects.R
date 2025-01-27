#' Temporal effects
#'
#' Matrix for constructing temporal effects into the [nowcast()].
#'
#' @param day_of_week Boolean. Whether to include an effect for each of the seven days of the week.
#'
#' @param weekend Boolean. Whether to include an effect for the weekend vs the weekday.
#'
#' @param day_of_month Boolean. Whether to include an effect for the day of the month (i.e. 1 to 31st)
#'
#' @param month_of_year Boolean. Whether to include an effect for the month.
#'
#' @param week_of_year Boolean. Whether to include an effect for the epidemiological week.
#'
#' @param holidays Either `NULL` or an [almanac::rcalendar()] from the [almanac()] package
#' specifying how to calculate holidays.
#'
#' @section US holidays:
#'
#' US Federal holidays can be passed by just passing the [almanac::cal_us_federal()] function
#' into temporal effects.
#'
#' ```
#' library(almanac)
#' temporal_effects(holidays = cal_us_federal())
#' ```
#'
#' @examples
#' #Create day of week and week of year class
#' temporal_effects(day_of_week = TRUE, week_of_year = TRUE)
#'
#' #Set up a holiday effect on Christmass using the almanac package
#' if(!requireNamespace("almanac", quietly = T)){
#'   cal <- rcalendar(hol_christmas())
#'   temporal_effects(holidays = cal)
#' }
#' @export
temporal_effects <- function(day_of_week = FALSE, weekend = FALSE, day_of_month = FALSE,
                             month_of_year = FALSE, week_of_year = FALSE, holidays = NULL){


  if (!is.null(holidays) & !inherits(holidays, "almanac_rcalendar")){
    cli::cli_abort(
      "Invalid holidays. Please set up an {.code almanac::rcalendar} for the holidays using the {.code almanac} package."
    )
  }

  #Get the priors
  t_effects <- list(
    day_of_week = day_of_week,
    weekend = weekend,
    day_of_month = day_of_month,
    month_of_year = month_of_year,
    week_of_year = week_of_year,
    holidays = holidays
  )

  class(t_effects) <- "temporal_effect"

  return(t_effects)


}

#' Print temporal effects
#'
#' Print function extension for a `temporal_effect` object created with [temporal_effects()].
#' Prints all of the temporal characteristics for a model
#'
#' @param temporal_effect A `temporal_effect` object created with [temporal_effects()]
#'
#' @examples
#' print(temporal_effects(day_of_week = TRUE, week_of_year = TRUE))
#' print(temporal_effects(day_of_week = FALSE, week_of_year = FALSE))
#'
#' @export
print.temporal_effect <- function(temporal_effect){

  #Get the length of the temporal effects:
  effects_considered <- which(unlist(temporal_effect[names(temporal_effect) != "holidays"]))
  is_holiday         <- !is.null(temporal_effect[["holidays"]])

  cli::cli_h2("Temporal effect object")
  if (length(effects_considered) + is_holiday > 0){
    cli::cli_par()
    cli::cli_text("The following effects are in place:")
    cli::cli_ul()
    for (eff in effects_considered){
      cli::cli_li("{.emph {names(temporal_effect[eff])}} ")
    }
    if (is_holiday){
      cli::cli_li("{.emph holidays}")
    }
    cli::cli_end()
  } else {
    cli::cli_par()
    cli::cli_text("No temporal effects are considered")
  }
}
