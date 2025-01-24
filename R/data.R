#' denguedat: Dengue fever individual-level reporting data from Puerto Rico
#'
#' Surveillance data from CDC Division of Vector-Borne Diseases.
#' 1990-2010 case reporting data included.
#'
#' @details
#' Each row represents a case with the columns indicating the following:
#' * `onset_week`:  the week of symptom onset.
#' * `report_week`: the week of case report.
#' * `gender`: the gender of the infected individual (randomly assigned with 0.5:0.5 probability
#' of "Male"/"Female").
#'
#' @note
#' Data originally from the `NobBS` package. While `onset_week` and `report_week` correspond to
#' actual observed data the `gender` was constructed exclusively for the examples of `NobBS`. Its
#' a synthetic (simulated) variable and does not correspond to any reality.
#'
#' @docType data
#'
#' @usage data(denguedat)
#'
#' @format A data frame.
#'
#' @keywords dengue
#'
#' @references
#' MCGOUGH, Sarah F., et al. Nowcasting by Bayesian Smoothing: A flexible, generalizable model for
#' real-time epidemic tracking. PLoS computational biology, 2020, vol. 16, no 4, p. e1007735.
#'
#' @examples
#' data(denguedat)
#' @md
"denguedat"

#' mpoxdat: M-pox line-list reporting data for New York City
#'
#' Surveillance data from from the New York City Health Department’s mpox surveillance database.

#' @details
#' This is line-list data with each row representing case counts. The columns are as follows:
#' * `dx_date`: first positive mpox laboratory result
#' * `dx_report_date`: the first positive mpox laboratory result was received by the NYC Health Department
#' * `n`: the case count of individuals within those dates.
#' * `race`: the race corresponding to those cases. Race was randomly assigned with probabilities
#' "Non-Hispanic White" = 0.309, "Hispanic" = 0.283, "Black" = 0.202, "Asian" = 0.156, and "Other" = 0.05
#' which follow what has been reported for the US Census.
#'
#' @note
#' While `dx_date`, `dx_report_date` and `n` correspond to actual observed data the `race` was
#' constructed exclusively for the examples of this package. Its a synthetic (simulated) variable
#' and does not correspond to any reality.
#'
#' @references
#' ROHRER, Rebecca, et al. Nowcasting to Monitor Real-Time Mpox Trends During the 2022
#' Outbreak in New York City: Evaluation Using Reportable Disease Data Stratified by Race
#' or Ethnicity. Online Journal of Public Health Informatics, 2025, vol. 17, no 1, p. e56495.
#'
#' @docType data
#'
#' @usage data(mpoxdat)
#'
#' @format A data frame.
#'
#' @keywords mpox
#'
#' @examples
#' data(mpoxdat)
#' @md
"mpoxdat"
