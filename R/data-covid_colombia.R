#' COVID-19 Notifications -- Colombia 2020-2023
#'
#' Daily case counts of COVID-19 from Colombia's national epidemiological
#' surveillance system (INS), aggregated by notification date, diagnosis date,
#' and sex.  Each row is a unique combination of these three variables together
#' with the number of cases `n`.
#'
#' In the nowcasting context the **event date** is `notification_date` (when the
#' case symptom onset was recorded in the system) and the **report date** is
#' `diagnosis_date` (when the laboratory result was entered).  The delay between
#' the two reflects the time from symptom onset to laboratory confirmation and
#' data entry -- the quantity the nowcasting model estimates and corrects for.
#'
#' The dataset covers the full first three years of the Colombian epidemic
#' (2020-03-02 to 2023-03-03) and was used in the NobBS comparison study to
#' benchmark the dcast3 / diseasenowcast2 engine.
#'
#' @format A data frame with 35,501 rows and 4 variables:
#' \describe{
#'   \item{notification_date}{`Date`. The event date -- when the case was
#'     notified / symptom onset was recorded.}
#'   \item{diagnosis_date}{`Date`. The report date -- when the laboratory
#'     diagnosis was registered in the national system.}
#'   \item{sex}{`character`. Biological sex of the case: `"Female"` or
#'     `"Male"`.}
#'   \item{n}{`integer`. Number of cases with this
#'     (notification_date, diagnosis_date, sex) combination.}
#' }
#'
#' @source Instituto Nacional de Salud (INS), Colombia.  Data accessed via the
#'   SIVIGILA open-data platform and pre-processed for the diseasenowcast2
#'   benchmarking study.
#'
#' @examples
#' # Build a stratified tbl_now (event = notification, report = diagnosis,
#' # strata = sex).  Pipe through temporal effects for day-of-week covariates.
#' if (requireNamespace("tbl.now", quietly = TRUE)) {
#'   tn <- tbl.now::tbl_now(
#'     covid_colombia,
#'     event_date  = notification_date,
#'     report_date = diagnosis_date,
#'     strata      = sex,
#'     case_count  = n,
#'     data_type   = "count-incidence",
#'     verbose     = FALSE
#'   )
#'   print(tn)
#' }
"covid_colombia"
