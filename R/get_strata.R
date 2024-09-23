#' Get strata
#'
#' Function to preprocess the strata from .disease_data
#' @param .disease_data A data frame for which to collapse the strata
#' @param strata A vector with the names of the strata
#'
#' @return A list with three entries:
#' @keywords internal
preprocess_strata <- function(.disease_data, strata){

  #Create the strata column
  if (is.null(strata) || strata == "" || ncol(.disease_data) == 3){
    .disease_data <- .disease_data |>
      dplyr::mutate(!!as.symbol(".strata") := "No strata")
    strata        <- ".strata"
  }

  .disease_data <- .disease_data |>
    tidyr::unite(col = ".strata_unified", dplyr::all_of(strata), sep = " - ") |>
    dplyr::mutate(!!as.symbol(".strata") := as.numeric(as.factor(!!as.symbol(".strata_unified")))) |>
    dplyr::mutate_at(".delay", function(x) x + 1)

  #Get the strata dictionary
  .strata_dict <- .disease_data |>
    dplyr::distinct(!!as.symbol(".strata_unified"),!!as.symbol(".strata"))

  #Return the number of strata
  num_strata <- .strata_dict |>
    dplyr::tally() |>
    dplyr::pull()

  return(list(num_strata = num_strata, .strata_dict = .strata_dict, .disease_data = .disease_data))
}
