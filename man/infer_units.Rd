% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{infer_units}
\alias{infer_units}
\title{Automatically infer which value is \code{units}.}
\usage{
infer_units(.disease_data, units, date_column)
}
\arguments{
\item{.disease_data}{A time series of reporting data in aggregated line list format
such that each row has a column for onset date, report date, and (optionally) strata}

\item{units}{Time scale of reporting. Options: "1 day", "1 week".}

\item{date_column}{Name of a column of \code{.disease_data} that contains the dates.}
}
\value{
Whether the data's units are \code{days} or \code{weeks}
}
\description{
Function returns whether data is daily, weekly, monthly or yearly by
\code{date_column}.
}
\keyword{internal}
