% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{infer_temporal_effect}
\alias{infer_temporal_effect}
\title{Automatically infer the temporal effect based on the units}
\usage{
infer_temporal_effect(
  t_effect,
  units,
  .default = c("delay", "epidemic", "other")
)
}
\arguments{
\item{t_effect}{Either \code{"auto"} to infer the temporal effect or a \code{temporal_effect} object
constructed with the \code{\link[=temporal_effects]{temporal_effects()}} function.}

\item{units}{Either "weeks" or "days" for weekly or daily data.}

\item{.default}{A character indicating whether the default should be for delay or epidemic process
or for other (an empty effect)}
}
\value{
A \code{temporal_effect} object for the model
}
\description{
Function that returns a \code{temporal_effect} object if \code{t_effect} is \code{auto}. Else it only
checks whether an object is a temporal effect and whether it makes sense
given the units.
}
\keyword{internal}
