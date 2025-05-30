% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/priors.R
\name{get_distribution_number}
\alias{get_distribution_number}
\title{Function for setting the distribution from words to number}
\usage{
get_distribution_number(dist)
}
\arguments{
\item{dist}{Distribution. Either "NegativeBinomial", "Poisson", "Normal", or "Student"}
}
\value{
A number for the distribution code for Stan's \code{data}.
}
\description{
Function for setting the distribution from words to number
}
\keyword{internal}
