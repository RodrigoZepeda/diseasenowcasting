% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/priors.R
\name{get_link_number}
\alias{get_link_number}
\title{Function for sets the link required}
\usage{
get_link_number(link)
}
\value{
A number for the distribution code for Stan's \code{data}.
}
\description{
The following links are implemented:
\itemize{
\item identity: g(x) = x
\item log: g(x) = log(x)
\item softplus: g(x) = k*log(1 + exp(x/k))
\item dhyperbolic: g(x) = c\emph{x/2 + sqrt( (c}x/2)^2 + k)
}
}
\keyword{internal}
