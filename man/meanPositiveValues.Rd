% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UsefulFunctions.R
\name{meanPositiveValues}
\alias{meanPositiveValues}
\title{Calculate the mean of all stricly positive values.}
\usage{
meanPositiveValues(x)
}
\arguments{
\item{x}{A vector of values.}
}
\value{
The average of all the positive values. Returns \code{NaN} is no
stricly positive values are found.
}
\description{
Calculate the mean of all stricly positive values.
}
\examples{
meanPositiveValues(-10:10)
}
