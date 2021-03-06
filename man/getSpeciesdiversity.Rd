% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSpeciesDiversity.R
\name{getSpeciesdiversity}
\alias{getSpeciesdiversity}
\title{Calculates how many species compose the respose of each taxon.}
\usage{
getSpeciesdiversity(x)
}
\arguments{
\item{x}{A \code{\link{crestObj}} generated by the
\code{\link{crest.reconstruct}}, \code{\link{loo}} or
\code{\link{crest}} functions.}
}
\description{
Calculates how many species compose the respose of each taxon.
}
\examples{
data(reconstr)
getSpeciesdiversity(reconstr)
}
