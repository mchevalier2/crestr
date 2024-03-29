% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.R
\name{export}
\alias{export}
\title{Export the results}
\usage{
export(
  x,
  dataname = x$misc$site_info$site_name,
  climate = x$parameters$climate,
  loc = getwd(),
  as.csv = FALSE,
  fullUncertainties = FALSE,
  loo = FALSE,
  weights = FALSE,
  pdfs = FALSE
)
}
\arguments{
\item{x}{The name, age or depth of the rows of df (the samples).}

\item{dataname}{The name of the site (default: \code{x$misc$site_info$site_name})}

\item{climate}{The climate data to export. Data for all climate variables are
saved by default.}

\item{loc}{The path where to export the data (default: working directory)}

\item{as.csv}{Boolean to indicate if the data should be exported as csv (\code{TRUE}) or xlsx (\code{FALSE}, default)}

\item{fullUncertainties}{A boolean to export the full climate uncertainty distribution (default \code{FALSE})}

\item{loo}{A boolean to export the leave-one-out data if they exist (default \code{FALSE})}

\item{weights}{A boolean to export the weights derived from the percentages (default \code{FALSE})}

\item{pdfs}{A boolean to export the taxa's \code{pdfs} (default \code{FALSE})}
}
\value{
No return value, function called to export the results.
}
\description{
Export the results generated by the reconstruction
}
\examples{
\dontrun{
  data(crest_ex)
  data(crest_ex_pse)
  data(crest_ex_selection)
  reconstr <- crest(
    df = crest_ex, pse = crest_ex_pse, taxaType = 0,
    site_info = c(7.5, 7.5),
    climate = c("bio1", "bio12"), bin_width = c(2, 20),
    shape = c("normal", "lognormal"),
    selectedTaxa = crest_ex_selection, dbname = "crest_example",
    leave_one_out = TRUE
  )
  #> Replace 'tempdir()' by the location where the sample should be saved (e.g. 'getwd()')
  export(reconstr, dataname='crest_example',
         fullUncertainties=TRUE, weights=TRUE, loo=TRUE, pdfs=TRUE,
         loc=tempdir())
}

}
