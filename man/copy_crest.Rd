% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{copy_crest}
\alias{copy_crest}
\title{Copy crest data to the clipboard.}
\usage{
copy_crest(
  x,
  climate = x$parameters$climate,
  optima = TRUE,
  mean = FALSE,
  uncertainties = FALSE
)
}
\arguments{
\item{x}{A \code{\link{crestObj}} produced by the \code{\link{crest.reconstruct}} or \code{\link{crest}} functions.}

\item{climate}{A vector of the climate variables to extract. See
\code{\link{accClimateVariables}} for the list of accepted values.}

\item{optima}{A boolean value to indicate if the optima should be copied to the clipboard.}

\item{mean}{A boolean value to indicate if the means should be copied to the clipboard.}

\item{uncertainties}{A boolean value to indicate if the uncertainties should be copied to the clipboard.}
}
\value{
No return value. This function is called to copy the crest data to the clipboard.
}
\description{
Copy crest data to the clipboard for an easy extraction of the data from the
R environment.
}
\examples{
\dontrun{
 if(requireNamespace('clipr', quietly=TRUE)) {
   reconstr <- crest(
    df = crest_ex, pse = crest_ex_pse, taxaType = 0,
    climate = c("bio1", "bio12"), bin_width = c(2, 20),
    shape = c("normal", "lognormal"),
    selectedTaxa = crest_ex_selection, dbname = "crest_example",
    leave_one_out = TRUE
   )
   copy_crest(reconstr, uncertainties=TRUE)
   ## You can now paste the values in a spreadsheet.
 }
}

}
