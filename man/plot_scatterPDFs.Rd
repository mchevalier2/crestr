% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.scatterPDFS.R
\name{plot_scatterPDFs}
\alias{plot_scatterPDFs}
\title{Plot the pdf optima and uncertainty ranges in a climate biplot}
\usage{
plot_scatterPDFs(
  x,
  climate = x$parameters$climate[1:2],
  taxanames = x$input$taxa.name,
  uncertainties = x$parameters$uncertainties,
  xlim = range(x$modelling$climate_space[, climate[1]]),
  ylim = range(x$modelling$climate_space[, climate[2]]),
  add_modern = FALSE,
  save = FALSE,
  filename = "scatterPDFs.pdf",
  width = 5.51,
  height = 5.51,
  as.png = FALSE,
  png.res = 300
)
}
\arguments{
\item{x}{A \code{\link{crestObj}} generated by either the \code{\link{crest.calibrate}},
\code{\link{crest.reconstruct}} or \code{\link{crest}} functions.}

\item{climate}{Names of the two climate variables to be used to generate the plot. By default
plot. By default the first two variables are included.}

\item{taxanames}{A list of taxa to use for the plot (default is all the
recorded taxa).}

\item{uncertainties}{A (vector of) threshold value(s) indicating the error
bars that should be calculated (default are the values stored in x).}

\item{xlim, }{ylim The climate range to plot the data. Default is the full range
of the observed climate space.}

\item{ylim}{the y limits of the plot.}

\item{add_modern}{A boolean to add the location and the modern climate values
to the plot (default \code{FALSE}).}

\item{save}{A boolean to indicate if the diagram should be saved as a pdf file.
Default is \code{FALSE}.}

\item{filename}{An absolute or relative path that indicates where the diagram
should be saved. Also used to specify the name of the file. Default:
the file is saved in the working directory under the name
\code{'violinPDFs.pdf'}.}

\item{width}{The width of the output file in inches (default 7.48in ~ 19cm).}

\item{height}{The height of the output file in inches (default 3in ~ 7.6cm
per variables).}

\item{as.png}{A boolean to indicate if the output should be saved as a png.
Default is \code{FALSE} and the figure is saved as a pdf file.}

\item{png.res}{The resolution of the png file (default 300 pixels per inch).}
}
\value{
A table with the climate tolerances of all the taxa
}
\description{
Plot the pdf optima and uncertainty ranges in a climate biplot
}
\examples{
\dontrun{
  data(crest_ex_pse)
  data(crest_ex_selection)
  reconstr <- crest.get_modern_data(
    pse = crest_ex_pse, taxaType = 0,
    climate = c("bio1", "bio12"),
    selectedTaxa = crest_ex_selection, dbname = "crest_example"
  )
  reconstr <- crest.calibrate(reconstr,
    geoWeighting = TRUE, climateSpaceWeighting = TRUE,
    bin_width = c(2, 20), shape = c("normal", "lognormal")
  )
}
## example using pre-saved reconstruction obtained with the previous command.
data(reconstr)
dat <- plot_scatterPDFs(reconstr, save=FALSE,
                 taxanames=c(reconstr$inputs$taxa.name[c(2,4,5,1)]))
dat

}
