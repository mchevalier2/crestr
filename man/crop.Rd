% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{crop}
\alias{crop}
\title{Crop the dataset obtained from \code{\link{crest.get_modern_data}}}
\usage{
crop(x, shp)
}
\arguments{
\item{x}{A \code{\link{crestObj}} produced by the \code{\link{crest.get_modern_data}} function.}

\item{shp}{A shapefile (spatVect) to crop the data. Data points will be kept
if their centroid is within the shape.}
}
\value{
An cropped version of the input \code{crestObj}.
}
\description{
Crop the dataset obtained from \code{\link{crest.get_modern_data}} according
to an object of the class \code{SpatialPolygonsDataFrame}.
}
\examples{
\dontrun{
  data(M1)
  M1 <- terra::unwrap(M1)
  ## We want only the data covering Nigeria
  M2 <- M1[M1$COUNTRY == 'Nigeria', ]
  data(reconstr)
  reconstr.cropped <- crop(reconstr, M2)
  data1 <- terra::rast(reconstr$modelling$climate_space[, 1:3],
                                 crs=terra::crs(M1), type='xyz')
  data2 <- terra::rast(reconstr.cropped$modelling$climate_space[, 1:3],
                                 crs=terra::crs(M1), type='xyz')
  layout(matrix(c(1,2,3,4), byrow=FALSE, ncol=2), width=1, height=c(0.2, 0.8))
  plot_map_eqearth(data1, brks.pos=seq(13,29,2), colour_scale=TRUE,
                   title='Full dataset', zlim=c(13, 29))
  plot_map_eqearth(data2, brks.pos=seq(13,29,2), colour_scale=TRUE,
                   title='Cropped dataset', zlim=c(13, 29))
}

}
