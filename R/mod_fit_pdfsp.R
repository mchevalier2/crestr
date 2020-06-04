#' Fit the species pdfs.
#'
#' Fit the species pdfs.
#'
#' @param climate A vector of climatic values where the species is present.
#' @param ccs A \code{ccs} object returned by \code{\link{calib_clim_space}}.
#' @param bin_width The width of the climate bins.
#' @param shape The shape of the species pdfs. Use 'normal' or 'lognormal'.
#' @param xrange The climate gradient upon which the pdf with be defined.
#' @return The pdf of the species.
#' @export
#' @examples
#' #Creating one randomised species
#' climate_species <- round(stats::rnorm(50, 15, 2),1)
#' #Creating one randomised climate space
#' climate_space <- base::sample(0:300/10, 4000, replace=TRUE)
#' ccs <- calib_clim_space(climate_space, 2)
#' xrange <- fit_xrange(ccs, 'normal', 2)
#' pdfsp <- fit_pdfsp(climate_species, ccs, 2, 'normal', xrange)
#' plot(xrange, pdfsp, type='l')
#'
#' #Testing that the area under the curve is equal to 1.
#' sum(pdfsp * (xrange[2] - xrange[1])) == 1

fit_pdfsp <- function(climate, ccs, bin_width, shape, xrange) {
    w <- (climate - ccs[['k1']][1]) %/% bin_width
    w2 <- tabulate(w + 1, nbins = length(ccs[['k1']])) / ccs[['k2']]
    w <- w2[w + 1]
    p1 <- sum(w * climate) / sum(w)
    p2 <- sum(w * (climate - p1)**2) / sum(w)
    if (shape == 'normal') {
        return(exp(-0.5 * (((xrange - p1) / sqrt(p2))**2)) / sqrt(p2 * 2 * pi))
    } else {
        if (shape != 'lognormal'){
            print("Wrong shape selected for the species pdfs.")
            print("Please use either 'normal' or 'lognormal'.")
            return(NA)
        }
        mu <- log(p1) - 0.5 * log(1 + p2 / (p1**2))
        sigma2 <- log(1 + p2 / (p1**2))
        return(exp(-((log(xrange) - mu)**2) / (2 * sigma2)) / sqrt(2 * pi * sigma2 * xrange**2))
    }
}


#' Calibrate the distribution of the modern climate space.
#'
#' Calibrate the distribution of the modern climate space.
#'
#' @param climate All the climate values observed across the study area.
#' @param bin_width The width of the climate bins.
#' @return A \code{ccs} object that will be used by \code{\link{fit_pdfsp}}.
#' @export
#' @examples
#' #Extracting the number of taxa recorded in the database
#' calib_clim_space(sample(0:300/10, 4000, replace=TRUE), 2)
#'
calib_clim_space <- function(climate, bin_width){
    m <- min(climate, na.rm = TRUE) %/% 1
    nclass <- diff(range(climate, na.rm = TRUE)) %/% bin_width
    clim_norm <- (climate - m) %/% bin_width
    out <- list()
    out[['k1']] <- seq(m, max(climate, na.rm = TRUE) %/% 1 + 1, bin_width)
    out[['k2']] <- tabulate(clim_norm + 1, nbins = length(out[['k1']]))
    out
}



#' Define teh climate gradient to fit the pdfs.
#'
#' Define teh climate gradient to fit the pdfs.
#'
#' @param ccs A \code{ccs} object returned by \code{\link{calib_clim_space}}.
#' @param shape The shape of the species pdfs. Use 'normal' or 'lognormal'.
#' @param bin_width The width of the climate bins.
#' @param npoints The number of points to be used to fit the pdfs.
#' @return A regularly spaced climate gradient with \code{npoints} points.
#' @export
#' @examples
#' #Creating one randomised climate space
#' climate_space <- sample(0:300/10, 4000, replace=TRUE)
#' ccs <- calib_clim_space(climate_space, 2)
#' xrange <- fit_xrange(ccs, 'normal', 2)
#' head(xrange)

fit_xrange <- function(ccs, shape, bin_width, npoints = 500){
    if (shape == 'normal') {
        return(seq(ccs[['k1']][1]-10*bin_width, ccs[['k1']][length(ccs[['k1']])]+10*bin_width, length.out=npoints))
    }
    return(seq(1e-12, ccs[['k1']][length(ccs[['k1']])]+10*bin_width, length.out=npoints))
}
