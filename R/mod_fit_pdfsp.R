#' Connect to the gbif4crest database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @param request A SQL request to be executed.
#' @return The result of the request.
#' @export
#' @examples
#' #Extracting the number of taxa recorded in the database
#' dbRequest("SELECT count(*) FROM taxa")
#'
#' #Extracting all the taxa that have at least one occurrence in South Africa.
#' \dontrun{
#' southAfricaTaxa <- dbRequest(paste0("SELECT DISTINCT taxa.* ",
#'     "FROM taxa, distrib_qdgc, geo_qdgc ",
#'     "WHERE taxa.taxonid=distrib_qdgc.taxonid ",
#'     "AND   distrib_qdgc.latitude=geo_qdgc.latitude ",
#'     "AND   distrib_qdgc.longitude=geo_qdgc.longitude ",
#'     "AND geo_qdgc.countryname='South Africa'"))
#' head(southAfricaTaxa)
#' }

fit_pdfsp <- function(climate, ccs, bin_width, shape, xrange, npoints = 500) {
    w <- (climate - ccs[['k1']][1]) %/% bin_width
    w2 <- tabulate(w + 1, nbins = length(ccs[['k1']])) / ccs[['k2']]
    w <- w2[w + 1]
    p1 <- sum(w * climate) / sum(w)
    p2 <- sum(w * (climate - p1)**2) / sum(w)
    if (shape == 'normal') {
        xx=seq(ccs[['k1']][1]-10*bin_width, ccs[['k1']][length(ccs[['k1']])]+10*bin_width, length.out=500)
        return(exp(-0.5 * (((xx - p1) / sqrt(p2))**2)) / sqrt(p2 * 2 * pi))
    } else {
        xx=seq(1e-12, ccs[['k1']][length(ccs[['k1']])]+10*bin_width, length.out=500)
        mu <- log(p1) - 0.5 * log(1 + p2 / (p1**2))
        sigma2 <- log(1 + p2 / (p1**2))
        return(exp(-((log(xx) - mu)**2) / (2 * sigma2)) / sqrt(2 * pi * sigma2 * xx**2))
    }
}


calib_clim_space <- function(clim_space, bin_width){
    m <- min(clim_space, na.rm = TRUE) %/% 1
    nclass <- diff(range(clim_space, na.rm = TRUE)) %/% bin_width
    clim_norm <- (clim_space - m) %/% bin_width
    out <- list()
    out[['k1']] <- seq(m, max(clim_space, na.rm = TRUE) %/% 1 + 1, bin_width)
    out[['k2']] <- tabulate(clim_norm + 1, nbins = length(out[['k1']]))
    out
}
