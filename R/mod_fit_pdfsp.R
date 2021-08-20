#' Fit the species \code{pdfs}.
#'
#' Fit the species \code{pdfs}.
#'
#' @inheritParams crestObj
#' @param climate A vector of climatic values where the species is present.
#' @param ccs A \code{ccs} object returned by \code{\link{calib_clim_space}}.
#' @param xrange The climate gradient upon which the \code{pdf} with be defined.
#' @param use_ccs Boolean to indicate if the \code{pdfsp} should be corrected by
#'        the distribution of the modern climate space
#' @return The pdf of the species.
#' @export
#' @examples
#' # Creating one randomised species
#' climate_species <- round(stats::rnorm(50, 15, 2), 1)
#'
#' # Creating one randomised climate space
#' climate_space <- base::sample(0:300 / 10, 4000, replace = TRUE)
#'
#' ccs <- calib_clim_space(climate_space, 2)
#' xrange <- fit_xrange(ccs, "normal", 2)
#' pdfsp <- fit_pdfsp(climate_species, ccs, 2, "normal", xrange)
#' plot(xrange, pdfsp, type = "l")
#'
#' # Testing that the area under the curve is equal to 1.
#' all.equal(sum(pdfsp * (xrange[2] - xrange[1])), 1)
#'
fit_pdfsp <- function(climate, ccs, bin_width, shape, xrange, use_ccs = TRUE) {
    if(base::missing(climate)) climate
    if(base::missing(ccs)) ccs
    if(base::missing(bin_width)) bin_width
    if(base::missing(shape)) shape
    if(base::missing(xrange)) xrange

    climate <- climate[!is.na(climate)]
    if (use_ccs) {
        w <- (climate - ccs[["k1"]][1]) %/% bin_width
        w2 <- base::tabulate(w + 1, nbins = base::length(ccs[["k1"]])) / ccs[["k2"]]
        w <- w2[w + 1]
        p1 <- base::sum(w * climate) / base::sum(w)
        p2 <- base::sum(w * (climate - p1)**2) / base::sum(w)
    } else {
        p1 <- base::mean(climate)
        p2 <- stats::var(climate)
    }
    if (shape == "normal") {
        return(base::exp(-0.5 * (((xrange - p1) / base::sqrt(p2))**2))
        /
          base::sqrt(p2 * 2 * base::pi))
    } else {
        if (shape != "lognormal") {
            print("Wrong shape selected for the species pdfs.")
            print("Please use either 'normal' or 'lognormal'.")
            return(NA)
        }
        mu <- base::log(p1) - 0.5 * base::log(1 + p2 / (p1**2))
        sigma2 <- base::log(1 + p2 / (p1**2))
        return(base::exp(-((base::log(xrange) - mu)**2) / (2 * sigma2))
        /
          base::sqrt(2 * base::pi * sigma2 * xrange**2))
    }
}


#' Calibrate the distribution of the modern climate space.
#'
#' Calibrate the distribution of the modern climate space.
#'
#' @inheritParams fit_pdfsp
#' @return A \code{ccs} object that will be used by \code{\link{fit_pdfsp}}.
#' @export
#' @examples
#' calib_clim_space(sample(0:300 / 10, 4000, replace = TRUE), 2)
#'
calib_clim_space <- function(climate, bin_width) {
    if(base::missing(climate)) climate
    if(base::missing(bin_width)) bin_width

    m <- base::min(climate, na.rm = TRUE) %/% bin_width
    nclass <- base::diff(base::range(climate, na.rm = TRUE)) %/% bin_width
    clim_norm <- (climate - m*bin_width) %/% bin_width
    out <- list()
    out[["k1"]] <- base::seq(m*bin_width, (base::max(climate, na.rm = TRUE) %/% bin_width) * bin_width + bin_width, bin_width)
    out[["k2"]] <- base::tabulate(clim_norm + 1, nbins = base::length(out[["k1"]]))
    out
}



#' Define the climate gradient to fit the \code{pdfs}.
#'
#' Define the climate gradient to fit the \code{pdfs}.
#'
#' @inheritParams crestObj
#' @inheritParams fit_pdfsp
#' @return A regularly spaced climate gradient with \code{npoints} points.
#' @export
#' @examples
#' # Creating one randomised climate space
#' climate_space <- sample(0:300 / 10, 4000, replace = TRUE)
#' ccs <- calib_clim_space(climate_space, 2)
#' xrange <- fit_xrange(ccs, "normal", 2)
#' head(xrange)
#'
fit_xrange <- function(ccs, shape, bin_width, npoints = 500) {
    if(base::missing(ccs)) ccs
    if(base::missing(shape)) shape
    if(base::missing(bin_width)) bin_width

    if (shape == "normal") {
        return(base::seq(ccs[["k1"]][1] - 5 * bin_width,
          ccs[["k1"]][base::length(ccs[["k1"]])] + 5 * bin_width,
          length.out = npoints
        ))
    }
    return(base::seq(base::max( 1e-12, ccs[["k1"]][1] - 5 * bin_width),
        ccs[["k1"]][base::length(ccs[["k1"]])] + 5 * bin_width,
        length.out = npoints
    ))
}
