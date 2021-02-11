#' Includes the list of taxa into the reconstructions.
#'
#' Includes the list of taxa into the reconstructions.
#'
#' @param x A \code{\link{crestObj}} produced by one of the \code{\link{crest}},
#'        \code{\link{crest.get_modern_data}}, \code{\link{crest.calibrate}},
#'        \code{\link{crest.reconstrut}} or \code{\link{loo}} functions.
#' @param taxa A vector of taxa to include.
#' @param climate A vector of climate variables to link the taxa with.
#' @export

includeTaxa <- function(x, taxa, climate) {
    for (tax in taxa) {
        for (clim in c(climate)) {
            if (x$inputs$selectedTaxa[tax, ncol(x$inputs$selectedTaxa)] >= 0 ) {
                x$inputs$selectedTaxa[tax, climate] <- rep(1, length(climate))
            }
        }
    }
    x
}


#' Excludes the list of taxa from the reconstructions.
#'
#' Excludes the list of taxa from the reconstructions.
#'
#' @param x A \code{\link{crestObj}} produced by one of the \code{\link{crest}},
#'        \code{\link{crest.get_modern_data}}, \code{\link{crest.calibrate}},
#'        \code{\link{crest.reconstrut}} or \code{\link{loo}} functions.
#' @param taxa A vector of taxa to exclude.
#' @param climate A vector of climate variables to unlink the taxa with.
#' @export

excludeTaxa <- function(x, taxa, climate) {
    for (tax in taxa) {
        for (clim in c(climate)) {
          if (x$inputs$selectedTaxa[tax, ncol(x$inputs$selectedTaxa)] >= 0 ) {
                x$inputs$selectedTaxa[tax, climate] <- rep(0, length(climate))
            }
        }
    }
    x
}
