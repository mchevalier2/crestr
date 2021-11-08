#' Includes the list of taxa into the reconstructions.
#'
#' Includes the list of taxa into the reconstructions.
#'
#' @param x A \code{\link{crestObj}} produced by one of the \code{\link{crest}},
#'        \code{\link{crest.get_modern_data}}, \code{\link{crest.calibrate}},
#'        \code{\link{crest.reconstruct}} or \code{\link{loo}} functions.
#' @param taxa A vector of taxa to include.
#' @param climate A vector of climate variables to link the taxa with.
#' @return Return the updated \code{\link{crestObj}}.
#' @export
#' @examples
#' data(reconstr)
#' print(reconstr$inputs$selectedTaxa)
#' reconstr <- includeTaxa(reconstr, reconstr$inputs$taxa.name, 'bio12')
#' ## All the taxa are not selected for 'bio12', except for 'Taxon7' for which
#' ## data are unavailable.
#' print(reconstr$inputs$selectedTaxa)
#'
includeTaxa <- function(x, taxa, climate) {
    if(base::missing(x)) x
    if(base::missing(taxa)) taxa
    if(base::missing(climate)) climate

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
#'        \code{\link{crest.reconstruct}} or \code{\link{loo}} functions.
#' @param taxa A vector of taxa to exclude.
#' @param climate A vector of climate variables to unlink the taxa with.
#' @return Return the updated \code{\link{crestObj}}.
#' @export
#' @examples
#' data(reconstr)
#' print(reconstr$inputs$selectedTaxa)
#' reconstr <- excludeTaxa(reconstr, 'Taxon3', 'bio1')
#' ## 'Taxon3' is now excluded from the reconstruction of 'bio1'.
#' print(reconstr$inputs$selectedTaxa)
#'
excludeTaxa <- function(x, taxa, climate) {
    if(base::missing(x)) x
    if(base::missing(taxa)) taxa
    if(base::missing(climate)) climate

    for (tax in taxa) {
        for (clim in c(climate)) {
          if (x$inputs$selectedTaxa[tax, ncol(x$inputs$selectedTaxa)] >= 0 ) {
                x$inputs$selectedTaxa[tax, climate] <- rep(0, length(climate))
            }
        }
    }
    x
}
