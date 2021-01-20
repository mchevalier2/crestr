#' Includes the list of taxa to the reconstructions.
#'
#' Includes the list of taxa to the reconstructions.
#'
#' @param x A crestObj produced by one of the crest functions.
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


#' Excludes the list of taxa to the reconstructions.
#'
#' Excludes the list of taxa to the reconstructions.
#'
#' @param x A crestObj produced by one of the crest functions.
#' @param taxa A vector of taxa to Exclude.
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
