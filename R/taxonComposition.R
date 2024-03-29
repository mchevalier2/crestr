#' Return the size of the distribution of each composing species of each taxon
#'
#' Return the size of the distribution of each composing species of each taxon
#'
#' @param x A \code{\link{crestObj}} generated by either the \code{\link{crest.calibrate}},
#'        \code{\link{crest.reconstruct}} or \code{\link{crest}} functions.
#' @param taxanames A list of taxa to use (default is all the
#'        recorded taxa).
#' @return A list with the number of unique occurrences for each composing species
#' @export
#' @examples
#'
#' \dontrun{
#'   data(reconstr)
#'   taxonComposition(reconstr)
#'  }
#'
taxonComposition <- function( x,
                              taxanames = x$input$taxa.name
                              ) {

    if(base::missing(x)) x

    if (is.crestObj(x)) {
        rs <- list()
        for(tax in taxanames){
            taxonID = x$modelling$taxonID2proxy[x$modelling$taxonID2proxy[, 2] == tax, 'taxonID']
            distrib <- getDistribTaxa(taxonID,
                                      climate = x$parameters$climate[1],
                                      xmn = x$parameters$xmn,
                                      xmx = x$parameters$xmx,
                                      ymn = x$parameters$ymn,
                                      ymx = x$parameters$ymx,
                                      countries = x$parameters$countries,
                                      realms = x$parameters$realms,
                                      biomes = x$parameters$biomes,
                                      ecoregions = x$parameters$ecoregions,
                                      elev_min = x$parameters$elev_min,
                                      year_min = x$parameters$year_min,
                                      year_max = x$parameters$year_max,
                                      nodate = x$parameters$nodate,
                                      type_of_obs = x$parameters$type_of_obs,
                                      dbname = x$misc$dbname
                                  )
            rs[[tax]] <- sort(tapply(distrib$taxonid, distrib$taxonid, length))
        }
    } else {
        cat('This function only works with a crestObj.\n\n')
    }
    rs
}
