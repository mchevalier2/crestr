#' Connect to the gbif4crest database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @param df .
#' @param pse .
#' @param taxaType .
#' @return The parameters to be used by crest()
#' @export
#' @examples
#' \dontrun{
#' db <- connect_online()
#' }

crest.init <- function(df, pse, taxaType ) {
    if (is.character(df)) {
        df <- rio::import(df)
    }
    if (! is.data.frame(df)) {
        print("Problem here. Input data is not a data frame.")
        return()
    }

    if (is.character(pse)) {
        pse <- rio::import(pse)
    }
    if (! is.data.frame(pse)) {
        print("Problem here. proxy_species_equivalency is not a data frame.")
        return()
    }

    taxa <- colnames(df)[-1]
    time <- df[, 1]

    if (sum(taxa %in% pse$ProxyName) != length(taxa)) {
        print(paste("The following taxa are in the input file and are not in",
                    "the proxy_species_equivalency table."))
        missing_taxa <- taxa[! (taxa %in% pse$ProxyName)]
        print(paste(missing_taxa, collapse = ', '))
        x <- base::readline("Should we continue? [Y/N] ")
        while (! x %in% c('y', 'yes', 'Y', 'YES', 'n', 'N', 'no', 'NO') ) {
            x <- base::readline("Should we continue? [Y/N] ")
        }
        if( x %in% c('n', 'N', 'no', 'NO')) {
            return()
        } else {
            df.ok <- TRUE
            taxa <- taxa[taxa %in% pse$ProxyName]
        }
    }

    if (sum(unique(pse$ProxyName) %in% taxa) != length(unique(pse$ProxyName))) {
        print(paste("The following proxies are in the proxy_species_equivalency",
                    "table but not in the input data file."))
        missing_taxa <- unique(pse$ProxyName)[! (unique(pse$ProxyName) %in% taxa)]
        print(paste(missing_taxa, collapse = ', '))
        x <- base::readline("Should we continue? [Y/N] ")
        while (! x %in% c('y', 'yes', 'Y', 'YES', 'n', 'N', 'no', 'NO') ) {
            x <- base::readline("Should we continue? [Y/N] ")
        }
        if( x %in% c('n', 'N', 'no', 'NO')) {
            return()
        } else {
            pse.ok <- TRUE
            pse <- pse[pse$ProxyName %in% taxa, ]
            pse <- pse[pse$Level < 4, ]
        }
    }

    #Getting list of species ---------------------------------------------------
    taxonID2proxy <- matrix(ncol = 2)
    colnames(taxonID2proxy) <- c("taxonID", "proxyName")
    for (taxLevel in 1:3){
        idx <- which(pse$Level == taxLevel)
        for (tax in pse$ProxyName[idx]) {
            for (w in which(pse$ProxyName == tax)) {
                taxonIDs <- getTaxonID( pse$Family[w],
                                        pse$Genus[w],
                                        pse$Species[w],
                                        taxaType
                                       )
                if(length(taxonIDs) > 0) {
                    existingTaxa <- taxonIDs %in% taxonID2proxy[, 1]
                    if (sum(existingTaxa) > 0) {
                        taxonID2proxy[taxonID2proxy[, 1] %in% taxonIDs, 2] <- tax
                    }
                    taxonID2proxy <- rbind( taxonID2proxy,
                                            cbind(taxonIDs[!existingTaxa], rep(tax, sum(!existingTaxa)))
                                           )
                } else {
                    print(paste("No match for taxon ", paste(pse[w, 2:5], collapse = ', ')))
                }
            }
        }
    }
    taxonID2proxy <- taxonID2proxy[order(taxonID2proxy[, 2]), ]
}
