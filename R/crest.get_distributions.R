
#' Extract distributions from the database
#'
#' This function will extract the distributions of all the species composing each
#' taxon and return them as a list.
#'
#' @inheritParams crestObj
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A crest() object containing the spatial distributions
#' @export
#' @examples
#' \dontrun{
#' data(crest_ex)
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' x <- crest.get_distributions(taxa.name=colnames(crest_ex)[-1],
#'                  pse=crest_ex_pse, taxaType = 0,
#'                  climate = c('bio1', 'bio12'),
#'                  selectedTaxa = crest_ex_selection, dbname = 'crest_example')
#' x
#' lapply(x$modelling$distributions, head)
#' }

crest.get_distributions <- function ( taxa.name, pse, taxaType, climate,
                                      xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                                      continents = NA, countries = NA,
                                      realms = NA, biomes = NA, ecoregions = NA,
                                      minGridCells = 20,
                                      selectedTaxa = NA,
                                      dbname = 'gbif4crest_02'
                                    ) {

    ##. Testing if the input variables are in the correct format ---------------
    if (is.character(pse)) {
        pse <- rio::import(pse)
    }
    if (! is.data.frame(pse)) {
        cat("Problem here. proxy_species_equivalency is not a data frame.\n")
        return()
    }

    ##. Change the climate variable ID for the climate variable name -----------
    for (clim in 1:length(climate)) {
        climVar <- accClimateVariables()
        new_clim <- climate
        if (! (climate[clim] %in% climVar[,1] | climate[clim] %in% climVar[,2]) ) {
            cat(paste("Problem here. The variable '", climate[clim], "' is not an accepted value. Please select a name or ID from the following list.\n", sep=''))
            print(climVar)
            return()
        } else {
            defaultW <- getOption("warn")
            options(warn = -1)
            if (! is.na(as.numeric(climate[clim]))) {
                new_clim[clim] <- as.character(climVar[which(climVar[, 1] == as.numeric(climate[clim])), 2])
            }
            options(warn = defaultW)
        }
    }
    climate <- new_clim


    print('make a quality test for taxaType.')

    if (xmn >= xmx) {
        cat("xmn is larger than xmx. Inverting the two values and continuing.\n")
        tmp <- xmn
        xmn <- xmx
        xmx <- tmp
    }
    if (xmn < -180 | xmx > 180) {
        cat("WARNING: [xmn; xmx] range larger than accepted values [-180; 180]. Continuing.\n")
    }

    if (ymn >= ymx) {
        cat("ymn is larger than ymx. Inverting the two values and continuing.\n")
        tmp <- ymn
        ymn <- ymx
        ymx <- tmp
    }
    if (ymn < -90 | ymx > 90) {
        cat("WARNING: [ymn; ymx] range larger than accepted values [-90; 90]. Continuing.\n")
    }

    cont.list <- accContinentNames(dbname)
    if (! is.na(continents)) {
        for (cont in continents) {
            if (! cont %in% names(cont.list)) {
                cat(paste("Problem here. The continent '", cont, "' is not an accepted value. Please select a name from the following list.\n", sep=''))
                print(names(cont.list))
                return()
            }
        }
    }
    if (! is.na(countries)) {
        for (country  in countries) {
            if (! country %in% unlist(cont.list)) {
                cat(paste("Problem here. The country '", country, "' is not an accepted value. Please select a name from the following list.\n", sep=''))
                print(cont.list)
                return()
            }
        }
    }

    if (! is.na(countries[1]) | ! is.na(continents[1])) {
        res <- dbRequest(paste0( "SELECT DISTINCT continent, countryname, count(*) FROM geo_qdgc WHERE ",
                                 ifelse(is.na(continents)[1], "", paste0("continent IN ('", paste(continents, collapse = "', '"),"') ")),
                                 ifelse(is.na(continents)[1] | is.na(countries)[1], "", "AND "),
                                 ifelse(is.na(countries)[1], "", paste0("countryname IN ('", paste(countries, collapse = "', '"),"') ")),
                                 " GROUP BY continent, countryname"
                               ),
                          dbname
                         )
        if (length(res) == 0) {
            cat(paste("Problem here. No result for any of the combination continent x country.\n", sep=''))
        } else {
            cat(paste("The database is composed of these countries.\n", sep=''))
            res
        }
    }


    if (is.na(as.vector(t(selectedTaxa))[1])) {
       selectedTaxa <- data.frame( matrix( rep(1, length(climate) * length(taxa.name)),
                                           ncol = length(climate)
                                          )
                                  )
       rownames(selectedTaxa) = taxa.name
       colnames(selectedTaxa) = climate
    }
    selectedTaxa <- cbind(selectedTaxa, as.character(rep("", length(taxa.name))), stringsAsFactors = FALSE)
    colnames(selectedTaxa)[ncol(selectedTaxa)] <- 'notes'



    ##. Formatting data in the expected format ---------------------------------
    if (sum(unique(pse$ProxyName) %in% taxa.name) != length(unique(pse$ProxyName))) {
        missing_taxa <- unique(pse$ProxyName)[! (unique(pse$ProxyName) %in% taxa.name)]
        cat(paste("The following", ifelse( length(missing_taxa) > 1,
                                           'taxa are in the proxy_species_equivalency file and are',
                                           'taxon is in the proxy_species_equivalency file and is'),
                  " not in the input table.\n"))
        cat(paste(missing_taxa, collapse = ', ')) ; cat('\n')
        ss <- paste("Should",
                    ifelse(length(missing_taxa) > 1, 'these taxa', 'this taxon'),
                    "be ignored to continue? [Y/N] "
                  )
        x <- base::readline(ss)
        while (! x %in% c('y', 'yes', 'Y', 'YES', 'n', 'N', 'no', 'NO') ) {
            x <- base::readline(ss)
        }
        if( x %in% c('n', 'N', 'no', 'NO')) {
            return()
        } else {
            pse <- pse[pse$ProxyName %in% taxa.name, ]
            w <- which(pse$Level == 4)
            if (length(w) > 0) {
                cat(paste("The following", ifelse(length(w)>1, 'taxa have', 'taxon has'),
                            "not been classified and will not contribute to the reconstruction.",
                            ifelse(length(w)>1, 'They', 'It'), "will still contribute",
                            "to the estimation of the weights if either 'normalisation' or",
                            "'percentages' have been selected.\n"
                          )
                      )
                cat(unique(pse$ProxyName[w])) ; cat('\n')
                for (tax in unique(pse$ProxyName[w])) {
                    selectedTaxa[tax, ] <- c(rep(0, length(climate)), 'No association with vegetation')
                }
                pse <- pse[-w, ]
            }
        }
    }

    crest <- crestObj(taxa.name, pse, taxaType, climate,
                      xmn, xmx, ymn, ymx,
                      continents, countries,
                      realms, biomes, ecoregions,
                      selectedTaxa = selectedTaxa
                    )

    taxonID2proxy <- matrix(ncol = 2)
    colnames(taxonID2proxy) <- c("taxonID", "proxyName")
    pse$Level = as.numeric(as.character(pse$Level))
    pse$Family = as.character(pse$Family)
    pse$Genus = as.character(pse$Genus)
    pse$Species = as.character(pse$Species)
    pse$ProxyName = as.character(pse$ProxyName)

    for (taxLevel in 1:3){
        idx <- which(pse$Level == taxLevel)
        for (tax in pse$ProxyName[idx]) {
            for (w in which(pse$ProxyName == tax)) {
                taxonIDs <- getTaxonID( pse$Family[w],
                                        pse$Genus[w],
                                        pse$Species[w],
                                        taxaType,
                                        dbname
                                       )
                if(length(taxonIDs) > 0) {
                    existingTaxa <- taxonIDs %in% taxonID2proxy[, 'taxonID']
                    if (sum(existingTaxa) > 0) {
                        taxonID2proxy[taxonID2proxy[, 'taxonID'] %in% taxonIDs, 'proxyName'] <- tax
                    }
                    taxonID2proxy <- rbind( taxonID2proxy,
                                            cbind(taxonIDs[!existingTaxa], rep(tax, sum(!existingTaxa)))
                                           )
                } else {
                    cat(paste("No match for taxon ", paste(pse[w, 2:5], collapse = ', '), '\n'))
                    crest$inputs$selectedTaxa[tax, ] <- c(rep(0, length(climate)), 'No correspondance with vegetation')
              }
            }
        }
    }
    taxonID2proxy <- taxonID2proxy[-1, ]
    taxonID2proxy <- taxonID2proxy[order(taxonID2proxy[, 'proxyName']), ]
    crest$modelling$taxonID2proxy <- taxonID2proxy

    distributions <- list()
    cat('Extracting data from the online database.\n     ')
    pb  <- utils::txtProgressBar(0, length(taxa.name), style=3, width = min(50, getOption("width")/2))
    pbi <- 1
    for(tax in taxa.name) {
        utils::setTxtProgressBar(pb, pbi)
        if (sum(as.numeric(crest$inputs$selectedTaxa[tax, climate])) > 0) {
            distributions[[tax]] <- getDistribTaxa( taxIDs = taxonID2proxy[taxonID2proxy[, 'proxyName'] == tax, 1],
                                                    climate,
                                                    xmn, xmx, ymn, ymx,
                                                    continents, countries,
                                                    realms, biomes, ecoregions,
                                                    dbname
                                                   )
            extent_taxa <- table(distributions[[tax]][,1])
            extent_taxa <- as.numeric(names(extent_taxa)[extent_taxa >= minGridCells])
            distributions[[tax]] <- distributions[[tax]][distributions[[tax]][, 1] %in% extent_taxa, ]
            if (nrow(distributions[[tax]]) == 0) {
                cat(paste0("Insufficient data points to calibrate a pdf for ", tax, '\n'))
                distributions[[tax]] <- NA
                crest$inputs$selectedTaxa[tax, ] <- c(rep(0, length(climate)), 'Not enough data points')
            }
        } else {
            distributions[[tax]] <- NA
        }
        pbi <- pbi + 1
    }
    crest$modelling$distributions <- distributions
    close(pb)
    crest
}
