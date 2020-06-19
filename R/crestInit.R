#' Connect to the gbif4crest database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @param df .
#' @param pse .
#' @param taxaType .
#' @param climate A vectof of the climate variables to extract.
#' @param xmn,xmx,ymn,ymx The coordinates defining the study area.
#' @param continents A vector of the continent names defining the study area.
#' @param countries A vector of the country names defining the study area.
#' @param realms A vector of the studied botanical realms defining the study area.
#' @param biomes A vector of the studied botanical biomes defining the study area.
#' @param ecoregions A vector of the studied botanical ecoregions defining the study area.
#' @param minGridCells .
#' @param bin_width .
#' @param shape .
#' @param selectedTaxa .
#' @param npoints The number of points to be used to fit the pdfs.
#' @param geoWeighting The number of points to be used to fit the pdfs.
#' @param climateSpaceWeighting The number of points to be used to fit the pdfs.
#' @param presenceThreshold .
#' @param taxWeight 'originalData', 'presence/absence', percentages' or 'normalisation'
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return The parameters to be used by crest()
#' @export
#' @examples
#' data(crest_ex)
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' recons <- crest(crest_ex, crest_ex_pse, taxaType = 0,
#'                 climate = c('bio1', 'bio12'), bin_width = c(2, 20),
#'                 shape = c('normal', 'lognormal'),
#'                 selectedTaxa = crest_ex_selection, dbname = 'crest_example')
#' plot(recons)


crest <- function ( df, pse, taxaType, climate,
                    xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                    continents=NA, countries=NA,
                    realms=NA, biomes=NA, ecoregions=NA,
                    minGridCells=20,
                    bin_width=rep(1, length(climate)),
                    shape=rep('normal', length(climate)),
                    npoints = 500,
                    geoWeighting = TRUE,
                    climateSpaceWeighting = TRUE,
                    selectedTaxa = NA,
                    presenceThreshold = 0,
                    taxWeight = 'normalisation',
                    dbname = 'gbif4crest_02'
                  ) {

##.Testing if the input variables are in the correct format --------------------
    if (is.character(df)) {
        df <- rio::import(df)
    }
    if (! is.data.frame(df)) {
        cat("Problem here. Input data is not a data frame.\n")
        return()
    }

    if (is.character(pse)) {
        pse <- rio::import(pse)
    }
    if (! is.data.frame(pse)) {
        print("Problem here. proxy_species_equivalency is not a data frame.")
        return()
    }

    for (clim in 1:length(climate)) {
        climVar <- accClimateVariables()
        new_clim <- climate
        if (! (climate[clim] %in% climVar[,1] | climate[clim] %in% climVar[,2]) ) {
            print(paste("Problem here. The variable '", climate[clim], "' is not an accepted value. Please select a name or ID from the following list.", sep=''))
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


    if (xmn >= xmx) {
        print("xmn is larger than xmx. Inverting the two values and continuing.")
        tmp <- xmn
        xmn <- xmx
        xmx <- tmp
    }
    if (xmn < -180 | xmx > 180) {
        print("WARNING: [xmn; xmx] range larger than accepted values [-180; 180]. Continuing.")
    }

    if (ymn >= ymx) {
        print("ymn is larger than ymx. Inverting the two values and continuing.")
        tmp <- ymn
        ymn <- ymx
        ymx <- tmp
    }
    if (ymn < -90 | ymx > 90) {
        print("WARNING: [ymn; ymx] range larger than accepted values [-90; 90]. Continuing.")
    }

    cont.list <- accContinentNames(dbname)
    if (! is.na(continents)) {
        for (cont in continents) {
            if (! cont %in% names(cont.list)) {
                print(paste("Problem here. The continent '", cont, "' is not an accepted value. Please select a name from the following list.", sep=''))
                print(names(cont.list))
                return()
            }
        }
    }
    if (! is.na(countries)) {
        for (country  in countries) {
            if (! country %in% unlist(cont.list)) {
                print(paste("Problem here. The country '", country, "' is not an accepted value. Please select a name from the following list.", sep=''))
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
            print(paste("Problem here. No result for any of the combination continent x country.", sep=''))
        } else {
            print(paste("The database is composed of these stuff.", sep=''))
            res
        }
    }

##.Formatting data in the expected format --------------------------------------
    time <- df[, 1]
    time.name <- colnames(df)[1]
    taxa <- colnames(df)[-1]
    df <- df[, -1]

    if (is.na(as.vector(t(selectedTaxa))[1])) {
        selectedTaxa <- data.frame( matrix( rep(1, length(climate) * length(taxa)),
                                            ncol = length(climate)
                                           )
                                   )
        rownames(selectedTaxa) = taxa
        colnames(selectedTaxa) = climate
    }
    selectedTaxa <- cbind(selectedTaxa, as.character(rep("", length(taxa))), stringsAsFactors = FALSE)
    colnames(selectedTaxa)[ncol(selectedTaxa)] <- 'notes'

    if (sum(taxa %in% pse$ProxyName) != length(taxa)) {
        missing_taxa <- taxa[! (taxa %in% pse$ProxyName)]
        print(paste("The following", ifelse( length(missing_taxa) > 1,
                                             'taxa are in the input file and are',
                                             'taxon is in the input file and is'),
                    " not in the proxy_species_equivalency table."))
        print(paste(missing_taxa, collapse = ', '))
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
            for (tax in missing_taxa) {
                #selectedTaxa[tax, ncol(selectedTaxa)] <- as.character(selectedTaxa[tax, ncol(selectedTaxa)])
                selectedTaxa[tax, ] <- c(rep(0, length(climate)), as.factor('No association with vegetation'))
            }
        }
    }

    if (sum(unique(pse$ProxyName) %in% taxa) != length(unique(pse$ProxyName))) {
        missing_taxa <- unique(pse$ProxyName)[! (unique(pse$ProxyName) %in% taxa)]
        print(paste("The following", ifelse( length(missing_taxa) > 1,
                                             'taxa are in the proxy_species_equivalency file and are',
                                             'taxon is in the proxy_species_equivalency file and is'),
                    " not in the input table."))
        print(paste(missing_taxa, collapse = ', '))
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
            pse <- pse[pse$ProxyName %in% taxa, ]
            w <- which(pse$Level == 4)
            if (length(w) > 0) {
                print(paste("The following", ifelse(length(w)>1, 'taxa have', 'taxon has'),
                            "not been classified and will not contribute to the reconstruction.",
                            ifelse(length(w)>1, 'They', 'It'), "will still contribute",
                            "to the estimation of the weights if either 'normalisation' or",
                            "'percentages' have been selected."
                          )
                      )
                print(unique(pse$ProxyName[w]))
                for (tax in unique(pse$ProxyName[w])) {
                    selectedTaxa[tax, ] <- c(rep(0, length(climate)), as.character('No association with vegetation'))
                }
                pse <- pse[-w, ]
            }
        }
    }

    res <- crestObj(df, time, pse, taxaType, climate,
                            xmn, xmx, ymn, ymx,
                            continents, countries,
                            realms, biomes, ecoregions,
                            minGridCells,
                            bin_width, shape,
                            npoints,
                            geoWeighting,
                            climateSpaceWeighting,
                            selectedTaxa,
                            presenceThreshold,
                            taxWeight,
                            time.name
                          )

##.Getting list of species -----------------------------------------------------
    taxonID2proxy <- matrix(ncol = 2)
    colnames(taxonID2proxy) <- c("taxonID", "proxyName")
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
                    print(paste("No match for taxon ", paste(pse[w, 2:5], collapse = ', ')))
                    selectedTaxa[tax, ] <- c(rep(0, length(climate)), 'No correspondance with vegetation')
              }
            }
        }
    }
    taxonID2proxy <- taxonID2proxy[-1, ]
    taxonID2proxy <- taxonID2proxy[order(taxonID2proxy[, 'proxyName']), ]
    res$modelling$taxonID2proxy <- taxonID2proxy

    #Getting climates ----------------------------------------------------------
    climate_space <- getClimateSpace( climate,
                                      xmn, xmx, ymn, ymx,
                                      continents, countries,
                                      realms, biomes, ecoregions,
                                      dbname
                                     )
    colnames(climate_space)[-c(1,2)] <- climate
    res$modelling$climate_space <- climate_space


    distributions <- list()
    cat('Extracting data from the online database.\n     ')
    pb  <- utils::txtProgressBar(0, length(taxa), style=3, width = min(50, getOption("width")/2))
    pbi <- 1
    for(tax in taxa) {
        utils::setTxtProgressBar(pb, pbi)
        if (sum(as.numeric(selectedTaxa[tax, climate])) > 0) {
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
                print(paste0("Insufficient data points to calibrate a pdf for ", tax))
                distributions[[tax]] <- NA
                selectedTaxa[tax, ] <- c(rep(0, length(climate)), 'Not enough data points')
            }
        } else {
            distributions[[tax]] <- NA
        }
        pbi <- pbi + 1
    }
    res$modelling$distributions <- distributions
    close(pb)

    if (! unique(is.na(bin_width))) {
        bin_width <- as.data.frame(bin_width)
        rownames(bin_width) <- climate
    }
    if (! unique(is.na(shape))) {
        shape <- as.data.frame(shape)
        rownames(shape) <- climate
    }

    ccs <- list()
    xrange <- list()
    for (clim in climate) {
        ccs[[clim]] <- calib_clim_space(climate_space[, clim], bin_width[clim, ])
        xrange[[clim]] <- fit_xrange(ccs[[clim]], shape[clim, ], bin_width[clim, ], npoints)
    }

    pdfs <- list()
    for (tax in names(distributions)) {
        if (sum(as.numeric(selectedTaxa[tax, climate])) > 0) {
            pdfs[[tax]] <- list()
            for (clim in climate) {
                if (sum(as.numeric(selectedTaxa[tax, clim])) > 0) {
                    pdfs[[tax]][[clim]] <- list()
                    tmp <- xrange[[clim]]
                    pdfpol <- rep(0, npoints)
                    for(sp in unique(distributions[[tax]][, 'taxonid'])) {
                        w <- which(distributions[[tax]][, 'taxonid'] == sp)
                        tmp <- cbind( tmp,
                                      fit_pdfsp( climate = distributions[[tax]][w, clim],
                                                 ccs = ccs[[clim]],
                                                 bin_width = bin_width[clim, ],
                                                 shape = shape[clim, ],
                                                 xrange = xrange[[clim]],
                                                 use_ccs = climateSpaceWeighting
                                                )
                                     )
                        pdfpol <- pdfpol + tmp[, ncol(tmp)] * ifelse( geoWeighting,
                                                                      length(w),
                                                                      1
                                                                     )
                    }
                    pdfs[[tax]][[clim]][['pdfsp']] <- tmp[, -1]
                    pdfs[[tax]][[clim]][['pdfpol']] <- pdfpol / ifelse( geoWeighting,
                                                                        nrow(distributions[[tax]]),
                                                                        length(unique(distributions[[tax]][, 'taxonid']))
                                                                       )
                    pdfs[[tax]][[clim]][['pdfpol_log']] <- log(pdfs[[tax]][[clim]][['pdfpol']])
                } else {
                    pdfs[[tax]][[clim]] <- NA
                }
            }
        } else {
            pdfs[[tax]] <- NA
        }
    }
    res$modelling$pdfs <- pdfs

    if (tolower(taxWeight) == 'normalisation') {
        taxWeight <- normalise(df, col2convert=1:ncol(df))
    } else {
        if (tolower(taxWeight) == 'presence/absence') {
            taxWeight <- convert2presenceAbsence(df, presenceThreshold, col2convert=1:ncol(df))
        } else {
            if (tolower(taxWeight) == 'percentages') {
              taxWeight <- convert2percentages(df, col2convert=1:ncol(df))
            } else {
                taxWeight <- df
            }
        }
    }
    colnames(taxWeight) <- colnames(df)
    rownames(taxWeight) <- rownames(df)
    res$modelling$weights <- taxWeight


    reconstructions <- list()
    for (clim in climate) {
        if (sum(as.numeric(selectedTaxa[, clim])) > 0) {
            reconstructions[[clim]][['posterior']] <- matrix(rep(0, npoints * nrow(df)), ncol = npoints)
            reconstructions[[clim]][['optima']] <- rep(NA, nrow(df))
            for (s in 1:nrow(df)) {
                norm_factor <- 0
                for(tax in names(pdfs)) {
                    if (taxWeight[s, tax] > 0 & as.numeric(selectedTaxa[tax, clim]) > 0) {
                        norm_factor <- norm_factor + taxWeight[s, tax]
                        reconstructions[[clim]][['posterior']][s, ] <-
                                          reconstructions[[clim]][['posterior']][s, ] +
                                          pdfs[[tax]][[clim]][['pdfpol_log']]*taxWeight[s, tax]
                    }
                }
                reconstructions[[clim]][['posterior']][s, ] <-
                                        reconstructions[[clim]][['posterior']][s, ] * (1 / norm_factor)
                reconstructions[[clim]][['posterior']][s, ] <- exp(reconstructions[[clim]][['posterior']][s, ])
                reconstructions[[clim]][['posterior']][s, ] <-
                                        reconstructions[[clim]][['posterior']][s, ] /
                                        (  sum(reconstructions[[clim]][['posterior']][s,]) *
                                           (xrange[[clim]][2] -xrange[[clim]][1])
                                         )
                reconstructions[[clim]][['optima']][s] <- xrange[[clim]][which.max(reconstructions[[clim]][['posterior']][s, ])]

            }
            reconstructions[[clim]][['posterior']] <- rbind(xrange[[clim]], reconstructions[[clim]][['posterior']])
            reconstructions[[clim]][['optima']] <- cbind(time, reconstructions[[clim]][['optima']])

        } else {
            reconstructions[[clim]] <- NA
        }
    }
    for (tax in names(distributions)) {
        for (clim in climate) {
            if (as.numeric(selectedTaxa[tax, clim]) > 0) {
                pdfs[[tax]][[clim]][['pdfpol_log']] <- NULL
            }
        }
    }
    res$reconstructions <- reconstructions
    res
}
