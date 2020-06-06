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
#' @return The parameters to be used by crest()
#' @export
#' @examples
#' \dontrun{
#' db <- connect_online()
#' }

crest.init <- function(df, pse, taxaType, climate, xmn = -180, xmx = 180, ymn = -90, ymx = 90, continents=NA, countries=NA, realms=NA, biomes=NA, ecoregions=NA, minGridCells=20, bin_width=NA, shape=NA, npoints = 500, geoWeighting = TRUE, climateSpaceWeighting = TRUE, selectedTaxa = NA, presenceThreshold = 0, taxWeight = 'normalisation' ) {
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


    if (is.na(selectedTaxa)) {
        selectedTaxa <- data.frame( matrix( rep(1, length(climate) * length(taxa)),
                                           ncol = length(climate)
                                          )
                                   )
        rownames(selectedTaxa) = taxa
        colnames(selectedTaxa) = climate
    }

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
                    existingTaxa <- taxonIDs %in% taxonID2proxy[, 'taxonID']
                    if (sum(existingTaxa) > 0) {
                        taxonID2proxy[taxonID2proxy[, 'taxonID'] %in% taxonIDs, 'proxyName'] <- tax
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
    taxonID2proxy <- taxonID2proxy[-1, ]
    taxonID2proxy <- taxonID2proxy[order(taxonID2proxy[, 'proxyName']), ]

    climate=c('bio1','ai')
    xmx=-50; xmn=-85; ymn=-15; ymx=10
    continents=countries=realms=biomes=ecoregions=NA
    shape=c('normal','lognormal')
    bin_width=c(2,2500)
    #Getting climates ----------------------------------------------------------
    climate_space <- getClimateSpace( climate,
                                      xmn, xmx, ymn, ymx,
                                      continents, countries,
                                      realms, biomes, ecoregions
                                     )

    distributions <- list()
    for(tax in unique(taxonID2proxy[, 'proxyName'])){
        print(tax)
        distributions[[tax]] <- getDistribTaxa( taxIDs = taxonID2proxy[taxonID2proxy[, 'proxyName'] == tax, 1],
                                                climate,
                                                xmn, xmx, ymn, ymx,
                                                continents, countries,
                                                realms, biomes, ecoregions
                                               )
        extent_taxa <- table(distributions[[tax]][,1])
        extent_taxa <- as.numeric(names(extent_taxa)[extent_taxa >= minGridCells])
        distributions[[tax]] <- distributions[[tax]][distributions[[tax]][, 1] %in% extent_taxa, ]
        if (nrow(distributions[[tax]]) == 0) {
            print(paste0("Insufficient data points to calibrate a pdf for ", tax))
            distributions[[tax]] <- NULL
        }
    }
    save(distributions, file="/Users/mchevali1/GitHub/Rpackages/_crestr_testdata/LagunaFuquene_species_distributions.RData")
    load("/Users/mchevali1/GitHub/Rpackages/_crestr_testdata/LagunaFuquene_species_distributions.RData")

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
        pdfs[[tax]] <- list()
        for (clim in climate) {
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
        }
    }

    if (tolower(taxWeight) == 'percentages') {
        taxWeight <- convert2percentages(df)
    } else (
        if (tolower(taxWeight) == 'presence/absence') {
            taxWeight <- convert2presenceAbsence(df, presenceThreshold)
        } else {
            if (tolower(taxWeight) == 'normalisation') {
                taxWeight <- normalise(df)
            } else {
                taxWeight <- df
            }
        }
    )
    colnames(taxWeight) <- colnames(df)

    reconstructions <- list()
    for (clim in climate) {
        reconstructions[[clim]] <- rep(1, npoints)
        for(tax in names(pdfs)) {
            norm_factor <- 0
            if (taxWeight[tax, clim] > 0 & selectedTaxa[tax, clim] > 1) {
                norn_factor <- norm_factor + taxWeight[tax, clim]
                reconstructions[[clim]] <- reconstructions[[clim]]
                                             *
                                           pdfs[[tax]][[clim]][['pdfpol']]**taxWeight[tax, clim]
            }
        }
        reconstructions[[clim]] <- reconstructions[[clim]]**(1 / sum(norm_factor))
    }



}
