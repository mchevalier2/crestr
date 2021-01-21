
#' Extract distributions from the database
#'
#' This function will extract the distributions of all the species composing each
#' taxon and return them as a list.
#'
#' @inheritParams crestObj
#' @inheritParams crest
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @param verbose A boolean to print non-essential comments on the terminal (default TRUE).
#' @return A crest() object containing the spatial distributions
#' @export
#' @examples
#' \dontrun{
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' data(crest_ex)
#' x <- crest.get_modern_data( df = crest_ex,
#'   pse = crest_ex_pse, taxaType = 0,
#'   climate = c("bio1", "bio12"),
#'   selectedTaxa = crest_ex_selection, dbname = "crest_example"
#' )
#' x
#' lapply(x$modelling$distributions, head)
#' }
#'
crest.get_modern_data <- function( pse, taxaType, climate,
                                   df = NA,
                                   #taxa.name = unique(pse[, 'ProxyName']),
                                   xmn = NA, xmx = NA, ymn = NA, ymx = NA,
                                   continents = NA, countries = NA,
                                   realms = NA, biomes = NA, ecoregions = NA,
                                   minGridCells = 20,
                                   selectedTaxa = NA,
                                   dbname = "gbif4crest_02",
                                   verbose=TRUE) {

    if(verbose) cat('\n## Prepping data for database extraction\n')

    if(verbose) cat('  <> Checking pse .......................... ')

    ## . Testing if the input variables are in the correct format ---------------
    if (!is.data.frame(pse)) {
        cat("[FAILED]\n  ERROR: 'pse' (proxy_species_equivalency) must be a data frame.\n")
        return()
    }

    pse <- pse[!is.na((pse[, 'ProxyName'])), ]
    pse <- pse[(pse[, 'ProxyName'] != ''), ]

    taxa.name <- c(unique(as.character(pse[, 'ProxyName'])))
    if (is.data.frame(df)) taxa.name <- unique(c(taxa.name, colnames(df)[-1]))

    taxa_to_ignore=c()
    for(tax in taxa.name) {
        if (! tax %in% pse[, 'ProxyName']) taxa_to_ignore=c(taxa_to_ignore, tax)
    }
    #if (length(taxa_to_ignore)) {
    #    cat(paste0('WARNING: The following taxa are not in the pollen species equivalence file and will be ignored.\n'))
    #    cat(c(paste(taxa_to_ignore, collapse=', '), '\n'))
    #}

    if(verbose) cat('[OK]\n  <> Checking climate variables ............ ')
    ## . Change the climate variable ID for the climate variable name -----------
    for (clim in 1:length(climate)) {
        climVar <- accClimateVariables()
        new_clim <- climate
        if (!(climate[clim] %in% climVar[, 1] | climate[clim] %in% climVar[, 2])) {
            cat(paste("[FAILED]\n  ERROR: The variable '", climate[clim], "' is not an accepted value. Please select a name or ID from the following list.\n", sep = ""))
            return()
        } else {
            defaultW <- getOption("warn")
            options(warn = -1)
            if (!is.na(as.numeric(climate[clim]))) {
                new_clim[clim] <- as.character(climVar[which(climVar[, 1] == as.numeric(climate[clim])), 2])
            }
            options(warn = defaultW)
        }
    }
    climate <- new_clim

    if(verbose) cat('[OK]\n  <> Checking taxaType ..................... ')
    if(taxaType > 6 | taxaType < 0) {
        cat("[FAILED]\n  ERROR: taxaType should be an integer between 0 and 6. See ?crest.get_modern_data for more information.\n")
        return()
    }

    estimate_xlim <- estimate_ylim <- FALSE
    if(verbose) cat('[OK]\n  <> Checking coordinates .................. ')
    if (xmn < -180 | is.na(xmn) | xmx > 180 | is.na(xmx)) {
        xmn <- max(xmn, -180, na.rm=TRUE)
        xmx <- min(xmx, 180, na.rm=TRUE)
        estimate_xlim <- TRUE
        #cat("WARNING: [xmn; xmx] range larger than accepted values [-180; 180]. Adapting and continuing.\n")
    }
    if (xmn >= xmx) {
        #cat("WARNING: xmn is larger than xmx. Inverting the two values and continuing.\n")
        tmp <- xmn
        xmn <- xmx
        xmx <- tmp
    }

    if (ymn < -90| is.na(ymn)  | ymx > 90 | is.na(ymx) ) {
        ymn <- max(ymn, -90, na.rm=TRUE)
        ymx <- min(ymx, 90, na.rm=TRUE)
        estimate_ylim <- TRUE
        #cat("WARNING: [ymn; ymx] range larger than accepted values [-90; 90]. Adapting and continuing.\n")
    }
    if (ymn >= ymx) {
        #cat("WARNING: ymn is larger than ymx. Inverting the two values and continuing.\n")
        tmp <- ymn
        ymn <- ymx
        ymx <- tmp
    }

    if(verbose) cat('[OK]\n  <> Checking continent and country names .. ')
    cont.list <- accContinentNames(dbname)
    if (!is.na(continents)) {
        for (cont in continents) {
            if (!cont %in% names(cont.list)) {
                cat(paste("[FAILED]\n  ERROR: The continent '", cont, "' is not an accepted value. Please select a name from the following list.\n", sep = ""))
                print(names(cont.list))
                return()
            }
        }
    }
    if (!is.na(countries)) {
        for (country in countries) {
            if (!country %in% unlist(cont.list)) {
                cat(paste("[FAILED]\n  ERROR: The country '", country, "' is not an accepted value. Please select a name from the following list.\n", sep = ""))
                print(cont.list)
                return()
            }
        }
    }

    if (!is.na(countries[1]) | !is.na(continents[1])) {
        res <- dbRequest(
          paste0(
            "SELECT DISTINCT continent, countryname, count(*) FROM geo_qdgc WHERE ",
            ifelse(is.na(continents)[1], "", paste0("continent IN ('", paste(continents, collapse = "', '"), "') ")),
            ifelse(is.na(continents)[1] | is.na(countries)[1], "", "AND "),
            ifelse(is.na(countries)[1], "", paste0("countryname IN ('", paste(countries, collapse = "', '"), "') ")),
            " GROUP BY continent, countryname"
          ),
          dbname
        )
        if (length(res) == 0) {
            cat(paste("Problem here. No result for any of the combination continent x country.\n", sep = ""))
        } else {
            #cat(paste("The database is composed of these countries.\n", sep = ""))
            res
        }
    }

    if(verbose) cat('[OK]\n  <> Checking/Defining selectedTaxa ........ ')
    if (is.na(as.vector(t(selectedTaxa))[1])) {
        selectedTaxa <- data.frame(matrix(rep(1, length(climate) * length(taxa.name)),
          ncol = length(climate)
        ))
        rownames(selectedTaxa) <- taxa.name
        colnames(selectedTaxa) <- climate
    }

    taxa_notes <- list()
    for (tax in taxa_to_ignore) {
        taxa_notes[[tax]] <- 'Taxon not in the proxy_species_equivalency table.'
        selectedTaxa[tax, climate] <- rep(-1, length(climate))
    }
    taxa.name <- taxa.name[taxa.name %in% rownames(selectedTaxa[apply(selectedTaxa, 1, sum)>=0, ])]

    w <- !(taxa.name %in% rownames(selectedTaxa))
    if (sum(w) > 0) {
        #cat("WARNING: ymn is larger than ymx. Inverting the two values and continuing.\n")
        for(w in which(!(taxa.name %in% rownames(selectedTaxa)))) {
            selectedTaxa <- rbind(selectedTaxa, rep(1, length(climate)))
            rownames(selectedTaxa)[nrow(selectedTaxa)] <- taxa.name[w]
            taxa_notes[[taxa.name[w]]] <- 'Not present in the original selectedTaxa table. Added by default as 1s.'
        }
    }

    if(verbose) cat('[OK]\n  <> Checking the pse table ................ ')
    ## . Formatting data in the expected format ---------------------------------
    if (sum(unique(pse$ProxyName) %in% taxa.name) != length(unique(pse$ProxyName))) {
        missing_taxa <- unique(pse$ProxyName)[!(unique(pse$ProxyName) %in% taxa.name)]
        #cat(paste(
        #  "WARNING: The following",
        #  ifelse(length(missing_taxa) > 1,
        #         "taxa are in the proxy_species_equivalency file and are",
        #         "taxon is in the proxy_species_equivalency file and is"
        #  ),
        #  " not in the input table.\n"
        #))
        #cat(paste(missing_taxa, collapse = ", "))
        #cat("\n")
    }

    w <- (pse$Level == 4)
    if (sum(w) > 0) {
        #cat(paste("WARNING:",
        #  "The following", ifelse(sum(w) > 1, "taxa have", "taxon has"),
        #  "not been classified and will not directly contribute to the reconstruction.",
        #  ifelse(sum(w) > 1, "Their", "Its"), "presence will still contribute",
        #  "to the estimation of the weights if either 'normalisation' or",
        #  "'percentages' have been selected.\n"
        #))
        #cat(unique(pse$ProxyName[w]))
        #cat("\n")
        for (tax in unique(pse$ProxyName[w])) {
            selectedTaxa[tax, ] <- rep(-1, length(climate))
            taxa_notes[[tax]] <- "No association with vegetation"
        }
        pse <- pse[!w, ]
    }
    taxa.name <- taxa.name[taxa.name %in% rownames(selectedTaxa[apply(selectedTaxa, 1, sum)>=0, ])]


    if(verbose) {
      cat('[OK]\n  <> Extracting taxon species .............. \r')
    }

    crest <- crestObj(taxa.name, pse=pse, taxaType=taxaType, climate=climate,
        xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx,
        continents=continents, countries=countries,
        realms=realms, biomes=biomes, ecoregions=ecoregions,
        selectedTaxa = selectedTaxa
    )
    crest$misc[['taxa_notes']] <- taxa_notes

    if (is.data.frame(df)) {
        crest$inputs$x <- df[, 1]
        crest$inputs$x.name <- colnames(df)[1]
        crest$inputs$taxa.name <- taxa.name
        crest$inputs$df <- df[, -1]

        w <- (apply(crest$inputs$df, 2, sum) == 0)
        if (sum(w) > 0) {
            for (tax in colnames(crest$inputs$df)[w]) {
                crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                crest$misc$taxa_notes[[tax]] <- "All percentages equal to 0."
            }
        }

        w <- (! taxa.name %in% colnames(df)[-1])
        if (sum(w) > 0) {
            for (tax in taxa.name[w]) {
                crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                crest$misc$taxa_notes[[tax]] <- "Taxon not present in the data file."
            }
        }

    }


    taxonID2proxy <- data.frame("taxonID" = NA, "proxyName" = NA, stringsAsFactors = FALSE)
    pse$Level     <- as.numeric(as.character(pse$Level))
    pse$Family    <- as.character(pse$Family)
    pse$Genus     <- as.character(pse$Genus)
    pse$Species   <- as.character(pse$Species)
    pse$ProxyName <- as.character(pse$ProxyName)

    pbi <- 100
    for (taxLevel in 1:3) {
        for (tax in pse$ProxyName[ pse$Level == taxLevel ]) {
            if(verbose) {
                cat(paste0('  <> Extracting taxon species .............. ', stringr::str_pad(paste0(round(pbi / length(pse$ProxyName)),'%\r'), width=4, side='left')))
                utils::flush.console()
            }
            for (w in which(pse$ProxyName == tax)) {
                taxonIDs <- getTaxonID(
                  pse$Family[w],
                  pse$Genus[w],
                  pse$Species[w],
                  taxaType,
                  dbname
                )
                if (length(taxonIDs) > 0) {
                    existingTaxa <- taxonIDs %in% taxonID2proxy[, "taxonID"]
                    # If the taxon was first assigned to higher group, Reassign.
                    if (sum(existingTaxa) > 0) {
                        taxonID2proxy[taxonID2proxy[, "taxonID"] %in% taxonIDs, "proxyName"] <- tax
                    }
                    if (sum(existingTaxa) != length(taxonIDs)) {
                        taxonID2proxy <- rbind(
                          taxonID2proxy,
                          data.frame("taxonID" = taxonIDs[!existingTaxa],
                                     "proxyName" = rep(tax, sum(!existingTaxa)),
                                     stringsAsFactors = FALSE)
                        )
                    }
                } else {
                    #cat(paste("WARNING: No match for taxon ", paste(pse[w, 2:5], collapse = ", "), "\n"))
                    if (tax %in% crest$inputs$taxa.name) {
                      crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                      crest$misc$taxa_notes[[tax]] <- "No correspondance with vegetation"
                    }
                }
            }
            pbi <- pbi + 100
        }
    }
    crest$inputs$taxa.name <- crest$inputs$taxa.name[crest$inputs$taxa.name %in% rownames(crest$inputs$selectedTaxa[apply(crest$inputs$selectedTaxa, 1, sum)>=0, ])]


    if(verbose) {
      cat('  <> Extracting taxon species .............. [OK]\n  <> Extracting species distributions ...... \r')
    }
    pbi <- 100
    taxonID2proxy <- taxonID2proxy[-1, ]
    taxonID2proxy <- taxonID2proxy[order(taxonID2proxy[, "proxyName"]), ]
    crest$modelling$taxonID2proxy <- taxonID2proxy

    distributions <- list()

    for (tax in crest$inputs$taxa.name) {
        taxIDs <- taxonID2proxy[taxonID2proxy[, "proxyName"] == tax, 1]
        if (length(taxIDs) == 0) {
            crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
            crest$misc$taxa_notes[[tax]] <- paste(crest$misc$taxa_notes[[tax]], "No species corresponding to the proxy name.", sep='; ')
        }
        if(verbose) {
            cat(paste0('  <> Extracting species distributions ...... ', stringr::str_pad(paste0(round(pbi / length(taxa.name)),'%\r'), width=4, side='left')))
            utils::flush.console()
        }

        if (sum(crest$inputs$selectedTaxa[tax, climate]>=0) > 0) {
            distributions[[tax]] <- getDistribTaxa(
              taxIDs, climate,
              xmn, xmx, ymn, ymx,
              continents, countries,
              realms, biomes, ecoregions,
              dbname
            )
            if (nrow(distributions[[tax]]) == 0) {
                #cat(paste0("WARNING: Insufficient data points to calibrate a pdf for ", tax, "\n"))
                #print(extent_taxa)
                distributions[[tax]] <- NA
                crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                crest$misc$taxa_notes[[tax]] <- "No data point available in the study area."
            }else{
                extent_taxa <- table(distributions[[tax]][, 1])
                extent_taxa_id <- as.numeric(names(extent_taxa)[extent_taxa >= minGridCells])
                distributions[[tax]] <- distributions[[tax]][distributions[[tax]][, 1] %in% extent_taxa_id, ]
                if(nrow(distributions[[tax]]) == 0) {
                    distributions[[tax]] <- NA
                    crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                    crest$misc$taxa_notes[[tax]] <- "Present but insufficient data in the study area to fit a pdf"
                }
            }
        }
        pbi <- pbi + 100
    }
    crest$inputs$taxa.name <- crest$inputs$taxa.name[crest$inputs$taxa.name %in% rownames(crest$inputs$selectedTaxa[apply(crest$inputs$selectedTaxa, 1, sum)>=0, ])]

    crest$modelling$distributions <- distributions
    if(verbose) {
      cat('  <> Extracting species distributions ...... [OK]\n  <> Extracting climate space .............. ')
    }
    climate_space <- getClimateSpace(
      crest$parameters$climate,
      crest$parameters$xmn, crest$parameters$xmx, crest$parameters$ymn, crest$parameters$ymx,
      crest$parameters$continents, crest$parameters$countries,
      crest$parameters$realms, crest$parameters$biomes, crest$parameters$ecoregions,
      dbname
    )
    colnames(climate_space)[-c(1, 2)] <- crest$parameters$climate
    crest$modelling$climate_space <- climate_space

    if (estimate_xlim) {
      resol <- sort(unique(diff(sort(unique(crest$modelling$climate_space[,1])))))[1] / 2.0
      xx <- range(climate_space[, 1])
      crest$parameters$xmn <- xx[1] - resol
      crest$parameters$xmx <- xx[2] + resol
    }

    if (estimate_ylim) {
      resol <- sort(unique(diff(sort(unique(crest$modelling$climate_space[,1])))))[1] / 2.0
      yy <- range(climate_space[, 2])
      crest$parameters$ymn <- yy[1] - resol
      crest$parameters$ymx <- yy[2] + resol
    }

    if(verbose) {
      cat('[OK]\n')
      cat(paste0('## Data extraction completed.\n'))
    }
    crest
}
