
#' Extract distributions from the database
#'
#' This function will extract the distributions of all the species composing each
#' taxon and return them as a list.
#'
#' @inheritParams crestObj
#' @inheritParams crest
#' @param dbname The name of the database. Default is \code{'gbif4crest_02'}.
#' @param verbose A boolean to print non-essential comments on the terminal
#'        (default \code{TRUE}).
#' @return A \code{\link{crestObj}} object containing the spatial distributions.
#' @export
#' @examples
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' data(crest_ex)
#' x <- crest.get_modern_data( df = crest_ex,
#'   pse = crest_ex_pse, taxaType = 0,
#'   climate = c("bio1", "bio12"),
#'   selectedTaxa = crest_ex_selection, dbname = "crest_example",
#'   verbose = FALSE
#' )
#' x
#' lapply(x$modelling$distributions, head)
#'
crest.get_modern_data <- function( pse, taxaType, climate,
                                   df = NA, ai.sqrt = FALSE,
                                   xmn = NA, xmx = NA, ymn = NA, ymx = NA,
                                   continents = NA, countries = NA,
                                   basins = NA, sectors = NA,
                                   realms = NA, biomes = NA, ecoregions = NA,
                                   minGridCells = 20,
                                   selectedTaxa = NA,
                                   site_info = c(NA, NA),
                                   site_name = NA,
                                   dbname = "gbif4crest_02",
                                   verbose=TRUE) {

    if(verbose) cat('\n## Prepping data for database extraction\n')

    if(verbose) cat('  <> Checking pse .......................... ')

    ##. Testing if the input variables are in the correct format ---------------
    if (!is.data.frame(pse)) {
        cat("[FAILED]\n\n")
        stop("The 'pse' variable (proxy_species_equivalency) must be a data frame.\n")
        return()
    }

    pse <- pse[!is.na((pse[, 'ProxyName'])), ]
    pse <- pse[(pse[, 'ProxyName'] != ''), ]

    taxa.name <- unique(as.character(pse[, 'ProxyName']))
    if (is.data.frame(df)) taxa.name <- unique(c(taxa.name, colnames(df)[-1]))

    taxa_to_ignore=c()
    for(tax in taxa.name) {
        if (! tax %in% pse[, 'ProxyName']) taxa_to_ignore=c(taxa_to_ignore, tax)
    }

    if(verbose) cat('[OK]\n  <> Checking climate variables ............ ')
    ## . Change the climate variable ID for the climate variable name -----------
    for (clim in 1:length(climate)) {
        climVar <- accClimateVariables()
        new_clim <- climate
        if (!(climate[clim] %in% climVar[, 1] | climate[clim] %in% climVar[, 2])) {
            cat("[FAILED]\n\n")
            stop(paste0("The variable '", climate[clim], "' is not an accepted value. Check the list of accepted values using 'accClimateVariables()'.\n"))
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
        cat("[FAILED]\n\n")
        stop("'taxaType' should be an integer between 0 and 6. See ?crest.get_modern_data for more information.")
    }

    if(verbose) cat('[OK]\n  <> Checking coordinates .................. ')
    coords        <- check_coordinates(xmn, xmx, ymn, ymx)
    xmn           <- coords[1]
    xmx           <- coords[2]
    ymn           <- coords[3]
    ymx           <- coords[4]
    estimate_xlim <- coords[5]
    estimate_ylim <- coords[6]


    #' @param taxaType A numerical index (between 1 and 6) to define the type of
    #'        palaeoproxy used: 1 for plants, 2 for beetles, 3 for chironomids,
    #'        4 for foraminifers, 5 for diatoms and 6 for rodents. The example
    #'        dataset uses taxaType=0 (pseudo-data). Default is 1.


    if (taxaType %in% c(1, 2, 3, 6)) {
        if(verbose) cat('[OK]\n  <> Checking continent and country names .. ')
        cont.list <- accCountryNames()
        if (!is.na(continents[1])) {
            for (cont in continents) {
                if (!cont %in% names(cont.list)) {
                    stop(paste0("The continent '", cont, "' is not an accepted value. Please select a name from this list: '",paste(names(cont.list), collapse="', '"),"'.\n"))
                }
            }
        }
        if (!is.na(countries)[1]) {
            for (country in countries) {
                if (!country %in% unlist(cont.list)) {
                    acc_vals <- ifelse(is.na(continents[1]), "", paste0("c('",paste(continents, collapse="', '"),"')"))
                    stop(paste0("The country '", country, "' is not an accepted value. Get the list of accepted values using 'accCountryNames(",acc_vals,")'.\n"))
                }
            }
        }

        if (!is.na(countries[1]) | !is.na(continents[1])) {
            res <- dbRequest(
              paste0(
                "SELECT DISTINCT continent, name, count(*) FROM geopolitical_units WHERE ",
                ifelse(is.na(continents)[1], "", paste0("continent IN ('", paste(continents, collapse = "', '"), "') ")),
                ifelse(is.na(continents)[1] | is.na(countries)[1], "", "AND "),
                ifelse(is.na(countries)[1], "", paste0("name IN ('", paste(countries, collapse = "', '"), "') ")),
                " GROUP BY continent, name"
              ),
              dbname
            )
            if (length(res) == 0) {
                cat(paste("Problem here. No result for any of the combination continent x country.\n", sep = ""))
            } else {
                res
            }
        }

    } else {
        if(verbose) cat('[OK]\n  <> Checking basin and sector names ....... ')
        basin.list <- accBasinNames()
        if (!is.na(basins[1])) {
            for (bas in basins) {
                if (!bas %in% names(basin.list)) {
                    stop(paste0("The basin '", bas, "' is not an accepted value. Please select a name from this list: '",paste(names(basin.list), collapse="', '"),"'.\n"))
                }
            }
        }
        if (!is.na(sectors)[1]) {
            for (sector in sectors) {
                if (!sector %in% unlist(basin.list)) {
                    acc_vals <- ifelse(is.na(sectors[1]), "", paste0("c('",paste(basins, collapse="', '"),"')"))
                    stop(paste0("The sector '", sector, "' is not an accepted value. Get the list of accepted values using 'accBasinNames(",acc_vals,")'.\n"))
                }
            }
        }

        if (!is.na(basins[1]) | !is.na(sectors[1])) {
            res <- dbRequest(
              paste0(
                "SELECT DISTINCT basin, name, count(*) FROM geopolitical_units WHERE ",
                ifelse(is.na(basins)[1], "", paste0("basin IN ('", paste(basins, collapse = "', '"), "') ")),
                ifelse(is.na(basins)[1] | is.na(sectors)[1], "", "AND "),
                ifelse(is.na(sectors)[1], "", paste0("name IN ('", paste(sectors, collapse = "', '"), "') ")),
                " GROUP BY basin, name"
              ),
              dbname
            )
            if (length(res) == 0) {
                cat(paste("Problem here. No result for any of the combination basin x sector.\n", sep = ""))
            } else {
                res
            }
        }
    }

    if(verbose) cat('[OK]\n  <> Checking realm/biome/ecoregion names .. ')
    realm.list <- accRealmNames()
    if (!is.na(realms[1])) {
        for (realm in realms) {
            if (!realm %in% names(realm.list)) {
                stop(paste0("The realm '", realm, "' is not an accepted value. Please select a name from this list: '", paste(names(realm.list), collapse="', '"),"'.\n"))
            }
        }
    }
    if (taxaType %in% c(1, 2, 3, 6)){ # For all the terrestrial taxa
        if (!is.na(biomes)[1]) {
            for (biome in biomes) {
                if (!biome %in% unique(unlist(lapply(realm.list, function(x) return(unique(x[, 1])))))) {
                    acc_vals <- ifelse(is.na(realms[1]), "", paste0("c('",paste(realms, collapse="', '"),"')"))
                    stop(paste0("The realm '", realm, "' is not an accepted value. Get the list of accepted values using 'accRealmNames(",acc_vals,")'.\n"))
                }
            }
        }
        if (!is.na(ecoregions)[1]) {
            for (ecoregion in ecoregions) {
                if (!ecoregion %in% unique(unlist(lapply(realm.list, function(x) return(unique(x[, 2])))))) {
                    acc_vals <- ifelse(is.na(realms[1]), "", paste0("c('",paste(realms, collapse="', '"),"')"))
                    stop(paste0("The ecoregion '", ecoregion, "' is not an accepted value. Get the list of accepted values using 'accRealmNames(",acc_vals,")'.\n"))
                }
            }
        }

        if (!is.na(realms[1]) | !is.na(biomes[1]) | !is.na(ecoregions[1])) {
            s_realms     <- ifelse(is.na(realms)[1], '',  paste0("realm IN ('", paste(realms, collapse = "', '"), "') "))
            s_biomes     <- ifelse(is.na(biomes)[1], '',  paste0("biome IN ('", paste(biomes, collapse = "', '"), "') "))
            s_ecoregions <- ifelse(is.na(ecoregions)[1], '',  paste0("ecoregion IN ('", paste(ecoregions, collapse = "', '"), "') "))

            res <- dbRequest(
              paste0(
                "SELECT DISTINCT realm, biome, ecoregion, count(*) FROM biogeography WHERE ",
                s_realms,
                ifelse(s_realms != '' & ( s_biomes != '' | s_ecoregions != ''), ' AND ', ''),
                s_biomes,
                ifelse(s_biomes != '' & s_ecoregions != '', ' AND ', ''),
                s_ecoregions,
                " GROUP BY realm, biome,ecoregion"
              ),
              dbname
            )
            if (length(res) == 0) {
                cat(paste("Problem here. No result for any of the combination realm x biome x ecoregion .\n", sep = ""))
            } else {
                res
            }
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
        message <- 'Taxon not in the proxy_species_equivalency table.'
        if (! message %in% names(taxa_notes)) {
            taxa_notes[[message]] <- c()
            warning(paste0("One or more taxa were are not in the proxy-species equivalence table and have been ignored. Check 'x$misc$taxa_notes' for details."))
        }
        taxa_notes[[message]] <- append(taxa_notes[[message]], tax)
        selectedTaxa <- rbind(selectedTaxa, rep(-1, length(climate)))
        rownames(selectedTaxa)[nrow(selectedTaxa)] <- tax
    }
    taxa.name <- taxa.name[taxa.name %in% rownames(selectedTaxa)[apply(selectedTaxa, 1, sum)>=0]]

    w <- !(taxa.name %in% rownames(selectedTaxa))
    if (sum(w) > 0) {
        for(w in which(!(taxa.name %in% rownames(selectedTaxa)))) {
            selectedTaxa <- rbind(selectedTaxa, rep(1, length(climate)))
            rownames(selectedTaxa)[nrow(selectedTaxa)] <- taxa.name[w]
            for (tax in taxa_to_ignore) {
                message <- 'Not present in the original selectedTaxa table. Added by default as 1s.'
                if (! message %in% names(taxa_notes)) {
                    taxa_notes[[message]] <- c()
                    warning(paste0("One or more taxa were are not in the selectedTaxa table. They have been added but are not selected for any variable. Check 'x$misc$taxa_notes' for details."))
                }
                taxa_notes[[message]] <- append(taxa_notes[[message]], tax)
                selectedTaxa[tax, climate] <- rep(0, length(climate))
            }
        }
    }

    if(verbose) cat('[OK]\n  <> Checking the pse table ................ ')
    ## . Formatting data in the expected format ---------------------------------

    w <- (pse$Level == 4)
    if (sum(w) > 0) {
        for (tax in unique(pse$ProxyName[w])) {
            selectedTaxa <- rbind(selectedTaxa, rep(-1, length(climate)))
            rownames(selectedTaxa)[nrow(selectedTaxa)] <- tax
            message <- "No association between the proxy names and species"
            if (! message %in% names(taxa_notes)) {
                taxa_notes[[message]] <- c()
                warning(paste0("One or more taxa were not associated with species. Check 'x$misc$taxa_notes' for details."))
            }
            taxa_notes[[message]] <- append(taxa_notes[[message]], tax)
        }
        pse <- pse[!w, ]
    }
    taxa.name <- taxa.name[taxa.name %in% rownames(selectedTaxa)[apply(selectedTaxa, 1, sum)>=0]]

    if(verbose) {
      cat('[OK]\n  <> Extracting taxon species .............. \r')
    }

    crest <- crestObj(taxa.name, pse=pse, taxaType=taxaType, climate=climate,
        xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx,
        continents=continents, countries=countries,
        basins=basins, sectors=sectors,
        realms=realms, biomes=biomes, ecoregions=ecoregions,
        selectedTaxa = selectedTaxa,
        dbname=dbname
    )
    crest$misc[['taxa_notes']] <- taxa_notes
    crest$misc$site_info <- list()
    crest$misc$site_info[['long']]   <- site_info[1]
    crest$misc$site_info[['lat']]    <- site_info[2]
    crest$misc$site_info[['site_name']]    <- site_name
    if((!is.na(crest$misc$site_info[['long']])) & (!is.na(crest$misc$site_info[['lat']]))) {
        resol <- ifelse(dbname == 'crest_example', 0.5, 0.25)
        crest$misc$site_info[['climate']] <- climate_from_xy(crest$misc$site_info[['long']],
                                                             crest$misc$site_info[['lat']],
                                                             crest$parameters$climate,
                                                             resol = resol,
                                                             dbname = crest$misc$dbname)
    }

    if (is.data.frame(df)) {
        crest$inputs$x <- df[, 1]
        crest$inputs$x.name <- colnames(df)[1]
        crest$inputs$taxa.name <- taxa.name
        crest$inputs$df <- df[, -1]

        if(unique(is.numeric(crest$inputs$x))) {
            crest$inputs$df <- crest$inputs$df[order(crest$inputs$x), ]
            crest$inputs$x  <- crest$inputs$x[order(crest$inputs$x)]
        }

        w <- (apply(crest$inputs$df, 2, sum) == 0)
        if (sum(w) > 0) {
            for (tax in colnames(crest$inputs$df)[w]) {
                crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                message <- "All percentages equal to 0."
                if (! message %in% names(crest$misc[['taxa_notes']])) {
                    crest$misc[['taxa_notes']][[message]] <- c()
                    warning(paste0("The percentages of one or more taxa were always 0 and have been removed accordingly. Check 'x$misc$taxa_notes' for details."))
                }
                crest$misc[['taxa_notes']][[message]] <- append(crest$misc[['taxa_notes']][[message]], tax)
            }
        }

        w <- (! taxa.name %in% colnames(df)[-1])
        if (sum(w) > 0) {
            for (tax in taxa.name[w]) {
                crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                message <- "Taxon not recorded in the data file."
                if (! message %in% names(crest$misc[['taxa_notes']])) {
                    crest$misc[['taxa_notes']][[message]] <- c()
                    warning(paste0("One or more taxa were are not recorded in the data file. Check 'x$misc$taxa_notes' for details."))
                }
                crest$misc[['taxa_notes']][[message]] <- append(crest$misc[['taxa_notes']][[message]], tax)
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
        for (tax in unique(pse$ProxyName[ pse$Level == taxLevel ])) {
            if(verbose) {
                cat(paste0('  <> Extracting taxon species .............. ', stringr::str_pad(paste0(round(pbi / length(pse$ProxyName)),'%\r'), width=4, side='left')))
                utils::flush.console()
            }
            for (w in which(pse$ProxyName == tax & pse$Level == taxLevel)) {
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
                      message <- "No correspondance with specific species"
                      if (! message %in% names(crest$misc[['taxa_notes']])) {
                          crest$misc[['taxa_notes']][[message]] <- as.data.frame(matrix(0, ncol=5, nrow=0))
                          colnames(crest$misc[['taxa_notes']][[message]]) <- colnames(pse)
                          warning(paste0("The classification of one or more taxa into species was not successful. Check 'x$misc$taxa_notes' for details."))
                      }
                      crest$misc[['taxa_notes']][[message]] <- rbind(crest$misc[['taxa_notes']][[message]], pse[w, ])
                    }
                }
            }
            pbi <- pbi + 100
        }
    }
    crest$inputs$taxa.name <- crest$inputs$taxa.name[crest$inputs$taxa.name %in% rownames(crest$inputs$selectedTaxa)[apply(crest$inputs$selectedTaxa, 1, sum)>=0]]


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
            message <- "No species remained associated with the proxy name at the end of the classification."
            if (! message %in% names(crest$misc[['taxa_notes']])) {
                crest$misc[['taxa_notes']][[message]] <- c()
                warning(paste0("For one or more taxa, no species remained associated with the proxy name at the end of the classification. Check 'x$misc$taxa_notes' for details."))
            }
            crest$misc[['taxa_notes']][[message]] <- append(crest$misc[['taxa_notes']][[message]], tax)
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
              basins, sectors,
              realms, biomes, ecoregions,
              dbname
            )
            if (nrow(distributions[[tax]]) == 0) {
                distributions[[tax]] <- NA
                crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                message <- "No data point available in the study area."
                if (! message %in% names(crest$misc[['taxa_notes']])) {
                    crest$misc[['taxa_notes']][[message]] <- c()
                    warning(paste0("No data were available within the study area for one or more taxa. Check 'x$misc$taxa_notes' for details."))
                }
                crest$misc[['taxa_notes']][[message]] <- append(crest$misc[['taxa_notes']][[message]], tax)
            }else{
                ##++> Clean data here. Or in the request before.
                extent_taxa <- table(distributions[[tax]][, 'taxonid'])
                extent_taxa_id <- as.numeric(names(extent_taxa)[extent_taxa >= minGridCells])
                distributions[[tax]] <- distributions[[tax]][distributions[[tax]][, 'taxonid'] %in% extent_taxa_id, ]
                if(nrow(distributions[[tax]]) == 0) {
                    distributions[[tax]] <- NA
                    crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                    message <- "Present but insufficient data in the study area to fit a pdf"
                    if (! message %in% names(crest$misc[['taxa_notes']])) {
                        crest$misc[['taxa_notes']][[message]] <- c()
                        warning(paste0("An insufficient amount of calibration data points was available within the study area for one or more taxa. Consider reducing 'minGridCells'. Check 'x$misc$taxa_notes' for details."))
                    }
                    crest$misc[['taxa_notes']][[message]] <- append(crest$misc[['taxa_notes']][[message]], tax)
                }
            }
        }
        pbi <- pbi + 100
    }
    crest$inputs$taxa.name <- crest$inputs$taxa.name[crest$inputs$taxa.name %in% rownames(crest$inputs$selectedTaxa)[apply(crest$inputs$selectedTaxa, 1, sum)>=0]]

    class_names <- rep(NA, nrow(crest$inputs$pse))
    if (crest$parameters$taxaType == 1) {
        for (tax in crest$inputs$taxa.name) {
            for(w in which(crest$inputs$pse[ ,'ProxyName'] == tax)) {
                class_names[w] <- getTaxonomy(    family = crest$inputs$pse[w, 'Family'],
                                                   genus = crest$inputs$pse[w, 'Genus'],
                                                 species = crest$inputs$pse[w, 'Species'],
                                                taxaType = crest$parameters$taxaType,
                                               depth.out = 3,
                                                  dbname = dbname)[, 'class_name']
            }
        }
    }
    crest$inputs$pse <- cbind( crest$inputs$pse, 'Class_name' = class_names)



    crest$modelling$distributions <- distributions
    if(verbose) {
      cat('  <> Extracting species distributions ...... [OK]\n  <> Extracting climate space .............. ')
    }
    climate_space <- getClimateSpace(
      crest$parameters$climate,
      crest$parameters$xmn, crest$parameters$xmx, crest$parameters$ymn, crest$parameters$ymx,
      crest$parameters$continents, crest$parameters$countries,
      crest$parameters$basins, crest$parameters$sectors,
      crest$parameters$realms, crest$parameters$biomes, crest$parameters$ecoregions,
      dbname
    )

    if(nrow(climate_space) == 0) {
        stop(paste0("No climate values available in the defined study area N: ", crest$parameters$ymx," S: ", crest$parameters$ymn, " W: ",crest$parameters$xmn, " E: ",crest$parameters$xmx, ".\n\n"))
    }

    colnames(climate_space)[-c(1, 2)] <- crest$parameters$climate
    crest$modelling$climate_space <- climate_space

    if (ai.sqrt & 'ai' %in% crest$parameters$climate) {
        crest$modelling$climate_space[, "ai"] <- sqrt(crest$modelling$climate_space[, "ai"])
        for (tax in crest$inputs$taxa_names) {
            crest$modelling$distributions[[tax]][, 'ai'] <- sqrt(crest$modelling$distributions[[tax]][, 'ai'])
        }
    }

    resol <- sort(unique(diff(sort(unique(crest$modelling$climate_space[, 1])))))[1] / 2.0
    xx <- range(climate_space[, 1])
    if (estimate_xlim) {
        crest$parameters$xmn <- xx[1] - resol
        crest$parameters$xmx <- xx[2] + resol
    } else {
        if (crest$parameters$xmn > xx[1] - resol) crest$parameters$xmn <- xx[1] - resol
        if (crest$parameters$xmx < xx[2] + resol) crest$parameters$xmx <- xx[2] + resol
    }

    resol <- sort(unique(diff(sort(unique(crest$modelling$climate_space[, 2])))))[1] / 2.0
    yy <- range(climate_space[, 2])
    if (estimate_ylim) {
        crest$parameters$ymn <- yy[1] - resol
        crest$parameters$ymx <- yy[2] + resol
    }else {
        if (crest$parameters$ymn > yy[1] - resol) crest$parameters$ymn <- yy[1] - resol
        if (crest$parameters$ymx < yy[2] + resol) crest$parameters$ymx <- yy[2] + resol
    }

    if(verbose) {
      cat('[OK]\n')
      cat(paste0('## Data extraction completed.\n\n'))
    }
    crest
}
