
#' @export
print.crestObj <- function(x, as=x$misc$stage, ...) {
    if(base::missing(x)) x

    name <- find.original.name(x)
    is_formatted <- is_fitted <- is_reconstructed <- is_looed <- FALSE
    if(as == 'data_extracted' | as == 'data_inserted') {
        is_formatted <- TRUE
    } else if (as == 'PDFs_fitted') {
        is_formatted <- is_fitted <- TRUE
    }else if (as == 'climate_reconstructed') {
        is_formatted <- is_fitted <- is_reconstructed <- TRUE
    } else if (as == 'leave_one_out') {
        is_formatted <- is_fitted <- is_reconstructed <- is_looed <- TRUE
    }

    cat('*\n')
    cat(paste0('* Summary of the crestObj named `',name,'`:\n'))
    cat(paste0('*   x Calibration data formatted .. ', is_formatted,'\n'))
    cat(paste0('*   x PDFs fitted ................. ', is_fitted,'\n'))
    cat(paste0('*   x Climate reconstructed ....... ', is_reconstructed,'\n'))
    cat(paste0('*   x Leave-One-Out analysis ...... ', is_looed,'\n'))
    cat('*\n')
    if(is_formatted) {
        if(is.data.frame(x$inputs$df)) {
            cat(paste0('* The dataset to be reconstructed (`df`) is composed of ', nrow(x$inputs$df),' samples with ',ncol(x$inputs$df),' taxa.\n'))
        }
        cat(paste0('* Variable', ifelse(length(x$parameters$climate) > 1, 's', ''),' to analyse: ', paste(x$parameters$climate, collapse=', '),'\n'))
        cat(paste0('*\n'))

        taxa_type <- get_taxa_type(x$parameters$taxaType)
        cat(paste0('* The calibration dataset was defined using the following set of parameters:\n'))
        cat(paste0('*   x Proxy type ............ ', taxa_type, ifelse(x$parameters$taxaType == 0, '', 's'), '\n'))
        if(!is.na(x$parameters$xmn) | !is.na(x$parameters$xmx)) cat(paste0('*   x Longitude ............. [', x$parameters$xmn, ' - ', x$parameters$xmx,']\n'))
        if(!is.na(x$parameters$ymn) | !is.na(x$parameters$ymx)) cat(paste0('*   x Latitude .............. [', x$parameters$ymn, ' - ', x$parameters$ymx,']\n'))
        if(x$parameters$taxaType > 0) {
            if(!is.na(x$parameters$elev_min) | !is.na(x$parameters$elev_max)) cat(paste0('*   x Elevation ............. [', x$parameters$elev_min, ' - ', x$parameters$elev_max,']\n'))
            if(!is.na(x$parameters$elev_range)) cat(paste0('*   x Elevation range ....... ',x$parameters$elev_range, '\n'))
            if(!is.na(x$parameters$year_min) | !is.na(x$parameters$year_max)) cat(paste0('*   x Observation date ...... [', x$parameters$year_min, ' - ', x$parameters$year_max,']\n'))
            if(!is.na(x$parameters$nodate)) cat(paste0('*   x Undated observations .. ', x$parameters$nodate, '\n'))
            if(!unique(is.na(x$parameters$type_of_obs))) {
                OBSTYPES <- dbRequest("SELECT * FROM typeofobservations ORDER BY type_of_obs", x$misc$dbname)
                cat(paste0('*   x Type of observations .. ', paste(base::trimws(OBSTYPES[x$parameters$type_of_obs,2]), collapse=', '), '\n'))
            }
            if(!unique(is.na(x$parameters$continents))) cat(paste0('*   x Continents ............ ', paste(x$parameters$continents, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$countries))) cat(paste0('*   x Countries ............. ', paste(x$parameters$countries, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$basins))) cat(paste0('*   x Basins ................ ', paste(x$parameters$basins, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$sectors))) cat(paste0('*   x Sectors ............... ', paste(x$parameters$sectors, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$realms))) cat(paste0('*   x Realms ................ ', paste(x$parameters$realms, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$biomes))) cat(paste0('*   x Biomes ................ ', paste(x$parameters$biomes, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$ecoregions))) cat(paste0('*   x Ecoregions ............ ', paste(x$parameters$ecoregions, collapse=', '), '\n'))
        }
        cat(paste0('*\n'))
        if(is_fitted) {
            cat(paste0('* The PDFs were fitted using the following set of parameters:\n'))
            if(!is.na(x$parameters$minGridCells)) cat(paste0('*   x Minimum distinct of distinct occurences .. ', x$parameters$minGridCells, '\n'))
            if(!is.na(x$parameters$weightedPresences)) cat(paste0('*   x Weighted occurence data .................. ', x$parameters$weightedPresences, '\n'))
            if(!is.na(x$parameters$npoints)) cat(paste0('*   x Number of points to fit the PDFs ......... ', x$parameters$npoints, '\n'))
            if(!is.na(x$parameters$geoWeighting)) {
                cat(paste0('*   x Geographical weighting ................... ',x$parameters$geoWeighting, '\n'))
                cat(paste0('*       Using bins of width .................... ', x$parameters$climate[1], ': ', x$parameters$bin_width[x$parameters$climate[1], 1],'\n'))
                for(clim in x$parameters$climate[-1]) {
                    cat(paste0('*       ', paste(rep('_', nchar('Using bins of width ....................')), collapse=''), ' ', clim, ': ', x$parameters$bin_width[clim, 1],'\n'))
                }
            }
            if(!is.na(x$parameters$climateSpaceWeighting)) {
                cat(paste0('*   x Weighting of the climate space ........... ',x$parameters$climateSpaceWeighting, '\n'))
                cat(paste0("*       Using a ",x$parameters$climateSpaceWeighting.type," correction\n"))
            }
            cat(paste0('*   x Shape of the PDFs ........................ ',x$parameters$climate[1], ': ', x$parameters$shape[x$parameters$climate[1], 1], '\n'))
            for(clim in x$parameters$climate[-1]) {
                cat(paste0('*     ', paste(rep('_', nchar('Shape of the PDFs ........................')), collapse=''), ' ',clim, ': ', x$parameters$shape[clim, 1], '\n'))
            }
            cat(paste0('*\n'))
        }
        excluded_taxa <- sum(unlist(lapply(x$misc$taxa_notes, function(x){if(is.data.frame(x)){return(nrow(x))}else{return(length(x))}})))
        if(excluded_taxa > 0) {
            cat(paste0('* Of the ',nrow(x$inputs$selectedTaxa), ' taxa provided in `df` and `PSE`, ', excluded_taxa,' cannot be analysed.\n'))
            cat(paste0('* (This may be expected, but check `', name,'$misc$taxa_notes` for additional details.)\n'))
            cat(paste0('*\n'))
        }
        if(is_reconstructed) {
            cat(paste0('* The reconstructions were performed with the following set of parameters:\n'))
            if(!is.na(x$parameters$presenceThreshold)) cat(paste0('*   x Minimum presence value .................. ',  x$parameters$presenceThreshold,'\n'))
            if(!is.na(x$parameters$taxWeight)) cat(paste0('*   x Weighting of the taxa ................... ',  x$parameters$taxWeight,'\n'))
            if(!unique(is.na(x$parameters$uncertainties))) cat(paste0('*   x Calculated uncertainties ................ ',  paste(x$parameters$uncertainties, collapse=', '),'\n'))
            cat(paste0('*   x Number of taxa selected to reconstruct .. ', x$parameters$climate[1],': ', sum(x$inputs$selectedTaxa[, x$parameters$climate[1]] > 0),'\n'))
            for(clim in x$parameters$climate[-1]) {
                cat(paste0('*     ----------------------------------------- ', clim, ': ', sum(x$inputs$selectedTaxa[, clim] > 0),'\n'))
            }
            cat(paste0('*\n'))
        }
    }
}
