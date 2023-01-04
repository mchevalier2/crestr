#' Describes all the variables available in the database.
#'
#' Provides the index and the short and full names of all the variables
#' available in the database.
#'
#' @param v The name of a variable to quickly access its description and ID
#'          (default \code{NA} returns all possible values).
#' @param domain The domain ('Terrestrial' or 'Marine') of the variables.
#'        Default value is \cite{NA} and both terrestrial and marine climate
#'        variable names are returned.
#' @return A data frame descriptive of the climate variables available in the
#'         database (if \code{v=NA}) or the description of variable v.
#' @export
#' @seealso \url{https://www.worldclim.org/data/bioclim.html} for details on the 'bio' data, \url{https://cgiarcsi.community/2019/01/24/global-aridity-index-and-potential-evapotranspiration-climate-database-v2/} for details on 'ai', \url{https://www.ncei.noaa.gov/products/world-ocean-atlas} for details on the sea surface temperature, sea surface salinity and nutrient Concentration data, or \url{https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html} for the sea ice concentration data.
#' @examples
#' \dontrun{
#'   accClimateVariables()
#'   accClimateVariables(v='bio12')
#' }
#'
accClimateVariables <- function(v=NA, domain=NA) {
    res <- data.frame(
      1:39,
      c(paste("bio", 1:19, sep = ""),
        "ai",
        paste(rep(c('sst', 'sss', 'icec'), each=5), rep(c('ann', 'jfm', 'amj', 'jas', 'ond'), 3), sep='_'),
        'diss_oxy', 'nitrate', 'phosphate', 'silicate'),
      c(
        "Mean Annual Temperature (\u00b0C)",
        "Mean Diurnal Range (\u00b0C)",
        "Isothermality (x100)",
        "Temperature Seasonality (standard deviation x100) (\u00b0C)",
        "Max Temperature of Warmest Month (\u00b0C)",
        "Min Temperature of Coldest Month (\u00b0C)",
        "Temperature Annual Range (\u00b0C)",
        "Mean Temperature of Wettest Quarter (\u00b0C)",
        "Mean Temperature of Driest Quarter (\u00b0C)",
        "Mean Temperature of Warmest Quarter (\u00b0C)",
        "Mean Temperature of Coldest Quarter (\u00b0C)",
        "Annual Precipitation (mm)",
        "Precipitation of Wettest Month (mm)",
        "Precipitation of Driest Month (mm)",
        "Precipitation Seasonality (Coefficient of Variation) (mm)",
        "Precipitation of Wettest Quarter (mm)",
        "Precipitation of Driest Quarter (mm)",
        "Precipitation of Warmest Quarter (mm)",
        "Precipitation of Coldest Quarter (mm)",
        "Aridity Index",
        "Mean Annual Sea Surface Temperature (\u00b0C)",
        "Mean Winter (JFM) Sea Surface Temperature (\u00b0C)",
        "Mean Spring (AMJ) Sea Surface Temperature (\u00b0C)",
        "Mean Summer (JAS) Sea Surface Temperature (\u00b0C)",
        "Mean Fall (OND) Sea Surface Temperature (\u00b0C)",
        "Mean Annual Sea Surface Salinity (PSU)",
        "Mean Winter (JFM) Sea Surface Salinity (PSU)",
        "Mean Spring (AMJ) Sea Surface Salinity (PSU)",
        "Mean Summer (JAS) Sea Surface Salinity (PSU)",
        "Mean Fall (OND) Sea Surface Salinity (PSU)",
        "Mean Annual Sea Ice Concentration (%)",
        "Mean Winter (JFM) Sea Ice Concentration (%)",
        "Mean Spring (AMJ) Sea Ice Concentration (%)",
        "Mean Summer (JAS) Ice Concentration (%)",
        "Mean Fall (OND) Ice Concentration (%)",
        "Dissolved Oxygen Concentration (umol/L)",
        "Nitrate Concentration (umol/L)",
        "Phosphate Concentration (umol/L)",
        "Silicate Concentration (umol/L)"
    ), c(rep('Terrestrial', 20),rep('Marine', 19)),
      stringsAsFactors = FALSE
    )
    colnames(res) <- c("Variable ID", "Variable name", "Description", "Domain")
    if(!is.na(v[1])) {
        if(v[1] %in% res[,2]) {
            w=which(res[,2] == v[1])
            return(c(res[w,1], res[w,2], res[w,3], res[w,4]))
        }
    }
    w <- 1:nrow(res)
    if(is.na(domain)) {
        w <- 1:nrow(res)
    } else if(domain=='Terrestrial' | domain == 'Marine') {
        w <- (res[, 4] == domain)
    }
    res[w, ]
}



#' Return the list of the continents and associated countries.
#'
#' @param dbname The name of the database. Default is \code{'gbif4crest_02'}.
#' @return A list where each element is a vector of corresponding country names.
#' @keywords internal
getCountryNames <- function(dbname = "gbif4crest_02") {
    db <- connect_online(dbname = dbname)
    if(!methods::is(db, 'DBIConnection')) {
        cat("The connection to the database failed and the process has been stopped. check your internet connection and database IDs.\n")
        return(NA)
    }

    res <- list()
    req <- "SELECT DISTINCT continent FROM geopolitical_units WHERE continent IS NOT NULL ORDER BY continent"
    continents <- dbRequest(req, dbname)[, 1]
    for (i in continents) {
        req <- paste0(
          "  SELECT DISTINCT name ",
          "    FROM geopolitical_units ",
          "   WHERE continent='", i, "' ",
          "ORDER BY name"
        )
        res[[i]] <- dbRequest(req, dbname)[, 1]
    }
    names(res) <- continents
    res
}


#' Return the list of the continents and associated countries.
#'
#' Return the list of the continents and associated countries.
#'
#' @param continent A name of continent. Default is \code{NA} and returns a list
#'        of all the country names sorted by continent.
#' @return A list where each element is a vector of corresponding country names.
#' @export
#' @seealso \url{https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-countries/}
#' @examples
#' accCountryNames()
#' accCountryNames('Europe')
accCountryNames <- function(continent=NA) {
    if (is.na(continent[1])) return(continentNames)
    res <- list()
    for(cont in continent) {
        if(cont %in% names(continentNames)) {
            res[[cont]] <- continentNames[[cont]]
        }
    }
    if(length(res) != length(continent)) {
        w <- continent[!continent %in% names(res)]
        stop(paste0("The following continent names do not exist [", paste(w, collapse=', '), "] and should be part of the list [",  paste(names(continentNames), collapse=', '),"].\n\n"))
    }
    res
}


#' Return the list of oceans and seas.
#'
#' @param dbname The name of the database. Default is \code{'gbif4crest_02'}.
#' @return A list where each element is a vector of corresponding ocean/sea names.
#' @keywords internal
getBasinNames <- function(dbname = "gbif4crest_02") {
    db <- connect_online(dbname = dbname)
    if(!methods::is(db, 'DBIConnection')) {
        cat("The connection to the database failed and the process has been stopped. check your internet connection and database IDs.\n")
        return(NA)
    }

    res <- list()
    req <- "SELECT DISTINCT basin FROM geopolitical_units WHERE basin IS NOT NULL ORDER BY basin"
    basin <- dbRequest(req, dbname)[, 1]
    for (i in basin) {
        req <- paste0(
          "  SELECT DISTINCT name ",
          "    FROM geopolitical_units ",
          "   WHERE basin='", i, "' ",
          "ORDER BY name"
        )
        res[[i]] <- dbRequest(req, dbname)[, 1]
    }
    names(res) <- basin
    res
}


#' Return the list of oceans and seas.
#'
#' Return the list of oceans and seas.
#'
#' @param basin A name of basin. Default is \code{NA} and returns a list
#'        of all the accepted names.
#' @return A list of accepted names.
#' @export
#' @seealso \url{https://www.marineregions.org/downloads.php}
#' @examples
#' accBasinNames()
#' accBasinNames('Indian Ocean')
accBasinNames <- function(basin=NA) {
    if (is.na(basin[1])) return(basinNames)
    res <- list()
    for(bas in basin) {
        if(bas %in% names(basinNames)) {
            res[[bas]] <- basinNames[[bas]]
        }
    }
    if(length(res) != length(basin)) {
        w <- basin[!basin %in% names(res)]
        stop(paste0("The following basin names do not exist [", paste(w, collapse=', '), "] and should be part of the list [",  paste(names(basinNames), collapse=', '),"].\n\n"))
    }
    res
}



#' Return the list of the realms and associated biomes and ecoregions.
#'
#' @param terr A boolean to choose whether to get the terrestrial or the marine
#'        names. Default value is \code{NA} and will return both.
#' @param biome A boolean to choose whether to get the biome names.
#' @param ecoregion A boolean to choose whether to get the ecoregions names.
#' @param dbname The name of the database. Default is \code{'gbif4crest_02'}.
#' @return A list with elements that correspond to the biomes (and possibly
#'         ecoregions) of each realm.
#' @keywords internal
getRealmNames <- function(terr = NA, biome = TRUE, ecoregion = TRUE, dbname = "gbif4crest_02") {
    db <- connect_online(dbname = dbname)
    if(!methods::is(db, 'DBIConnection')) {
        cat("The connection to the database failed and the process has been stopped. check your internet connection and database IDs.\n")
        return(NA)
    }
    
    res <- list()
    if(is.na(terr)) {
        s <- ''
        terr <- TRUE
    } else {
        s <- paste0(" AND ecoID ", ifelse(terr, '> 100000 ', '< 100000'))
    }
    req <- paste0("SELECT DISTINCT realm FROM biogeography WHERE realm IS NOT NULL ", s, " ORDER BY realm")
    realms <- dbRequest(req, dbname)[, 1]

    if(terr) {
        if(biome) {
            for (i in realms) {
                req <- paste0(
                  "  SELECT DISTINCT biome ", ifelse(ecoregion, ", ecoregion ", ""),
                  "    FROM biogeography ",
                  "   WHERE realm='", i, "' ",
                  "ORDER BY biome", ifelse(ecoregion, ", ecoregion ", "")
                )
                res[[i]] <- dbRequest(req, dbname)
            }
            names(res) <- realms
        } else {
            res <- realms
        }
    } else {
        res <- realms
    }
    res
}



#' Return the list of the realms and associated biomes and ecoregions.
#'
#' Return the list of the realms and associated biomes and ecoregions.
#'
#' @param realm A name of accepted realm. Default is \code{NA} and returns a
#'        list of all the biome and ecoregion names sorted by realm.
#' @param ecoregion A boolean to choose whether to get the ecoregions names.
#' @return A list with elements that correspond to the biomes (and possibly
#'         ecoregions) of each realm.
#' @export
#' @seealso \url{https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world}
#' @examples
#' accRealmNames()
#' accRealmNames(realm='Africotropical')
#' accRealmNames(realm='Africotropical', ecoregion=FALSE)
accRealmNames <- function(realm=NA, ecoregion = TRUE) {
    if (is.na(realm[1])) realm <- names(realmNames)
    res <- list()
    for(r in realm) {
        if(r %in% names(realmNames)) {
            if(ecoregion) {
                res[[r]] <- realmNames[[r]]
            } else {
                res[[r]] <- unique(realmNames[[r]][, 1])
            }
        }
    }
    if(length(res) != length(realm)) {
        w <- realm[!realm %in% names(res)]
        stop(paste0("The following realm names do not exist [", paste(w, collapse=', '), "] and should be part of the list [",  paste(names(realmNames), collapse=', '),"].\n\n"))
    }
    res
}
