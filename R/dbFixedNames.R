#' Describes all the variables available in the database.
#'
#' Provides the index and the short and full names of all the variables
#' available in the database.
#'
#' @param v The name of a variable to quickly access its description and ID
#'          (default NA returns all possible values).
#' @return A data frame descriptive of the climate variables available in the
#'         database (if v=NA) or the description of variable v.
#' @export
#' @examples
#' accClimateVariables()
#' accClimateVariables(v='bio12')
accClimateVariables <- function(v=NA) {
    res <- data.frame(
      1:20,
      c(paste("bio", 1:19, sep = ""), "ai"),
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
        "Aridity Index"
    ), stringsAsFactors = FALSE
    )
    colnames(res) <- c("Variable ID", "Variable name", "Description")
    if(!is.na(v[1])) {
        if(v[1] %in% res[,2]) {
            w=which(res[,2] == v[1])
            return(c(res[w,1], res[w,2], res[w,3]))
        }
    }
    res
}



#' Return the list of the continents and associated countries.
#'
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A list where each element is a vector of corresponding country names.
.getCountryNames <- function(dbname = "gbif4crest_02") {
    res <- list()
    req <- "SELECT DISTINCT continent FROM geo_qdgc WHERE continent IS NOT NULL ORDER BY continent"
    continents <- dbRequest(req, dbname)[, 1]
    for (i in continents) {
        req <- paste0(
          "  SELECT DISTINCT countryname ",
          "    FROM geo_qdgc ",
          "   WHERE continent='", i, "' ",
          "ORDER BY countryname"
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
#' @param continent A name of continent. Default is NA and returns a list of all
#'        the country names sorted by continent.
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A list where each element is a vector of corresponding country names.
#' @export
#' @examples
#' accCountryNames()
accCountryNames <- function(continent=NA, dbname = "gbif4crest_02") {
    if (is.na(continent[1])) return(continentNames)
    res <- list()
    for(cont in continent) {
        if(cont %in% names(continentNames)) {
            res[[cont]] <- continentNames[[cont]]
        }
    }
    if(length(res) != length(continent)) {
        w <- continent[!continent %in% names(res)]
        cat(paste0("WARNING: The following continent names do not exist [", paste(w, collapse=', '), "] and should be part of the list [",  paste(names(continentNames), collapse=', '),"].\n"))
    }
    res
}


#' Return the list of the realms and associated biomes and ecoregions.
#'
#' @param ecoregion A boolean to choose whether to get the ecoregions names.
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A list with elements that correspond to the biomes (and possibly
#'         ecoregions) of each realm.
.getRealmNames <- function(ecoregion = TRUE, dbname = "gbif4crest_02") {
    res <- list()
    req <- "SELECT DISTINCT realm FROM wwf_qdgc WHERE realm IS NOT NULL ORDER BY realm"
    realms <- dbRequest(req, dbname)[, 1]
    for (i in realms) {
        req <- paste0(
          "  SELECT DISTINCT biome ", ifelse(ecoregion, ", ecoregion ", ""),
          "    FROM wwf_QDGC ",
          "   WHERE realm='", i, "' ",
          "ORDER BY biome", ifelse(ecoregion, ", ecoregion ", "")
        )
        res[[i]] <- dbRequest(req, dbname)
    }
    names(res) <- realms
    res
}



#' Return the list of the realms and associated biomes and ecoregions.
#'
#' Return the list of the realms and associated biomes and ecoregions.
#'
#' @param realm A name of accepted realm. Default is NA and returns a list of
#'        all the biome and ecoregion names sorted by realm.
#' @param ecoregion A boolean to choose whether to get the ecoregions names.
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A list with elements that correspond to the biomes (and possibly
#'         ecoregions) of each realm.
#' @export
#' @examples
#' accRealmNames()
#' accRealmNames(realm='Africotropical')
#' accRealmNames(realm='Africotropical', ecoregion=FALSE)
accRealmNames <- function(realm=NA, ecoregion = TRUE, dbname = "gbif4crest_02") {
    if (is.na(realm[1])) return(realmNames)
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
        cat(paste0("WARNING: The following realm names do not exist [", paste(w, collapse=', '), "] and should be part of the list [",  paste(names(realmNames), collapse=', '),"].\n"))
    }
    res
}
