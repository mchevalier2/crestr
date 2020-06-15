#' Describes all the variables available in the database.
#'
#' Describes all the variables available in the database.
#'
#' @return A data frame descriptive of the climate variables available in the database.
#' @export
#' @examples
#' accClimateVariables()

accClimateVariables <- function() {
    res <- data.frame( 1:20,
                       c(paste('bio', 1:19, sep=''), 'ai'),
                       c( 'Annual Mean Temperature',
                          'Mean Diurnal Range (Mean of monthly (max temp - min temp))',
                          'Isothermality (BIO2/BIO7) (x100)',
                          'Temperature Seasonality (standard deviation x100)',
                          'Max Temperature of Warmest Month',
                          'Min Temperature of Coldest Month',
                          'Temperature Annual Range (BIO5-BIO6)',
                          'Mean Temperature of Wettest Quarter',
                          'Mean Temperature of Driest Quarter',
                          'Mean Temperature of Warmest Quarter',
                          'Mean Temperature of Coldest Quarter',
                          'Annual Precipitation',
                          'Precipitation of Wettest Month',
                          'Precipitation of Driest Month',
                          'Precipitation Seasonality (Coefficient of Variation)',
                          'Precipitation of Wettest Quarter',
                          'Precipitation of Driest Quarter',
                          'Precipitation of Warmest Quarter',
                          'Precipitation of Coldest Quarter',
                          "Aridity Index. Low/High values indicate dry/wet environmental conditions"
                         )
                       )
    colnames(res) <- c('Variable ID', 'Variable name', 'Description')
    res
}



#' Return the list of the continents and associated countries.
#'
#' Return the list of the continents and associated countries.
#'
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A list with elements that correspond to the country names of each continent.
#' @export
#' @examples
#' accContinentNames()

accContinentNames <- function(dbname = 'gbif4crest_02') {
  res <- list()
  req <- "SELECT DISTINCT continent FROM geo_qdgc WHERE continent IS NOT NULL ORDER BY continent"
  continents <- dbRequest(req, dbname)[, 1]
  for (i in continents) {
      req <- paste0("  SELECT DISTINCT countryname ",
                    "    FROM geo_qdgc ",
                    "   WHERE continent='",i,"' ",
                    "ORDER BY countryname"
                    )
      res[[i]] <- dbRequest(req, dbname)[, 1]
  }
  names(res) <- continents
  res
}



#' Return the list of the realms and associated biomes and ecoregions.
#'
#' Return the list of the realms and associated biomes and ecoregions.
#'
#' @param ecoregion A boolean to choose whether to get the ecoregions names.
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A list with elements that correspond to the biomes (and possibly
#'     ecoregions) of each realm.
#' @export
#' @examples
#' accRealmNames()

accRealmNames <- function(ecoregion=TRUE, dbname = 'gbif4crest_02') {
    res <- list()
    req <- "SELECT DISTINCT realm FROM wwf_qdgc WHERE realm IS NOT NULL ORDER BY realm"
    realms <- dbRequest(req, dbname)[, 1]
    for (i in realms) {
        req <- paste0("  SELECT DISTINCT biome ", ifelse(ecoregion, ", ecoregion ", ""),
                      "    FROM wwf_QDGC ",
                      "   WHERE realm='",i,"' ",
                      "ORDER BY biome", ifelse(ecoregion, ", ecoregion ", "")
                      )
        res[[i]] <- dbRequest(req, dbname)
    }
    names(res) <- realms
    res
}
