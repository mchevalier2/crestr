#' Extract taxonID(s) corresponding to the taxonomic description
#'
#' Extract all possible TaxonIDs corresponding to the provided taxonomical
#' description, which can be at the family, the genus or the species levels.
#'
#' @param taxIDs A vector of accepted Taxa IDs (as returned by \code{\link{getTaxonID}}).
#' @param climate A vectof of the climate variables to extract.
#' @param xmn,xmx,ymn,ymx The coordinates defining the study area.
#' @param continents A vector of the continent names defining the study area.
#' @param countries A vector of the country names defining the study area.
#' @param realms A vector of the studied botanical realms defining the study area.
#' @param biomes A vector of the studied botanical biomes defining the study area.
#' @param ecoregions A vector of the studied botanical ecoregions defining the study area.
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A matrix of occurrence records with the associated climate.
#' @seealso \code{\link{getTaxonID}} for taxIDs, \code{\link{accClimateVariables}}
#'     for a list of accepted climate variable names, \code{\link{accContinentNames}}
#'     for a list of accepted continent and country names, \code{\link{accRealmNames}}
#'     for a list of accepted realm, biome and ecoregion names.
#' @export
#' @examples
#' taxIDs <- getTaxonID("Zamiaceae", "Ceratozamia")
#' distrib <- getDistribTaxa(taxIDs, 'bio1', -90, 90, -90, 90,
#'    continents = 'Europe',
#'    countries = c('Germany', 'Netherlands', 'Sweden'),
#'    realms = 'Palaearctic' )
#' distrib

getDistribTaxa <- function(taxIDs,
                            climate,
                            xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                            continents=NA, countries=NA,
                            realms=NA, biomes=NA, ecoregions=NA,
                            dbname = 'gbif4crest_02') {

    #Formatting subsets of the request------------------------------------------
    #Formatting the geographical subsetting
    if (is.na(continents)[1] & is.na(countries)[1]) {
       GEO <- ""
    } else {
        GEO <- paste0(
            "AND (distrib_qdgc.longitude, distrib_qdgc.latitude) IN ",
            "  (SELECT distinct longitude, latitude ",
            "     FROM geo_qdgc ",
            "    WHERE ",
            ifelse(is.na(continents)[1], "", paste0("continent IN ('", paste(continents, collapse = "', '"),"') ")),
            ifelse(is.na(continents)[1] | is.na(countries)[1], "", "AND "),
            ifelse(is.na(countries)[1], "", paste0("countryname IN ('", paste(countries, collapse = "', '"),"') ")),
            "   ) "
       )
    }

    #Formatting the botanical subsetting
    if (is.na(realms)[1] & is.na(biomes)[1] & is.na(ecoregions)[1]) {
       WWF <- ""
    } else {
        WWF <- paste0(
            "AND (distrib_qdgc.longitude, distrib_qdgc.latitude) IN ",
            "  (SELECT distinct longitude, latitude ",
            "     FROM wwf_qdgc ",
            "    WHERE ",
            ifelse(is.na(realms)[1], "", paste0("realm IN ('", paste(realms, collapse = "', '"),"') ")),
            ifelse(is.na(realms)[1] | is.na(biomes)[1], "", "AND "),
            ifelse(is.na(biomes)[1], "", paste0("biome IN ('", paste(biomes, collapse = "', '"),"') ")),
            ifelse(is.na(biomes)[1] | is.na(ecoregions)[1], "", "AND "),
            ifelse(is.na(ecoregions)[1], "", paste0("ecoregion IN ('", paste(ecoregions, collapse = "', '"),"') ")),
            "   ) "
       )
    }

    #Formatting the request-----------------------------------------------------
    req <- paste0("  SELECT DISTINCT taxonid, distrib_qdgc.longitude, ",
                  "  distrib_qdgc.latitude, ", paste(climate, collapse=', '), " ",
                  "    FROM distrib_qdgc, wc_qdgc ",
                  "   WHERE taxonID IN (", paste(taxIDs, collapse=', '), ') ',
                  "     AND distrib_qdgc.longitude = wc_qdgc.longitude AND distrib_qdgc.latitude = wc_qdgc.latitude ",
                  "     AND distrib_qdgc.longitude >= ", xmn, " AND distrib_qdgc.longitude <= ", xmx," ",
                  "     AND distrib_qdgc.latitude >= ", ymn," AND distrib_qdgc.latitude <= ", ymx, " ",
                  "     ", GEO, ' ',
                  "     ", WWF, ' ',
                  "ORDER BY taxonid, distrib_qdgc.longitude, distrib_qdgc.latitude"
                )

    #Executing the request------------------------------------------------------
    dbRequest(req, dbname)
}
