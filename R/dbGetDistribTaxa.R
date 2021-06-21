#' Extract taxonID(s) corresponding to the taxonomic description
#'
#' Extract all possible TaxonIDs corresponding to the provided taxonomical
#' description, which can be at the family, the genus or the species levels.
#'
#' @inheritParams crestObj
#' @param taxIDs A vector of accepted Taxa IDs (as returned by \code{\link{getTaxonID}}).
#' @return A matrix of occurrence records with the associated climate.
#' @seealso \code{\link{getTaxonID}} for taxIDs, \code{\link{accClimateVariables}}
#'     for a list of accepted climate variable names, \code{\link{accCountryNames}}
#'     for a list of accepted continent and country names, \code{\link{accRealmNames}}
#'     for a list of accepted realm, biome and ecoregion names.
#' @export
#' @examples
#' taxIDs <- getTaxonID("Zamiaceae", "Ceratozamia")
#' distrib <- getDistribTaxa(taxIDs, "bio1", -90, 90, -90, 90,
#'   continents = "Europe",
#'   countries = c("Germany", "Netherlands", "Sweden"),
#'   realms = "Palaearctic"
#' )
#' distrib
getDistribTaxa <- function(taxIDs,
                           climate = NA,
                           xmn = NA, xmx = NA, ymn = NA, ymx = NA,
                           continents = NA, countries = NA,
                           basins = NA, sectors = NA,
                           realms = NA, biomes = NA, ecoregions = NA,
                           dbname = "gbif4crest_02") {

    coords <- check_coordinates(xmn, xmx, ymn, ymx)

    # Formatting subsets of the request------------------------------------------
    # Formatting the geographical subsetting
    if (is.na(continents)[1] & is.na(countries)[1]) {
        GEO_terr <- ""
    } else {
        GEO_terr <- paste0(
          "AND countryID IN ",
          "  (SELECT distinct geopoID ",
          "     FROM geopolitical_units ",
          "    WHERE ",
          ifelse(is.na(continents)[1], "", paste0("continent IN ('", paste(continents, collapse = "', '"), "') ")),
          ifelse(is.na(continents)[1] | is.na(countries)[1], "", "AND "),
          ifelse(is.na(countries)[1], "", paste0("name IN ('", paste(countries, collapse = "', '"), "') ")),
          "   ) "
        )
    }

    # Formatting subsets of the request------------------------------------------
    # Formatting the geographical subsetting
    if (is.na(basins)[1] & is.na(sectors)[1]) {
        GEO_mari <- ""
    } else {
        GEO_mari <- paste0(
          "AND oceanID IN ",
          "  (SELECT distinct geopoID ",
          "     FROM geopolitical_units ",
          "    WHERE ",
          ifelse(is.na(basins)[1], "", paste0("basin IN ('", paste(basins, collapse = "', '"), "') ")),
          ifelse(is.na(basins)[1] | is.na(sectors)[1], "", "AND "),
          ifelse(is.na(sectors)[1], "", paste0("name IN ('", paste(sectors, collapse = "', '"), "') ")),
          "   ) "
        )
    }

    # Formatting the botanical subsetting
    if (is.na(realms)[1] & is.na(biomes)[1] & is.na(ecoregions)[1]) {
        WWF <- ""
    } else {
        WWF <- paste0(
          "AND terr_ecoID IN ",
          "  (SELECT distinct ecoID ",
          "     FROM biogeography ",
          "    WHERE ",
          ifelse(is.na(realms)[1], "", paste0("realm IN ('", paste(realms, collapse = "', '"), "') ")),
          ifelse(is.na(realms)[1] | is.na(biomes)[1], "", "AND "),
          ifelse(is.na(biomes)[1], "", paste0("biome IN ('", paste(biomes, collapse = "', '"), "') ")),
          ifelse(is.na(biomes)[1] | is.na(ecoregions)[1], ifelse(is.na(realms)[1] | is.na(ecoregions)[1], "", "AND "), "AND "),
          ifelse(is.na(ecoregions)[1], "", paste0("ecoregion IN ('", paste(ecoregions, collapse = "', '"), "') ")),
          "   ) "
        )
    }

    # If no climate variables are provided
    if (unique(is.na(climate))) {
        CLIM1 <- " "
        CLIM2 <- " "
        CLIM3 <- " "
    } else {
        CLIM1 <- ", data_qdgc "
        CLIM2 <- "     "
        CLIM3 <- paste(', ', paste(climate, collapse = ", "))
    }

    # Formatting the request-----------------------------------------------------
    req <- paste0("SELECT DISTINCT taxonid, locid FROM distrib_qdgc WHERE taxonID IN (", paste(taxIDs, collapse = ", "), ")")
    res <- dbRequest(req, dbname)

    req2 <- paste0(
      "  SELECT DISTINCT locid, longitude, latitude", CLIM3,
      "    FROM data_qdgc ",
      "   WHERE locid IN (", paste(unique(res[, 2]), collapse = ", "), ")",
      "     ", CLIM2, " ",
      "     AND longitude >= ", coords[1], " AND longitude <= ", coords[2], " ",
      "     AND latitude >= ", coords[3], " AND latitude <= ", coords[4], " ",
      "     ", GEO_terr, " ",
      "     ", GEO_mari, " ",
      "     ", WWF, " "
    )
    res2 <- dbRequest(req2, dbname)

    # Executing the request------------------------------------------------------

    res <- merge(res, res2, by='locid')
    res[, c('taxonid', 'longitude', 'latitude', climate)]
}
