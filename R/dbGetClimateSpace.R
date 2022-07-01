#' Extract the distribution of the studied climate gradient(s) across the study area.
#'
#' Extract the distribution of the studied climate gradient(s) across the study area.
#'
#' @inheritParams crestObj
#' @return A matrix of occurrence records with the associated climate.
#' @seealso \code{\link{accClimateVariables}}
#'     for a list of accepted climate variable names, \code{\link{accCountryNames}}
#'     for a list of accepted continent and country names, \code{\link{accBasinNames}}
#'     for a list of accepted basin and sector names, \code{\link{accRealmNames}}
#'     for a list of accepted realm, biome and ecoregion names.
#' @export
#' @examples
#' \dontrun{
#'   climate <- getClimateSpace("bio1", -90, 90, -90, 90,
#'     continents = "Europe",
#'     countries = c("Germany", "Netherlands", "Sweden"),
#'     realms = "Palaearctic"
#'   )
#'   head(climate)
#'   raster::plot(raster::rasterFromXYZ(climate), asp=1)
#' }
#'
getClimateSpace <- function(climate,
                            xmn = NA, xmx = NA, ymn = NA, ymx = NA,
                            continents = NA, countries = NA,
                            basins = NA, sectors = NA,
                            realms = NA, biomes = NA, ecoregions = NA,
                            dbname = "gbif4crest_02") {

    if(base::missing(climate)) climate

    coords <- check_coordinates(xmn, xmx, ymn, ymx)

    # Formatting subsets of the request------------------------------------------
    # Formatting the geographical subsetting
    if (is.na(continents)[1] & is.na(countries)[1]) {
        GEO_terr <- ""
    } else {
        GEO_terr <- paste0(
          "AND countryID IN ",
          "  (SELECT distinct geopoID ",
          "     FROM geopolitical_units  ",
          "    WHERE ",
          ifelse(is.na(continents)[1], "", paste0("continent IN ('", paste(continents, collapse = "', '"), "') ")),
          ifelse(is.na(continents)[1] | is.na(countries)[1], "", "AND "),
          ifelse(is.na(countries)[1], "", paste0("name IN ('", paste(countries, collapse = "', '"), "') ")),
          "   ) "
        )
    }

    if (is.na(basins)[1] & is.na(sectors)[1]) {
        GEO_mari <- ""
    } else {
        GEO_mari <- paste0(
          "AND oceanID IN ",
          "  (SELECT distinct geopoID ",
          "     FROM geopolitical_units  ",
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

    # Removing the 'NULL' when using the SQLite3 database
    NULLS <- ""
    if(stringr::str_detect(base::tolower(dbname), '.sqlite3')) {
        for(clim in climate) {
            NULLS <- paste0(NULLS, paste0("  AND ", clim, " IS NOT 'NULL' ") )
        }
    } 

    # Formatting the request-----------------------------------------------------
    req <- paste0(
      "  SELECT DISTINCT longitude, latitude, ",
      "        ", paste(climate, collapse = ", "), " ",
      "   FROM  data_qdgc ",
      "   WHERE longitude >= ", coords[1], " AND longitude <= ", coords[2], " ",
      "     AND latitude >= ", coords[3], " AND latitude <= ", coords[4], " ",
      "     ", GEO_terr, " ",
      "     ", GEO_mari, " ",
      "     ", WWF, " ",
      "     ", NULLS,
      "ORDER BY longitude, latitude"
    )

    # Executing the request------------------------------------------------------
    res <- dbRequest(req, dbname)
    res
}
