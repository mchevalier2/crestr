#' Extract the distribution of the studied climate gradient(s) across the study area.
#'
#' Extract the distribution of the studied climate gradient(s) across the study area.
#'
#' @param climate A vectof of the climate variables to extract.
#' @param xmn,xmx,ymn,ymx The coordinates defining the study area.
#' @param continents A vector of the continent names defining the study area.
#' @param countries A vector of the country names defining the study area.
#' @param realms A vector of the studied botanical realms defining the study area.
#' @param biomes A vector of the studied botanical biomes defining the study area.
#' @param ecoregions A vector of the studied botanical ecoregions defining the study area.
#' @param dbname The name of the database. Default is \code{'gbif4crest_02'}.
#' @return A matrix of occurrence records with the associated climate.
#' @seealso \code{\link{accClimateVariables}}
#'     for a list of accepted climate variable names, \code{\link{accCountryNames}}
#'     for a list of accepted continent and country names, \code{\link{accRealmNames}}
#'     for a list of accepted realm, biome and ecoregion names.
#' @export
#' @examples
#' climate <- getClimateSpace("bio1", -90, 90, -90, 90,
#'   continents = "Europe",
#'   countries = c("Germany", "Netherlands", "Sweden"),
#'   realms = "Palaearctic"
#' )
#' head(climate)
#' raster::plot(raster::rasterFromXYZ(climate), asp=1)
#'
getClimateSpace <- function(climate,
                            xmn = NA, xmx = NA, ymn = NA, ymx = NA,
                            continents = NA, countries = NA,
                            realms = NA, biomes = NA, ecoregions = NA,
                            dbname = "gbif4crest_02") {

    coords        <- check_coordinates(xmn, xmx, ymn, ymx)

    # Formatting subsets of the request------------------------------------------
    # Formatting the geographical subsetting
    if (is.na(continents)[1] & is.na(countries)[1]) {
        GEO <- ""
    } else {
        GEO <- paste0(
          "AND (longitude, latitude) IN ",
          "  (SELECT distinct longitude, latitude ",
          "     FROM geo_qdgc ",
          "    WHERE ",
          ifelse(is.na(continents)[1], "", paste0("continent IN ('", paste(continents, collapse = "', '"), "') ")),
          ifelse(is.na(continents)[1] | is.na(countries)[1], "", "AND "),
          ifelse(is.na(countries)[1], "", paste0("countryname IN ('", paste(countries, collapse = "', '"), "') ")),
          "   ) "
        )
    }

    # Formatting the botanical subsetting
    if (is.na(realms)[1] & is.na(biomes)[1] & is.na(ecoregions)[1]) {
        WWF <- ""
    } else {
        WWF <- paste0(
          "AND (longitude, latitude) IN ",
          "  (SELECT distinct longitude, latitude ",
          "     FROM wwf_qdgc ",
          "    WHERE ",
          ifelse(is.na(realms)[1], "", paste0("realm IN ('", paste(realms, collapse = "', '"), "') ")),
          ifelse(is.na(realms)[1] | is.na(biomes)[1], "", "AND "),
          ifelse(is.na(biomes)[1], "", paste0("biome IN ('", paste(biomes, collapse = "', '"), "') ")),
          ifelse(is.na(biomes)[1] | is.na(ecoregions)[1], "", "AND "),
          ifelse(is.na(ecoregions)[1], "", paste0("ecoregion IN ('", paste(ecoregions, collapse = "', '"), "') ")),
          "   ) "
        )
    }

    # Formatting the request-----------------------------------------------------
    req <- paste0(
      "  SELECT DISTINCT longitude, latitude, ",
      "        ", paste(climate, collapse = ", "), " ",
      "   FROM  wc_qdgc ",
      "   WHERE longitude >= ", coords[1], " AND longitude <= ", coords[2], " ",
      "     AND latitude >= ", coords[3], " AND latitude <= ", coords[4], " ",
      "     ", GEO, " ",
      "     ", WWF, " ",
      "ORDER BY longitude, latitude"
    )

    # Executing the request------------------------------------------------------
    res <- dbRequest(req, dbname)
    if ('ai' %in% climate) {
        res[, 'ai'] <- res[, 'ai'] / 10000
    }
    res
}
