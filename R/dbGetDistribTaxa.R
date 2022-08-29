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
#' \dontrun{
#'   taxIDs <- getTaxonID("Zamiaceae", "Ceratozamia")
#'   distrib <- getDistribTaxa(taxIDs, "bio1", -90, 90, -90, 90,
#'     continents = "Europe",
#'     countries = c("Germany", "Netherlands", "Sweden"),
#'     realms = "Palaearctic"
#'   )
#'   distrib
#' }
#'
getDistribTaxa <- function(taxIDs,
                           climate = NA,
                           xmn = NA, xmx = NA, ymn = NA, ymx = NA,
                           continents = NA, countries = NA,
                           basins = NA, sectors = NA,
                           realms = NA, biomes = NA, ecoregions = NA,
                           elev_min = NA, elev_max = NA, elev_range = NA,
                           year_min = 1900, year_max = 2021, nodate = TRUE,
                           type_of_obs = c(1, 2, 3, 8, 9),
                           dbname = "gbif4crest_02") {

    if(base::missing(taxIDs)) taxIDs

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

    # If no climate variables are provided, return values for ALL variables.
    if (unique(is.na(climate))) {
        taxaType <- getTaxaTypeFromTaxID(taxIDs[1])
        if(taxaType %in% c(1, 2, 3, 6)) {
            climate <- accClimateVariables(domain='Terrestrial')[, 2]
        } else {
            climate <- accClimateVariables(domain='Marine')[, 2]
        }
        CLIM3 <- paste(', ', paste(climate, collapse = ", "))
    } else {
        CLIM3 <- paste(', ', paste(climate, collapse = ", "))
    }

    ## Excluding grid cells without any climate values (eg. marine plant observations)
    CLIM4 <- paste0('AND (', climate[1], ' IS NOT NULL')
    for(clim in climate[-1]){
        CLIM4 <- paste(CLIM4, " OR ", clim, " IS NOT NULL")
    }
    CLIM4 <- paste0(CLIM4, ')')

    # Formatting the request-----------------------------------------------------
    if(dbname == 'crest_example') { # Some parameters are not availble in the example database
        DATE <- ''
        ELEVMIN <- ELEVMAX <- ELEVRANGE <- ''
        TYPEOFOBS <- ''
    } else {
        DATEMIN   <- ifelse(is.na(year_min), '', paste0(" AND last_occ >= ", year_min))
        DATEMAX   <- ifelse(is.na(year_max), '', paste0(" AND first_occ <=", year_max))
        NODATE    <- ifelse(is.na(nodate), '', paste0(" no_date = ", nodate))
        DATE <- paste0(DATEMIN, DATEMAX)
        if(nchar(DATE) > 0)  DATE <- paste0('( ', substr(DATE, 5, nchar(DATE)), ') ')
        if(nchar(NODATE) > 0) {
            if(nchar(DATE) == 0) {
                DATE <- paste0('AND ', NODATE)
            } else {
                DATE <- paste0('AND ( ', DATE, ' OR ', NODATE, ')')
            }
        } else {
            DATE <- paste0('AND ', DATE)
        }
        ELEVMIN   <- ifelse(is.na(elev_min), '', paste0('    AND elevation >= ', elev_min))
        ELEVMAX   <- ifelse(is.na(elev_max), '', paste0('    AND elevation <= ', elev_max))
        ELEVRANGE <- ifelse(is.na(elev_range), '', paste0('    AND elev_range <= ', elev_range))
        TYPEOFOBS <- ''
        if(!unique(is.na(type_of_obs))) {
            res <- dbRequest("SELECT * FROM typeofobservations ORDER BY type_of_obs", dbname)
            for(i in type_of_obs) {
                TYPEOFOBS <- paste(TYPEOFOBS, 'OR ', base::trimws(res[i,2]), '= TRUE ')
            }
            TYPEOFOBS <- paste('AND (', substr(TYPEOFOBS, 4, nchar(TYPEOFOBS)), ')')
        }
    }
    req <- paste0(
        "  SELECT DISTINCT taxonid, locid ",
        "    FROM distrib_qdgc ",
        "   WHERE taxonID IN (", paste(taxIDs, collapse = ", "), ")",
        "    ", DATE, '   ',
        "    ", TYPEOFOBS, '   '
       )
    res <- dbRequest(req, dbname)

    if(nrow(res) == 0) return(stats::setNames(data.frame(matrix(ncol = length(c('taxonid', 'longitude', 'latitude', climate)), nrow = 0)), c('taxonid', 'longitude', 'latitude', climate)))


    # Removing the 'NULL' when using the SQLite3 database
    NULLS <- ""
    if(stringr::str_detect(base::tolower(dbname), '.sqlite3')) {
        for(clim in climate) {
            NULLS <- paste0(NULLS, paste0("  AND ", clim, " IS NOT 'NULL' ") )
        }
    }

    req2 <- paste0(
      "  SELECT DISTINCT locid, longitude, latitude", CLIM3,
      "    FROM data_qdgc ",
      "   WHERE locid IN (", paste(unique(res[, 2]), collapse = ", "), ")",
      "     AND longitude >= ", coords[1], " AND longitude <= ", coords[2], " ",
      "     AND latitude >= ", coords[3], " AND latitude <= ", coords[4], " ",
      "     ", ELEVMIN, '   ',
      "     ", ELEVMAX, '   ',
      "     ", ELEVRANGE, '   ',
      "     ", GEO_terr, " ",
      "     ", GEO_mari, " ",
      "     ", WWF, " ",
      "     ", NULLS,
      "     ", CLIM4, " "
    )
    res2 <- dbRequest(req2, dbname)

    # Executing the request------------------------------------------------------
    res <- merge(res, res2, by='locid')
    res[, c('taxonid', 'longitude', 'latitude', climate)]
}
