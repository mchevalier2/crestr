#' Extract taxonID(s) corresponding to the taxonomic description
#'
#' Extract all possible TaxonIDs corresponding to the provided taxonomical
#' description, which can be at the family, the genus or the species levels.
#'
#' @inheritParams crestObj
#' @param family The name of the family.
#' @param genus The name of the genus.
#' @param species The name of the species.
#' @return A vector of unique taxonIDs.
#' @export
#' @examples
#' \dontrun{
#'   getTaxonID("Zamiaceae")
#'   getTaxonID("Zamiaceae", "Ceratozamia")
#'   ## \code{taxaType = 2} searches for beetles and not plants, so the next line returns nothing.
#'   getTaxonID("Zamiaceae", "Ceratozamia", taxaType = 2)
#' }
#'
getTaxonID <- function(family = "", genus = "", species = "", taxaType = 1, dbname = "gbif4crest_02") {
    family  <- base::trimws(family, 'both')
    genus   <- base::trimws(genus, 'both')
    species <- base::trimws(species, 'both')

    family <- ifelse(is.na(family), "", tools::toTitleCase(base::tolower(family)))
    genus <- ifelse(is.na(genus), "", tools::toTitleCase(base::tolower(genus)))
    species <- ifelse(is.na(species), "", paste0(base::toupper(substr(species, 1, 1)), substr(base::tolower(species), 2, nchar(species))))
    req <- paste0(
      "  SELECT taxonid ",
      "    FROM taxa ",
      "   WHERE taxonid >= ", taxaType * 1000000, " ",
      "     AND taxonid <= ", (taxaType + 1) * 1000000, " ",
      ifelse(family == "",
        "",
        paste0(" AND family = '", family, "' ")
      ),
      ifelse(genus == "",
        "",
        paste0(" AND genus = '", genus, "' ")
      ),
      ifelse(species == "",
        "",
        paste0(" AND species = '", species, "' ")
      ),
      "ORDER BY taxonid"
    )
    res <- dbRequest(req, dbname)
    if (length(res) > 0) res <- res[, 1]
    res
}
