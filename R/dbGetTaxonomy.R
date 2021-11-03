#' Extract taxonID(s) corresponding to the taxonomic description
#'
#' Extract all possible TaxonIDs corresponding to the provided taxonomical
#' description, which can be at the family, the genus or the species levels.
#'
#' @inheritParams crestObj
#' @param family The name of the family.
#' @param genus The name of the genus.
#' @param species The name of the species.
#' @param taxaType A numerical index (between 1 and 5) to define the type of
#'     palaeoproxy used: 1 for plants, 2 for beetles, 3 for chironomids,
#'     4 for foraminifers, 5 for diatoms and 6 for rodents.
#' @param depth.out The taxonomic resolution of the output table. 1 for Kingdom,
#'   2 for phylum, 3 for class_name, 4 for order_name, 5 for family, 6 for genus,
#'   7 for species and 8 to also include the taxonID.
#' @return A vector of unique taxonIDs.
#' @export
#' @examples
#' \dontrun{
#'   getTaxonomy("Zamiaceae    ")
#'   getTaxonomy(genus="Ceratozamia", depth.out=6)
#'   ## \code{taxaType = 2} searches for beetles and not plants, so the next line returns nothing.
#'   getTaxonomy("Zamiaceae", "Ceratozamia", taxaType = 2)
#' }
#'
getTaxonomy <- function(family = "", genus = "", species = "", taxaType = 1, depth.out = 8, dbname = "gbif4crest_02") {
    if (family == "" & genus == "" & species == "") {
        stop('No family, genus or species name were provided.\n')
    }
    family  <- base::trimws(family, 'both')
    genus   <- base::trimws(genus, 'both')
    species <- base::trimws(species, 'both')

    family  <- ifelse(is.na(family), "", tools::toTitleCase(base::tolower(family)))
    genus   <- ifelse(is.na(genus), "", tools::toTitleCase(base::tolower(genus)))
    species <- ifelse(is.na(species), "", paste0(base::toupper(substr(species, 1, 1)), substr(base::tolower(species), 2, nchar(species))))
    req <- paste0(
      "  SELECT DISTINCT kingdom, phylum, class_name, order_name, family, genus, species, taxonid ",
      "    FROM taxa ",
      "   WHERE taxonID >= 0 ",
      ifelse(family == "",
        "",
        paste0(" AND family LIKE '%", family, "%' ")
      ),
      ifelse(genus == "",
        "",
        paste0(" AND genus LIKE '%", genus, "%' ")
      ),
      ifelse(species == "",
        "",
        paste0(" AND species LIKE '%", species, "%' ")
      ),
      "     AND taxonid >= ", taxaType * 1000000, " ",
      "     AND taxonid <= ", (taxaType + 1) * 1000000, " ",
      "ORDER BY family, genus, species"
    )
    res <- dbRequest(req, dbname)
    if (length(res) == 0) {
        cat('Nothing matches your request.\n')
        return(invisible())
    }
    unique(res[, 1:depth.out])
}
