#' Extract taxonID(s) corresponding to the taxonomic description
#'
#' Extract all possible TaxonIDs corresponding to the provided taxonomical
#' description, which can be at the family, the genus or the species levels.
#'
#' @param family The name of the family.
#' @param genus The name of the genus.
#' @param species The name of the species.
#' @param taxaType A numerical index (between 1 and 5) to define the type of
#'     palaeoproxy used: 1 for plants, 2 for beetles, 3 for foraminifers,
#'     4 for diatoms and 5 for chironomids.
#' @return A vector of unique taxonIDs.
#' @export
#' @examples
#' extractTaxonID("Zamiaceae")
#' extractTaxonID("Zamiaceae", "Ceratozamia")
#' extractTaxonID("Zamiaceae", "Ceratozamia", taxaType = 2)

extractTaxonID <- function (family, genus = "", species = "", taxaType = 1) {
    req <- paste0("  SELECT taxonid ",
                  "    FROM taxa ",
                  "   WHERE family = '",family,"' ",
                    ifelse( genus == "",
                            "",
                            paste0(" AND genus = '",genus, "' ")
                           ),
                    ifelse( species == "",
                            "",
                            paste0(" AND species = '",species, "' ")
                           ),
                  "     AND taxonid >= ", taxaType*1000000, " ",
                  "     AND taxonid <= ", (taxaType+1)*1000000, ' ',
                  "ORDER BY taxonid"
                  )
    dbRequest(req)
}
