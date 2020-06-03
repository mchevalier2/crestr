#' Connect to the gbif4crest database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @param request A SQL request to be executed.
#' @return The result of the request.
#' @export
#' @examples
#' dbRequest("SELECT count(*) FROM taxa")
#' southAfricaTaxa <- dbRequest(paste0("SELECT taxa.* ",
#'     "FROM taxa, distrib_qdgc, geo_qdgc ",
#'     "WHERE taxa.taxonid=distrib_qdgc.taxonid ",
#'     "AND   distrib.qdgc.latitude=geo_qdgc.latitude ",
#'     "AND   distrib_qdgc.longitude=geo_qdgc.longitude ",
#'     "AND geo_qdgc.countryname='South Africa'"))

dbRequest <- function(request) {
    db <- connect_online()
    res <- DBI::dbGetQuery(db, request)
    close_db_connection(db)
    res
}
