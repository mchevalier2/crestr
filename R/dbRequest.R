#' Connect to the gbif4crest database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @param request A SQL request to be executed.
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return The result of the request.
#' @export
#' @examples
#' #Extracting the number of taxa recorded in the database
#' dbRequest("SELECT count(*) FROM taxa")
#'
#' #Extracting all the taxa that have at least one occurrence in South Africa.
#' \dontrun{
#' southAfricaTaxa <- dbRequest(paste0("SELECT DISTINCT taxa.* ",
#'     "FROM taxa, distrib_qdgc, geo_qdgc ",
#'     "WHERE taxa.taxonid=distrib_qdgc.taxonid ",
#'     "AND   distrib_qdgc.latitude=geo_qdgc.latitude ",
#'     "AND   distrib_qdgc.longitude=geo_qdgc.longitude ",
#'     "AND geo_qdgc.countryname='South Africa'"))
#' head(southAfricaTaxa)
#' }

dbRequest <- function(request, dbname='gbif4crest_02') {
    db <- connect_online(dbname)
    res <- DBI::dbGetQuery(db, request)
    close_db_connection(db)
    res
}
