#' Connect to the gbif4crest database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @inheritParams crestObj
#' @param request A SQL request to be executed.
#' @return The result of the request.
#' @export
#' @examples
#' \dontrun{
#'   # Extracting the number of taxa recorded in the database
#'   dbRequest("SELECT count(*) FROM taxa")
#'
#'   # Extracting all the taxa that have at least one occurrence in South Africa.
#'   southAfricaTaxa <- dbRequest(paste0(
#'     "SELECT DISTINCT taxa.* ",
#'     "FROM taxa, distrib_qdgc, geo_qdgc ",
#'     "WHERE taxa.taxonid=distrib_qdgc.taxonid ",
#'     "AND   distrib_qdgc.latitude=geo_qdgc.latitude ",
#'     "AND   distrib_qdgc.longitude=geo_qdgc.longitude ",
#'     "AND geo_qdgc.countryname='South Africa'"
#'   ))
#'   head(southAfricaTaxa)
#' }
#'
dbRequest <- function(request, dbname = "gbif4crest_02") {
    if(stringr::str_detect(base::tolower(dbname), '.sqlite3')) {
        db <- connect_local_sqlite3(dbname)
    } else {
        db <- connect_online(dbname)
    }
    if(!methods::is(db, 'DBIConnection')) return(NA)
    res <- DBI::dbGetQuery(db, request)
    close_db_connection(db)
    res
}
