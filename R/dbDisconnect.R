#' Disconnect the database connection.
#'
#' Disconnect the database connection.
#'
#' @param db An active database connection
#' @export
#' @examples
#' db <- connect_online()
#' close_db_connection(db)
#' \dontrun{
#'   db <- connect_online()
#'   close_db_connection(db)
#' }
#'
close_db_connection <- function(db) {
    DBI::dbDisconnect(db)
}
