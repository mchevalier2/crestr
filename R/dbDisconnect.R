#' Disconnect the database connection.
#'
#' Disconnect the database connection.
#'
#' @param db An active database connection
#' @return No return value, function called to close the connection to the database.
#' @export
#' @examples
#' \dontrun{
#'   db <- connect_online()
#'   close_db_connection(db)
#' }
#'
close_db_connection <- function(db) {
    DBI::dbDisconnect(db)
}
