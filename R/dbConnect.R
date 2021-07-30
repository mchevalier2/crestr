#' Connect to the gbif4crest calibration database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @param host The host of the database server.
#'        Default is \code{'gbif4crest.cvqgy2mnjwtg.eu-west-3.rds.amazonaws.com'}.
#' @param port The port to connect to the server. Default is 5432.
#' @param dbname The name of the database. Default is \code{'gbif4crest_02'}.
#' @param user The user name to use to connect. Default is \code{'guestuser'}.
#' @param password The password associated with the user name. Default is \code{'pwd12345'}.
#' @return An active connection to a database
#' @export
#' @examples
#' \dontrun{
#'   db <- connect_online()
#' }
#'
connect_online <- function(dbname = "gbif4crest_02", port = 5432, host = "gbif4crest.cvqgy2mnjwtg.eu-west-3.rds.amazonaws.com", user = "guestuser", password = "pwd12345") {
    db <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = dbname,
      host = host,
      port = port,
      user = user,
      password = password
    )
    return(db)
}
