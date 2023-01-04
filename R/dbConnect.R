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
    out <- tryCatch(
        {
            db <- DBI::dbConnect(
              drv = RPostgres::Postgres(),
              dbname = dbname,
              host = host,
              port = port,
              user = user,
              password = password
            )
            db
        },
        error = function(cond) {
            return(NA)
        },
        warning = function(cond) {
            return(cond)
        })
    out
}



#' Connect to the gbif4crest calibration database
#'
#' Connect to the gbif4crest_02 database using a local SQLite3 copy.
#'
#' @param dbname The complete path to the SQLite3 file. The name should end by '.sqlite3'
#' @return An active connection to a database
#' @export
#' @seealso The SQLite3 database can be downloaded from \url{https://figshare.com/articles/dataset/GBIF_for_CREST_database/6743207}.
#' @examples
#' \dontrun{
#'   db <- connect_online()
#' }
#'
connect_local_sqlite3 <- function(dbname = "gbif4crest_02.sqlite3") {
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("The package 'RSQLite' is required. Use install.packages('RSQLite').\n")
    }
    db <- DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname
    )
    return(db)
}
