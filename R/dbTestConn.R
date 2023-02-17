#' Test if a connection can be established with the calibration data
#'
#' Test if a connection can be established with the calibration data
#'
#' @inheritParams crestObj
#' @return \code{TRUE} if the connection can be established, else \code{FALSE}.
#' @export
#'
testConnection <- function(dbname = "gbif4crest_02") {
    if(stringr::str_detect(base::tolower(dbname), '.sqlite3')) {
        db <- connect_local_sqlite3(dbname)
        err <- '[FAILED]\nThe connection to the database failed and the process has been stopped. Check if the path to your calibration dataset is correct.\n'
    } else {
        db <- connect_online(dbname)
        err <- '[FAILED]\nThe connection to the database failed and the process has been stopped. Check your internet connection and/or database IDs.\n'
    }
    if(!methods::is(db, 'DBIConnection')){
        cat(err)
        return(FALSE)
    }
    close_db_connection(db)
    TRUE
}
