#' Extract The climate values associated to a set of coordinates.
#'
#' Extract The climate values associated to a set of coordinates.
#'
#' @inheritParams crestObj
#' @param long The longitude of the site.
#' @param lat The latitude of the site.
#' @param climate The climate variables to extract the values from. Returns all
#'        possible values by default.
#' @param resol The resolution of the target climatology (default 0.25 degrees).
#' @return A data frame containing the climate values.
#' @export
#' @examples
#' \dontrun{
#'   climate_from_xy(50, 10, c('bio1', 'ai'))
#'   climate_from_xy(50, 10)
#' }
#'
climate_from_xy <- function(long, lat,
                            climate = accClimateVariables()[, 2],
                            resol = 0.25, dbname = "gbif4crest_02") {

    if(base::missing(long)) long
    if(base::missing(lat)) lat

    if(!is.numeric(long) | !is.numeric(lat)) {
        stop('The coordinates are not numeric.\n')
    }

    db <- connect_online(dbname = dbname)
    if(!methods::is(db, 'DBIConnection')) {
        cat("The connection to the database failed and the process has been stopped. check your internet connection and database IDs.\n")
        return(NA)
    }

    long <- resol * (long %/% resol) + resol/2;
    lat  <- resol * (lat %/% resol) + resol/2;

    req <- paste0(
      "  SELECT DISTINCT ", paste(climate, collapse=', '),
      "    FROM data_qdgc ",
      "   WHERE longitude = ", long, " AND latitude = ", lat
    )

    res <- dbRequest(req, dbname)
    if (nrow(res) == 0) {
        warning('No climate associated with these coordinates.')
    }
    colnames(res) <- climate
    res
}
