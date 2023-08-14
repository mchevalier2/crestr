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

    err <- c()
    for(clim in climate) {
        if(! clim %in% accClimateVariables()[, 2]) err <- c(err, clim)
    }
    if(length(err) > 0) {
        stop(paste0("The following variables are not available in your crestObj: '", paste(err, collapse="', '"), "'\n\n"))
        return(invisible(NA))
    }

    if(!testConnection(dbname)) return(NA)

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
        res <- rbind(res, rep(NA, length(climate)))
    }
    colnames(res) <- climate
    res
}
