#' Extract The climate values associated to a set of coordinates.
#'
#' Extract The climate values associated to a set of coordinates.
#'
#' @param long The longitude of the site.
#' @param lat The latitude of the site.
#' @param climate The climate variables to extract the values from.
#' @param resol The resolution of the target climatology (default 0.25 degrees)
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A data frame containing the climate values.
#' @export
#' @examples
#' climate_from_xy(50, 10, c('bio1', 'ai'))
climate_from_xy <- function(long, lat, climate, resol = 0.25, dbname = "gbif4crest_02") {
    long <- resol * (long %/% resol) + resol/2;
    lat  <- resol * (lat %/% resol) + resol/2;

    req <- paste0(
      "  SELECT DISTINCT ", paste(climate, collapse=', '),
      "    FROM wc_qdgc ",
      "   WHERE longitude = ", long, " AND latitude = ", lat
    )

    res <- dbRequest(req, dbname)
    if (nrow(res) == 0) {
        cat('No climate associated with these coordinates.\n')
        res <- as.data.frame(matrix(rep(NA, length(climate)), nrow=1))
    }
    colnames(res) <- climate
    res
}
