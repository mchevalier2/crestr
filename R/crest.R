#' Connect to the gbif4crest database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @inheritParams crestObj
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return The parameters to be used by crest()
#' @export
#' @examples
#' data(crest_ex)
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' recons <- crest(
#'   df = crest_ex, pse = crest_ex_pse, taxaType = 0,
#'   climate = c("bio1", "bio12"), bin_width = c(2, 20),
#'   shape = c("normal", "lognormal"),
#'   selectedTaxa = crest_ex_selection, dbname = "crest_example"
#' )
#' plot(recons)
crest <- function(df, pse, taxaType, climate,
                  xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                  continents = NA, countries = NA,
                  realms = NA, biomes = NA, ecoregions = NA,
                  minGridCells = 20,
                  selectedTaxa = NA,
                  bin_width = rep(1, length(x$parameters$climate)),
                  shape = rep("normal", length(x$parameters$climate)),
                  npoints = 500,
                  geoWeighting = TRUE,
                  climateSpaceWeighting = TRUE,
                  presenceThreshold = 0,
                  taxWeight = "normalisation",
                  dbname = "gbif4crest_02") {


  x <- crest.get_modern_data(
    taxa.name = colnames(df)[-1], pse = pse, taxaType = taxaType, climate = climate,
    xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx,
    continents = continents, countries = countries,
    realms = realms, biomes = biomes, ecoregions = ecoregions,
    minGridCells = minGridCells,
    selectedTaxa = selectedTaxa,
    dbname = dbname
  )

  x <- crest.calibrate(x,
    npoints = npoints,
    geoWeighting = geoWeighting,
    climateSpaceWeighting = geoWeighting
  )

  x <- crest.reconstruct(x,
    df = df,
    presenceThreshold = presenceThreshold,
    taxWeight = taxWeight
  )
  x
}
