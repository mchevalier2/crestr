#' Connect to the gbif4crest database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @inheritParams crestObj
#' @param leave_one_out A boolean to indicate whether the leave one out (loo)
#'        reconstructions should be computed (default FALSE).
#' @param verbose A boolean to print non-essential comments on the terminal (default TRUE).
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
#'   selectedTaxa = crest_ex_selection, dbname = "crest_example",
#'   leave_one_out = TRUE
#' )
#' plot(recons)
#' plot_loo(recons)
#'
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
                  uncertainties = c(0.5, 0.95),
                  leave_one_out = FALSE,
                  verbose=TRUE,
                  dbname = "gbif4crest_02") {


  x <- crest.get_modern_data(
    pse = pse, taxaType = taxaType, climate = climate,
    taxa.name = colnames(df)[-1],
    xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx,
    continents = continents, countries = countries,
    realms = realms, biomes = biomes, ecoregions = ecoregions,
    minGridCells = minGridCells,
    selectedTaxa = selectedTaxa,
    verbose = verbose,
    dbname = dbname
  )

  x <- crest.calibrate(x,
    npoints = npoints,
    shape = shape,
    bin_width = bin_width,
    geoWeighting = geoWeighting,
    climateSpaceWeighting = geoWeighting,
    verbose = verbose
  )

  x <- crest.reconstruct(x,
    df = df,
    presenceThreshold = presenceThreshold,
    taxWeight = taxWeight,
    uncertainties = uncertainties,
    skip_for_loo = FALSE, verbose = verbose
  )

  if(leave_one_out) x <- loo(x, verbose = verbose)

  x
}
