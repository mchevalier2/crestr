#' Extract the climate space
#'
#' This function will extract the climate space associated with the species
#' distributions
#'
#' @param x a crestObj produced by the crest.get_distributions function.
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A crest() object containing the spatial distributions and the climate space
#' @export
#' @examples
#' \dontrun{
#' data(crest_ex)
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' x <- crest.get_distributions(
#'   taxa.name = colnames(crest_ex)[-1],
#'   pse = crest_ex_pse, taxaType = 0,
#'   climate = c("bio1", "bio12"),
#'   selectedTaxa = crest_ex_selection, dbname = "crest_example"
#' )
#' x <- crest.climate_space(x, dbname = "crest_example")
#' x
#' head(x$modelling$climate_space)
#' dim(x$modelling$climate_space)
#' }
#'
crest.climate_space <- function(x, dbname = "gbif4crest_02") {
  climate_space <- getClimateSpace(
    x$parameters$climate,
    x$parameters$xmn, x$parameters$xmx, x$parameters$ymn, x$parameters$ymx,
    x$parameters$continents, x$parameters$countries,
    x$parameters$realms, x$parameters$biomes, x$parameters$ecoregions,
    dbname
  )
  colnames(climate_space)[-c(1, 2)] <- x$parameters$climate
  x$modelling$climate_space <- climate_space
  x
}
