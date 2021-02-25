#' A wrapper for all the crest functions.
#'
#' Runs all the different steps of a CREST reconstruction in one function.
#'
#' @inheritParams crestObj
#' @param site_info A vector containing the coordinates of the study site.
#'        Default \code{c(NA, NA)}.
#' @param site_name The name of the dataset (default \code{NA}).
#' @param ai.sqrt A boolean to indicate whether ai values should be square-root
#'        transformed (default \code{FALSE}).
#' @param leave_one_out A boolean to indicate whether the leave one out (loo)
#'        reconstructions should be computed (default \code{FALSE}).
#' @param verbose A boolean to print non-essential comments on the terminal
#'        (default \code{TRUE}).
#' @param dbname The name of the database. Default is \code{'gbif4crest_02'}.
#' @return A \code{\link{crestObj}} containing the reconstructions.
#' @export
#' @examples
#' data(crest_ex)
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' reconstr <- crest(
#'   df = crest_ex, pse = crest_ex_pse, taxaType = 0,
#'   site_info = c(7.5, 7.5), site_name = 'crest_example',
#'   climate = c("bio1", "bio12"), bin_width = c(2, 20),
#'   shape = c("normal", "lognormal"),
#'   selectedTaxa = crest_ex_selection, dbname = "crest_example",
#'   leave_one_out = TRUE,
#'   verbose = FALSE
#' )
#' plot(reconstr)
#' plot_loo(reconstr)
#'
crest <- function(df, pse, taxaType, climate,
                  site_info = rep(NA, length(climate)),
                  site_name = NA,
                  xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                  continents = NA, countries = NA,
                  realms = NA, biomes = NA, ecoregions = NA,
                  minGridCells = 20,
                  selectedTaxa = NA,
                  bin_width = rep(1, length(x$parameters$climate)),
                  shape = rep("normal", length(x$parameters$climate)),
                  npoints = 500,
                  ai.sqrt = FALSE,
                  geoWeighting = TRUE,
                  climateSpaceWeighting = TRUE,
                  presenceThreshold = 0,
                  taxWeight = "normalisation",
                  uncertainties = c(0.5, 0.95),
                  leave_one_out = FALSE,
                  verbose=TRUE,
                  dbname = "gbif4crest_02") {


    x <- crest.get_modern_data(
        pse = pse, taxaType = taxaType, climate = climate, df = df,
        xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx,
        continents = continents, countries = countries,
        realms = realms, biomes = biomes, ecoregions = ecoregions,
        minGridCells = minGridCells,
        selectedTaxa = selectedTaxa,
        verbose = verbose,
        site_info = site_info,
        site_name = site_name,
        ai.sqrt = ai.sqrt,
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
        presenceThreshold = presenceThreshold,
        taxWeight = taxWeight,
        uncertainties = uncertainties,
        skip_for_loo = FALSE, verbose = verbose
    )

    if(leave_one_out) x <- loo(x, verbose = verbose)

    x
}
