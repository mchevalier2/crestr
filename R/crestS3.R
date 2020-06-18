crestObj <- function(df, pse, taxaType, climate,
                   xmn, xmx, ymn, ymx,
                   continents, countries,
                   realms, biomes, ecoregions,
                   minGridCells,
                   bin_width, shape,
                   npoints,
                   geoWeighting,
                   climateSpaceWeighting,
                   selectedTaxa,
                   presenceThreshold,
                   taxWeight) {

    inputs <- list( df = df,
                    pse = pse,
                    selectedTaxa = selectedTaxa
                   )

    parameters <- list( climate = climate,
                        taxaType = taxaType,
                        xmn = xmn,
                        xmx = xmx,
                        ymn = ymn,
                        ymx = ymx,
                        continents = continents,
                        countries = countries,
                        realms = realms,
                        biomes = biomes,
                        ecoregions = ecoregions,
                        taxWeight = taxWeight,
                        minGridCells = minGridCells,
                        bin_width = bin_width,
                        shape = shape,
                        npoints = npoints,
                        geoWeighting = geoWeighting,
                        climateSpaceWeighting = climateSpaceWeighting,
                        presenceThreshold = presenceThreshold
                       )

    modelling <- list(taxonID2proxy = NA, climate_space = NA, pdfs = NA, weights = NA)

    reconstructions <- list()

    value <- list( inputs = inputs,
                   parameters = parameters,
                   modelling = modelling,
                   reconstructions = reconstructions
                  )
    # class can be set using class() or attr() function
    attr(value, "class") <- "crestObj"
    value
}


#' @export
print.crestObj <- function(obj) {
    print(lapply(obj, names))
}


#' @export
plot.crestObj <- function(obj) {
    if (length(obj$reconstructions) == 0) {
        cat('No data available for plotting.')
        invisible(obj)
    }
    print(obj)
}
