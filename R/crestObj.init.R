#' Create a \code{crestObj} object.
#'
#' Creates a \code{crestObj} object with all default parameters.
#'
#' @param taxa.name A vector that contains the names of the taxa to study.
#' @param pse A pollen-Species equivalency table. See \code{\link{createPSE}} for
#'        details.
#' @param taxaType A numerical index (between 1 and 6) to define the type of
#'        palaeoproxy used: 1 for plants, 2 for beetles, 3 for chironomids,
#'        4 for foraminifers, 5 for diatoms and 6 for rodents. The example
#'        dataset uses taxaType=0 (pseudo-data). Default is 1.
#' @param climate A vector of the climate variables to extract. See
#'        \code{\link{accClimateVariables}} for the list of accepted values.
#' @param df A data frame containing the data to reconstruct (counts,
#'        percentages or presence/absence data).
#' @param x.name A string describing the x axis (e.g. 'Sample Name', 'Age',
#'        'Depth').
#' @param x The name, age or depth of the rows of df (the samples).
#' @param xmn,xmx,ymn,ymx The coordinates defining the study area.
#' @param elev_min,elev_max Parameters to only selected grid cells with an
#'        elevation higher than elev_min or lower than elev_max (default is
#'        '\code{NA} ).
#' @param elev_range Parameters discard the grid cell with a high elevation
#'        range (default is \code{NA}).
#' @param year_min,year_max The oldest and youngest occurrences accepted
#'        (default is 1900-2021).
#' @param nodate A boolean to accept occurrences without a date (can overlap
#'        with occurrences with a date; default \code{TRUE}).
#' @param type_of_obs The type of observation to use in the study. 1: human
#'        observation, 2: observation, 3: preserved specimen, 4: living specimen,
#'        5: fossil specimen, 6: material sample, 7: machine observation, 8:
#'        literature, 9: unknown (Default \code{c(1, 2, 3, 8, 9)})
#' @param dbname The name of the data source database.
#' @param continents A vector of the continent names defining the study area.
#' @param countries A vector of the country names defining the study area.
#' @param basins A vector of the ocean names defining the study area.
#' @param sectors A vector of the marine sector names defining the study area.
#' @param realms A vector of the studied botanical realms defining the study area.
#' @param biomes A vector of the studied botanical biomes defining the study area.
#' @param ecoregions A vector of the studied botanical ecoregions defining the
#'        study area.
#' @param distributions A dataframe containing the presence records of the
#'        studied proxies and their associated climate values.
#' @param minGridCells The minimum number of unique presence data necessary to
#'        estimate a species' climate response. Default is 20.
#' @param weightedPresences A boolean to indicate whether the presence records
#'        should be weighted. Default is \code{FALSE}.
#' @param bin_width The width of the bins used to correct for unbalanced climate
#'        state. Use values that split the studied climate gradient in
#'        15-25 classes (e.g. 2°C for temperature variables). Default is 1.
#' @param shape The imposed shape of the species \code{pdfs}. We recommend using
#'        'normal' for temperature variables and 'lognormal' for the
#'        variables that can only take positive values, such as
#'        precipitation or aridity. Default is 'normal' for all.
#' @param selectedTaxa A data frame assigns which taxa should be used for each
#'        variable (1 if the taxon should be used, 0 otherwise). The colnames
#'        should be the climate variables' names and the rownames the taxa
#'        names. Default is 1 for all taxa and all variables.
#' @param npoints The number of points to be used to fit the \code{pdfs}. Default 200.
#' @param geoWeighting A boolean to indicate if the species should be weighting
#'        by the square root of their extension when estimating a genus/family
#'        level taxon-climate relationships.
#' @param climateSpaceWeighting A boolean to indicate if the species \code{pdfs}
#'        should be corrected for the modern distribution of the climate space
#'        (default \code{TRUE}).
#' @param climateSpaceWeighting.type A correction factor for the clame space
#'        weighting correction to limit the edge effects. Either 'linear'
#'        (default), 'sqrt' or 'log'.
#' @param presenceThreshold All values above that threshold will be used in the
#'        reconstruction (e.g. if set at 1, all percentages below 1 will be set
#'        to 0 and the associated presences discarded). Default is 0.
#' @param taxWeight One value among the following: 'originalData',
#'        'presence/absence', 'percentages' or 'normalisation' (default).
#' @param uncertainties A (vector of) threshold value(s) indicating the error
#'        bars that should be calculated (default both 50 and 95% ranges).
#' @return A \code{crestObj} object that is used to store data and information
#'         for reconstructing climate
#' @export
#' @seealso See \code{vignette('technicalities')} for details about the structure
#'          of the object. See also \url{https://gbif.github.io/parsers/apidocs/org/gbif/api/vocabulary/BasisOfRecord.html}
#'          for a detailed explanation of the types of observation.
crestObj <- function(taxa.name, taxaType, climate,
                     pse = NA, dbname = NA,
                     continents = NA, countries = NA,
                     basins = NA, sectors = NA,
                     realms = NA, biomes = NA, ecoregions = NA,
                     xmn = NA, xmx = NA, ymn = NA, ymx = NA,
                     elev_min = NA, elev_max = NA, elev_range = NA,
                     year_min = 1900, year_max = 2021, nodate = TRUE,
                     type_of_obs = c(1, 2, 3, 8, 9),
                     df = NA, x = NA, x.name = "",
                     minGridCells = 20, weightedPresences = FALSE,
                     bin_width = NA,
                     shape = NA,
                     npoints = 200,
                     geoWeighting = TRUE,
                     climateSpaceWeighting = TRUE,
                     climateSpaceWeighting.type = 'linear',
                     selectedTaxa = NA,
                     distributions = NA,
                     presenceThreshold = 0,
                     taxWeight = "normalisation",
                     uncertainties = c(0.5, 0.95)) {

    if(base::missing(taxa.name)) taxa.name
    if(base::missing(taxaType)) taxaType
    if(base::missing(climate)) climate

    if(is.na(bin_width)) {
        bin_width <- as.data.frame(matrix(rep(1, length(climate)), ncol=1))
        rownames(bin_width) <- climate
    }
    if(is.na(shape)) {
        shape <- as.data.frame(matrix(rep("normal", length(climate)), ncol=1))
        rownames(shape) <- climate
    }

    inputs <- list(
        df = df,
        taxa.name = taxa.name,
        x = x,
        pse = pse,
        selectedTaxa = selectedTaxa,
        x.name = x.name
    )

    parameters <- list(
        climate = climate,
        taxaType = taxaType,
        xmn = xmn,
        xmx = xmx,
        ymn = ymn,
        ymx = ymx,
        elev_min = elev_min,
        elev_max = elev_max,
        elev_range = elev_range,
        year_min = year_min,
        year_max = year_max,
        nodate = nodate,
        type_of_obs = type_of_obs,
        continents = continents,
        countries = countries,
        basins = basins,
        sectors = sectors,
        realms = realms,
        biomes = biomes,
        ecoregions = ecoregions,
        taxWeight = taxWeight,
        minGridCells = minGridCells,
        weightedPresences = weightedPresences,
        bin_width = bin_width,
        shape = shape,
        npoints = npoints,
        geoWeighting = geoWeighting,
        climateSpaceWeighting = climateSpaceWeighting,
        climateSpaceWeighting.type = climateSpaceWeighting.type,
        presenceThreshold = presenceThreshold,
        uncertainties = uncertainties
    )

    modelling <- list(taxonID2proxy = NA, climate_space = NA, pdfs = NA, weights = NA, xrange = NA, distributions = distributions)

    reconstructions <- list()

    misc <- list(dbname = dbname, stage = 'init')

    value <- list(
        inputs = inputs,
        parameters = parameters,
        modelling = modelling,
        reconstructions = reconstructions,
        misc = misc
    )
    # class can be set using class() or attr() function
    attr(value, "class") <- "crestObj"
    value
}
