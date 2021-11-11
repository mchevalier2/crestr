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


#' @export
print.crestObj <- function(x, ...) {
    if(base::missing(x)) x

    name <- find.original.name(x)
    is_formatted <- is_fitted <- is_reconstructed <- is_looed <- FALSE
    if(x$misc$stage == 'data_extracted' | x$misc$stage == 'data_inserted') {
        is_formatted <- TRUE
    } else if (x$misc$stage == 'PDFs_fitted') {
        is_formatted <- is_fitted <- TRUE
    }else if (x$misc$stage == 'climate_reconstructed') {
        is_formatted <- is_fitted <- is_reconstructed <- TRUE
    } else if (x$misc$stage == 'leave_one_out') {
        is_formatted <- is_fitted <- is_reconstructed <- is_looed <- TRUE
    }

    cat('*\n')
    cat(paste0('* Summary of the crestObj named `',name,'`:\n'))
    cat(paste0('*   x Calibration data formatted .. ', is_formatted,'\n'))
    cat(paste0('*   x PDFs fitted ................. ', is_fitted,'\n'))
    cat(paste0('*   x Climate reconstructed ....... ', is_reconstructed,'\n'))
    cat(paste0('*   x Leave-One-Out analysis ...... ', is_looed,'\n'))
    cat('*\n')
    if(is_formatted) {
        if(is.data.frame(x$inputs$df)) {
            cat(paste0('* The dataset to be reconstructed (`df`) is composed of ', nrow(x$inputs$df),' samples with ',ncol(x$inputs$df)-1,' taxa.\n'))
        }
        cat(paste0('* Variable', ifelse(length(x$parameters$climate) > 1, 's', ''),' to analyse: ', paste(x$parameters$climate, collapse=', '),'\n'))
        cat(paste0('*\n'))

        taxa_type <- get_taxa_type(x$parameters$taxaType)
        cat(paste0('* The calibration dataset was defined using the following set of parameters:\n'))
        cat(paste0('*   x Proxy type ............ ', taxa_type, ifelse(x$parameters$taxaType == 0, '', 's'), '\n'))
        if(!is.na(x$parameters$xmn) | !is.na(x$parameters$xmx)) cat(paste0('*   x Longitude ............. [', x$parameters$xmn, ' - ', x$parameters$xmx,']\n'))
        if(!is.na(x$parameters$ymn) | !is.na(x$parameters$ymx)) cat(paste0('*   x Latitude .............. [', x$parameters$ymn, ' - ', x$parameters$ymx,']\n'))
        if(x$parameters$taxaType > 0) {
            if(!is.na(x$parameters$elev_min) | !is.na(x$parameters$elev_max)) cat(paste0('*   x Elevation ............. [', x$parameters$elev_min, ' - ', x$parameters$elev_max,']\n'))
            if(!is.na(x$parameters$elev_range)) cat(paste0('*   x Elevation range ....... ',x$parameters$elev_range, '\n'))
            if(!is.na(x$parameters$year_min) | !is.na(x$parameters$year_max)) cat(paste0('*   x Observation date ...... [', x$parameters$year_min, ' - ', x$parameters$year_max,']\n'))
            if(!is.na(x$parameters$nodate)) cat(paste0('*   x Undated observations .. ', x$parameters$nodate, '\n'))
            if(!unique(is.na(x$parameters$type_of_obs))) {
                OBSTYPES <- dbRequest("SELECT * FROM typeofobservations ORDER BY type_of_obs", x$misc$dbname)
                cat(paste0('*   x Type of observations .. ', paste(base::trimws(OBSTYPES[x$parameters$type_of_obs,2]), collapse=', '), '\n'))
            }
            if(!unique(is.na(x$parameters$continents))) cat(paste0('*   x Continents ............ ', paste(x$parameters$continents, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$countries))) cat(paste0('*   x Countries ............. ', paste(x$parameters$countries, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$basins))) cat(paste0('*   x Basins ................ ', paste(x$parameters$basins, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$sectors))) cat(paste0('*   x Sectors ............... ', paste(x$parameters$sectors, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$realms))) cat(paste0('*   x Realms ................ ', paste(x$parameters$realms, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$biomes))) cat(paste0('*   x Biomes ................ ', paste(x$parameters$biomes, collapse=', '), '\n'))
            if(!unique(is.na(x$parameters$ecoregions))) cat(paste0('*   x Ecoregions ............ ', paste(x$parameters$ecoregions, collapse=', '), '\n'))
        }
        cat(paste0('*\n'))
        if(is_fitted) {
            cat(paste0('* The PDFs were fitted using the following set of parameters:\n'))
            if(!is.na(x$parameters$minGridCells)) cat(paste0('*   x Minimum distinct of distinct occurences .. ', x$parameters$minGridCells, '\n'))
            if(!is.na(x$parameters$weightedPresences)) cat(paste0('*   x Weighted occurence data .................. ', x$parameters$weightedPresences, '\n'))
            if(!is.na(x$parameters$npoints)) cat(paste0('*   x Number of points to fit the PDFs ......... ', x$parameters$npoints, '\n'))
            if(!is.na(x$parameters$geoWeighting)) {
                cat(paste0('*   x Geographical weighting ................... ',x$parameters$geoWeighting, '\n'))
                cat(paste0('*       Using bins of width .................... ', x$parameters$climate[1], ': ', x$parameters$bin_width[x$parameters$climate[1], 1],'\n'))
                for(clim in x$parameters$climate[-1]) {
                    cat(paste0('*       ', paste(rep('_', nchar('Using bins of width ....................')), collapse=''), ' ', clim, ': ', x$parameters$bin_width[clim, 1],'\n'))
                }
            }
            if(!is.na(x$parameters$climateSpaceWeighting)) cat(paste0('*   x Weighting of the climate space ........... ',x$parameters$climateSpaceWeighting, '\n'))
            cat(paste0('*   x Shape of the PDFs ........................ ',x$parameters$climate[1], ': ', x$parameters$shape[x$parameters$climate[1], 1], '\n'))
            for(clim in x$parameters$climate[-1]) {
                cat(paste0('*     ', paste(rep('_', nchar('Shape of the PDFs ........................')), collapse=''), ' ',clim, ': ', x$parameters$shape[clim, 1], '\n'))
            }
            cat(paste0('*\n'))
        }
        excluded_taxa <- sum(unlist(lapply(x$misc$taxa_notes, function(x){if(is.data.frame(x)){return(nrow(x))}else{return(length(x))}})))
        if(excluded_taxa > 0) {
            cat(paste0('* Of the ',nrow(x$inputs$selectedTaxa), ' taxa provided in `df` and `PSE`, ', excluded_taxa,' cannot be analysed.\n'))
            cat(paste0('* (This may be expected, but check `', name,'$misc$taxa_notes` for additional details.)\n'))
            cat(paste0('*\n'))
        }
        if(is_reconstructed) {
            cat(paste0('* The reconstructions were performed with the following set of parameters:\n'))
            if(!is.na(x$parameters$presenceThreshold)) cat(paste0('*   x Minimum presence value .................. ',  x$parameters$presenceThreshold,'\n'))
            if(!is.na(x$parameters$taxWeight)) cat(paste0('*   x Weighting of the taxa ................... ',  x$parameters$taxWeight,'\n'))
            if(!unique(is.na(x$parameters$uncertainties))) cat(paste0('*   x Calculated uncertainties ................ ',  paste(x$parameters$uncertainties, collapse=', '),'\n'))
            cat(paste0('*   x Number of taxa selected to reconstruct .. ', x$parameters$climate[1],': ', sum(x$inputs$selectedTaxa[, x$parameters$climate[1]] > 0),'\n'))
            for(clim in x$parameters$climate[-1]) {
                cat(paste0('*     ----------------------------------------- ', clim, ': ', sum(x$inputs$selectedTaxa[, clim] > 0),'\n'))
            }
            cat(paste0('*\n'))
        }
    }
}


#' Plot the reconstructions.
#'
#' Plot the reconstructions and their uncertainties if they exist.
#'
#' @inheritParams graphics::plot
#' @param x A \code{\link{crestObj}} produced by either the
#'        \code{\link{crest.reconstruct}} or \code{\link{crest}}) functions.
#' @param climate The climate variables to plot (default is all the
#'        reconstructed variables from x)
#' @param uncertainties A (vector of) threshold value(s) indicating the error
#'        bars that should be calculated (default are the values stored in x).
#' @param optima A boolean to indicate whether to plot the optimum (\code{TRUE})
#'        or the mean (\code{FALSE}) estimates.
#' @param pt.cex The size of the points (default 0.8).
#' @param pt.lwd The thickness of the lines (default 0.8).
#' @param pt.col The colour of the points and lines.
#' @param simplify A boolean to indicate if the full distribution of uncertainties
#'        should be plotted (\code{FALSE}, default) or if they should be
#'        simplified to the uncertainty range(s).
#' @param add_modern Adds the modern climate values to the plot.
#' @param save A boolean to indicate if the diagram should be saved as a pdf file.
#'        Default is \code{FALSE}.
#' @param as.png A boolean to indicate if the output should be saved as a png.
#'        Default is \code{FALSE} and the figure is saved as a pdf file.
#' @param png.res The resolution of the png file (default 300 pixels per inch).
#' @param width,height The dimensions of the pdf file (default 5.51in ~14cm).
#' @param filename An absolute or relative path that indicates where the diagram
#'        should be saved. Also used to specify the name of the file. Default:
#'        the file is saved in the working directory under the name
#'        \code{'Reconstruction_climate.pdf'}.
#' @param col A colour gradient.
#' @return No return value, this function is used to plot.
#' @export
#' @examples
#' \dontrun{
#'   data(crest_ex)
#'   data(crest_ex_pse)
#'   data(crest_ex_selection)
#'   reconstr <- crest(
#'     df = crest_ex, pse = crest_ex_pse, taxaType = 0,
#'     climate = c("bio1", "bio12"), bin_width = c(2, 20),
#'     shape = c("normal", "lognormal"),
#'     selectedTaxa = crest_ex_selection, dbname = "crest_example"
#'   )
#'   reconstr <- loo(reconstr)
#' }
#' ## example using pre-saved reconstruction obtained with the previous command.
#' data(reconstr)
#' plot(reconstr)
#' plot(reconstr, climate='bio1', simplify = TRUE)
#'
plot.crestObj <- function(x,
                          climate = x$parameters$climate,
                          uncertainties = x$parameters$uncertainties,
                          optima = TRUE,
                          add_modern = FALSE,
                          simplify = FALSE,
                          xlim = NA, ylim = NA,
                          pt.cex = 0.8, pt.lwd = 0.8,
                          pt.col=ifelse(simplify, 'black', 'white'),
                          save = FALSE, width = 5.51, height = 5.51,
                          as.png = FALSE, png.res=300,
                          filename = 'Reconstruction.pdf',
                          col=viridis::viridis(125)[26:125],
                          ...) {

    if(base::missing(x)) x

    if (length(x$reconstructions) == 0 || is.null(climate)) {
        stop("No reconstruction available for plotting.\n")
    }

    idx <- 0
    filename <- base::strsplit(filename, '.pdf')[[1]]

    if(unique(is.na(xlim))) {
        if(is.character(x$inputs$x) | is.factor(x$inputs$x)) {
            xlim <- c(1, length(x$inputs$x))
        } else {
            xlim <- range(x$inputs$x)
        }
    }

    if(add_modern) {
        if (length(x$misc$site_info) <= 3) {
            add_modern <- FALSE
        }
    }

    if(!save) {
        par_usr <- graphics::par(no.readonly = TRUE)
    }

    for (clim in climate) {
        if (idx == 0 && !save) {
            graphics::par(mfrow = grDevices::n2mfrow(length(climate)))[[1]]
        }
        idx <- idx + 1
        pdf <- t(x$reconstructions[[clim]][["posterior"]])[-1, ]
        pdfter <- pdf

        for (i in 2:ncol(pdf)) {
            oo <- rev(order(pdf[, i]))
            tmp1 <- pdf[oo, 1]
            tmp2 <- pdf[oo, i]
            oo <- order(tmp1)
            pdfter[, i] <- cumsum(tmp2 / sum(tmp2))[oo]
        }

        var_to_plot <- ifelse(optima, 2, 3)
        xmn <- which.min(x$reconstructions[[clim]][["optima"]][, var_to_plot])
        xmx <- which.max(x$reconstructions[[clim]][["optima"]][, var_to_plot])

        if (unique(is.na(ylim))) {
            ymn <- pdfter[which(pdfter[, xmn + 1] <= 0.99)[1], 1]
            ymx <- pdfter[rev(which(pdfter[, xmx + 1] <= 0.99))[1], 1]
        } else {
            ymn <- ylim[idx * 2 - 1]
            ymx <- ylim[idx * 2]
        }
        climate_names <- accClimateVariables()

        if(is.character(x$inputs$x) | is.factor(x$inputs$x)) {
            if(simplify) warning("The plotting function is not adapted to non-numeric x values. The sample names were replaced by numeric indexes.")
            xx <- base::seq_along(x$inputs$x)
        } else {
            xx <- sort(base::jitter(x$inputs$x, 0.0001))
        }



        val <- apply(pdfter[, -1], 2, function(x) {
            if(is.na(x[1])) return(c(NA, NA))
            w <- which(x <= uncertainties[length(uncertainties)])
            return(c(w[1], w[length(w)]))
            }
        )
        ylim2 <- pdfter[c(min(val[1, ], na.rm=TRUE),max(val[2, ], na.rm=TRUE)), 1]
        ylim2[1] <- max(ylim[1], ylim2[1], na.rm=TRUE)
        ylim2[2] <- min(ylim[2], ylim2[2], na.rm=TRUE)

        if(save) {
            if(as.png) {
                grDevices::png(paste0(strsplit(filename, '.png')[[1]],'_',clim,'.png'), width = width, height = height, units='in', res=png.res)
            } else {
                grDevices::pdf(paste0(strsplit(filename, '.pdf')[[1]],'_',clim,'.pdf'), width = width, height = height)
            }
        }
        graphics::par(ps=8)

        if(!simplify) {
            if (!requireNamespace("plot3D", quietly = TRUE)) {
                warning("The package 'plot3D' is required to plot the full distributions of the data. The data are plotted using `simplify=TRUE` instead.\n")
                simplify=TRUE
            }
        }

        if(simplify) {
            graphics::par(mar = c(2, 2.2, 0.5, 0.5), ps=8, lwd=1)
            graphics::plot(0,0, type='n', xlim=xlim, ylim = ylim2,
                 xaxs='i', yaxs='i', frame = TRUE, axes=FALSE)

            for( u in length(uncertainties):1) {
                val <- apply(pdfter[, -1], 2, function(x) {
                    if(is.na(x[1])) return(c(NA, NA))
                    w <- which(x <= uncertainties[u])
                    return(c(w[1], w[length(w)]))
                    }
                )

                if (sum(is.na(x$reconstructions[[clim]]$optima[, var_to_plot])) > 0) {
                    j <- 1
                    na_cnt <- 0
                    w <- which(is.na(x$reconstructions[[clim]]$optima[, var_to_plot]))
                    k <- 0
                    while(k < length(xx)) {
                        na_cnt <- na_cnt + 1
                        k <- min(w[na_cnt] - 1, length(xx), na.rm=TRUE)
                        graphics::polygon(c(xx[j:k], rev(xx[j:k])), c(pdfter[val[1, ], 1][j:k], rev(pdfter[val[2, ], 1][j:k])), col=crestr::makeTransparent('cornflowerblue', alpha=1 - u / (length(uncertainties) + 1)), border='grey90', lwd=0.1)
                        j <- w[na_cnt] + 1
                        while(is.na(x$reconstructions[[clim]]$optima[j, var_to_plot]) & na_cnt < length(w)) {
                            j <- j + 1
                            na_cnt <- na_cnt + 1
                        }
                    }
                } else {
                    graphics::polygon(c(xx, rev(xx)), c(pdfter[val[1, ], 1], rev(pdfter[val[2, ], 1])), col=crestr::makeTransparent('cornflowerblue', alpha=1 - u / (length(uncertainties) + 1)), border='grey90', lwd=0.1)
                }
            }
            if(add_modern) {
                graphics::segments(xlim[1], x$misc$site_info$climate[, clim], xlim[2], x$misc$site_info$climate[, clim],
                    col = "grey70", cex = 0.5, lty = 2
                )
            }
            graphics::points(xx, x$reconstructions[[clim]]$optima[, var_to_plot], type='o', pch=18, col=pt.col, cex=pt.cex, lwd=pt.lwd)

            graphics::par(mgp=c(2,0.3,0), las=1)
            graphics::axis(2, lwd.ticks=0.8, lwd=0, tck=-0.01, cex.axis=6/7)
            graphics::par(las=0)
            graphics::mtext(climate_names[climate_names[, 2] == clim, 3], side=2, line=1.3, cex=1, font=2)

            graphics::par(mgp=c(1,0,0), las=1)
            graphics::mtext(x$inputs$x.name, side=1, line=0.7, cex=1, font=2)
            if(is.character(x$inputs$x) | is.factor(x$inputs$x)) {
                graphics::axis(1, at=xx, labels=x$inputs$x, cex.axis=6/7, lwd.ticks=0.8, tck=-0.01)
            } else {
                graphics::axis(1, cex.axis=6/7, lwd.ticks=0.8, tck=-0.01)
            }

        } else {

            graphics::par(mar = c(2, 2.2, 3, 0.5), lwd=0.8)
            plot3D::image2D(
              z = (1 - as.matrix(t(pdfter[, -1]))),
              y = pdfter[, 1],
              x = xx,
              xlim = xlim,
              ylim = c(ymn, ymx),
              zlim = c(0, 1),
              col = col,
              axes=FALSE,
              colkey = FALSE,
              resfac = 1,
              tck = -.013,
              mgp = c(2, .3, 0),
              las = 1,
              hadj = c(1, 1),
              xlab = "",
              ylab = '',
              cex.lab = 6 / 7
            )

            graphics::par(mgp=c(2,0.3,0), las=1)
            graphics::axis(2, lwd.ticks=0.8, lwd=0, tck=-0.01, cex.axis=6/7)
            graphics::par(las=0)
            graphics::mtext(climate_names[climate_names[, 2] == clim, 3], side=2, line=1.3, cex=1, font=2)

            graphics::par(mgp=c(1,0,0), las=1)
            graphics::mtext(x$inputs$x.name, side=1, line=0.7, cex=1, font=2)
            if(is.character(x$inputs$x) | is.factor(x$inputs$x)) {
                graphics::axis(1, at=xx, labels=x$inputs$x, cex.axis=6/7, lwd.ticks=0.8, tck=-0.01)
            } else {
                graphics::axis(1, cex.axis=6/7, lwd.ticks=0.8, tck=-0.01)
            }

            graphics::par(lwd=pt.lwd)
            for (e in uncertainties) {
                val <- apply(pdfter[, -1], 2, function(x) {
                    if(is.na(x[1])) return(c(NA, NA))
                    w <- which(x <= e)
                    return(c(w[1], w[length(w)]))
                    }
                )
                graphics::points(xx, pdfter[val[1, ], 1],
                    type = "l", col = pt.col, lty = 3
                )
                graphics::points(xx, pdfter[val[2, ], 1],
                    type = "l", col = pt.col, lty = 3
                )
            }
            if(add_modern) {
                graphics::segments(xlim[1], x$misc$site_info$climate[, clim], xlim[2], x$misc$site_info$climate[, clim],
                    col = "red", cex = 0.5, lty = 2
                )
            }
            graphics::points(x$reconstructions[[clim]][["optima"]],
                pch = 18, col = pt.col, cex = pt.cex, type='o'
            )

            graphics::par(mgp=c(2,0,0), las=1, lwd=0.8)

            plot3D::colkey(
              side = 3,
              length = 0.8,
              dist = -0.01,
              lwd = 0.1,
              cex.axis = 6 / 7,
              clim = c(1, 0),
              col = col,
              clab = "Confidence level",
              font.clab = 1,
              line.clab = 1,
              adj.clab = 0.5,
              add = TRUE,
              tck = -0.3,
              mgp = c(0, .2, 0),
              lwd.tick = 0.8
            )
        }
        if(save) grDevices::dev.off()
    }
    if(!save)  graphics::par(par_usr)
    invisible(x)
}
