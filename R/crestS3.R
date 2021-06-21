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
#' @param dbname The name of the data source database.
#' @param continents A vector of the continent names defining the study area.
#' @param countries A vector of the country names defining the study area.
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
#'        by the squareroot of their extension when estimating a genus/family
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

crestObj <- function(taxa.name, taxaType, climate,
                     pse = NA, dbname = NA,
                     continents = NA, countries = NA,
                     basins = NA, sectors = NA,
                     realms = NA, biomes = NA, ecoregions = NA,
                     xmn = NA, xmx = NA, ymn = NA, ymx = NA,
                     df = NA, x = NA, x.name = "",
                     minGridCells = 20, weightedPresences = FALSE,
                     bin_width = rep(1, length(climate)),
                     shape = rep("normal", length(climate)),
                     npoints = 200,
                     geoWeighting = TRUE,
                     climateSpaceWeighting = TRUE,
                     selectedTaxa = NA,
                     distributions = NA,
                     presenceThreshold = 0,
                     taxWeight = "normalisation",
                     uncertainties = c(0.5, 0.95)) {

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

  misc <- list(dbname = dbname)

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
  print(lapply(x, names))
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
#'        should be plooted (\code{FALSE}, default) or if they should be
#'        simplified to the uncertainty range(s).
#' @param add_modern Adds the modern climate values to the plot.
#' @param save A boolean to indicate if the diagram shoud be saved as a pdf file.
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
