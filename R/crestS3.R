#' Create a crest() object.
#'
#' Creates a crest() object with all default parameters.
#'
#' @param taxa.name A vector that contains the names of the taxa to study.
#' @param pse A pollen-Species equivalency table. See \code{\link{createPSE}} for
#'        details.
#' @param taxaType A numerical index (between 1 and 6) to define the type of
#'        palaeoproxy used: 1 for plants, 2 for beetles, 3 for
#'        foraminifers, 4 for diatoms, 5 for chironomids and 6 for
#'        rodents. The example dataset uses taxaType=0. Default is 1.
#' @param climate A vector of the climate variables to extract. See
#'        \code{\link{accClimateVariables}} for the list of accepted values.
#' @param df A data frame containing the data to reconstruct (counts,
#'        percentages or presence/absence data).
#' @param x.name A string describing the x axis (e.g. 'Sample Name', 'Age',
#'        'Depth').
#' @param x The name, age or depth of the rows of df (the samples).
#' @param xmn,xmx,ymn,ymx The coordinates defining the study area.
#' @param continents A vector of the continent names defining the study area.
#' @param countries A vector of the country names defining the study area.
#' @param realms A vector of the studied botanical realms defining the study area.
#' @param biomes A vector of the studied botanical biomes defining the study area.
#' @param ecoregions A vector of the studied botanical ecoregions defining the
#'        study area.
#' @param minGridCells The minimum number of unique presence data necessary to
#'        estimate a species' climate response. Default is 20.
#' @param bin_width The width of the bins used to correct for unbalanced climate
#'        state. Use values that split the studied climate gradient in
#'        15-25 classes (e.g. 2°C for temperature variables). Default is 1.
#' @param shape The imposed shape of the species pdfs. We recommend using
#'        'normal' for temperature variables and 'lognormal' for the
#'        variables that can only take positive values, such as
#'        precipitation or aridity. Default is 'normal' for all.
#' @param selectedTaxa A data frame assigns which taxa should be used for each
#'        variable (1 if the taxon should be used, 0 otherwise). The colnames
#'        should be the climate variables' names and the rownames the taxa
#'        names. Default is 1 for all taxa and all variables.
#' @param npoints The number of points to be used to fit the pdfs. Default 200.
#' @param geoWeighting A boolean to indicate if the species should be weighting
#'        by the squareroot of their extension when estimating a genus/family
#'        level taxon-climate relationships.
#' @param climateSpaceWeighting A boolean to indicate if the species pdfs should
#'        be corrected for the modern distribution of the climate space (default
#'        TRUE).
#' @param presenceThreshold All values above that threshold will be used in the
#'        reconstruction (e.g. if set at 1, all percentages below 1 will be set
#'        to 0 and the associated presences discarded). Default is 0.
#' @param taxWeight One value among the following: 'originalData',
#'        'presence/absence', 'percentages' or 'normalisation' (default).
#' @param uncertainties A (vector of) threshold value(s) indicating the error
#'        bars that should be calculated (default both 50 and 95% ranges).
#' @return A CREST object that is used to store data and information for
#'         reconstructing climate
#' @export

crestObj <- function(taxa.name, taxaType, climate,
                     pse = NA,
                     continents = NA, countries = NA,
                     realms = NA, biomes = NA, ecoregions = NA,
                     xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                     df = NA, x = NA, x.name = "",
                     minGridCells = 20,
                     bin_width = rep(1, length(climate)),
                     shape = rep("normal", length(climate)),
                     npoints = 200,
                     geoWeighting = TRUE,
                     climateSpaceWeighting = TRUE,
                     selectedTaxa = NA,
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
    presenceThreshold = presenceThreshold,
    uncertainties = uncertainties
  )

  modelling <- list(taxonID2proxy = NA, climate_space = NA, pdfs = NA, weights = NA, xrange = NA)

  reconstructions <- list()

  misc <- list()

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
#' Plot the reconstructions and their uncertainties
#'
#' @inheritParams graphics::plot
#' @param x A crestObj produced by the crest.reconstruct() or crest() functions.
#' @param climate The climate variables to plot (default is all the reconstructed variables from x)
#' @param uncertainties A (vector of) threshold value(s) indicating the error
#'        bars that should be calculated (default are the values stored in x).
#' @param optima A boolean to indicate whether to plot the optimum (TRUE) or the
#'        mean (FALSE) estimates.
#' @param save A boolean to indicate if the diagram shoud be saved as a pdf file.
#'        Default is FALSE.
#' @param loc An absolute or relative path that indicates the folder where the
#'        diagram(s) hould be saved. Also used to specify the name of the file.
#'        Default: the file is saved in the working directory with a file
#'        created for each variable as variable.pdf.
#' @export
plot.crestObj <- function(x,
                          climate = x$parameters$climate,
                          uncertainties = x$parameters$uncertainties,
                          optima = TRUE,
                          xlim = NA,
                          ylim = NA,
                          save = FALSE,
                          loc = getwd(),
                          ...) {
    if (length(x$reconstructions) == 0 || is.null(climate)) {
        cat("No reconstruction available for plotting.\n")
        return(invisible(x))
    }

    idx <- 0
    par_usr <- list()

    if(is.na(xlim)) {
    if(is.character(x$inputs$x) | is.factor(x$inputs$x)) {
        xlim <- c(1, length(x$inputs$x))
    } else {
        xlim <- range(x$inputs$x)
    }
    }

    for (clim in climate) {
        if (idx == 0 && !save) {
            par_usr$mfrow <- graphics::par(mfrow = grDevices::n2mfrow(length(climate)))[[1]]
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
            xx <- seq_along(x$inputs$x)
        } else {
            xx <- x$inputs$x
        }

        if(save) pdf(paste0(loc, .Platform$file.sep, clim, ".pdf"), width = 5.51, height = 5)
        par_usr$mar <- graphics::par(mar = c(3, 3, 3.2, 0.5))[[1]]
        plot3D::image2D(
          z = (1 - as.matrix(t(pdfter[, -1]))),
          y = pdfter[, 1],
          x = xx,
          xlim = xlim,
          ylim = c(ymn, ymx),
          zlim = c(0, 1),
          col = viridis::viridis(125)[26:125],
          axes=FALSE,
          colkey = FALSE,
          resfac = 1,
          tck = -.013,
          mgp = c(2, .3, 0),
          las = 1,
          hadj = c(1, 1),
          xlab = x$inputs$x.name,
          ylab = climate_names[climate_names[, 2] == clim, 3],
          cex.lab = 6 / 7
        )
        graphics::axis(2, cex.axis=6/7)
        if(is.character(x$inputs$x) | is.factor(x$inputs$x)) {
            graphics::axis(1, at=xx, labels=x$inputs$x, cex.axis=6/7)
        } else {
            graphics::axis(1, cex.axis=6/7)
        }
        if (substr(loc, nchar(loc), nchar(loc)) == .Platform$file.sep) {
            loc <- substr(loc, 1, nchar(loc) - 1)
        }
        for (e in uncertainties) {
            val <- apply(pdfter[, -1], 2, function(x) {
                if(is.na(x[1])) return(c(NA, NA))
                w <- which(x <= e)
                return(c(w[1], w[length(w)]))
                }
            )
            graphics::points(x$reconstructions[[clim]][["optima"]][, 1], pdfter[val[1, ], 1],
                type = "l", col = "white", lty = 3
            )
            graphics::points(x$reconstructions[[clim]][["optima"]][, 1], pdfter[val[2, ], 1],
                type = "l", col = "white", lty = 3
            )
        }

        graphics::points(x$reconstructions[[clim]][["optima"]],
            pch = 18, col = "white", cex = 0.8
        )
        graphics::points(x$reconstructions[[clim]][["optima"]],
            col = "white", cex = 0.5, type = "l"
        )
        plot3D::colkey(
          side = 3,
          length = 0.8,
          dist = -0.01,
          lwd = 0.1,
          cex.axis = 6 / 7,
          clim = c(1, 0),
          col = viridis::viridis(125)[26:125],
          clab = "Confidence level",
          font.clab = 1,
          line.clab = 1.3,
          adj.clab = 0.5,
          add = TRUE,
          tck = -0.4,
          mgp = c(3, .25, 0),
          lwd.tick = 0.7
        )
        if(save) grDevices::dev.off()
    }
    if(!save) graphics::par(par_usr)
    invisible(x)
}
