#' Create a crest() object.
#'
#' Creates a crest() object with all default parameters.
#'
#' @param taxa.name .
#' @param pse .
#' @param taxaType .
#' @param climate A vectof of the climate variables to extract.
#' @param df .
#' @param x.name .
#' @param x .
#' @param xmn,xmx,ymn,ymx The coordinates defining the study area.
#' @param continents A vector of the continent names defining the study area.
#' @param countries A vector of the country names defining the study area.
#' @param realms A vector of the studied botanical realms defining the study area.
#' @param biomes A vector of the studied botanical biomes defining the study area.
#' @param ecoregions A vector of the studied botanical ecoregions defining the study area.
#' @param minGridCells .
#' @param bin_width .
#' @param shape .
#' @param selectedTaxa .
#' @param npoints The number of points to be used to fit the pdfs.
#' @param geoWeighting The number of points to be used to fit the pdfs.
#' @param climateSpaceWeighting The number of points to be used to fit the pdfs.
#' @param presenceThreshold .
#' @param taxWeight 'originalData', 'presence/absence', 'percentages' or 'normalisation'

#' @return A CREST object that is used to store data and information for reconstructing climate
#' @export

crestObj <- function(taxa.name, pse, taxaType, climate,
                     xmn, xmx, ymn, ymx,
                     continents, countries,
                     realms, biomes, ecoregions,
                     df = NA, x = NA, x.name = "",
                     minGridCells = 20,
                     bin_width = rep(1, length(climate)), shape = rep("normal", length(climate)),
                     npoints = 500,
                     geoWeighting = TRUE,
                     climateSpaceWeighting = TRUE,
                     selectedTaxa = NA,
                     presenceThreshold = 0,
                     taxWeight = "normalisation") {
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
    presenceThreshold = presenceThreshold
  )

  modelling <- list(taxonID2proxy = NA, climate_space = NA, pdfs = NA, weights = NA, xrange = NA)

  reconstructions <- list()

  value <- list(
    inputs = inputs,
    parameters = parameters,
    modelling = modelling,
    reconstructions = reconstructions
  )
  # class can be set using class() or attr() function
  attr(value, "class") <- "crestObj"
  value
}


#' @export
print.crestObj <- function(x, ...) {
  print(lapply(x, names))
}


#' @export
plot.crestObj <- function(x,
                          climate = x$parameters$climate,
                          errors = c(0.5, 0.95),
                          xlim = range(x$inputs$x),
                          ylim = NA,
                          save = FALSE,
                          plot = TRUE,
                          loc = getwd(),
                          ...) {
  if (length(x$reconstructions) == 0 || is.null(climate)) {
    cat("No reconstructions available for plotting.")
    return(invisible(x))
  }
  if (!(save || plot)) {
    cat("Please set either 'plot' or 'save' to TRUE.\n")
    return(invisible(x))
  } else {
    idx <- 0
    for (clim in climate) {
      if (idx == 0 && plot) {
        graphics::par(mfrow = grDevices::n2mfrow(length(climate)))
      }
      idx <- idx + 1
      pdf <- t(x$reconstructions[[clim]][["posterior"]])[-1, ]
      # MAT.ysmooth=gausmooth(MAT[,c(1,2)], XX.interp, mean(diff(MAT[,1])))
      pdfter <- pdf

      for (i in 2:ncol(pdf)) {
        oo <- rev(order(pdf[, i]))
        tmp1 <- pdf[oo, 1]
        tmp2 <- pdf[oo, i]
        oo <- order(tmp1)
        pdfter[, i] <- cumsum(tmp2 / sum(tmp2))[oo]
      }

      xmn <- which.min(x$reconstructions[[clim]][["optima"]][, 2])
      xmx <- which.max(x$reconstructions[[clim]][["optima"]][, 2])

      if (unique(is.na(ylim))) {
        ymn <- pdfter[which(pdfter[, xmn + 1] <= 0.99)[1], 1]
        ymx <- pdfter[rev(which(pdfter[, xmx + 1] <= 0.99))[1], 1]
      } else {
        ymn <- ylim[idx * 2 - 1]
        ymx <- ylim[idx * 2]
      }
      climate_names <- accClimateVariables()

      if (plot) {
        graphics::par(mar = c(4, 4, 4, 0.5))
        plot3D::image2D(
          z = (1 - as.matrix(t(pdfter[, -1]))),
          y = pdfter[, 1],
          x = x$reconstructions[[clim]][["optima"]][, 1],
          xlim = xlim,
          ylim = c(ymn, ymx),
          zlim = c(0, 1),
          col = viridis::viridis(125)[26:125],
          cex.axis = 6 / 7,
          colkey = FALSE,
          resfac = 2,
          tck = -.013,
          mgp = c(2, .3, 0),
          las = 1,
          hadj = c(1, 1),
          xlab = x$inputs$x.name,
          ylab = climate_names[climate_names[, 2] == clim, 3],
          cex.lab = 6 / 7
        )
        for (e in errors) {
          val <- apply(pdfter[, -1], 2, function(x) {
            if(is.na(x[1])) return(c(NA, NA))
            w <- which(x <= e)
            return(c(w[1], w[length(w)]))
          })
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
      }
      if (save) {
        if (substr(loc, nchar(loc), nchar(loc)) == .Platform$file.sep) {
          loc <- substr(loc, 1, nchar(loc) - 1)
        }
        pdf(paste0(loc, .Platform$file.sep, clim, ".pdf"), width = 5.51, height = 5)
        graphics::par(mar = c(3, 3, 3.2, 0.5))
        plot3D::image2D(
          z = (1 - as.matrix(t(pdfter[, -1]))),
          y = pdfter[, 1],
          x = x$reconstructions[[clim]][["optima"]][, 1],
          xlim = xlim,
          ylim = c(ymn, ymx),
          zlim = c(0, 1),
          col = viridis::viridis(125)[26:125],
          cex.axis = 6 / 7,
          colkey = FALSE,
          resfac = 1,
          tck = -.013,
          mgp = c(1.3, .3, 0),
          las = 1,
          hadj = c(1, 1),
          xlab = x$inputs$x.name,
          ylab = paste0(climate_names[climate_names[, 2] == clim, 3], "\n"),
          cex.lab = 6 / 7
        )
        for (e in errors) {
          val <- apply(pdfter[, -1], 2, function(x) {
            if(is.na(x[1])) return(c(NA, NA))
            w <- which(x <= e)
            return(c(w[1], w[length(w)]))
          })
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
        grDevices::dev.off()
      }
    }
  }
  invisible(x)
}
