crestObj <- function(df, x, pse, taxaType, climate,
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
                   taxWeight,
                   x.name ) {

    inputs <- list( df = df,
                    x = x,
                    pse = pse,
                    selectedTaxa = selectedTaxa,
                    x.name = x.name
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
print.crestObj <- function(x, ...) {
    print(lapply(x, names))
}


#' @export
plot.crestObj <- function(x, climate = x$parameters$climate, errors = c(0.5, 0.95), ...) {
    if (length(x$reconstructions) == 0 || is.null(climate)) {
        cat('No data available for plotting.')
        invisible(x)
    }
    idx <- 0
    for(clim in climate) {
        if (idx > 0)  {  grDevices::dev.new()  }
        idx <- idx + 1
        pdf = t(x$reconstructions[[clim]][['posterior']])[-1,]
        #MAT.ysmooth=gausmooth(MAT[,c(1,2)], XX.interp, mean(diff(MAT[,1])))
        pdfter=pdf

        for(i in 2:ncol(pdf)){
            oo=rev(order(pdf[,i]))
            tmp1=pdf[oo,1]
            tmp2=pdf[oo,i]
            oo=order(tmp1)
            pdfter[,i]=cumsum(tmp2/sum(tmp2))[oo]
        }

        xmn = which.min(x$reconstructions[[clim]][['optima']][,2])
        xmx = which.max(x$reconstructions[[clim]][['optima']][,2])

        ymn=pdfter[which(pdfter[,xmn+1]<=0.99)[1],1]
        ymx=pdfter[rev(which(pdfter[,xmx+1]<=0.99))[1],1]

        climate_names <- accClimateVariables()


        graphics::par(mar=c(4,4,4,0.5))
        plot3D::image2D( z = (1 - as.matrix(t(pdfter[, -1]))),
                         y = pdfter[, 1],
                         x = x$reconstructions[[clim]][['optima']][, 1],
                         xlim = range(x$reconstructions[[clim]][['optima']][, 1]),
                         ylim = c(ymn, ymx),
                         zlim = c(0,1),
                         col = viridis::viridis(125)[26:125],
                         cex.axis = 6/7,
                         colkey = FALSE,
                         resfac = 2,
                         tck = -.013,
                         mgp = c(2, .3, 0),
                         las = 1,
                         hadj = c(1,1),
                         xlab = x$inputs$x.name,
                         ylab = climate_names[climate_names[,2] == clim, 3],
                         cex.lab = 6/7
                        )
        for (e in errors) {
            val <- apply(pdfter[, -1], 2, function(x) {w=which(x<=e); return(c(w[1], w[length(w)]))} )
            graphics::points( x$reconstructions[[clim]][['optima']][, 1], pdfter[val[1, ], 1],
                              type = 'l', col = 'white', lty = 3 )
            graphics::points( x$reconstructions[[clim]][['optima']][, 1], pdfter[val[2, ], 1],
                              type = 'l', col = 'white', lty = 3 )
        }

        graphics::points( x$reconstructions[[clim]][['optima']],
                          pch = 18, col = 'white', cex = 0.8 )
        graphics::points( x$reconstructions[[clim]][['optima']],
                          col = 'white', cex = 0.5, type = 'l' )
        plot3D::colkey( side = 3,
                        length = 0.8,
                        dist = -0.01,
                        lwd = 0.1,
                        cex.axis = 6/7,
                        clim = c(1,0),
                        col = viridis::viridis(125)[26:125],
                        clab = 'A - Confidence level',
                        font.clab = 1,
                        line.clab = 1.3,
                        adj.clab = 0.5,
                        add = TRUE,
                        tck = -0.4,
                        mgp = c(3, .25, 0),
                        lwd.tick = 0.7
                       )


    }
    invisible(x)
}
