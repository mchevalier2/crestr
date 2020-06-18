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
                   taxWeight) {

    inputs <- list( df = df,
                    x = x,
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
plot.crestObj <- function(obj, climate = obj$parameters$climate) {
    if (length(obj$reconstructions) == 0 || is.null(climate)) {
        cat('No data available for plotting.')
        invisible(obj)
    }
    for(clim in climate) {
        dev.new()
        pdf = t(obj$reconstructions[[clim]][['posterior']])[-1,]
        #MAT.ysmooth=gausmooth(MAT[,c(1,2)], XX.interp, mean(diff(MAT[,1])))
        pdfter=pdf

        for(i in 2:ncol(pdf)){
            oo=rev(order(pdf[,i]))
            tmp1=pdf[oo,1]
            tmp2=pdf[oo,i]
            oo=order(tmp1)
            pdfter[,i]=cumsum(tmp2/sum(tmp2))[oo]
        }

        par(mar=c(2.3,2.2,3,0.5))
        plot3D::image2D(z=(1-as.matrix(t(pdfter[,-1]))),y=pdfter[,1], x=obj$reconstructions[[clim]][['optima']][, 1], xlim=range(obj$reconstructions[[clim]][['optima']][, 1]), ylim=range(pdfter[,1]), zlim=c(0,1), col = viridis::viridis(125)[26:125], cex.axis=6/7, colkey=FALSE, resfac=2, tck=-.013, mgp=c(1.3, .3, 0), las=1, hadj=c(1,1), xlab='Age (calendar yr BP x1000)', ylab='Mean Annual Temperature (°C)', cex.lab=6/7)
        points(obj$reconstructions[[clim]][['optima']], pch=18, col='white', cex=0.8)
        points(obj$reconstructions[[clim]][['optima']], col='white', cex=0.3, type='l')
        plot3D::colkey(side=3, length=0.8, dist=-0.01, lwd=0.1, cex.axis=6/7, clim=c(1,0), col=viridis::viridis(125)[26:125], clab='A - Confidence level', font.clab=1, line.clab=1.3, adj.clab=0.5, add=TRUE, tck=-0.4, mgp=c(3, .25, 0), lwd.tick=0.7)
    }


    invisible(obj)
}
