#' Calculates the extent of the plot in the equal earth projection.
#'
#' Calculates the extent of the plot in the equal earth projection.
#'
#' @param ext A set of coordinates.
#' @param npoints The number of points used to draw the polygon along each dimension.
#' @return The set of coordinates ext projected in equal earth.
#' @export
#' @examples
#' \dontrun{
#'   eqearth_get_ext(c(-15, 50, 30, 70))
#' }
#'
eqearth_get_ext <- function(ext, npoints=15) {
    if(base::missing(ext)) ext

    bckg <- cbind(id = 1, part = 1,
                  lon = c(rep(ext[1], npoints), rep(ext[2], npoints)),
                  lat = c(seq(ext[3], ext[4], length.out=npoints), seq(ext[4], ext[3], length.out=npoints)))
    bckg <- terra::vect(bckg, type="polygons", crs="+proj=longlat +datum=WGS84 +no_defs")

    PROJ <- paste0("+proj=eqearth +lon_0=", mean(c(ext[2], ext[1])),
                           " +x_0=", mean(c(ext[2], ext[1])),
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    bckg.eqearth <- terra::project(bckg, PROJ)

    ext.eqearth <- as.vector(terra::ext(bckg.eqearth))
    ext.eqearth[1:2] <- ext.eqearth[1:2] + c(-0.1, 0.02)*diff(ext.eqearth[1:2])
    ext.eqearth[3:4] <- ext.eqearth[3:4] + c(-0.1, 0.02)*diff(ext.eqearth[3:4])
    ext.eqearth
}


#' Plots raster data in equal earth projection.
#'
#' Plots raster data in equal earth projection.
#'
#' @param dat The raster data to plot.
#' @param ext The extent to use to plot the data. (default is extent of dat)
#' @param zlim The range of the values to plot. (default is estimated from dat)
#' @param col The color gradient to use. (default is viridis)
#' @param brks.pos The position where to draw tick marks on the legend
#' @param brks.lab The labels to add where the tickmarks are draw (default is
#'        tickmarks position)
#' @param npoints The number of points used to draw the polygons and lines along
#'        each dimension. (default is 15 for a smooth result)
#' @param nlines The number of coordinate lines to add in the background
#'        (default is 9)
#' @param title A description title (default is empty).
#' @param colour_scale A boolean to add the colour scale to the plot (default
#'        \code{TRUE}).
#' @param top_layer A raster to overlay on top of the map (e.g. a distribution).
#' @param top_layer.col A colour for plotting top_layer (default 'ghostwhite').
#' @param site_xy Coordinates of a location to add to the plot.
#' @param dim The dimension of the plotting window in inches (default dev.size()).
#' @return The set of coordinates ext projected in equal earth.
#' @export
#'
plot_map_eqearth <- function(dat, ext=as.vector(terra::ext(dat)), zlim=range(terra::values(dat), na.rm=TRUE), col=viridis::viridis(20), brks.pos=c(0,1), brks.lab=brks.pos, npoints=15, nlines=9, title='', colour_scale=TRUE, top_layer=NA, top_layer.col='ghostwhite', site_xy=NA, dim=NA) {

    if(base::missing(dat)) dat

    utils::data(M1, package='crestr', envir = environment())
    M1 <- terra::unwrap(M1)
    M1 <- terra::crop(M1, ext)

    PROJ <- paste0("+proj=eqearth +lon_0=", mean(c(ext[2], ext[1])),
                           " +x_0=", mean(c(ext[2], ext[1])),
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    M2 <- terra::project(M1, PROJ)

    if (inherits(dat, 'SpatRaster'))  dat <- terra::project(dat, PROJ, align=FALSE)

    if ('SpatRaster' %in% methods::is(top_layer)) top_layer <- terra::project(top_layer, PROJ)

    idx=1
    verticals <- cbind(id = NA, part = NA, lon = NA, lat = NA)
    for(i in seq(ext[1], ext[2], length.out=nlines)){
        verticals <- rbind(verticals,
                           cbind(id = 1, part = idx,
                                 lon = rep(i, npoints),
                                 lat = seq(ext[3], ext[4], length.out=npoints))
                           )
        idx = idx + 1
    }
    verticals <- verticals[-1, ]
    verticals <- terra::vect(verticals, type="lines", crs="+proj=longlat +datum=WGS84 +no_defs")
    verticals.eqearth <- terra::project(verticals, PROJ)
    verticals.eqearth.x <- sort((terra::crds(verticals.eqearth, df=TRUE)$x)[seq(1, npoints*nlines, by=npoints) ])

    idx=1
    horizontals <- cbind(id = NA, part = NA, lon = NA, lat = NA)
    for(i in seq(ext[3], ext[4], length.out=nlines)){
        horizontals <- rbind(horizontals,
                             cbind(id = 1, part = idx,
                                   lon = seq(ext[1], ext[2], length.out=npoints),
                                   lat = rep(i, npoints))
                             )
        idx = idx + 1
    }
    horizontals <- horizontals[-1, ]
    horizontals <- terra::vect(horizontals, type="lines", crs="+proj=longlat +datum=WGS84 +no_defs")
    horizontals.eqearth <- terra::project(horizontals, PROJ)
    horizontals.eqearth.y <- sort((terra::crds(horizontals.eqearth, df=TRUE)$y)[seq(1, npoints*nlines, by=npoints)])
    horizontals.eqearth.xy <- (terra::crds(horizontals.eqearth, df=TRUE))[seq(1, npoints*nlines, by=npoints), ]


    if (length(site_xy) == 2) {
        XY <- terra::vect(matrix(site_xy, ncol=2), crs="+proj=longlat +datum=WGS84 +no_defs")
        XY <- terra::project(XY, PROJ)
    }

    bckg <- cbind(id = 1, part = 1,
                  lon = c(rep(ext[1], npoints), rep(ext[2], npoints)),
                  lat = c(seq(ext[3], ext[4], length.out=npoints), seq(ext[4], ext[3], length.out=npoints)))
    bckg <- terra::vect(bckg, type="polygons", crs="+proj=longlat +datum=WGS84 +no_defs")
    bckg.eqearth <- terra::project(bckg, PROJ)

    if (inherits(dat, 'SpatRaster'))  dat <- terra::crop(dat, bckg.eqearth)

    ext.eqearth <- terra::ext(bckg.eqearth)
    if(is.na(dim)[1]) dim <- grDevices::dev.size('in')

    max_length_y_labs <- 1.25*max(graphics::strwidth(paste0('', round(seq(ext[3], ext[4], length.out=npoints),2)), units='inches', cex=6/8))
    max_length_x_labs <- 1.1*max(graphics::strwidth(paste0(' ', round(ext[2],2)), units='inches', cex=6/8))

    max_height_y_labs <- 1.25*max(graphics::strheight(paste0('', round(seq(ext[3], ext[4], length.out=npoints),2)), units='inches', cex=6/8))
    max_height_x_labs <- 1.5*max(graphics::strheight(paste0('', round(seq(ext[1], ext[2], length.out=npoints),2)), units='inches', cex=6/8))

    ## Ratio of map unit range to inches avail for map
    inches2map_units <- diff(ext.eqearth[1:2]) / (dim[1]-max_length_y_labs-max_length_x_labs/2)

    ## left: we add size of the largest y-axis label
    ## right: we add half the width of the last x-axis label
    ext.eqearth[1:2] <- ext.eqearth[1:2] + c(-max_length_y_labs*inches2map_units, max_length_x_labs*inches2map_units/2)
    ext.eqearth[3:4] <- ext.eqearth[3:4] + c(-max_height_x_labs*inches2map_units, max_height_y_labs*inches2map_units/2)

    ## ...........................................................................
    ## Plotting colour scale .....................................................

    if(colour_scale) {
        xlab <- c(-0.2,1.2)
        xlab <- xlab + diff(xlab)/dim[1] * c(-max_length_y_labs, max_length_x_labs/2)

        plot(NA, NA, type='n', xlab='', ylab='', main='', axes=FALSE, frame=FALSE, xlim=xlab, ylim=c(-0.05,1), xaxs='i', yaxs='i')

        brks2 <- (brks.pos - brks.pos[1]) / diff(range(brks.pos))
        for(i in 1:length(col)) {
            graphics::rect((i-1)/length(col), 0, i/length(col), 0.3, lwd=0.3, border=col[i], col=col[i])
        }

        cex <- 1
        while(graphics::strwidth(title, font=2, cex=cex) >= 1.4) cex <- cex - 0.05

        cont <- TRUE
        res <- 1
        while(cont) {
            brks2.pos <- brks.pos[c(TRUE, rep(FALSE, res-1))]
            brks2.lab <- brks.lab[c(TRUE, rep(FALSE, res-1))]
            sizes <- graphics::strwidth(paste(' ',brks2.lab,' ', sep=''), cex=min(cex, 6/8))
            x1 = (brks2.pos - brks.pos[1]) / diff(range(brks.pos)) + sizes/2 ; x1 = x1[1:(length(x1)-1)]
            x2 = (brks2.pos - brks.pos[1]) / diff(range(brks.pos)) - sizes/2 ; x2 = x2[2:length(x2)]
            if(min(x2-x1) <= 0) {
                res <- res + 1
            } else {
                cont <- FALSE
            }
        }

        for(i in 1:length(brks2.pos)) {
            graphics::text((brks2.pos[i] - brks2.pos[1])/diff(range(brks.pos)), 0.35, brks2.lab[i] , cex=min(cex, 6/8), adj=c(0.5,0))
        }
        graphics::rect(0,0,1,0.3, lwd=0.5)

        graphics::text(0.5, 0.9, title, font=2, cex=cex, adj=c(0.5,1))
    }

    ## ...........................................................................
    ## Plotting map ..............................................................

    plot(0, 0, type='n',
         xlim=c(ext.eqearth[1], ext.eqearth[2]), ylim=c(ext.eqearth[3], ext.eqearth[4]),
         main='', ylab='', xlab='', xaxs='i', yaxs='i',
         asp=1, frame=FALSE, axes=FALSE)  ;  {

        terra::plot(bckg.eqearth, col='grey80', border=NA, cex=0.2, add=TRUE)
        terra::plot(horizontals.eqearth,col='white',lwd=0.5, add=TRUE )
        terra::plot(verticals.eqearth,col='white',lwd=0.5, add=TRUE )

        terra::plot(M2, col='black', border=NA, add=TRUE)
        if (inherits(dat, 'SpatRaster')) {
            terra::image(dat, colNa='black', add=TRUE, interpolate=FALSE, zlim=zlim, col=col)
            terra::plot(M2, border='black', lwd=0.5, add=TRUE)
        }

        if ('SpatRaster' %in% methods::is(top_layer)) {
            terra::image(top_layer, add=TRUE, col=top_layer.col)
        }

        if(length(site_xy) == 2) {
            terra::plot(XY, col='white', bg='red', cex=2, lwd=2, pch=23, add=TRUE)
        }
        labels.lon <- rep(FALSE, length(verticals.eqearth.x))
        names.lon <- round(seq(ext[1], ext[2], length.out=nlines), 2)
        last_plot <- 0
        for(v in 1:length(verticals.eqearth.x)) {
            overlap <- FALSE
            if(v > 1 ) {
                if (labels.lon[last_plot]) {
                    d1 <- 1.25*graphics::strwidth(paste0('', names.lon[last_plot], ' '), cex=6/8)
                    d2 <- graphics::strwidth(paste0('', names.lon[v], ' '), cex=6/8)
                    if (verticals.eqearth.x[last_plot] + d1 / 2 >= verticals.eqearth.x[v] - d2 / 2) {
                        overlap <- TRUE
                    }
                }
            }
            if (!overlap){
                graphics::text(verticals.eqearth.x[v], min(horizontals.eqearth.xy[,2])-max_height_x_labs/3*inches2map_units,
                                names.lon[v],
                                cex=6/8, adj=c(0.5,1)
                )
                labels.lon[v] <- TRUE
                last_plot <- v
            }
        }

        labels.lat <- rep(FALSE, length(horizontals.eqearth.y))
        names.lat <- round(seq(ext[3], ext[4], length.out=nlines), 2)
        last_plot <- nrow(horizontals.eqearth.xy)
        for(h in nrow(horizontals.eqearth.xy):1) {
            overlap <- FALSE
            if(h < nrow(horizontals.eqearth.xy)) {
                if (labels.lat[last_plot]) {
                    d1 <- 1.5*graphics::strheight(names.lat[last_plot], cex=6/8)
                    d2 <- graphics::strheight(names.lat[h], cex=6/8)
                    if (horizontals.eqearth.y[last_plot] - d1 / 2 <= horizontals.eqearth.y[h] + d2 / 2) {
                        overlap <- TRUE
                    }
                }
            }
            if (!overlap){
                graphics::text(horizontals.eqearth.xy[h, 1], horizontals.eqearth.xy[h, 2],
                                paste0(names.lat[h], ' '),
                                cex=6/8, adj=c(1,0.4)
                )
                labels.lat[h] <- TRUE
                last_plot <- h
            }
        }

        terra::plot(bckg.eqearth, col=NA, border='black', cex=0.2, add=TRUE)
    }
    invisible(ext.eqearth)
}
