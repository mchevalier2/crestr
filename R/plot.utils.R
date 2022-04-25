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

    bckg <- sp::Polygons(list(sp::Polygon(cbind(c(rep(ext[1], npoints),
                                                  rep(ext[2], npoints)),
                                                c(seq(ext[3], ext[4], length.out=npoints),
                                                  seq(ext[4], ext[3], length.out=npoints))))), ID = as.character(1))
    Sl1 <- sp::SpatialPolygons(list(bckg))
    raster::crs(Sl1) <- sp::CRS("+init=epsg:4326")

    PROJ <- sp::CRS(paste0("+proj=eqearth +lon_0=", mean(c(ext[2], ext[1])),
                           " +x_0=", mean(c(ext[2], ext[1])),
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
                        )

    bckg.eqearth <- sp::spTransform(Sl1, PROJ)

    ext <- raster::extent(bckg.eqearth)
    ext[1:2] <- ext[1:2] + c(-0.1, 0.02)*diff(ext[1:2])
    ext[3:4] <- ext[3:4] + c(-0.1, 0.02)*diff(ext[3:4])
    ext
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
plot_map_eqearth <- function(dat, ext=raster::extent(dat), zlim=range(raster::values(dat), na.rm=TRUE), col=viridis::viridis(20), brks.pos=c(0,1), brks.lab=brks.pos, npoints=15, nlines=9, title='', colour_scale=TRUE, top_layer=NA, top_layer.col='ghostwhite', site_xy=NA, dim=NA) {

    if(base::missing(dat)) dat

    ## Useless lines to avoid WARNINGs ------------------------- #
    slss <- rgdal::projInfo(type = "proj") # ------------------- #
    p1 = rgeos::readWKT("POLYGON((0 0,3 0,3 3,0 3,0 0))") # ---- #
    ## --------------------------------------------------------- #

    utils::data(M1, package='crestr', envir = environment())
    M1 <- raster::crop(M1, ext)

    PROJ <- sp::CRS(paste0("+proj=eqearth +lon_0=",mean(ext[1:2]),
                           " +x_0=",mean(ext[1:2]),
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
                        )

    M2 <- sp::spTransform(M1, PROJ)
    if (inherits(dat, 'RasterLayer'))  dat <- raster::projectRaster(from=dat, crs=PROJ)

    if ('Raster' %in% methods::is(top_layer)) top_layer <- raster::projectRaster(from=top_layer, crs=PROJ)

    ll=list()
    idx=1
    for(i in seq(ext[1], ext[2], length.out=nlines)){
        ll[[idx]] <- sp::Lines(list(sp::Line(cbind(rep(i,npoints), seq(ext[3], ext[4], length.out=npoints)))), ID = as.character(i))
        idx = idx  +1
    }
    Sl1 <- sp::SpatialLines(ll)
    raster::crs(Sl1) <- sp::CRS("+init=epsg:4326")
    verticals.eqearth <- sp::spTransform(Sl1, raster::crs(PROJ))
    verticals.eqearth.x <- unlist(lapply(sp::coordinates(verticals.eqearth), function(x) return(x[[1]][1,1])))

    ll=list()
    idx=1
    for(i in seq(ext[3], ext[4], length.out=nlines)){
        ll[[idx]] <- sp::Lines(list(sp::Line(cbind(seq(ext[1], ext[2], length.out=npoints), rep(i, npoints)))), ID = as.character(i))
        idx = idx  +1
    }
    Sl1 <- sp::SpatialLines(ll)
    raster::crs(Sl1) <- sp::CRS("+init=epsg:4326")
    horizontals.eqearth <- sp::spTransform(Sl1, raster::crs(PROJ))
    horizontals.eqearth.xy <- t(data.frame(lapply(sp::coordinates(horizontals.eqearth), function(x) return(x[[1]][1,]))))
    horizontals.eqearth.y <- unlist(lapply(sp::coordinates(horizontals.eqearth), function(x) return(x[[1]][1,2])))


    if (length(site_xy) == 2) {
        Sl1 <- sp::SpatialPoints(matrix(site_xy, ncol=2))
        raster::crs(Sl1) <- sp::CRS("+init=epsg:4326")
        XY <- sp::spTransform(Sl1, raster::crs(PROJ))
    }

    bckg <- sp::Polygons(list(sp::Polygon(cbind(c(rep(ext[1], npoints),
                                                  rep(ext[2], npoints)),
                                                c(seq(ext[3], ext[4], length.out=npoints),
                                                  seq(ext[4], ext[3], length.out=npoints))))), ID = as.character(1))
    Sl1 <- sp::SpatialPolygons(list(bckg))
    raster::crs(Sl1) <- sp::CRS("+init=epsg:4326")
    bckg.eqearth <- sp::spTransform(Sl1, raster::crs(PROJ))

    if (inherits(dat, 'RasterLayer'))  dat <- raster::mask(dat, bckg.eqearth)

    ext <- raster::extent(bckg.eqearth)
    ext_factor_x <- max(graphics::strwidth(paste0('     ', round(as.numeric(names(horizontals.eqearth)),2)), units='inches', cex=6/8))
    ext_factor_y <- max(graphics::strheight(paste0('\n', round(as.numeric(names(verticals.eqearth)),2)), units='inches', cex=6/8))

    if(is.na(dim)[1]) dim <- grDevices::dev.size('in')

    ext[1:2] <- ext[1:2] + (ext[2]-ext[1])/dim[1] * c(-ext_factor_x, 0.05)
    ext[3:4] <- ext[3:4] + (ext[4]-ext[3])/dim[2] * c(-ext_factor_y, 0.05)

    ## ...........................................................................
    ## Plotting colour scale .....................................................

    if(colour_scale) {
        xlab <- c(-0.2,1.2)
        xlab <- xlab + diff(xlab)/dim[1] * c(-ext_factor_x, 0.05)

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

        graphics::text(0.5, 0.85, title, font=2, cex=cex, adj=c(0.5,1))
    }

    ## ...........................................................................
    ## Plotting map ..............................................................

    plot(0, 0, type='n',
         xlim=c(ext[1], ext[2]), ylim=c(ext[3], ext[4]),
         main='', ylab='', xlab='', xaxs='i', yaxs='i',
         asp=1, frame=FALSE, axes=FALSE)  ;  {

        sp::plot(bckg.eqearth, col='grey80', border=NA, cex=0.2, add=TRUE)
        sp::plot(horizontals.eqearth,col='white',lwd=0.5, add=TRUE )
        sp::plot(verticals.eqearth,col='white',lwd=0.5, add=TRUE )

        sp::plot(M2, col='black', border=NA, add=TRUE)
        if (inherits(dat, 'RasterLayer')) {
            raster::image(dat, colNa='black', add=TRUE, interpolate=FALSE, zlim=zlim, col=col)
            sp::plot(M2, border='black', lwd=0.5, add=TRUE)
        }

        if ('Raster' %in% methods::is(top_layer)) {
            raster::image(top_layer, add=TRUE, col=top_layer.col)
        }

        if(length(site_xy) == 2) {
            sp::plot(XY, col='white', bg='red', cex=2, lwd=2, pch=23, add=TRUE)
        }
        labels.lon <- rep(FALSE, length(verticals.eqearth.x))
        for(v in 1:length(verticals.eqearth.x)) {
            overlap <- FALSE
            if(v > 1 ) {
                if (labels.lon[v-1]) {
                    d1 <- graphics::strwidth(paste0('\n ', round(as.numeric(names(verticals.eqearth))[v-1],2), ' '), cex=6/8)
                    d2 <- graphics::strwidth(paste0('\n ', round(as.numeric(names(verticals.eqearth))[v],2), ' '), cex=6/8)
                    if (verticals.eqearth.x[v-1] + d1 / 2 >= verticals.eqearth.x[v] - d2 / 2) {
                        overlap <- TRUE
                    }
                }
            }
            if (!overlap){
                graphics::text(verticals.eqearth.x[v], min(horizontals.eqearth.xy[,2]),
                                paste0('\n', round(as.numeric(names(verticals.eqearth))[v],2)),
                                cex=6/8, adj=c(0.5,0.7)
                )
                labels.lon[v] <- TRUE
            }
        }

        labels.lat <- rep(FALSE, length(horizontals.eqearth.xy))
        for(h in nrow(horizontals.eqearth.xy):1) {
            overlap <- FALSE
            if(h < nrow(horizontals.eqearth.xy)) {
                if (labels.lat[h+1]) {
                    d1 <- graphics::strheight(paste0('', round(as.numeric(names(horizontals.eqearth))[h+1],2), ' '), cex=6/8)
                    d2 <- graphics::strheight(paste0('', round(as.numeric(names(horizontals.eqearth))[h],2), ' '), cex=6/8)
                    if (horizontals.eqearth.y[h+1] - d1 / 2 <= horizontals.eqearth.y[h] + d2 / 2) {
                        overlap <- TRUE
                    }
                }
            }
            if (!overlap){
                graphics::text(horizontals.eqearth.xy[h, 1], horizontals.eqearth.xy[h, 2],
                                paste0(round(as.numeric(names(horizontals.eqearth))[h], 2),' '),
                                cex=6/8, adj=c(1,0.4)
                )
                labels.lat[h] <- TRUE
            }
        }

        sp::plot(bckg.eqearth, col=NA, border='black', cex=0.2, add=TRUE)
    }
    invisible(ext)
}
