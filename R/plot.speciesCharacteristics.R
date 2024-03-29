#' Plot the distribution and responses of the studied taxa
#'
#' Plot the distribution and responses of the studied taxa
#'
#' @inheritParams plot_diagram
#' @param x A \code{\link{crestObj}} generated by either the
#'        \code{\link{crest.calibrate}}, \code{\link{crest.reconstruct}},
#'        \code{\link{loo}} or \code{\link{crest}} functions.
#' @param taxanames A list of taxa to use for the plot (default is all the
#'        recorded taxa).
#' @param climate Climate variables to be used to generate the plot. By default
#'        all the variables are included.
#' @param add_modern A boolean to add the location and the modern climate values
#'        to the plot (default \code{FALSE}).
#' @param filename An absolute or relative path that indicates where the diagram
#'        should be saved. Also used to specify the name of the file. Default:
#'        the file is saved in the working directory under the name
#'        \code{'taxaCharacteristics.pdf'}.
#' @param col.density The colour gradient to use to map the density of species
#'        (top left map).
#' @param col.climate The colour gradient to use to map the climate gradients
#'        (left column).
#' @param width The width of the output file in inches (default 7.48in ~ 19cm).
#' @param w0 The width of the left column with the names.
#' @param height The height of the output file in inches (default 3in ~ 7.6cm
#'        per variables).
#' @param h0 The vertical space used for the x-axes.
#' @param resol For advanced users only: if higher resolution data are used to
#'        estimate the \code{pdfs}, use this parameter to define the resolution
#'        of the maps on the figures. (default is 0.25 degrees to match with the
#'        default database)
#' @return No return value, this function is used to plot.
#' @export
#' @examples
#' \dontrun{
#'   data(crest_ex_pse)
#'   data(crest_ex_selection)
#'   reconstr <- crest.get_modern_data(
#'     pse = crest_ex_pse, taxaType = 0, df = crest_ex,
#'     climate = c("bio1", "bio12"),
#'     selectedTaxa = crest_ex_selection, dbname = "crest_example"
#'   )
#'   reconstr <- crest.calibrate(reconstr,
#'     geoWeighting = TRUE, climateSpaceWeighting = TRUE,
#'     bin_width = c(2, 20), shape = c("normal", "lognormal")
#'   )
#'   plot_taxaCharacteristics(reconstr, taxanames='Taxon1')
#' }
#'
plot_taxaCharacteristics <- function( x, taxanames = x$inputs$taxa.name,
                                      climate = x$parameters$climate,
                                      col.density = viridis::plasma(20)[5:20],
                                      col.climate = viridis::viridis(22)[3:20],
                                      save = FALSE, filename = 'taxaCharacteristics.pdf',
                                      as.png = FALSE, png.res=300,
                                      width = 7.48, w0 = 0.2,
                                      height = 3*length(climate)+h0, h0 = 0.4,
                                      add_modern = FALSE,
                                      resol = 0.25
                                      ) {

    if(base::missing(x)) x

    if (is.crestObj(x)) {
        if (length(x$modelling$pdfs) == 1 ) {
            if(is.na(x$modelling$pdfs)) {
                stop('The pdfs are required to generate the plot. Run crest.calibrate() on your data.\n')
            }
        }

        err <- c()
        for(clim in climate) {
            if(! clim %in% x$parameters$climate) err <- c(err, clim)
        }
        if(length(err) > 0) {
            stop(paste0("The following variables are not available in your crestObj: '", paste(err, collapse="', '"), "'\n\n"))
            return(invisible(NA))
        }

        err <- c()
        for(tax in taxanames) {
            if(! tax %in% x$input$taxa.name) err <- c(err, tax)
        }
        if(length(err) > 0) {
            stop(paste0("The following taxa names are not available in your dataset: '", paste(err, collapse="', '"), "'\n\n"))
            return(invisible(NA))
        }

        if(add_modern) {
            if (length(x$misc$site_info) <= 3) {
                add_modern <- FALSE
            }
        }

        site_xy <- NA
        if (is.na(x$misc$site_info$long) | is.na(x$misc$site_info$lat)) add_modern <- FALSE
        if(add_modern) {
            site_xy <- c(x$misc$site_info$long, x$misc$site_info$lat)
        }

        if(sum(taxanames %in% x$inputs$taxa.name) != length(taxanames)) {
            warning(paste0("Data are not available for the following names: '", paste(taxanames[!(taxanames %in% x$inputs$taxa.name)], collapse="', '"), "'.\n"))
        }
        taxanames <- taxanames[taxanames %in% x$inputs$taxa.name]

        ext <- c(x$parameters$xmn, x$parameters$xmx, x$parameters$ymn, x$parameters$ymx)
        ext_eqearth <- eqearth_get_ext(ext)
        xy_ratio <- diff(ext_eqearth[1:2]) / diff(ext_eqearth[3:4])

        x0 <- width - w0
        x3 <- x0 / 3
        y1 <- height / (length(climate)+1)
        y2 <- 0.25

        if(save) {
            y1.tmp <- x3 / xy_ratio
            opt_height <- round(length(climate) * y1 + y1.tmp, 3)
            if ((opt_height / height) >= 1.05 | (opt_height / height) <= 0.95) {
                cat('SUGGEST: Using height =', opt_height, 'would get rid of all the white spaces.\n')
            }
        } else {
            par_usr <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(par_usr))
            if(length(taxanames) > 1)  graphics::par(ask = TRUE)
        }

        climate_space <- x$modelling$climate_space
        climate_space[, 1] = resol * (climate_space[, 1] %/% resol) + resol/2;
        climate_space[, 2] = resol * (climate_space[, 2] %/% resol) + resol/2;
        climate_space = stats::aggregate(. ~ longitude+latitude, data = climate_space, mean)


        distribs <- list()
        continue <- TRUE
        for(tax in taxanames) {
            if(save) {
                if(as.png) {
                    if(length(taxanames) > 1) {
                        grDevices::png(paste0(strsplit(filename, '.png')[[1]], '_', tax, '.png'), width = width, height = height, units='in', res=png.res)
                    } else {
                        grDevices::png(paste0(strsplit(filename, '.png')[[1]], '.png'), width = width, height = height, units='in', res=png.res)
                    }
                } else if(continue) {
                    grDevices::pdf(paste0(strsplit(filename, '.pdf')[[1]], '.pdf'), width=width, height=height)
                    continue <- FALSE
                }
            }

            ## If data are unavailable
            if (sum(x$inputs$selectedTaxa[tax, x$parameters$climate]) < 0) {

                ## Defining plotting matrix --------------------------------------------
                m1 <- matrix(c(6,3,2,1,6,3,4,4,6,3,5,5), ncol=4, byrow=TRUE)
                m2 <- matrix(rep(c(1,2,3,5,1,2,4,6) , length(climate)) + 6 + 6*rep(1:length(climate)-1, each=8), ncol=4, byrow=TRUE)
                graphics::layout(rbind(m1, m2),
                       width  = c(w0, x3, x3, x3),
                       height = c(h0, y1-2*h0, h0, rep(c(y1-h0, h0), times=length(climate))))


                graphics::par(mar=c(0,0,0,0), ps=8*3/2)
                graphics::plot(NA, NA, type='n', frame=FALSE, axes=FALSE, xlim=c(0,1), ylim=c(0,1))

                graphics::plot(NA, NA, frame=TRUE, axes=FALSE, xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i')
                graphics::segments(0,0,1,1, col='grey70', lty=3)
                graphics::segments(0,1,1,0, col='grey70', lty=3)
                graphics::text(0.5, 0.5, 'No data\navailable', adj=c(0.5, 0.5), cex=1)

                if (is.data.frame(x$inputs$df)) {
                    if(is.numeric(x$inputs$x)) {
                        xval <- range(x$inputs$x)
                        xx   <- x$inputs$x
                    } else {
                        warning("The plotting function is not adapted to non-numeric x values. The sample names were replaced by numeric indexes.")
                        xx   <- 1:length(x$inputs$x)
                        xval <- range(xx)
                    }

                    labs_width <- graphics::strwidth('100.0  ', cex=6/8, units='inches')
                    space_width <- graphics::strwidth(' ', cex=6/8, units='inches')

                    dX <- diff(xval)
                    xval <- xval + c(-labs_width, 1.1*space_width) * diff(xval) / (2*x3 - labs_width - space_width)

                    graphics::par(mar=c(0,0,0.2,0.2))
                    opar <- graphics::par(lwd=0.8)
                    graphics::plot(NA, NA, type='n', xlim=xval, ylim=c(0, 1.03 * max(x$inputs$df[, tax])), axes=FALSE, frame=FALSE, main='', xaxs='i', yaxs='i')  ;  {

                        space_width <- diff(range(range(x$inputs$x))) * space_width / (2*x3 - labs_width - 2*space_width)

                        for(yval in graphics::axTicks(2)){
                            if (yval < max(x$inputs$df[, tax])) {
                                graphics::segments(min(xx)-space_width, yval, max(xx)+space_width, yval, col=ifelse(yval==0, 'black', 'grey90'))
                                graphics::segments(min(xx)-space_width, yval, min(xx), yval)
                                graphics::segments(max(xx)+space_width, yval, max(xx), yval)
                                graphics::text(min(xx)-2*space_width, yval, yval, cex=6/8, adj=c(1,ifelse(yval==0, 0, 0.4)))
                            }
                        }
                        graphics::rect(min(xx)-space_width,0,max(xx)+space_width, 1.02*max(x$inputs$df[, tax]))
                        graphics::points(xx, x$inputs$df[, tax], type='o', pch=15, lwd=0.5)
                    }

                    graphics::par(mar=c(0.2,0,0,0.2))
                    graphics::plot(NA, NA, type='n', xlim=xval, ylim=c(0, 1), axes=FALSE, main='', xaxs='i', yaxs='i')
                    graphics::text(mean(range(xx)), 0.3, x$inputs$x.name, font=1, adj=c(0.5, 0.5), cex=6/8)
                    for(xvl in graphics::axTicks(1)){
                        if(xvl >= min(xx)) {
                            graphics::segments(xvl, 1, xvl, 0.9)
                            dff <- xvl+graphics::strwidth(xvl, cex=6/8, units='user')/2 - xval[2]
                            graphics::text(xvl - ifelse(dff < 0, 0, dff),
                            0.85, xvl, cex=6/8, adj=c(0.5,1))
                        }
                    }

                    graphics::par(opar)
                } else {
                    graphics::par(mar=c(0.1,0.1,0.1,0.1))
                    graphics::plot(NA, NA, frame=TRUE, axes=FALSE, xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i')
                    graphics::segments(0,0,1,1, col='grey70', lty=3)
                    graphics::segments(0,1,1,0, col='grey70', lty=3)
                    graphics::text(0.5, 0.5, 'No data\navailable', adj=c(0.5, 0.5), cex=1)
                    graphics::plot(NA, NA, frame=FALSE, axes=FALSE, xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i')
                }

                graphics::par(mar=c(0.1,0.1,0.1,0.1))
                graphics::plot(NA, NA, frame=FALSE, axes=FALSE, xlim=c(0,1), ylim=c(0,1))
                graphics::text(0.5, 0.5, tax, font=2, adj=c(0.5, 0.5), srt=90, cex=10/8)

                for(clim in climate) {
                    graphics::par(mar=c(0,0,0,0))
                    graphics::plot(NA, NA, frame=FALSE, axes=FALSE, xlim=c(0,1), ylim=c(0,1))

                    i=8
                    lab_width <- graphics::strwidth(accClimateVariables(clim)[3], cex=1, units='inches')
                    lab_width <- lab_width * 1 / y1
                    while(lab_width > 1) {
                        i <- i-1
                        lab_width <- graphics::strwidth(accClimateVariables(clim)[3], cex=i/8, units='inches')
                        lab_width <- lab_width * 1 / y1
                    }
                    graphics::text(0.5, 0.5, accClimateVariables(clim)[3], font=1, adj=c(0.5, 0.5), srt=90, cex=i/8)

                    graphics::par(mar=c(0.1,0.1,0.1,0.1))
                    for(i in 1:3) {
                        graphics::plot(NA, NA, frame=TRUE, axes=FALSE, xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i')
                        graphics::segments(0,0,1,1, col='grey70', lty=3)
                        graphics::segments(0,1,1,0, col='grey70', lty=3)
                        graphics::text(0.5, 0.5, 'No data\navailable', adj=c(0.5, 0.5), cex=1)
                    }
                }
            } else { ## If data are available for the taxon --------------------------

                tmp <- x$modelling$distributions[[tax]]
                if(is.data.frame(tmp)) {
                    tmp[, 2] = resol * (tmp[, 2] %/% resol) + resol/2;
                    tmp[, 3] = resol * (tmp[, 3] %/% resol) + resol/2;
                    distribs[[tax]] <- stats::aggregate(. ~ longitude+latitude+taxonid, data = tmp, mean, na.action = NULL)
                } else {
                    distribs[[tax]] <- NA
                }

                ## Defining plotting matrix --------------------------------------------
                m1 <- matrix(c(6,3,2,1,6,3,4,4,6,3,5,5), ncol=4, byrow=TRUE)
                m2 <- matrix(rep(c(1,2,3,5,1,2,4,6) , length(climate)) + 6 + 6*rep(1:length(climate)-1, each=8), ncol=4, byrow=TRUE)

                graphics::layout(rbind(m1, m2),
                       width  = c(w0, x3, x3, x3),
                       height = c(h0, y1-h0-y2, y2, rep(c(y1-y2, y2), times=length(climate))))


                veg_space      <- distribs[[tax]][, c('longitude', 'latitude')]
                veg_space[, 1] <- resol * (veg_space[, 1] %/% resol) + resol/2
                veg_space[, 2] <- resol * (veg_space[, 2] %/% resol) + resol/2
                veg_space      <- plyr::count(veg_space)
                veg_space      <- veg_space[!is.na(veg_space[, 1]), ]
                veg_space[, 3] <- base::log10(veg_space[, 3])
                veg_space      <- terra::rast(veg_space, type='xyz', crs=terra::crs("+proj=longlat +datum=WGS84 +no_defs"))


                graphics::par(mar=c(0,0,0,0))
                graphics::plot(NA, NA, frame=FALSE, axes=FALSE, xlim=c(0,1), ylim=c(0,1))
                graphics::text(0.5, 0.7, tax, font=2, adj=c(0.5, 0.5), cex=10/8)
                graphics::text(0.5, 0.3, paste0('(', ncol(x$modelling$pdfs[[tax]][[clim]]$pdfsp), ' species)'), adj=c(0.5, 0.5), cex=7/8)


                ## Plot species abundance --------------------------------------------------
                zlab=c(0, ceiling(max(terra::values(veg_space), na.rm=TRUE)))

                xlab <- c(-0.2,1.2)
                xlab <- xlab + c(-0.15, 0.02)*diff(xlab)

                clab=c()
                i <- 0
                while(i <= max(zlab)){
                    clab <- c( clab, c(1,2,5)*10**i )
                    i <- i+1
                }
                clab <- c(clab[log10(clab) <= max(terra::values(veg_space), na.rm=TRUE)], clab[log10(clab) > max(terra::values(veg_space), na.rm=TRUE)][1])
                zlab[2] <- log10(clab[length(clab)])

                graphics::par(mar=c(0,0,0,0), ps=8*3/2)
                plot_map_eqearth(veg_space, ext, zlim = zlab,
                                 brks.pos=log10(clab), brks.lab=clab,
                                 col=col.density, site_xy = site_xy,
                                 title='Number of species occurences per grid cell',
                                 dim=c(x3*width/(w0+3*x3), y1*height/(y1+y1*length(climate))))
                                 #dim=c(x3*width/(w0+x1+x2), y2*height/(y1+y2*length(climate))))


                ## Plot the time series ------------------------------------------------
                if (is.data.frame(x$inputs$df)) {
                    if(is.numeric(x$inputs$x)) {
                        xval <- range(x$inputs$x)
                        xx   <- x$inputs$x
                    } else {
                        warning("The plotting function is not adapted to non-numeric x values. The sample names were replaced by numeric indexes.")
                        xx   <- 1:length(x$inputs$x)
                        xval <- range(xx)
                    }

                    labs_width <- graphics::strwidth('100.0  ', cex=6/8, units='inches')
                    space_width <- graphics::strwidth(' ', cex=6/8, units='inches')

                    dX <- diff(xval)
                    xval <- xval + c(-labs_width, 1.1*space_width) * diff(xval) / (2*x3 - labs_width - space_width)

                    graphics::par(mar=c(0,0,0.2,0.4)) #@ Double the left margin relative to the other lines which have half the size.
                    opar <- graphics::par(lwd=0.8)
                    graphics::plot(NA, NA, type='n', xlim=xval, ylim=c(0, 1.03 * max(x$inputs$df[, tax])), axes=FALSE, frame=FALSE, main='', xaxs='i', yaxs='i')  ;  {

                        space_width <- diff(range(range(x$inputs$x))) * space_width / (2*x3 - labs_width - 2*space_width)
                        for(yval in graphics::axTicks(2)){
                            if (yval < max(x$inputs$df[, tax])) {
                                graphics::segments(min(xx)-space_width, yval, max(xx)+space_width, yval, col=ifelse(yval==0, 'black', 'grey90'))
                                graphics::segments(min(xx)-space_width, yval, min(xx), yval)
                                graphics::segments(max(xx)+space_width, yval, max(xx), yval)
                                graphics::text(min(xx)-2*space_width, yval, yval, cex=6/8, adj=c(1,ifelse(yval==0, 0, 0.4)))
                            }
                        }
                        graphics::rect(min(xx)-space_width,0,max(xx)+space_width, 1.02*max(x$inputs$df[, tax]))
                        graphics::points(xx, x$inputs$df[, tax], type='o', pch=15, lwd=0.5, cex=0.8)
                    }

                    graphics::par(mar=c(0.2,0,0,0.4))
                    graphics::plot(NA, NA, type='n', xlim=xval, ylim=c(0, 1), axes=FALSE, main='', xaxs='i', yaxs='i')
                    graphics::text(mean(range(xx)), 0.25, x$inputs$x.name, font=1, adj=c(0.5, 0.5), cex=6/8)
                    for(xvl in graphics::axTicks(1)){
                        if(xvl >= min(xx)) {
                            graphics::segments(xvl, 1, xvl, 0.9)
                            dff <- xvl+graphics::strwidth(xvl, cex=6/8, units='user')/2 - xval[2]
                            graphics::text(xvl - ifelse(dff < 0, 0, dff),
                            0.80, xvl, cex=6/8, adj=c(0.5,1))
                        }
                    }
                    graphics::rect(min(xx)-space_width,1,max(xx)+space_width, 1)
                    graphics::text(min(xx)-2*space_width, 1, '0', cex=6/8, adj=c(1,0))

                    graphics::par(opar)
                } else {
                    graphics::par(mar=c(0.1,0.1,0.1,0.1))
                    graphics::plot(NA, NA, frame=TRUE, axes=FALSE, xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i')
                    graphics::segments(0,0,1,1, col='grey70', lty=3)
                    graphics::segments(0,1,1,0, col='grey70', lty=3)
                    graphics::text(0.5, 0.5, 'No data\navailable', adj=c(0.5, 0.5), cex=1)
                    graphics::plot(NA, NA, frame=FALSE, axes=FALSE, xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i')
                }

                ## Plot the taxon name -------------------------------------------------
                graphics::par(mar=c(0,0,0,0))
                graphics::plot(NA, NA, frame=FALSE, axes=FALSE, xlim=c(0,1), ylim=c(0,1))
                graphics::text(0.5, 0.5, 'Density distribution', font=1, adj=c(0.5, 0.5), srt=90, cex=8/8)


                #max_hist <- max_pdfs <- 0
                #for(clim in climate) {
                #    h1 <- graphics::hist(climate_space[, clim],
                #               breaks=c(x$modelling$ccs[[clim]]$k1, max(x$modelling$ccs[[clim]]$k1)+diff(x$modelling$ccs[[clim]]$k1[1:2])),
                #               plot=FALSE)

                #    max_hist <- max(max_hist, max(graphics::strwidth(grDevices::axisTicks(c(0, 1.02*max(h1$counts)), FALSE), cex=6/8, units='inches')))*1.3
                #    max_pdfs <- max(max_pdfs, max(graphics::strwidth(grDevices::axisTicks(c(0, 1.02*max(x$modelling$pdfs[[tax]][[clim]]$pdfsp)), FALSE), cex=6/8, units='inches')))*1.3
                #}

                for(clim in climate) {
                    ## Plot the variable name --------------------------------------------
                    graphics::par(mar=c(0,0,0,0))
                    graphics::plot(NA, NA, frame=FALSE, axes=FALSE, xlim=c(0,1), ylim=c(0,1))

                    i=8
                    lab_width <- graphics::strwidth(accClimateVariables(clim)[3], cex=1, units='inches')
                    lab_width <- lab_width * 1 / y1
                    while(lab_width > 1) {
                        i <- i-1
                        lab_width <- graphics::strwidth(accClimateVariables(clim)[3], cex=i/8, units='inches')
                        lab_width <- lab_width * 1 / y1
                    }
                    graphics::text(0.5, 0.5, accClimateVariables(clim)[3], font=1, adj=c(0.5, 0.5), srt=90, cex=i/8)


                    ## Plot the distribution over climate --------------------------------
                    brks <- c(x$modelling$ccs[[clim]]$k1, max(x$modelling$ccs[[clim]]$k1)+diff(x$modelling$ccs[[clim]]$k1[1:2]))
                    R1 <- terra::rast(cbind(climate_space[, 1:2],
                                            climate_space[, clim] ),
                                      type= 'xyz',
                                      crs = terra::crs("+proj=longlat +datum=WGS84 +no_defs"))

                    graphics::par(mar = c(0, 0, 0, 0), ps=8*3/2)
                    plot_map_eqearth(R1, ext,
                                     zlim=range(brks), col=grDevices::colorRampPalette(col.climate)( length(brks)-1 ),
                                     brks.pos = brks, brks.lab = brks,
                                     title=accClimateVariables(clim)[3],
                                     site_xy = site_xy,
                                     colour_scale=FALSE, top_layer=veg_space,
                                     dim=c(x3*width/(w0+3*x3), y1*height/(y1+y1*length(climate))))


                    ## Plot the histogram ------------------------------------------------
                    h1 <- graphics::hist(climate_space[, clim],
                               breaks=c(x$modelling$ccs[[clim]]$k1, max(x$modelling$ccs[[clim]]$k1)+diff(x$modelling$ccs[[clim]]$k1[1:2])),
                               plot=FALSE)
                    h2 <- graphics::hist(unique(distribs[[tax]][, colnames(distribs[[tax]]) != 'taxonid'])[, clim],
                               breaks=c(x$modelling$ccs[[clim]]$k1, max(x$modelling$ccs[[clim]]$k1)+diff(x$modelling$ccs[[clim]]$k1[1:2])),
                               plot=FALSE)

                    graphics::par(mar=c(0,0,0.2,0.2))
                    xval <- range(h1$breaks)
                    labs_width <- graphics::strwidth('9999999 ', cex=6/8, units='inches')
                    labs_width <- max(labs_width, graphics::strwidth(max(c(h1$counts, h2$counts)), cex=6/8, units='inches'))
                    space_width <- graphics::strwidth(' ', cex=6/8, units='inches')

                    dX <- diff(xval)
                    xval <- xval + c(-labs_width, 1.1*space_width) * diff(xval) / (x3 - labs_width - space_width)

                    yval <- c(0, 1.03*max(h1$counts))
                    opar <- graphics::par(lwd=0.8)
                    graphics::plot(NA, NA, type='n', xlim=xval, ylim=yval, axes=FALSE, frame=FALSE, main='', xaxs='i', yaxs='i')  ;  {

                        space_width <- diff(range(h1$breaks)) * space_width / (x3 - labs_width - 2*space_width)

                        for(yvl in graphics::axTicks(2)){
                            if(yvl <= 1.02*max(h1$counts)) {
                                graphics::segments(h1$breaks[1], yvl, max(h1$breaks), yvl, col=ifelse(yvl==0, 'black', 'grey90'))
                                graphics::segments(h1$breaks[1], yvl, h1$breaks[1]+space_width, yvl)
                                graphics::segments(max(h1$breaks), yvl, max(h1$breaks)-space_width, yvl)
                                dff <- yvl+graphics::strheight(yvl, cex=6/8, units='user')/2 - yval[2]
                                graphics::text(h1$breaks[1]-space_width, yvl - ifelse(dff < 0, 0, dff*2), yvl, cex=6/8, adj=c(1,ifelse(yvl==0, 0, 0.4)))
                            }
                        }
                        graphics::rect(h1$breaks[1],0,max(h1$breaks), 1.02*max(h1$counts))
                        graphics::plot(h1, add=TRUE, col=grDevices::colorRampPalette(col.climate)( length(brks)-1 ))
                        graphics::plot(h2, add=TRUE, col='black')
                    }
                    graphics::text(xval[1], 1.02*max(h1$counts)/2, 'Number of occurrences', cex=6/8, adj=c(0.5, 1), srt=90)

                    graphics::par(mar=c(0.2,0,0,0.2))
                    graphics::plot(NA, NA, type='n', xlim=xval, ylim=c(0, 1), axes=FALSE, frame=FALSE, main='', xaxs='i', yaxs='i')
                    graphics::text(mean(range(h1$breaks)), 0.25, accClimateVariables(clim)[3], font=1, adj=c(0.5, 0.5), cex=6/8)
                    if(add_modern) {
                        if(is.numeric(x$misc$site_info$climate[, clim])) {
                            graphics::points(x$misc$site_info$climate[, clim], 0.9, pch=24, col=NA, bg='red', cex=0.75, lwd=1.5)
                        }
                    }
                    for(xvl in graphics::axTicks(1)){
                        if(xvl >= h1$breaks[1]) {
                            graphics::segments(xvl, 1, xvl, 0.9)
                            dff <- xvl+graphics::strwidth(xvl, cex=6/8, units='user')/2 - xval[2]
                            graphics::text(xvl - ifelse(dff < 0, 0, dff), 0.80, xvl, cex=6/8, adj=c(0.5,1))
                        }
                    }
                    graphics::segments(h1$breaks[1],1,max(h1$breaks), 1)
                    graphics::text(h1$breaks[1]-space_width,  1, '0', cex=6/8, adj=c(1,0))



                    ## Plot the pdfs -----------------------------------------------------
                    graphics::par(mar=c(0,0,0.2,0.2))
                    xval <- range(x$modelling$xrange[[clim]], na.rm=TRUE)
                    labs_width <- graphics::strwidth('9999999 ', cex=6/8, units='inches')
                    #labs_width <- max(labs_width, graphics::strwidth(max(c(h1$counts, h2$counts)), cex=6/8, units='inches'))
                    space_width <- graphics::strwidth(' ', cex=6/8, units='inches')

                    dX <- diff(xval)
                    xval <- xval + c(-labs_width, 1.1*space_width) * diff(xval) / (x3 - labs_width - 2*space_width)

                    yval <- c(0, 1.03*max(x$modelling$pdfs[[tax]][[clim]]$pdfsp))
                    graphics::plot(NA, NA, type='n', xlim=xval, ylim=yval, axes=FALSE, frame=FALSE, main='', xaxs='i', yaxs='i')

                    space_width <- diff(range(x$modelling$xrange[[clim]], na.rm=TRUE)) * space_width / (x3 - labs_width - 2*space_width)

                    npoints <- x$parameters$npoints
                    for(yvl in graphics::axTicks(2)){
                        if(yvl <= 1.02*max(x$modelling$pdfs[[tax]][[clim]]$pdfsp)) {
                            graphics::segments(x$modelling$xrange[[clim]][1], yvl, x$modelling$xrange[[clim]][npoints], yvl, col=ifelse(yvl==0, 'black', 'grey90'))
                            graphics::segments(x$modelling$xrange[[clim]][1], yvl, x$modelling$xrange[[clim]][1]+space_width, yvl)
                            graphics::segments(x$modelling$xrange[[clim]][npoints], yvl, x$modelling$xrange[[clim]][npoints]-space_width, yvl)
                            dff <- yvl+graphics::strheight(yvl, cex=6/8, units='user')/2 - yval[2]
                            graphics::text(x$modelling$xrange[[clim]][1]-space_width, yvl - ifelse(dff < 0, 0, dff*2), yvl, cex=6/8, adj=c(1,ifelse(yvl==0, 0, 0.4)))
                        }
                    }

                    for(i in 1:ncol(x$modelling$pdfs[[tax]][[clim]]$pdfsp)) {
                        graphics::points(x$modelling$xrange[[clim]], x$modelling$pdfs[[tax]][[clim]]$pdfsp[, i], col='grey70', type='l')
                    }
                    graphics::polygon(x$modelling$xrange[[clim]][c(1,1:npoints, npoints)], c(0, x$modelling$pdfs[[tax]][[clim]]$pdfpol, 0), col='black')
                    graphics::rect(x$modelling$xrange[[clim]][1],0,x$modelling$xrange[[clim]][npoints], 1.02*max(x$modelling$pdfs[[tax]][[clim]]$pdfsp))
                    graphics::text(xval[1], 1.02*max(x$modelling$pdfs[[tax]][[clim]]$pdfsp)/2, 'Density of probability', cex=6/8, adj=c(0.5, 1), srt=90)

                    graphics::par(mar=c(0.2,0,0,0.2))
                    graphics::plot(NA, NA, type='n', xlim=xval, ylim=c(0, 1), axes=FALSE, main='', xaxs='i', yaxs='i')
                    graphics::text(mean(range(x$modelling$xrange[[clim]])), 0.25, accClimateVariables(clim)[3], font=1, adj=c(0.5, 0.5), cex=6/8)
                    if(add_modern) {
                        if(is.numeric(x$misc$site_info$climate[, clim])) {
                            graphics::points(x$misc$site_info$climate[, clim], 0.9, pch=24, col=NA, bg='red', cex=0.75, lwd=1.5)
                        }
                    }
                    for(xvl in graphics::axTicks(1)){
                        if(xvl >= ifelse(x$parameters$shape[clim,]=='normal',x$modelling$xrange[[clim]][1], 0)) {
                            graphics::segments(xvl, 1, xvl, 0.9)
                            dff <- xvl+graphics::strwidth(xvl, cex=6/8, units='user')/2 - xval[2]
                            graphics::text(xvl - ifelse(dff < 0, 0, dff), 0.80, xvl, cex=6/8, adj=c(0.5,1))
                        }
                    }
                    graphics::rect(x$modelling$xrange[[clim]][1],1,x$modelling$xrange[[clim]][npoints], 1)
                    graphics::text(x$modelling$xrange[[clim]][1]-space_width, 1, '0', cex=6/8, adj=c(1,0))

                    graphics::par(opar)
                }
            }
            if(save & as.png) grDevices::dev.off()
        }

        if(save) {
            if(!as.png) grDevices::dev.off()
        }
    } else {
        cat('This function only works with a crestObj.\n\n')
    }
    invisible()
}
