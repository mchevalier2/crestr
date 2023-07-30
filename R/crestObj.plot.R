#' Plot the reconstructions.
#'
#' Plot the reconstructions and their uncertainties if they exist.
#'
#' @inheritParams graphics::plot
#' @param x A \code{\link{crestObj}} produced by the \code{\link{crest}},
#'        \code{\link{crest.reconstruct}} or \code{\link{loo}} functions.
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
#' @param as.anomaly A boolean to indicate if the reconstructions should be
#'        plotted as absolute values (Default, \code{FALSE}) or anomalies
#'        '(\code{TRUE}).
#' @param anomaly.base The anomaly value. Should be a vector with the same
#'        length as \code{climate}. Default values are the climate values
#'        correpsonding to the location of the record (\code{site_info} in
#'        \code{crest.get_modern_data}).
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
#' @param col.hiatus A colour for the hiatus(es) of the record (default white)
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
#' plot(reconstr, climate='bio1', simplify = TRUE, as.anomaly=TRUE)
#'
plot.crestObj <- function(x,
                          climate = x$parameters$climate[1],
                          uncertainties = x$parameters$uncertainties,
                          optima = TRUE,
                          add_modern = FALSE,
                          simplify = FALSE,
                          as.anomaly = FALSE,
                          anomaly.base = x$misc$site_info$climate[climate],
                          xlim = NA, ylim = NA,
                          pt.cex = 0.8, pt.lwd = 0.8,
                          pt.col=ifelse(simplify, 'black', 'white'),
                          col.hiatus = 'white',
                          save = FALSE, width = 5.51, height = 5.51,
                          as.png = FALSE, png.res=300,
                          filename = 'Reconstruction.pdf',
                          col=viridis::viridis(125)[26:125],
                          ...) {

    if(base::missing(x)) x

    if(!is.crestObj(x)) {
        cat('\nx should be a crestObj.\n\n')
        return(invisible(NA))
    }

    if (length(x$reconstructions) == 0 || is.null(climate)) {
        stop("No reconstruction available for plotting.\n")
    }

    if(as.anomaly) {
        if(!is.na(anomaly.base)[1]) {
            anomaly.base <- as.data.frame(anomaly.base)
            anomaly.base[is.na(anomaly.base)] <- 0
            colnames(anomaly.base) <- climate
        } else {
            as.anomaly <- FALSE
            warning("No values available to calculate anomalies. Provide coordinates to crest.get_modern_data() to allow crestr to extract the local conditions, or assign values using 'anomaly.base = c(val1, val2, ...)' in the plot function.'")
        }
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
        on.exit(graphics::par(par_usr))
    }

    for (clim in climate) {
        if (idx == 0 && !save) {
            graphics::par(mfrow = grDevices::n2mfrow(length(climate)))[[1]]
        }
        idx <- idx + 1
        pdf <- t(x$reconstructions[[clim]][["likelihood"]])[-1, ]
        pdfter <- pdf

        for (i in 2:ncol(pdf)) {
            oo <- rev(order(pdf[, i]))
            tmp1 <- pdf[oo, 1]
            tmp2 <- pdf[oo, i]
            oo <- order(tmp1)
            pdfter[, i] <- cumsum(tmp2 / sum(tmp2))[oo]
        }
        if(as.anomaly) pdfter[, 1] <- pdfter[, 1] - as.numeric(anomaly.base[clim])

        var_to_plot <- ifelse(optima, 2, 3)
        xmn <- which.min(x$reconstructions[[clim]][["optima"]][, var_to_plot])
        xmx <- which.max(x$reconstructions[[clim]][["optima"]][, var_to_plot])

        ## This is for the fill uncertainty plot.
        if (unique(is.na(ylim))) {
            ymn <- pdfter[which(pdfter[, xmn + 1] <= 0.99)[1], 1]
            ymx <- pdfter[rev(which(pdfter[, xmx + 1] <= 0.99))[1], 1]
        } else {
            ymn <- ylim[idx * 2 - 1]
            ymx <- ylim[idx * 2]
        }

        ## This is for the simplified plot
        if (unique(is.na(ylim))) {
            uncertainties <- sort(uncertainties)
            val <- apply(pdfter[, -1], 2, function(x) {
                if(is.na(x[1])) return(c(NA, NA))
                w <- which(x <= uncertainties[length(uncertainties)])
                return(c(w[1], w[length(w)]))
                }
            )
            ylim2 <- pdfter[c(min(val[1, ], na.rm=TRUE),max(val[2, ], na.rm=TRUE)), 1]
            ylim2[1] <- max(ylim[1], ylim2[1], na.rm=TRUE)
            ylim2[2] <- min(ylim[2], ylim2[2], na.rm=TRUE)
        } else {
            ylim2 <- ylim[c(idx * 2 - 1, idx * 2)]
        }

        climate_names <- accClimateVariables()

        if(is.character(x$inputs$x) | is.factor(x$inputs$x)) {
            if(simplify) warning("The plotting function is not adapted to non-numeric x values. The sample names were replaced by numeric indexes.")
            xx <- base::seq_along(x$inputs$x)
        } else {
            xx <- sort(base::jitter(x$inputs$x, 0.0001))
        }



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
                 xaxs='i', yaxs='i', frame = TRUE, axes=FALSE, xlab='', ylab='')

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
                modval <- x$misc$site_info$climate[, clim]
                if(as.anomaly) modval <- modval - as.numeric(anomaly.base[clim])
                graphics::segments(xlim[1], modval, xlim[2], modval,
                    col = "grey70", cex = 0.5, lty = 2
                )
            }
            rcnstrctn <- x$reconstructions[[clim]]$optima[, var_to_plot]
            if(as.anomaly) rcnstrctn <- rcnstrctn - as.numeric(anomaly.base[clim])
            graphics::points(xx, rcnstrctn, type='o', pch=18, col=pt.col, cex=pt.cex, lwd=pt.lwd)

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
              NAcol = col.hiatus,
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
                modval <- x$misc$site_info$climate[, clim]
                if(as.anomaly) modval <- modval - as.numeric(anomaly.base[clim])
                graphics::segments(xlim[1], modval, xlim[2], modval,
                    col = "red", cex = 0.5, lty = 2
                )
            }
            rcnstrctn <- x$reconstructions[[clim]]$optima[, var_to_plot]
            if(as.anomaly) rcnstrctn <- rcnstrctn - as.numeric(anomaly.base[clim])
            graphics::points(xx, rcnstrctn,
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
    invisible(x)
}
