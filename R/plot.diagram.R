#' Plot stratigraphic data as polygons or barplots.
#'
#' This function plots stratigraphic data either as polygons or bars.
#'
#' @inheritParams plot.crestObj
#' @param x A data frame of the data to plot (first column with age or depth)
#'        and the taxa in the following columns. x can also be a
#'        \code{\link{crestObj}}.
#' @param bars A boolean that indicates if the data should be plotted as polygons
#'        (default: \code{bars=FALSE}) or vertical bars (\code{bars=TRUE}).
#' @param col Colours to be used for the polygons. If the number of colours does
#'        not match the number of taxa, colors will be recycled.
#' @param amplif A factor the show exaggeration on the diagram. Only for polygon
#'        plot. Default 5.
#' @param filename An absolute or relative path that indicates where the diagram
#'        should be saved. Also used to specify the name of the file. Default:
#'        the file is saved in the working directory under the name
#'        \code{'Diagram.pdf'}.
#' @param width The width of the output file in inches (default 3.54in ~ 9cm).
#' @param height The height of the output file in inches (default 9in ~ 23cm).
#' @param yax_incr Graphical parameters describing the increment size on the
#'        y-axis (default 5).
#' @param bar_width Width of the bars of the barplot (default 1/50th of the x range).
#' @param xlim The range covered by the x-axis. Can be adjusted to get round
#'        numbers on the x-axis. If smaller than the range covered by the data,
#'        the data will be truncated (default: range of the data).
#' @param tickAtSample Boolean that indicates whether a tick mark should be added
#'         on the x-axis at the location of each sample (default \code{TRUE}).
#' @param col_pos Graphical parameter for the barplot. Colour of all the
#'        positive values (default black).
#' @param col_neg Graphical parameter for the barplot. Colour of all the
#'        negative values (default light grey).
#' @param title Name to be added on top of the plot (default \code{NA}).
#' @return No return value, this function is used to plot.
#' @export
#' @examples
#' data(crest_ex)
#' plot_diagram(crest_ex, bars=TRUE, col='black', bar_width=0.8)
#' plot_diagram(crest_ex,  col=1:7, tickAtSample=FALSE)
#' #> Replace 'tempdir()' by the location where the sample should be saved (e.g. 'getwd()')
#' plot_diagram(crest_ex, save=TRUE,
#'              filename=file.path(tempdir(), 'testDiagram.pdf'),
#'              bars=TRUE, col_pos='cornflowerblue', col_neg='darkgreen',
#'              bar_width=0.8, xlim=c(3,15))
#'
plot_diagram <- function(x, bars=FALSE,
                         col = 'black',
                         amplif = 5,
                         save=FALSE, filename='Diagram.pdf',
                         width=3.54, height= 9,
                         as.png = FALSE, png.res=300,
                         yax_incr = 5, bar_width = diff(range(x$inputs$x))/50,
                         xlim=NA, tickAtSample=TRUE,
                         col_pos = 'black', col_neg='grey80', title=NA) {

    if(base::missing(x)) x

    if (! isColourStr(col_pos))  {
        warning("'",col_pos,"' is not a valid colour. Using 'black' instead.\n")
        col_pos='black'
    }
    if (! isColourStr(col_neg)) {
        warning("'",col_neg,"' is not a valid colour. Using 'grey80' instead.\n")
        col_neg='grey80'
    }

    #x.w <- NA
    if (methods::is(x)[1] == 'crestObj') {
        #if (!unique(is.na(x$modelling$weights))) {
        #  x.w <- cbind(x=x$inputs$x, x$modelling$weights)
        #  x.w <- x.w[order(x$inputs$x), ]
        #}
        if(unique(is.na(unlist(x$inputs$df)))) {
          stop(paste0("No data available for a stratigraphic diagram.\n\n"))
        }
        #col_names <- c(x$inputs$x.name, x$inputs$taxa.name)
        x <- cbind(x=x$inputs$x, x$inputs$df)
        #colnames(x) <- col_names
    }
    if(is.numeric(x[, 1])) {
        x <- x[order(x[, 1]), ]
    } else {
        warning("The plotting function is not adapted to non-numeric x values. The sample names were replaced by numeric indexes.")
        x[, 1] <- 1:nrow(x)
    }

    if (length(col) != ncol(x)-1) col = base::rep_len(col,ncol(x)-1)

    cs <- apply(abs(x[, -1]), 2, max)
    cs <- ifelse(cs > yax_incr, cs, yax_incr)
    cs <- c(0, 0, cumsum(cs + sum(cs)*0.01 ) )# Adding 1% of the total space to each row

    if(unique(is.na(xlim))) {
        xrange <- range(x[, 1])
        xlim <- xrange
    } else {
        xrange <- xlim
        x <- x[x[, 1] >= xlim[1] & x[, 1] <= xlim[2], ]
        #if(!unique(is.na(x.w))) {
        #  x.w <- x.w[x.w[, 1] >= xlim[1] & x.w[, 1] <= xlim[2], ]
        #}
    }
    if(bars) {
        xrange[1] <- min(c(xrange[1]), x[1, 1]-bar_width/2)
        xrange[2] <- max(c(xrange[2]), x[nrow(x), 1]+bar_width/2)
        xlim_init <- xlim
        xlim <- xrange
    }

    x.scale=3.54/width
    dX <- 0.1*(xrange[2]-xrange[1])*x.scale

    yrange = range(cs)
    dY <- 0.1*(yrange[2]-yrange[1])
    yrange <- yrange + c(-0.45*dY, 0)
    if(bars & is.na(title)) title=' ' # If there is no title, but plotting bars I need the space for the caption
    if(!is.na(title)) yrange <- yrange + c(0, 0.3*dY*(0.3+length(strsplit(title, '\n')[[1]]))) ## Counts how many lines the title has.


    if(save) {
        if(as.png) {
            grDevices::png(paste0(strsplit(filename, '.png')[[1]], '.png'), width = width, height = height, units='in', res=png.res)
        } else {
            grDevices::pdf(filename, width=width, height=height)
        }
    } else {
        par_usr <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(par_usr))
    }


    graphics::par(mar=c(0,0,0,0), ps=8)

    str_max_left <- max(graphics::strwidth(colnames(x)[-1], cex=6/8, units='inches'))
    str_max_right <- max(c(0.1, graphics::strwidth(as.character(max(seq(0, max(c(max(abs(x[, -1])), yax_incr)), 2*yax_incr))), cex=6/8, units='inches')))

    xrange2 <- xrange
    xrange2[1] <- xrange[1] - str_max_left*diff(xrange)/(width-str_max_left-str_max_right) - 0.50*dX
    xrange2[2] <- xrange[2] + str_max_right*diff(xrange)/(width-str_max_left-str_max_right) + 0.50*dX

    plot(x[, 1], x[, 1], type='n', xlim=xrange2, ylim=yrange, axes=FALSE, frame=FALSE, xaxs='i', yaxs='i', main='', xlab='', ylab='')

    if(bars) {
        print(x[,1])
        print(cs)
        bar_width <- bar_width/2
        for(i in 2:ncol(x)) {
            for(j in 1:nrow(x)) {
                graphics::rect(x[j,1]-bar_width, cs[i], x[j,1]+bar_width, cs[i]+abs(x[j,i]), col=ifelse(x[j,i] > 0, col_pos, col_neg), border=NA)
            }
            graphics::segments(xlim[1], cs[i], xlim[2], cs[i], lwd=0.5)
            graphics::segments(xlim[2], cs[i], xlim[2], cs[i] + max(c(max(abs(x[, i])), yax_incr)), lwd=0.5)
            add.axs <- TRUE
            for(j in seq(0, max(c(max(abs(x[, i])), yax_incr)), yax_incr)) {
                graphics::segments(xlim[2], cs[i] + j, xlim[2] + 0.1*dX, cs[i] + j, lwd=0.5)
                if (add.axs) {
                    graphics::text(xlim[2] + 0.15*dX, cs[i] + j, j, cex=6/8, adj=c(0, 0.4))
                    add.axs <- FALSE
                } else {
                    add.axs <- TRUE
                }
            }
            graphics::text(xlim[1] - 0.15*dX, (cs[i]+cs[i+1])/2, colnames(x)[i], cex=6/8, adj=c(1,0.5))
        }
        dX <- diff(xrange2)
        graphics::rect(xrange2[1] + 0.02*dX, mean(c(max(cs), yrange[2])) + 0.03*dY, xrange2[1] + 0.08*dX, mean(c(max(cs), yrange[2])) + 0.13*dY, col=col_pos)
        graphics::text(xrange2[1] + 0.10*dX, mean(c(max(cs), yrange[2])) + 0.08*dY, '(+) anomaly', cex=6/8, adj=c(0, 0.45))
        graphics::rect(xrange2[1] + 0.02*dX, mean(c(max(cs), yrange[2])) - 0.03*dY, xrange2[1] + 0.08*dX, mean(c(max(cs), yrange[2])) - 0.13*dY, col=col_neg)
        graphics::text(xrange2[1] + 0.10*dX, mean(c(max(cs), yrange[2])) - 0.08*dY, '(-) anomaly', cex=6/8, adj=c(0, 0.45))

    } else {
        for(i in 2:ncol(x)) {
            if(amplif > 1) {
              graphics::points(x[, 1], cs[i] + x[, i]*amplif,
                       col='grey80', lwd=0.5, lty=2, type='l'
                      )
              graphics::rect(x[1, 1], cs[i+1], x[nrow(x), 1], cs[i] + max(abs(x[, i])*amplif), col='white', border=NA)
            }
            graphics::polygon(c(x[1, 1], x[, 1], x[nrow(x), 1]),
              cs[i] + c(0, x[, i], 0),
              col=col[i-1], border=NA
              )
            graphics::segments(xlim[1], cs[i], xlim[2], cs[i], lwd=0.5)
            graphics::segments(xlim[2], cs[i], xlim[2], cs[i] + max(c(max(abs(x[, i])), yax_incr)), lwd=0.5)
            add.axs <- TRUE
            for(j in seq(0, max(c(max(abs(x[, i])), yax_incr)), yax_incr)) {
                graphics::segments(xlim[2], cs[i] + j, xlim[2] + 0.1*dX, cs[i] + j, lwd=0.5)
                if (add.axs) {
                    graphics::text(xlim[2] + 0.15*dX, cs[i] + j, j, cex=6/8, adj=c(0, 0.4))
                    add.axs <- FALSE
                } else {
                    add.axs <- TRUE
                }
            }
            graphics::text(xlim[1] - 0.15*dX, (cs[i]+cs[i+1])/2, colnames(x)[i], cex=6/8, adj=c(1,0.5))
        }
    }

    if(!is.na(title)) graphics::text(mean(xlim), mean(c(max(cs), yrange[2])), title, cex=1, font=2, adj=c(0.5, 0.5))

    graphics::segments(xlim[1], -0.06*dY, xlim[2], -0.06*dY, lwd=0.5)
    for( tck in graphics::axTicks(1)) {
        if( tck < xlim[1] ) {
            tck <- xlim[1]
            if( bars ) tck <- xlim_init[1]
            tck <- round(tck, 3)
        }
        if( tck > xlim[2] ) {
            tck <- xlim[2]
            if( bars ) tck <- xlim_init[2]
            tck <- round(tck, 3)
        }
        graphics::segments(tck, -0.06*dY, tck, -0.1*dY, lwd=0.5)
        graphics::text(tck, -0.12*dY, tck, adj=c(0.5, 1), cex=6/8)
    }
    if( tickAtSample ) {
        for( tck in x[, 1]) {
            graphics::segments(tck, -0.05*dY, tck, -0.01*dY, lwd=0.5)
        }
    }

    graphics::text(mean(xlim), -0.40*dY, colnames(x)[1], cex=1, adj=c(0.5,0))
    if(save) {
        grDevices::dev.off()
    }

    invisible()
}
