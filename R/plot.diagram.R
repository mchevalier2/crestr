#' Plot stratigraphic data as polygons or barplots.
#'
#' This function plots stratigraphic data either as polygons or bars.
#'
#' @param x A data frame of the data to plot (first column with age or depth)
#'        and the taxa in the following columns. x can also be a crestObj.
#' @param bars A boolean that indicates if the data should be plotted as polygons
#'        (default: bars=FALSE) or vertical bars (bars=TRUE).
#' @param col Colours to be used for the polygons. If the number of colours does
#'        not match the number of taxa, colors will be recyled.
#' @param amplif A factor the show exageration on the diagram. Only for polygon
#'        plot. Default 5.
#' @param save A boolean to indicate if the diagram shoud be saved as a pdf file.
#'        Default is FALSE.
#' @param filename An absolute or relative path that indicates where the diagram
#'        should be saved. Also used to specify the name of the file. Default:
#'        the file is saved in the working directory under the name Diagram.pdf.
#' @param width The width of the output file in inches (default 3.54in ~ 9cm).
#' @param height The height of the output file in inches (default 9in ~ 23cm).
#' @param yax_incr Graphical parameters describing the increment size on the y-axis (default 5).
#' @param bar_width Width of the bars of the barplot (default 1).
#' @param xlim The range covered by the x-axis. Canbe adjusted to get round numbers on the x-ais. If smaller than the range overed by the data, the data will be truncated (default: range of the data).
#' @param tickAtSample Boolean that indicates whether a tick mark should be added on the x-axis at the location of each sample (default TRUE).
#' @param col_pos Graphical parameter for the barplot. Colour of all the positive values (default black).
#' @param col_neg Graphical parameter for the barplot. Colour of all the negative values (default light grey).
#' @param title Name to be added on top of the plot (default NA).
#' @export
#' @examples
#' data(crest_ex)
#' plot_diagram(crest_ex, bars=TRUE, col='black', bar_width=0.8)
#' plot_diagram(crest_ex,  col=1:7, tickAtSample=FALSE)
#' \dontrun{
#' plot_diagram(crest_ex, save=TRUE, filename='testDiagram.pdf',
#'              bars=TRUE, col_pos='cornflowerblue', col_neg='darkgreen',
#'              bar_width=0.8, xlim=c(3,15))
#' }
#'
plot_diagram <- function(x, bars=FALSE,
                         col = 'black',
                         amplif = 5,
                         save=FALSE, filename='Diagram.pdf',
                         width=3.54, height= 9,
                         yax_incr = 5, bar_width=1,
                         xlim=NA, tickAtSample=TRUE,
                         col_pos = 'black', col_neg='grey80', title=NA) {

  if (! isColourStr(col_pos))  {
    cat(paste0("WARNING: '",col_pos,"' is not a valid colour. Using 'black' instead.\n"))
    col_pos='black'
  }
  if (! isColourStr(col_neg)) {
    cat(paste0("WARNING: '",col_neg,"' is not a valid colour. Using 'grey80' instead.\n"))
    col_neg='grey80'
  }

  par_usr <- list()

  #x.w <- NA
  if (methods::is(x)[1] == 'crestObj') {
    #if (!unique(is.na(x$modelling$weights))) {
    #  x.w <- cbind(x=x$inputs$x, x$modelling$weights)
    #  x.w <- x.w[order(x$inputs$x), ]
    #}
    if(unique(is.na(unlist(x$inputs$df)))) {
      cat(paste0("ERROR: No data available for a stratigraphic diagram.\n"))
      return(invisible())
    }
    col_names <- c(x$inputs$x.name, x$inputs$taxa.name)
    x <- cbind(x=x$inputs$x, x$inputs$df)
    colnames(x) <- col_names
  }
  x <- x[order(x[, 1]), ]

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
  if(!is.na(title)) yrange <- yrange + c(0, 0.3*dY)


  if(save) {
    grDevices::pdf(filename, width=width, height=height)
    plot(x[, 1], x[, 1], type='n', xlim=xrange, ylim=yrange, axes=FALSE, frame=FALSE, xaxs='i', yaxs='i', main='', xlab='', ylab='')
    str_max_left <- 0.7*max(graphics::strwidth(colnames(x)[-1], cex=0.5, units='inches'))
    str_max_right <- max(c(0.1, graphics::strwidth(as.character(max(seq(0, max(c(max(abs(x[, -1])), yax_incr)), 2*yax_incr))), cex=0.5, units='inches')))
    grDevices::dev.off()
    grDevices::pdf(filename, width=width, height=height)

    wd <- width - str_max_left - str_max_right
    k <- diff(xrange) / wd *2
    xrange <- xrange + k * c(-str_max_left, str_max_right)
  } else {
    xrange <- xrange + c(-2.5*dX, 0.9 *dX)
  }

  par_usr$mar <- graphics::par(mar=c(0,0,0,0))[[1]]

  plot(x[, 1], x[, 1], type='n', xlim=xrange, ylim=yrange, axes=FALSE, frame=FALSE, xaxs='i', yaxs='i', main='', xlab='', ylab='')
  if(!is.na(title)) graphics::text(mean(xlim), mean(c(max(cs), yrange[2])), title, cex=0.8, font=1, adj=c(0.5, 1))
  if(bars) {
    bar_width <- bar_width/2
    for(i in 2:ncol(x)) {
      for(j in 1:nrow(x)) {
        graphics::rect(x[j,1]-bar_width, cs[i], x[j,1]+bar_width, cs[i]+abs(x[j,i]), col=ifelse(x[j,i] > 0, col_pos, col_neg), border=NA)
      }
      graphics::segments(xlim[1], cs[i], xlim[2], cs[i], lwd=0.5)
      graphics::segments(xlim[2], cs[i], xlim[2], cs[i] + max(c(max(abs(x[, i])), yax_incr)), lwd=0.5)
      for(j in seq(0, max(c(max(abs(x[, i])), yax_incr)), yax_incr)) {
        graphics::segments(xlim[2], cs[i] + j, xlim[2] + 0.1*dX, cs[i] + j, lwd=0.5)
        if (j %% (2*yax_incr) == 0) graphics::text(xlim[2] + 0.15*dX, cs[i] + j, j, cex=0.5, adj=c(0, 0.4))
      }
      graphics::text(xlim[1] - 0.15*dX, (cs[i]+cs[i+1])/2, colnames(x)[i], cex=0.5, adj=c(1,0.5))
    }
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
      for(j in seq(0, max(c(max(abs(x[, i])), yax_incr)), yax_incr)) {
        graphics::segments(xlim[2], cs[i] + j, xlim[2] + 0.1*dX, cs[i] + j, lwd=0.5)
        if (j %% (2*yax_incr) == 0) graphics::text(xlim[2] + 0.15*dX, cs[i] + j, j, cex=0.5, adj=c(0, 0.4))
      }
      graphics::text(xlim[1] - 0.15*dX, (cs[i]+cs[i+1])/2, colnames(x)[i], cex=0.5, adj=c(1,0.5))
    }
  }
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
    }
    graphics::segments(tck, -0.06*dY, tck, -0.1*dY, lwd=0.5)
    graphics::text(tck, -0.12*dY, tck, adj=c(0.5, 1), cex=0.5)
  }
  if( tickAtSample ) {
    for( tck in x[, 1]) {
      graphics::segments(tck, -0.05*dY, tck, -0.01*dY, lwd=0.5)
    }
  }

  graphics::text(mean(xlim), -0.40*dY, colnames(x)[1], cex=0.6, adj=c(0.5,0))
  if(save) {
    grDevices::dev.off()
  } else {
    graphics::par(par_usr)
  }
  invisible()
}
