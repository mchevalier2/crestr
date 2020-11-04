#' Plot stratigraphic data as polygons or barplots.
#'
#' This function plots stratigraphic data either as polygons or bars.
#'
#' @param df A data frame of the data to plot (first column with age or depth)
#'        and the taxa in the following columns. df can also be a crestObj.
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
#' @param col_neg Graphical parameter for the barplot. Colour of all the negative values (default white).
#' @export
#' @examples
#' data(crest_ex)
#' plot_diagram(crest_ex, bars=TRUE, col='black')
#' plot_diagram(crest_ex,  col=1:7, tickAtSample=FALSE)
#' \dontrun{
#' plot_diagram(crest_ex, save=TRUE, filename='testDiagram.pdf',
#'              bars=TRUE, col_pos='cornflowerblue', col_neg='darkgreen',
#'              bar_width=0.8, xlim=c(3,15))
#' }
#'
plot_diagram <- function(df, bars=FALSE,
                         col = 'black',
                         amplif = 5,
                         save=FALSE, filename='Diagram.pdf',
                         width=3.54, height= 9,
                         yax_incr = 5, bar_width=1,
                         xlim=NA, tickAtSample=TRUE,
                         col_pos = 'black', col_neg='white') {

  if (! isColourStr(col_pos))  {
    cat(paste0("WARNING: '",col_pos,"' is not a valid colour. Using 'black' instead.\n"))
    col_pos='black'
  }
  if (! isColourStr(col_neg)) {
    cat(paste0("WARNING: '",col_neg,"' is not a valid colour. Using 'white' instead.\n"))
    col_neg='white'
  }

  #df.w <- NA
  if (methods::is(df)[1] == 'crestObj') {
    #if (!unique(is.na(df$modelling$weights))) {
    #  df.w <- cbind(x=df$inputs$x, df$modelling$weights)
    #  df.w <- df.w[order(df$inputs$x), ]
    #}
    df <- cbind(x=df$inputs$x, df$inputs$df)
  }
  df <- df[order(df[, 1]), ]

  if (length(col) != ncol(df)-1) col = base::rep_len(col,ncol(df)-1)

  if(save) grDevices::pdf(filename, width=width, height=height)
  par_usr <- graphics::par()

  graphics::par(mar=c(0,0,0,0))

  cs <- apply(df[, -1], 2, max)
  cs <- ifelse(cs > yax_incr, cs, yax_incr)
  cs <- c(0, 0, cumsum(cs + sum(cs)*0.01 ) )# Adding 1% of the total space to each row

  if(unique(is.na(xlim))) {
    xrange <- range(df[, 1])
    xlim <- xrange
  } else {
    xrange <- xlim
    df <- df[df[, 1] >= xlim[1] & df[, 1] <= xlim[2], ]
    #if(!unique(is.na(df.w))) {
    #  df.w <- df.w[df.w[, 1] >= xlim[1] & df.w[, 1] <= xlim[2], ]
    #}
  }
  if(bars) {
    xrange[1] <- min(c(xrange[1]), df[1, 1]-bar_width/2)
    xrange[2] <- max(c(xrange[2]), df[nrow(df), 1]+bar_width/2)
    xlim_init <- xlim
    xlim <- xrange
  }

  x.scale=3.54/width
  dX <- 0.1*(xrange[2]-xrange[1])*x.scale
  xrange <- xrange + c(-2.5*dX, 0.7*dX)

  yrange = range(cs)
  dY <- 0.1*(yrange[2]-yrange[1])
  yrange <- yrange + c(-0.6*dY, 0)

  plot(df[, 1], df[, 1], type='n', xlim=xrange, ylim=yrange, axes=FALSE, frame=FALSE, xaxs='i', yaxs='i', main='', xlab='', ylab='')
  if(bars) {
    bar_width <- bar_width/2
    for(i in 2:ncol(df)) {
      for(j in 1:nrow(df)) {
        graphics::rect(df[j,1]-bar_width, cs[i], df[j,1]+bar_width, cs[i]+abs(df[j,i]), col=ifelse(df[j,i] > 0, col_pos, col_neg), lwd=0.1)
      }
      graphics::segments(xlim[1], cs[i], xlim[2], cs[i], lwd=0.5)
      graphics::segments(xlim[2], cs[i], xlim[2], cs[i] + max(c(max(df[, i]), yax_incr)), lwd=0.5)
      for(j in seq(0, max(c(max(df[, i]), yax_incr)), yax_incr)) {
        graphics::segments(xlim[2], cs[i] + j, xlim[2] + 0.1*dX, cs[i] + j, lwd=0.5)
        if (j %% 2*yax_incr == 0) graphics::text(xlim[2] + 0.5*dX, cs[i] + j, j, cex=0.5, adj=c(1, 0.4))
      }
      graphics::text(xlim[1] - 0.15*dX, (cs[i]+cs[i+1])/2, colnames(df)[i], cex=0.7, adj=c(1,0.5))
    }
  } else {
    for(i in 2:ncol(df)) {
      graphics::polygon(c(df[1, 1], df[, 1], df[nrow(df), 1]),
              cs[i] + c(0, df[, i], 0),
              col=col[i-1], border=NA
              )
      if(amplif > 1) {
        graphics::points(df[, 1], cs[i] + df[, i]*amplif,
                 col='grey70', lwd=0.5, lty=2, type='l'
                )
        graphics::rect(df[1, 1], cs[i+1], df[nrow(df), 1], cs[i] + max(df[, i]*amplif), col='white', border=NA)
      }
      graphics::segments(xlim[1], cs[i], xlim[2], cs[i], lwd=0.5)
      graphics::segments(xlim[2], cs[i], xlim[2], cs[i] + max(c(max(df[, i]), yax_incr)), lwd=0.5)
      for(j in seq(0, max(c(max(df[, i]), yax_incr)), yax_incr)) {
        graphics::segments(xlim[2], cs[i] + j, xlim[2] + 0.1*dX, cs[i] + j, lwd=0.5)
        if (j %% 2*yax_incr == 0) graphics::text(xlim[2] + 0.5*dX, cs[i] + j, j, cex=0.5, adj=c(1, 0.4))
      }
      graphics::text(xlim[1] - 0.15*dX, (cs[i]+cs[i+1])/2, colnames(df)[i], cex=0.7, adj=c(1,0.5))
    }
  }
  graphics::segments(xlim[1], -0.1*dY, xlim[2], -0.1*dY, lwd=0.5)
  for( tck in graphics::axTicks(1)) {
    if( tck < xlim[1] ) {
      tck <- xlim[1]
      if( bars ) tck <- xlim_init[1]
    }
    if( tck > xlim[2] ) {
      tck <- xlim[2]
      if( bars ) tck <- xlim_init[2]
    }
    graphics::segments(tck, -0.1*dY, tck, -0.15*dY, lwd=0.5)
    graphics::text(tck, -0.18*dY, tck, adj=c(0.5, 1), cex=0.7)
  }
  if( tickAtSample ) {
    for( tck in df[, 1]) {
      graphics::segments(tck, -0.1*dY, tck, -0.05*dY, lwd=0.5)
    }
  }

  graphics::text(mean(xlim), -0.5*dY, colnames(df)[1], cex=0.8, adj=c(0.5,0))
  if(save)  grDevices::dev.off()

  #par(par_usr)
  invisible()
}
