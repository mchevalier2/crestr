
#' Extract distributions from the database
#'
#' This function will extract the distributions of all the species composing each
#' taxon and return them as a list.
#'
#' @inheritParams crestObj
#' @param dbname The name of the database. Default is gbif4crest_02.
#' @return A crest() object containing the spatial distributions
#' @export
#' @examples
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#'
plot_diagram <- function(df, bars=FALSE,
                         col = rep('black', ncol(df)),
                         amplif = 5,
                         plot=TRUE,
                         save=FALSE, filename='Diagram.pdf',
                         width=3.54, height= 9,
                         perc_incr = 5,
                         xlim=NA, xax_incr=NA, tickAtSample=TRUE) {


  df.w <- NA
  if (is(df)[1] == 'crestObj') {
    if (!unique(is.na(df$modelling$weights))) {
      df.w <- cbind(x=df$inputs$x, df$modelling$weights)
      df.w <- df.w[order(df$inputs$x), ]
    }
    df <- cbind(x=df$inputs$x, df$inputs$df)
  }
  df <- df[order(df[, 1]), ]

  par_usr <- par()
  if(save) pdf(filename, width=width, height=height)

  par(mar=c(0,0,0,0))

  cs <- apply(df[, -1], 2, max)
  cs <- ifelse(cs > perc_incr, cs, perc_incr)
  cs <- c(0, 0, cumsum(cs + sum(cs)*0.01 ) )# Adding 1% of the total space to each row

  if(unique(is.na(xlim))) {
    xrange <- range(df[, 1])
    xlim <- xrange
  } else {
    xrange <- xlim
    df <- df[df[, 1] >= xlim[1] & df[, 1] <= xlim[2], ]
    if(!unique(is.na(df.w))) {
      df.w <- df.w[df.w[, 1] >= xlim[1] & df.w[, 1] <= xlim[2], ]
    }
  }
  x.scale=3.54/width
  dX <- 0.1*(xrange[2]-xrange[1])*x.scale
  xrange <- xrange + c(-2.5*dX, 0.7*dX)

  yrange = range(cs)
  dY <- 0.1*(yrange[2]-yrange[1])
  yrange <- yrange + c(-0.6*dY, 0)

  plot(df[, 1], df[, 1], type='n', xlim=xrange, ylim=yrange, axes=FALSE, frame=FALSE, xaxs='i', yaxs='i')
  if(bars) {

  } else {
    for(i in 2:ncol(df)) {
      polygon(c(df[1, 1], df[, 1], df[nrow(df), 1]),
              cs[i] + c(0, df[, i], 0),
              col=col[i], border=NA
              )
      if(amplif > 1) {
        points(df[, 1], cs[i] + df[, i]*amplif,
                 col='grey70', lwd=0.5, lty=2, type='l'
                )
        rect(df[1, 1], cs[i+1], df[nrow(df), 1], cs[i] + max(df[, i]*amplif), col='white', border=NA)
      }
      segments(xlim[1], cs[i], xlim[2], cs[i], lwd=0.5)
      segments(xlim[2], cs[i], xlim[2], cs[i] + max(c(max(df[, i]), perc_incr)), lwd=0.5)
      for(j in seq(0, max(c(max(df[, i]), perc_incr)), perc_incr)) {
        segments(xlim[2], cs[i] + j, xlim[2] + 0.1*dX, cs[i] + j, lwd=0.5)
        if (j %% 2*perc_incr == 0) text(xlim[2] + 0.5*dX, cs[i] + j, j, cex=0.5, adj=c(1, 0.4))
      }
      text(xlim[1] - 0.15*dX, (cs[i]+cs[i+1])/2, colnames(df)[i], cex=0.7, adj=c(1,0.5))
    }
  }
  segments(xlim[1], -0.1*dY, xlim[2], -0.1*dY, lwd=0.5)
  for( tck in axTicks(1)) {
    if( tck < xlim[1]) tck <- xlim[1]
    if( tck > xlim[2]) tck <- xlim[2]
    segments(tck, -0.1*dY, tck, -0.15*dY, lwd=0.5)
    text(tck, -0.18*dY, tck, adj=c(0.5, 1), cex=0.7)
  }
  if( tickAtSample ) {
    for( tck in df[, 1]) {
      segments(tck, -0.1*dY, tck, -0.05*dY, lwd=0.5)
    }
  }

  text(mean(xlim), -0.5*dY, colnames(df)[1], cex=0.8, adj=c(0.5,0))
  #rect(xlim[1], -0.1*dY, xlim[2], max(cs), lwd=0.5, col=NA, border='black')
  if(save)  dev.off()
}
