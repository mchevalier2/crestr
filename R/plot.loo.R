#' Plot de results of the leave-one-out analysis.
#'
#' Plot de results of the leave-one-out analysis.
#'
#' @inheritParams plot_diagram
#' @param optima A boolean to indicate whether to plot the optimum (\code{TRUE})
#'        or the mean (\code{FALSE}) estimates.
#' @param climate Climate variables to be used to generate the plot. By default
#'        all the variables are included.
#' @param filename An absolute or relative path that indicates where the diagram
#'        should be saved. Also used to specify the name of the file. Default:
#'        the file is saved in the working directory under the name
#'        \code{'Diagram_loo_climate.pdf'}.
#' @param yax_incr Graphical parameters describing the increment size on the
#'        y-axis (default 5).
#' @param bar_width Width of the bars of the barplot (default 1).
#' @param col_pos Graphical parameter for the barplot. Colour of all the
#'        positive values (default black).
#' @param col_neg Graphical parameter for the barplot. Colour of all the
#'        negative values (default grey80).
#' @export
#' @examples
#' \dontrun{
#'   data(crest_ex)
#'   data(crest_ex_pse)
#'   data(crest_ex_selection)
#'   recons <- crest(
#'     df = crest_ex, pse = crest_ex_pse, taxaType = 0,
#'     climate = c("bio1", "bio12"), bin_width = c(2, 20),
#'     shape = c("normal", "lognormal"),
#'     selectedTaxa = crest_ex_selection, dbname = "crest_example"
#'   )
#'   recons <- loo(recons)
#'   plot_loo(recons, yax_incr=c(0.5, 50), bar_width=0.8,
#'            col_pos=c('blue','cornflowerblue'), col_neg=c('red', 'goldenrod3'))
#' }
#'
plot_loo <- function( x, optima=TRUE, climate=x$parameters$climate,
                      save=FALSE, filename='Diagram_loo.pdf',
                      width=3.54, height= 9,
                      yax_incr = NA, bar_width=1,
                      xlim=NA, tickAtSample=FALSE,
                      col_pos = 'black', col_neg='grey80', title=NA ) {

    if (methods::is(x)[1] == 'crestObj') {

        if(! 'loo' %in% names(x$reconstructions[[climate[1]]])) {
            cat('ERROR: No leave-one-out data available. Please run the loo() function first.\n')
            return(invisible())
        }

        par_usr <- list()

        var_to_plot <- ifelse(optima, 1, 2)

        filename <- strsplit(filename, '.pdf')[[1]]

        if (length(col_pos) != length(climate)) col_pos = base::rep_len(col_pos,length(climate))
        if (length(col_neg) != length(climate)) col_neg = base::rep_len(col_neg,length(climate))
        if (length(yax_incr) != length(climate)) yax_incr = base::rep_len(yax_incr,length(climate))
        if ((!is.na(unique(title)[1])) & (length(title) != length(climate))) title = base::rep_len(title,length(climate))

        names(col_pos) = names(col_neg) = names(yax_incr) = climate
        if(!is.na(title[1])) names(title) = climate

        if(!save) {
            par_usr <- graphics::par(no.readonly = TRUE)
            graphics::par(mfrow=c(1, length(climate)))
        }

        for( clim in climate ) {
            df <- list()
            if(is.numeric(x$inputs$x)) {
                df[[x$inputs$x.name]] <- x$inputs$x
            } else {
                cat('WARNING: The plotting function does not yet deal with non-numerical x values. Replacing x values by integers.\n')
                df[[x$inputs$x.name]] <- 1:length(x$inputs$x)
            }
            loo_na <- rep(0, length(x$inputs$x))
            for( tax in names(x$reconstructions[[clim]]$loo)) {
                if(is.na(x$reconstructions[[clim]]$loo[[tax]][1])) {
                    df[[tax]] <- loo_na
                } else {
                    df[[tax]] <- x$reconstructions[[clim]]$loo[[tax]][, var_to_plot]
                }
            }
            df <- do.call(cbind, df)

            xlim <- range(df[, 1])
            bar_width2 <- bar_width
            yax_incr2 <- yax_incr[clim]
            if(is.na(unique(yax_incr)[1])) yax_incr2 <- round(max(abs(df[, -1])))/10
            if(is.na(bar_width)) bar_width2 <- round(diff(xlim) / nrow(df))
            if(is.na(unique(title)[1])) {
                title2 <- accClimateVariables(clim)[3]
            } else {
                title2 <- title[clim]
            }

            if (yax_incr2 == 0) yax_incr2 <- 1

            plot_diagram(df, bars=TRUE,
                       save=save, filename=paste0(filename,'_',clim,'.pdf'),
                       width=width, height=height,
                       yax_incr=yax_incr2, bar_width=bar_width2, xlim=xlim,
                       tickAtSample=tickAtSample,
                       col_pos=col_pos[clim], col_neg=col_neg[clim],
                       title=title2)
        }
    } else {
        xlim <- range(x[, 1])
        bar_width2 <- bar_width
        yax_incr2 <- yax_incr
        if(is.na(yax_incr)) yax_incr2 <- round(max(abs(x[, -1]))/10)
        if(is.na(bar_width)) bar_width2 <- round(diff(xlim) / nrow(x))

        plot_diagram(x, bars=TRUE,
                     save=save, filename=paste0(filename,'_',clim,'.pdf'),
                     width=width, height=height,
                     yax_incr=yax_incr2, bar_width=bar_width2, xlim=xlim,
                     tickAtSample=tickAtSample, col_pos=col_pos, col_neg=col_neg,
                     title=title)
    }
    if(!save)  graphics::par(par_usr)
    invisible()
}
