#' Plot the results of the leave-one-out analysis.
#'
#' Plot the results of the leave-one-out analysis.
#'
#' @inheritParams plot_diagram
#' @param optima A boolean to indicate whether to plot the optimum (\code{TRUE})
#'        or the mean (\code{FALSE}) estimates.
#' @param climate Climate variables to be used to generate the plot. By default
#'        all the variables are included.
#' @param taxanames A list of taxa to use for the plot (default is all the
#'        recorded taxa).
#' @param filename An absolute or relative path that indicates where the diagram
#'        should be saved. Also used to specify the name of the file. Default:
#'        the file is saved in the working directory under the name
#'        \code{'Diagram_loo_climate.pdf'}.
#' @param yax_incr Graphical parameters describing the increment size on the
#'        y-axis (default 5).
#' @param bar_width Width of the bars of the barplot (default 1).
#' @param sort A string to sort the order of the taxa from the highest to lowest
#'        anomalies (sort='incr') or from the lowest to highest (sort='decr').
#'        Use the default value \code{NA} to keep the taxa unsorted.
#' @param filter A threshold value that determines the mean absolute anomaly
#'        value required for the taxon to be plotted (default 0 means that all
#'        taxa are plotted)
#' @param col_pos Graphical parameter for the barplot. Colour of all the
#'        positive values (default black).
#' @param col_neg Graphical parameter for the barplot. Colour of all the
#'        negative values (default grey80).
#' @return When used with a crestObj, it returns the average leave-one-out
#'         values for each selected taxa
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
#' loo_vals <- plot_loo(reconstr, yax_incr=c(0.5, 50), bar_width=0.8,
#'                      col_pos=c('blue','cornflowerblue'),
#'                      col_neg=c('red', 'goldenrod3'))
#'
plot_loo <- function( x, optima=TRUE,
                      climate=x$parameters$climate[unlist(lapply(x$reconstructions, function(y) return('loo' %in% names(y))))],
                      taxanames=x$inputs$taxa.name,
                      save=FALSE, filename='Diagram_loo.pdf',
                      as.png = FALSE, png.res=300,
                      width=3.54, height= 9,
                      yax_incr = NA, bar_width = diff(range(x$inputs$x))/length(x$inputs$x),
                      xlim=NA, tickAtSample=FALSE,
                      sort=NA, filter = 0,
                      col_pos = 'black', col_neg='grey80', title=NA ) {

    if(base::missing(x)) x

    if (is.crestObj(x)) {

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

        if(! 'loo' %in% names(x$reconstructions[[climate[1]]])) {
            stop('No leave-one-out data available. Run the loo() function first.\n')
        }

        var_to_plot <- ifelse(optima, 1, 2)

        filename <- base::strsplit(filename, '.pdf')[[1]]

        if (length(col_pos) != length(climate)) col_pos = base::rep_len(col_pos,length(climate))
        if (length(col_neg) != length(climate)) col_neg = base::rep_len(col_neg,length(climate))
        if (length(yax_incr) != length(climate)) yax_incr = base::rep_len(yax_incr,length(climate))
        if ((!is.na(unique(title)[1])) & (length(title) != length(climate))) title = base::rep_len(title,length(climate))

        names(col_pos) = names(col_neg) = names(yax_incr) = climate
        if(!is.na(title[1])) names(title) = climate


        if(!save) {
            par_usr <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(par_usr))
            graphics::par(mfrow=c(1, length(climate)))
        }

        rs <- list()

        for( clim in climate ) {
            df <- list()
            if(is.numeric(x$inputs$x)) {
                df[[x$inputs$x.name]] <- x$inputs$x
            } else {
                warning("The plotting function is not adapted to non-numeric x values. The sample names were replaced by numeric indexes.")
                df[[x$inputs$x.name]] <- 1:length(x$inputs$x)
            }
            loo_na <- rep(0, length(x$inputs$x))
            for( tax in taxanames ) {
                if(tax %in% names(x$reconstructions[[clim]]$loo)) {
                    if(is.na(x$reconstructions[[clim]]$loo[[tax]][1])) {
                        df[[tax]] <- loo_na
                    } else {
                        df[[tax]] <- x$reconstructions[[clim]]$loo[[tax]][, var_to_plot]
                    }
                } else {
                    df[[tax]] <- loo_na
                }
            }
            df <- do.call(cbind, df)
            if(is.na(xlim)[1]) {
                xlim <- range(df[, 1])
            } else {
                df <- df[which(df[,1] >= xlim[1] & df[,1] <= xlim[2]), ]
            }
            bar_width2 <- bar_width
            yax_incr2 <- yax_incr[clim]
            if(is.na(unique(yax_incr)[1])) yax_incr2 <- round(max(abs(df[, -1])))/10
            if(is.na(bar_width)) bar_width2 <- round(diff(xlim) / nrow(df))
            if(is.na(unique(title)[1])) {
                title2 <- paste('Leave-one-out anomalies for', accClimateVariables(clim)[3], sep='\n')
            } else {
                title2 <- paste('Leave-one-out anomalies for', title[clim], sep='\n')
            }

            if (yax_incr2 == 0) yax_incr2 <- x$parameters$bin_width[clim, ] / 10

            if(!is.na(sort)) {
                df <- df[, c(1, order(apply(df[, -1], 2, function(x) mean(x[abs(x)>0])), decreasing = ifelse(sort=='incr', FALSE, TRUE)) + 1)]
            }

            w <- which(apply(df[, -1], 2, function(x) mean(abs(x)[abs(x)>=0])) >= filter)
            if(length(w) == 0) {
                warning("No taxa remain after filtering. Adjust the filter value to include more taxa.")
            } else {
                df <- df[, c(1, w + 1)]
                plot_diagram(df, bars=TRUE,
                           save=save, filename=paste0(strsplit(filename, ifelse(as.png, '.png', '.pdf'))[[1]],'_',clim,ifelse(as.png, '.png', '.pdf')),
                           width=width, height=height, as.png=as.png, png.res=png.res,
                           yax_incr=yax_incr2, bar_width=bar_width2, xlim=xlim,
                           tickAtSample=tickAtSample,
                           col_pos=col_pos[clim], col_neg=col_neg[clim],
                           title=title2, src='loo')

                rs[[clim]] <- sort(unlist(lapply(x$reconstructions[[clim]]$loo, function(y) if(!unique(as.vector(is.na(y)))){return(mean(y[, var_to_plot][abs(y[, var_to_plot])>0]))})))
            }
        }
        return(invisible(rs))

    } else {
        xlim <- range(x[, 1])
        bar_width2 <- bar_width
        yax_incr2 <- yax_incr
        if(is.na(yax_incr)) yax_incr2 <- round(max(abs(x[, -1]))/10)
        if(is.na(bar_width)) bar_width2 <- round(diff(xlim) / nrow(x))

        plot_diagram(x, bars=TRUE,
                     save=save, filename=paste0(strsplit(filename, ifelse(as.png, '.png', '.pdf'))[[1]],'_',clim,ifelse(as.png, '.png', '.pdf')),
                     width=width, height=height, as.png=as.png, png.res=png.res,
                     yax_incr=yax_incr2, bar_width=bar_width2, xlim=xlim,
                     tickAtSample=tickAtSample, col_pos=col_pos, col_neg=col_neg,
                     title=title)
    }

    invisible()
}
