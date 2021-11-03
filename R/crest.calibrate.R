#' Fit the species and proxy \code{pdfs}
#'
#' This function fits the climate response of the selected taxa to the selected
#' climate variables.
#'
#' @inheritParams crestObj
#' @inheritParams crest
#' @param x A \code{\link{crestObj}} produced by the \code{\link{crest.get_modern_data}} function.
#' @return A \code{\link{crestObj}} object containing the spatial distributions
#'         and the climate space.
#' @export
#' @examples
#' \dontrun{
#'   data(crest_ex_pse)
#'   data(crest_ex_selection)
#'   data(crest_ex)
#'   x <- crest.get_modern_data( df = crest_ex,
#'     pse = crest_ex_pse, taxaType = 0,
#'     climate = c("bio1", "bio12"),
#'     selectedTaxa = crest_ex_selection, dbname = "crest_example",
#'     verbose = FALSE
#'   )
#'   x <- crest.calibrate(x,
#'     geoWeighting = TRUE, climateSpaceWeighting = TRUE,
#'     bin_width = c(2, 50), shape = c("normal", "lognormal"),
#'     verbose = FALSE
#'   )
#' }
#'
crest.calibrate <- function(x,
                            bin_width = x$parameters$bin_width,
                            shape = x$parameters$shape,
                            npoints = x$parameters$npoints,
                            geoWeighting = x$parameters$geoWeighting,
                            climateSpaceWeighting = x$parameters$climateSpaceWeighting,
                            verbose=TRUE) {

    if(base::missing(x)) x

    if(verbose) cat('\n## Calibration of the taxon-climate relationships\n')

    if(verbose) cat('  <> Preparing data ........................ ')
    x$parameters$npoints <- npoints
    x$parameters$geoWeighting <- geoWeighting
    x$parameters$climateSpaceWeighting <- climateSpaceWeighting

    if (unique(is.na(bin_width)) | length(bin_width) != length(x$parameters$climate)) {
        cat("[FAILED]\n\n")
        stop(paste0("'bin_width' should be of the same size than 'climate', i.e. ",length(x$parameters$climate)," elements.\n\n"))
    } else if (!unique(is.na(bin_width))) {
        bin_width <- as.data.frame(bin_width)
        rownames(bin_width) <- x$parameters$climate
    }
    x$parameters$bin_width <- bin_width

    if (unique(is.na(shape)) | length(shape) != length(x$parameters$climate)) {
        cat("[FAILED]\n\n")
        stop(paste0("'shape' should be of the same size than 'climate', i.e. ",length(x$parameters$climate)," elements.\n\n"))
    } else if (FALSE %in% (shape %in% c('normal', 'lognormal'))) {
        cat("[FAILED]\n\n")
        stop(paste0("'shape' should be either 'normal' or 'lognormal'.\n"))
    } else if (!unique(is.na(shape))) {
        shape <- as.data.frame(shape)
        rownames(shape) <- x$parameters$climate
    }
    x$parameters$shape <- shape

    if(verbose) cat('[OK]\n  <> Calibrating climate space ............. ')
    ccs <- list()
    x$modelling$xrange <- list()
    for (clim in x$parameters$climate) {
        ccs[[clim]] <- calib_clim_space(x$modelling$climate_space[, clim], x$parameters$bin_width[clim, ])
        x$modelling$xrange[[clim]] <- fit_xrange(ccs[[clim]], x$parameters$shape[clim, ], x$parameters$bin_width[clim, ], x$parameters$npoints)
    }
    x$modelling$ccs <- ccs

    resol <- sort(unique(diff(sort(unique(x$modelling$climate_space[,1])))))[1] / 2.0
    if(x$parameters$xmn == -180) {
        x$parameters$xmn <- min(x$modelling$climate_space[, 1]) - resol
    }
    if(x$parameters$xmx == 180) {
        x$parameters$xmx <- max(x$modelling$climate_space[, 1]) + resol
    }
    if(x$parameters$ymn == -90) {
        x$parameters$ymn <- min(x$modelling$climate_space[, 2]) - resol
    }
    if(x$parameters$ymx == 90) {
        x$parameters$ymx <- max(x$modelling$climate_space[, 2]) + resol
    }


    if(verbose) {
      cat('[OK]\n  <> Fitting relationships ................. \r')
    }
    pbi <- 100
    pdfs <- list()
    for (tax in x$inputs$taxa.name) {
        if (sum(x$inputs$selectedTaxa[tax, x$parameters$climate]>=0) > 0) {
            pdfs[[tax]] <- list()
            for (clim in x$parameters$climate) {
                if (sum(x$inputs$selectedTaxa[tax, clim]>=0) >= 0) {
                    pdfs[[tax]][[clim]] <- list()
                    tmp <- x$modelling$xrange[[clim]]
                    pdfpol <- rep(0, npoints)
                    for (sp in unique(x$modelling$distributions[[tax]][, "taxonid"])) {
                        w <- which(x$modelling$distributions[[tax]][, "taxonid"] == sp)
                        tmp <- cbind(
                          tmp,
                          fit_pdfsp(
                            climate = x$modelling$distributions[[tax]][w, clim],
                            ccs = x$modelling$ccs[[clim]],
                            bin_width = x$parameters$bin_width[clim, ],
                            shape = x$parameters$shape[clim, ],
                            xrange = x$modelling$xrange[[clim]],
                            use_ccs = x$parameters$climateSpaceWeighting
                          )
                        )
                        pdfpol <- pdfpol + tmp[, ncol(tmp)] * ifelse(x$parameters$geoWeighting,
                          length(w),
                          1
                        )
                    }
                    pdfs[[tax]][[clim]][["pdfsp"]] <- as.data.frame(tmp[, -1])
                    pdfs[[tax]][[clim]][["pdfpol"]] <- pdfpol / ifelse(x$parameters$geoWeighting,
                      nrow(x$modelling$distributions[[tax]]),
                      length(unique(x$modelling$distributions[[tax]][, "taxonid"]))
                    )
                    pdfs[[tax]][[clim]][["pdfpol_log"]] <- log(pdfs[[tax]][[clim]][["pdfpol"]])
                } else {
                    pdfs[[tax]][[clim]] <- NA
                }
            }
        } else {
            pdfs[[tax]] <- NA
        }
        if(verbose) {
            cat(paste0('  <> Fitting relationships ................. ', stringr::str_pad(paste0(round(pbi / length(x$modelling$distributions)),'%\r'), width=4, side='left')))
            utils::flush.console()
        }
        pbi <- pbi + 100
    }
    x$modelling$pdfs <- pdfs

    if(verbose) {
        cat('  <> Fitting relationships ................. [OK]\n')
        cat(paste0('## Taxa-climate relationships fitted.\n'))
    }
    x$misc$stage <- 'PDFs_fitted'
    x
}
