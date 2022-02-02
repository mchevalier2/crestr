#' Reconstruct climate from fossil data
#'
#' This function fits the climate response of the selected taxa to the selected
#' climate variables.
#'
#' @inheritParams crestObj
#' @inheritParams crest
#' @param x A \code{\link{crestObj}} produced by the
#'        \code{\link{crest.calibrate}} function.
#' @param skip_for_loo A boolean that tells the \code{\link{loo}} function to
#'        skip parts and fasten the process. Not for users, always leave to
#'        \code{FALSE}.
#' @return A \code{\link{crestObj}} object containing the reconstructions and
#'         all the associated data.
#' @export
#' @examples
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' data(crest_ex)
#' \dontrun{
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
#'   x <- crest.reconstruct(x,
#'     verbose = FALSE)
#'   plot(x)
#' }
#'
crest.reconstruct <- function(x,
                              presenceThreshold = 0,
                              taxWeight = "normalisation",
                              uncertainties = c(0.5, 0.95),
                              skip_for_loo = FALSE, verbose=TRUE) {

    if(base::missing(x)) x

    if(verbose) cat('\n## Last data checks and reconstruction\n')
    if(! skip_for_loo) {

        if(verbose) cat('  <> Checking data ......................... ')
        if (!is.data.frame(x$inputs$df)) {
            cat("[FAILED]\n\n")
            stop(paste0("Fossil data not provided. Check the 'df' parameter in crest.get_modern_data().\n"))
        }

        if(verbose) cat('[OK]\n  <> Checking taxa ......................... ')
        x$parameters$taxWeight <- taxWeight
        x$parameters$presenceThreshold <- presenceThreshold

        if(verbose) cat('[OK]\n  <> Defining taxa weights ................. ')

        if (tolower(x$parameters$taxWeight) == "normalisation") {
            taxWeight <- normalise(x$inputs$df, col2convert = 1:ncol(x$inputs$df))
        } else {
            if (tolower(x$parameters$taxWeight) == "presence/absence") {
                taxWeight <- convert2presenceAbsence(x$inputs$df, threshold = x$parameters$presenceThreshold, col2convert = 1:ncol(x$inputs$df))
            } else {
                if (tolower(x$parameters$taxWeight) == "percentages") {
                    taxWeight <- convert2percentages(x$inputs$df, col2convert = 1:ncol(x$inputs$df))
                } else {
                    taxWeight <- x$inputs$df
                }
            }
        }
        taxWeight[is.na(taxWeight)] <- 0
        colnames(taxWeight) <- colnames(x$inputs$df)
        rownames(taxWeight) <- rownames(x$inputs$df)
        x$modelling$weights <- taxWeight

        if(verbose) {
          cat('[OK]\n  <> Reconstructing ........................ \r')
        }
        pbi <- 100
    }

    ptm <- proc.time()
    reconstructions <- list()
    for (clim in x$parameters$climate) {
        if (sum(x$inputs$selectedTaxa[, clim]>0) > 0) {
            reconstructions[[clim]][["likelihood"]] <- matrix(rep(0, x$parameters$npoints * nrow(x$inputs$df)), ncol = x$parameters$npoints)
            reconstructions[[clim]][["uncertainties"]] <- matrix(rep(0, 2 * length(x$parameters$uncertainties) * nrow(x$inputs$df)), nrow = nrow(x$inputs$df))
            reconstructions[[clim]][["optima"]] <- matrix(rep(NA, 2 * nrow(x$inputs$df)), nrow = nrow(x$inputs$df))
            for (s in 1:nrow(x$inputs$df)) {
                if (is.na(x$modelling$weights[s, names(x$modelling$pdfs)[1]])) {
                    reconstructions[[clim]][["likelihood"]][s, ] <- rep(NA, x$parameters$npoints)
                    reconstructions[[clim]][["uncertainties"]][s, ] <- rep(NA, 2 * length(x$parameters$uncertainties))
                } else if (sum((x$inputs$selectedTaxa[x$inputs$taxa.name, clim]>0) * ifelse(x$modelling$weights[s, x$inputs$taxa.name]>0, 1, 0)) == 0) {
                    reconstructions[[clim]][["likelihood"]][s, ] <- rep(NA, x$parameters$npoints)
                    reconstructions[[clim]][["uncertainties"]][s, ] <- rep(NA, 2 * length(x$parameters$uncertainties))
                } else {
                    norm_factor <- 0
                    for (tax in x$inputs$taxa.name) {
                        if (x$modelling$weights[s, tax] > 0 & x$inputs$selectedTaxa[tax, clim] > 0) {
                            norm_factor <- norm_factor + x$modelling$weights[s, tax]
                            reconstructions[[clim]][["likelihood"]][s, ] <-
                              reconstructions[[clim]][["likelihood"]][s, ] +
                              x$modelling$pdfs[[tax]][[clim]][["pdfpol_log"]] * x$modelling$weights[s, tax]
                        }
                    }
                    reconstructions[[clim]][["likelihood"]][s, ] <-
                      reconstructions[[clim]][["likelihood"]][s, ] * (1 / norm_factor)
                    reconstructions[[clim]][["likelihood"]][s, ] <- exp(reconstructions[[clim]][["likelihood"]][s, ])
                    reconstructions[[clim]][["likelihood"]][s, ] <-
                      reconstructions[[clim]][["likelihood"]][s, ] /
                        (sum(reconstructions[[clim]][["likelihood"]][s, ]) *
                          (x$modelling$xrange[[clim]][2] - x$modelling$xrange[[clim]][1])
                        )
                    reconstructions[[clim]][["optima"]][s, 1] <- x$modelling$xrange[[clim]][which.max(reconstructions[[clim]][["likelihood"]][s, ])]
                    reconstructions[[clim]][["optima"]][s, 2] <- sum(x$modelling$xrange[[clim]] * reconstructions[[clim]][["likelihood"]][s, ]) / sum(reconstructions[[clim]][["likelihood"]][s, ])

                    oo <- order(reconstructions[[clim]][["likelihood"]][s, ], decreasing=TRUE)
                    tmp2 <- reconstructions[[clim]][["likelihood"]][s, ][oo]
                    tmp1 <- x$modelling$xrange[[clim]][oo]
                    oo <- order(tmp1)
                    pdfter <- cumsum(tmp2 / sum(tmp2))[oo]
                    tmp1 <- tmp1[oo]

                    err <- c()
                    for(e in x$parameters$uncertainties) {
                        w <- which(pdfter <= e)
                        err <- c(err, tmp1[w[1]], tmp1[w[length(w)]])
                    }
                    reconstructions[[clim]][["uncertainties"]][s, ] <- err
                }
                if(! skip_for_loo) {
                    if(verbose) {
                        cat(paste0('  <> Reconstructing ........................ ', stringr::str_pad(paste0(round(pbi / (length(x$parameters$climate)*nrow(x$inputs$df))),'%\r'), width=4, side='left')))
                        utils::flush.console()
                    }
                    pbi <- pbi + 100
                }
            }
            reconstructions[[clim]][["likelihood"]] <- rbind(x$modelling$xrange[[clim]], reconstructions[[clim]][["likelihood"]])
            reconstructions[[clim]][["uncertainties"]] <- cbind('x'=x$inputs$x, reconstructions[[clim]][["uncertainties"]])
            reconstructions[[clim]][["optima"]] <- data.frame('x'=x$inputs$x,
                                                              'optima'=reconstructions[[clim]][["optima"]][, 1],
                                                              'mean'=reconstructions[[clim]][["optima"]][, 2]
                                                              )
            colnames(reconstructions[[clim]][["uncertainties"]])[1] <- colnames(reconstructions[[clim]][["optima"]])[1] <- x$inputs$x.name
            colnames(reconstructions[[clim]][["uncertainties"]])[-1] <- paste(rep(x$parameters$uncertainties,each=2), c("inf", "sup"),sep='_')

        } else {
            reconstructions[[clim]] <- NA
        }
    }
    x$misc$reconstruction_time <- (proc.time() - ptm)[3]
    if(verbose) {
        cat('  <> Reconstructing ........................ [OK]\n')
        cat(paste0('## Reconstruction completed in ', x$misc$reconstruction_time %/% 60, 'min ', round(x$misc$reconstruction_time %% 60, 2), 's.\n\n'))
    }
    gc()
    x$reconstructions <- reconstructions
    x$misc$stage <- 'climate_reconstructed'
    x
}
