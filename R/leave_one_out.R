#' Performs the leave-one-out analysis
#'
#' Repeat the reconstructions by removing one taxon at a time.
#'
#' @param x a \code{\link{crestObj}} produced by the
#'        \code{\link{crest.reconstruct}} or \code{\link{crest}} functions.
#' @param verbose A boolean to print non-essential comments on the terminal
#'        (default \code{TRUE}).
#' @return A \code{\link{crestObj}} object containing the reconstructions and
#'         all the associated data.
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
#' lapply(reconstr$reconstructions$bio12$loo, head)
#' plot_loo(reconstr)
#'
loo <- function(x, verbose=TRUE) {
    if(base::missing(x)) x

    if(!is.crestObj(x)) {
        cat('\nx should be a crestObj.\n\n')
        return(invisible(NA))
    }

    if(verbose) cat('\n## Prepping data for LOO reconstructions\n')

    if(verbose) cat('  <> Checking data ......................... ')

    if(verbose) cat('[OK]\n  <> Checking taxa ................... ......')
    taxa_list <- rownames(x$inputs$selectedTaxa)[apply(as.data.frame(x$inputs$selectedTaxa[, x$parameters$climate]), 1, sum) > 0]
    estimated_time <- x$misc$reconstruction_time / nrow(x$inputs$df) * sum(ifelse(x$modelling$weights > 0, 1, 0))
    if(verbose) cat(paste0('[OK]\n  *i Estimated time for the LOO reconstructions: ', estimated_time %/% 60, 'min ', round(estimated_time %% 60, 2), 's.\n'))
    if(verbose) cat('  <> LOO reconstructions ...................   0%\r')
    pbi <- 100
    time0 <- proc.time()
    for(clim in x$parameters$climate) {
        x$reconstructions[[clim]][['loo']] = list()
    }
    recons_tmp <- x
    for(tax in taxa_list) {
        #recons_tmp$inputs$selectedTaxa <- x$inputs$selectedTaxa
        #recons_tmp <- excludeTaxa(recons_tmp, tax, x$parameters$climate)
        recons_tmp$inputs$df <- x$modelling$weights ## We do not recalculate the weight matrix. We assign it as the 'default' data
        recons_tmp$inputs$x <- x$inputs$x[recons_tmp$inputs$df[, tax] > 0 ]# Only re-runnig for the samples that contain the taxon
        recons_tmp$inputs$df <- recons_tmp$inputs$df[recons_tmp$inputs$df[, tax] > 0, ]
        recons_tmp$inputs$df[, tax] <- 0 # Excluding the taxon from the recon
        recons_tmp$modelling$weights <- recons_tmp$inputs$df # Seeting df as the weight matrix since this is not updateed with skip_for_loo=TRUE
        recons_tmp <- crest.reconstruct(recons_tmp,
                                        presenceThreshold = 0,
                                        taxWeight = 'originalData',
                                        skip_for_loo = TRUE, verbose=FALSE
                                      )
        for(clim in x$parameters$climate) {
            if(x$inputs$selectedTaxa[tax, clim] > 0) {
                tmp_opt <- tmp_mean <- rep(NA, nrow(x$reconstructions[[clim]]$optima))
                tmp_opt[x$modelling$weights[, tax] > 0] <- recons_tmp$reconstructions[[clim]]$optima[, 2]
                tmp_mean[x$modelling$weights[, tax] > 0] <- recons_tmp$reconstructions[[clim]]$optima[, 3]
                x$reconstructions[[clim]][['loo']][[tax]] <- cbind('optima' = tmp_opt - x$reconstructions[[clim]]$optima[, 2],
                                                                   'mean'   = tmp_mean - x$reconstructions[[clim]]$optima[, 3]
                                                                  )
                x$reconstructions[[clim]][['loo']][[tax]][is.na(x$reconstructions[[clim]][['loo']][[tax]])] <- 0
            } else {
                x$reconstructions[[clim]][['loo']][[tax]] <- NA
            }
        }

        if(verbose) {
            cat(paste0('  <> LOO reconstructions ................... ', stringr::str_pad(paste0(round(pbi / length(taxa_list)),'%\r'), width=4, side='left')))
            utils::flush.console()
        }
        pbi <- pbi + 100
    }
    time1 <- (proc.time() - time0)[3]
    if(verbose) {
        cat('  <> LOO reconstructions ................... [OK]\n')
        cat(paste0('## LOO reconstruction completed in ', time1 %/% 60, 'min ', round(time1 %% 60, 2), 's.\n\n'))
    }
    x$misc$stage <- 'leave_one_out'
    x
}
