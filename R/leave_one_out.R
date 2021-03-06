#' Performs the leave-one-out analysis
#'
#' Repeat the repetation by removing one taxon at a time.
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
#' reconstr$reconstructions$bio12$loo
#' plot_loo(reconstr)
#'
loo <- function(x, verbose=TRUE) {
    if(verbose) cat('\n## Prepping data for LOO reconstructions\n')

    if(verbose) cat('  <> Checking data ......................... ')

    if(verbose) cat('[OK]\n  <> Checking taxa ................... ......')
    taxa_list <- rownames(x$inputs$selectedTaxa)[apply(as.data.frame(x$inputs$selectedTaxa[, x$parameters$climate]), 1, sum) > 0]
    estimated_time <- x$misc$reconstruction_time * length(taxa_list)
    if(verbose) cat(paste0('[OK]\n  *i Estimated time for the LOO reconstructions: ', estimated_time %/% 60, 'min ', round(estimated_time %% 60, 2), 's.\n'))
    if(verbose) cat('  <> LOO reconstructions ...................   0%\r')
    pbi <- 100
    time0 <- proc.time()
    for(clim in x$parameters$climate) {
        x$reconstructions[[clim]][['loo']] = list()
    }
    recons_tmp <- x
    for(tax in taxa_list) {
        recons_tmp$inputs$selectedTaxa <- x$inputs$selectedTaxa
        recons_tmp <- excludeTaxa(recons_tmp, tax, x$parameters$climate)
        recons_tmp <- crest.reconstruct(recons_tmp,
                                        presenceThreshold = x$parameters$presenceThreshold,
                                        taxWeight = x$parameters$taxWeight,
                                        skip_for_loo = TRUE, verbose=FALSE
                                      )
        for(clim in x$parameters$climate) {
            if(x$inputs$selectedTaxa[tax, clim] > 0) {
                x$reconstructions[[clim]][['loo']][[tax]] <- cbind('optima' = recons_tmp$reconstructions[[clim]]$optima[, 2] - x$reconstructions[[clim]]$optima[, 2],
                                                                   'mean'   = recons_tmp$reconstructions[[clim]]$optima[, 3] - x$reconstructions[[clim]]$optima[, 3]
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
    x
}
