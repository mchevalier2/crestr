#' Connect to the gbif4crest database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @param x a crestObj produced by the crest.reconstruct() or crest() functions.
#' @param verbose A boolean to print non-essential comments on the terminal (default TRUE).
#' @return A crest() object containing the reconstructions and all the associated data.
#' @export
#' @examples
#' data(crest_ex)
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' recons <- crest(
#'   df = crest_ex, pse = crest_ex_pse, taxaType = 0,
#'   climate = c("bio1", "bio12"), bin_width = c(2, 20),
#'   shape = c("normal", "lognormal"),
#'   selectedTaxa = crest_ex_selection, dbname = "crest_example"
#' )
#' recons <- loo(recons)
#' recons$reconstructions$bio12$loo
#' plot_loo(recons)
#'
loo <- function(x, verbose=TRUE) {
  if(verbose) cat('\n## Prepping data for LOO reconstructions\n')

  if(verbose) cat('  <> Checking data ......................... ')

  if(verbose) cat('[OK]\n  <> Checking taxa ................... ......')
  taxa_list <- rownames(x$inputs$selectedTaxa)[apply(x$inputs$selectedTaxa[, x$parameters$climate], 1, function(y) return(sum(as.numeric(y)))) > 0]
  estimated_time <- x$misc$reconstruction_time * length(taxa_list)
  if(verbose) cat(paste0('[OK]\n  *i Estimated time for the LOO reconstructions: ', estimated_time %/% 60, 'min ', round(estimated_time %% 60, 2), 's.\n'))
  if(verbose) cat('  <> LOO reconstructions ...................   0%\r')
  pbi <- 100
  time0 <- proc.time()
  for(clim in x$parameters$climate) {
    x$reconstructions[[clim]][['loo']] = list()
  }
  recons_tmp <- x
  df_tmp = cbind( x$inputs$x, x$inputs$df )  ;  colnames(df_tmp) <- c(x$inputs$x.name, x$inputs$taxa.name)
  for(tax in taxa_list) {
    recons_tmp$inputs$selectedTaxa <- x$inputs$selectedTaxa
    recons_tmp$inputs$selectedTaxa[tax, x$parameters$climate] <- rep(0, length(x$parameters$climate))
    recons_tmp <- crest.reconstruct(recons_tmp,
                                    df = df_tmp,
                                    presenceThreshold = x$parameters$presenceThreshold,
                                    taxWeight = x$parameters$taxWeight,
                                    skip_for_loo = TRUE, verbose=FALSE
                                  )
    for(clim in x$parameters$climate) {
      if(as.numeric(x$inputs$selectedTaxa[tax, clim]) > 0) {
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
