#' Connect to the gbif4crest database
#'
#' Connect to the gbif4crest_02 database by accessing the server on Amazon.
#'
#' @param x a crestObj produced by the crest.reconstruct() or crest() functions.
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
loo <- function(x) {
  for(clim in x$parameters$climate) {
    x$reconstructions[[clim]][['loo']] = list()
  }
  for(tax in rownames(x$inputs$selectedTaxa)[apply(x$inputs$selectedTaxa[, x$parameters$climate], 1, function(y) return(sum(as.numeric(y)))) > 0]) {
    recons_tmp <- x
    recons_tmp$inputs$selectedTaxa[tax, x$parameters$climate] <- rep(0, length(x$parameters$climate))
    df_tmp = cbind( x$inputs$x, x$inputs$df )
    colnames(df_tmp) <- c(x$inputs$x.name, x$inputs$taxa.name)
    recons_tmp <- crest.reconstruct(recons_tmp,
                                    df = df_tmp,
                                    presenceThreshold = x$parameters$presenceThreshold,
                                    taxWeight = x$parameters$taxWeight
                                  )
    for(clim in x$parameters$climate) {
      if(as.numeric(x$inputs$selectedTaxa[tax, clim]) > 0) {
        x$reconstructions[[clim]][['loo']][[tax]] <- recons_tmp$reconstructions[[clim]][['optima']][, 2]
        x$reconstructions[[clim]][['loo']][[tax]] <- x$reconstructions[[clim]][['loo']][[tax]] - x$reconstructions[[clim]]$optima[,2]
        x$reconstructions[[clim]][['loo']][[tax]][is.na(x$reconstructions[[clim]][['loo']][[tax]])] <- 0
      } else {
        x$reconstructions[[clim]][['loo']][[tax]] <- NA
      }
    }
  }
  x
}
