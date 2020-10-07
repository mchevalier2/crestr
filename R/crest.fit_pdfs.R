#' Fit the species and proxy pdfs
#'
#' This function fits the climate response of the selected taxa to the selected
#' climate variables.
#'
#' @inheritParams crestObj
#' @param x a crestObj produced by the crest.climate_space function.
#' @return A crest() object containing the spatial distributions and the climate space
#' @export
#' @examples
#' \dontrun{
#' data(crest_ex)
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' x <- crest.get_distributions(
#'   taxa.name = colnames(crest_ex)[-1],
#'   pse = crest_ex_pse, taxaType = 0,
#'   climate = c("bio1", "bio12"),
#'   selectedTaxa = crest_ex_selection, dbname = "crest_example"
#' )
#' x <- crest.climate_space(x, dbname = "crest_example")
#' x <- crest.fit_pdfs(x,
#'   geoWeighting = TRUE, climateSpaceWeighting = TRUE,
#'   bin_width = c(2, 20), shape = c("normal", "lognormal")
#' )
#' }
#'
crest.fit_pdfs <- function(x,
                           bin_width = rep(1, length(x$parameters$climate)),
                           shape = rep("normal", length(x$parameters$climate)),
                           npoints = 500,
                           geoWeighting = TRUE,
                           climateSpaceWeighting = TRUE) {
  x$parameters$npoints <- npoints
  x$parameters$geoWeighting <- geoWeighting
  x$parameters$climateSpaceWeighting <- climateSpaceWeighting

  if (!unique(is.na(bin_width))) {
    bin_width <- as.data.frame(bin_width)
    rownames(bin_width) <- x$parameters$climate
  }
  x$parameters$bin_width <- bin_width

  if (!unique(is.na(shape))) {
    shape <- as.data.frame(shape)
    rownames(shape) <- x$parameters$climate
  }
  x$parameters$shape <- shape

  ccs <- list()
  x$modelling$xrange <- list()
  for (clim in x$parameters$climate) {
    ccs[[clim]] <- calib_clim_space(x$modelling$climate_space[, clim], x$parameters$bin_width[clim, ])
    x$modelling$xrange[[clim]] <- fit_xrange(ccs[[clim]], x$parameters$shape[clim, ], x$parameters$bin_width[clim, ], x$parameters$npoints)
  }

  pdfs <- list()
  for (tax in names(x$modelling$distributions)) {
    if (sum(as.numeric(x$inputs$selectedTaxa[tax, x$parameters$climate])) > 0) {
      pdfs[[tax]] <- list()
      for (clim in x$parameters$climate) {
        if (sum(as.numeric(x$inputs$selectedTaxa[tax, clim])) > 0) {
          pdfs[[tax]][[clim]] <- list()
          tmp <- x$modelling$xrange[[clim]]
          pdfpol <- rep(0, npoints)
          for (sp in unique(x$modelling$distributions[[tax]][, "taxonid"])) {
            w <- which(x$modelling$distributions[[tax]][, "taxonid"] == sp)
            tmp <- cbind(
              tmp,
              fit_pdfsp(
                climate = x$modelling$distributions[[tax]][w, clim],
                ccs = ccs[[clim]],
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
          pdfs[[tax]][[clim]][["pdfsp"]] <- tmp[, -1]
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
  }
  x$modelling$pdfs <- pdfs
  x
}
