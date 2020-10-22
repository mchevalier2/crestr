#' Fit the species and proxy pdfs
#'
#' This function fits the climate response of the selected taxa to the selected
#' climate variables.
#'
#' @inheritParams crestObj
#' @param x a crestObj produced by the crest.fit_pdfs function.
#' @return A crest() object containing the reconstructions and all the associated data.
#' @export
#' @examples
#' data(crest_ex)
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' x <- crest.get_modern_data(
#'   pse = crest_ex_pse, taxaType = 0,
#'   climate = c("bio1", "bio12"),
#'   taxa.name = colnames(crest_ex)[-1],
#'   selectedTaxa = crest_ex_selection, dbname = "crest_example"
#' )
#' x <- crest.calibrate(x,
#'   geoWeighting = TRUE, climateSpaceWeighting = TRUE,
#'   bin_width = c(2, 20), shape = c("normal", "lognormal")
#' )
#' x <- crest.reconstruct(x, crest_ex, selectedTaxa = crest_ex_selection)
#' plot(x)
crest.reconstruct <- function(x, df,
                              selectedTaxa = x$inputs$selectedTaxa,
                              presenceThreshold = 0,
                              taxWeight = "normalisation") {
  if (is.character(df)) {
    df <- rio::import(df)
  }
  if (!is.data.frame(df)) {
    cat("Problem here. Input data is not a data frame.\n")
    return()
  }

  x$inputs$x <- df[, 1]
  x$inputs$x.name <- colnames(df)[1]
  x$inputs$taxa.name <- colnames(df)[-1]
  x$inputs$df <- df[, -1]
  x$parameters$taxWeight <- taxWeight
  x$parameters$presenceThreshold <- presenceThreshold


  if (sum(x$inputs$taxa.name %in% x$inputs$pse$ProxyName) != length(x$inputs$taxa.name)) {
    missing_taxa <- x$inputs$taxa.name[!(x$inputs$taxa.name %in% x$inputs$pse$ProxyName)]
    cat(paste(
      "The following", ifelse(length(missing_taxa) > 1,
        "taxa are in the input file and are",
        "taxon is in the input file and is"
      ),
      " not in the proxy_species_equivalency table.\n"
    ))
    cat(paste(missing_taxa, collapse = ", "))
    cat("\n")
    ss <- paste(
      "Should",
      ifelse(length(missing_taxa) > 1, "these taxa", "this taxon"),
      "be ignored to continue? [Y/N] "
    )
    txt <- base::readline(ss)
    while (!txt %in% c("y", "yes", "Y", "YES", "n", "N", "no", "NO")) {
      txt <- base::readline(ss)
    }
    if (txt %in% c("n", "N", "no", "NO")) {
      return()
    } else {
      for (tax in missing_taxa) {
        # selectedTaxa[tax, ncol(selectedTaxa)] <- as.character(selectedTaxa[tax, ncol(selectedTaxa)])
        x$inputs$selectedTaxa[tax, ] <- c(rep(0, length(x$parameters$climate)), "No association with vegetation")
      }
    }
  }

  if (tolower(x$parameters$taxWeight) == "normalisation") {
    taxWeight <- normalise(x$inputs$df, col2convert = 1:ncol(x$inputs$df))
  } else {
    if (tolower(x$parameters$taxWeight) == "presence/absence") {
      taxWeight <- convert2presenceAbsence(x$inputs$df, x$parameters$presenceThreshold, col2convert = 1:ncol(x$inputs$df))
    } else {
      if (tolower(x$parameters$taxWeight) == "percentages") {
        taxWeight <- convert2percentages(x$inputs$df, col2convert = 1:ncol(x$inputs$df))
      } else {
        taxWeight <- x$inputs$df
      }
    }
  }
  colnames(taxWeight) <- colnames(x$inputs$df)
  rownames(taxWeight) <- rownames(x$inputs$df)
  x$modelling$weights <- taxWeight

  reconstructions <- list()
  for (clim in x$parameters$climate) {
    if (sum(as.numeric(selectedTaxa[, clim])) > 0) {
      reconstructions[[clim]][["posterior"]] <- matrix(rep(0, x$parameters$npoints * nrow(x$inputs$df)), ncol = x$parameters$npoints)
      reconstructions[[clim]][["optima"]] <- rep(NA, nrow(x$inputs$df))
      for (s in 1:nrow(x$inputs$df)) {
        if (is.na(x$modelling$weights[s, names(x$modelling$pdfs)[1]])) {
          reconstructions[[clim]][["posterior"]][s, ] <- rep(NA, x$parameters$npoints)
        } else {
          norm_factor <- 0
          for (tax in names(x$modelling$pdfs)) {
            if (x$modelling$weights[s, tax] > 0 & as.numeric(x$inputs$selectedTaxa[tax, clim]) > 0) {
              norm_factor <- norm_factor + x$modelling$weights[s, tax]
              reconstructions[[clim]][["posterior"]][s, ] <-
                reconstructions[[clim]][["posterior"]][s, ] +
                x$modelling$pdfs[[tax]][[clim]][["pdfpol_log"]] * x$modelling$weights[s, tax]
            }
          }
          reconstructions[[clim]][["posterior"]][s, ] <-
            reconstructions[[clim]][["posterior"]][s, ] * (1 / norm_factor)
          reconstructions[[clim]][["posterior"]][s, ] <- exp(reconstructions[[clim]][["posterior"]][s, ])
          reconstructions[[clim]][["posterior"]][s, ] <-
            reconstructions[[clim]][["posterior"]][s, ] /
              (sum(reconstructions[[clim]][["posterior"]][s, ]) *
                (x$modelling$xrange[[clim]][2] - x$modelling$xrange[[clim]][1])
              )
          reconstructions[[clim]][["optima"]][s] <- x$modelling$xrange[[clim]][which.max(reconstructions[[clim]][["posterior"]][s, ])]
        }
      }
      reconstructions[[clim]][["posterior"]] <- rbind(x$modelling$xrange[[clim]], reconstructions[[clim]][["posterior"]])
      reconstructions[[clim]][["optima"]] <- cbind(x$inputs$x, reconstructions[[clim]][["optima"]])
    } else {
      reconstructions[[clim]] <- NA
    }
  }
  for (tax in names(x$modelling$distributions)) {
    for (clim in x$parameters$climate) {
      if (as.numeric(x$inputs$selectedTaxa[tax, clim]) > 0) {
        x$modelling$pdfs[[tax]][[clim]][["pdfpol_log"]] <- NULL
      }
    }
  }
  x$reconstructions <- reconstructions
  x
}
