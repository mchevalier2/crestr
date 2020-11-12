#' Fit the species and proxy pdfs
#'
#' This function fits the climate response of the selected taxa to the selected
#' climate variables.
#'
#' @inheritParams crestObj
#' @inheritParams crest
#' @param x A crestObj produced by the crest.fit_pdfs function.
#' @param skip_for_loo A boolean that tells the loo() functiont to skip parts
#'        and fasten the process. Not for users.
#' @return A crest() object containing the reconstructions and all the associated data.
#' @export
#' @examples
#' data(crest_ex)
#' data(crest_ex_pse)
#' data(crest_ex_selection)
#' x <- crest.get_modern_data(
#'   pse = crest_ex_pse, taxaType = 0,
#'   climate = c("bio1", "bio12"),
#'   selectedTaxa = crest_ex_selection, dbname = "crest_example"
#' )
#' x <- crest.calibrate(x,
#'   geoWeighting = TRUE, climateSpaceWeighting = TRUE,
#'   bin_width = c(2, 20), shape = c("normal", "lognormal")
#' )
#' x <- crest.reconstruct(x, crest_ex)
#' plot(x)

crest.reconstruct <- function(x, df,
                              presenceThreshold = 0,
                              taxWeight = "normalisation",
                              uncertainties = c(0.5, 0.95),
                              skip_for_loo = FALSE, verbose=TRUE) {

  if(verbose) cat('\n## Last data checks and reconstruction\n')
  if(! skip_for_loo) {

    if(verbose) cat('  <> Checking data ......................... ')
    if (is.character(df)) {
      df <- rio::import(df)
    }
    if (!is.data.frame(df)) {
      if(verbose) cat("\nERROR: Problem here. Input data is not a data frame.\n")
      return()
    }

    if(verbose) cat('[OK]\n  <> Checking taxa ......................... ')
    x$inputs$x <- df[, 1]
    x$inputs$x.name <- colnames(df)[1]
    x$inputs$taxa.name <- colnames(df)[-1]
    x$inputs$df <- df[, -1]
    x$parameters$taxWeight <- taxWeight
    x$parameters$presenceThreshold <- presenceThreshold

    if (sum(x$inputs$taxa.name %in% x$inputs$pse$ProxyName) == 0) {
      cat('\nERROR: None of the recorded proxy were listed in the proxy_species_equivalency table. Reconstruction stopped.\n')
      return(x)
    }

    if (sum(x$inputs$taxa.name %in% x$inputs$pse$ProxyName) != length(x$inputs$taxa.name)) {
      missing_taxa <- x$inputs$taxa.name[!(x$inputs$taxa.name %in% x$inputs$pse$ProxyName)]
      #cat(paste(
      #  "\nThe following", ifelse(length(missing_taxa) > 1,
      #    "taxa are in the input file and are",
      #    "taxon is in the input file and is"
      #  ),
      #  "not in the proxy_species_equivalency table.\n"
      #))
      #cat(paste(missing_taxa, collapse = ", "))
      #cat("\n")
      #ss <- paste(
      #  "Should",
      #  ifelse(length(missing_taxa) > 1, "these taxa", "this taxon"),
      #  "be ignored to continue? [Y/N] "
      #)
      #txt <- base::readline(ss)
      #while (!txt %in% c("y", "yes", "Y", "YES", "n", "N", "no", "NO")) {
      #  txt <- base::readline(ss)
      #}
      #if (txt %in% c("n", "N", "no", "NO")) {
      #  return()
      #} else {
        for (tax in missing_taxa) {
          x$inputs$selectedTaxa[tax, ] <- c(rep(0, length(x$parameters$climate)), "No association with vegetation")
        }
      #}
    }
    if(verbose) cat('[OK]\n  <> Defining taxa weights ................. ')

    if (tolower(x$parameters$taxWeight) == "normalisation") {
      taxWeight <- normalise(x$inputs$df, threshold = x$parameters$presenceThreshold, col2convert = 1:ncol(x$inputs$df))
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
    if (sum(as.numeric(x$inputs$selectedTaxa[, clim])) > 0) {
      reconstructions[[clim]][["posterior"]] <- matrix(rep(0, x$parameters$npoints * nrow(x$inputs$df)), ncol = x$parameters$npoints)
      reconstructions[[clim]][["uncertainties"]] <- matrix(rep(0, 2 * length(x$parameters$uncertainties) * nrow(x$inputs$df)), nrow = nrow(x$inputs$df))
      reconstructions[[clim]][["optima"]] <- matrix(rep(NA, 2 * nrow(x$inputs$df)), nrow = nrow(x$inputs$df))
      for (s in 1:nrow(x$inputs$df)) {
        if (is.na(x$modelling$weights[s, names(x$modelling$pdfs)[1]])) {
          reconstructions[[clim]][["posterior"]][s, ] <- rep(NA, x$parameters$npoints)
          reconstructions[[clim]][["uncertainties"]][s, ] <- rep(NA, 2 * length(x$parameters$uncertainties))
        } else if (sum(as.numeric(x$inputs$selected[, clim]) * x$modelling$weights[s, ]) == 0) {
          reconstructions[[clim]][["posterior"]][s, ] <- rep(NA, x$parameters$npoints)
          reconstructions[[clim]][["uncertainties"]][s, ] <- rep(NA, 2 * length(x$parameters$uncertainties))
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
          reconstructions[[clim]][["optima"]][s, 1] <- x$modelling$xrange[[clim]][which.max(reconstructions[[clim]][["posterior"]][s, ])]
          reconstructions[[clim]][["optima"]][s, 2] <- sum(x$modelling$xrange[[clim]] * reconstructions[[clim]][["posterior"]][s, ]) / sum(reconstructions[[clim]][["posterior"]][s, ])

          oo <- order(reconstructions[[clim]][["posterior"]][s, ], decreasing=TRUE)
          tmp2 <- reconstructions[[clim]][["posterior"]][s, ][oo]
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
      reconstructions[[clim]][["posterior"]] <- rbind(x$modelling$xrange[[clim]], reconstructions[[clim]][["posterior"]])
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
  x
}
