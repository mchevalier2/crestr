#' Returns the list of references associated to the reconstruction.
#'
#' Returns the list of references associated to the reconstruction.
#'
#' @inheritParams crest
#' @param x A \code{\link{crestObj}} produced by one of the \code{\link{crest}},
#'        \code{\link{crest.get_modern_data}}, \code{\link{crest.calibrate}},
#'        \code{\link{crest.reconstruct}} or \code{\link{loo}} functions.
#' @export
#'
cite_crest <- function(x, verbose=TRUE) {
    tocite <- list()
    tocite$distrib <- cite_distrib_data(x, verbose=FALSE)
    tocite$climate <- cite_climate_data(x, verbose=FALSE)
    tocite$method  <- cite_method(x, verbose=FALSE)

    n_ref <- sum(unlist(lapply(tocite, length)))

    if (verbose) {
        cat('This analysis is built on the following ', ifelse(n_ref>1, paste0(n_ref, ' ', sep=''), ''), 'reference', ifelse(n_ref>1, 's', ''), '. Please cite ', ifelse(n_ref>1, 'them', 'it'),' appropriately.\n\n', sep='')
        if (length(tocite$method) > 0) cat('Methodology: \n')
        for(s in tocite$method)  cat('   -->  ', s, '\n')
        if (length(tocite$distrib) > 0) cat('Distribution data: \n')
        for(s in tocite$distrib)  cat('   -->  ', s, '\n')
        if (length(tocite$climate) > 0) cat('Climate data: \n')
        for(s in tocite$climate)  cat('   -->  ', s, '\n')
    }

    invisible(tocite)
}


#' Returns the references associated with the GBIF data used to fit the \code{pdfs}.
#'
#' Returns the references associated with the GBIF data used to fit the \code{pdfs}.
#'
#' @inheritParams crest
#' @param x A \code{\link{crestObj}} produced by one of the \code{\link{crest}},
#'        \code{\link{crest.get_modern_data}}, \code{\link{crest.calibrate}},
#'        \code{\link{crest.reconstruct}} or \code{\link{loo}} functions.
#' @export
#'
cite_distrib_data <- function(x, verbose=TRUE) {
    if (x$parameters$taxaType == 1) {
        tocite <- c()
        list_of_classes <- unique(stats::na.omit(x$inputs$pse[, 'Class_name']))
        citations = list()
        citations[['Pinopsida']] <- 'GBIF.org (Date accessed: 24 September 2020) Pinopsida occurrence data. https://doi.org/10.15468/dl.x2r7pa.'
        citations[['Cycadopsida']] <- 'GBIF.org (Date accessed: 24 September 2020) Cycadopsida occurrence data. https://doi.org/10.15468/dl.sfjzxu.'
        citations[['Gnetopsida']] <- 'GBIF.org (Date accessed: 24 September 2020) Gnetopsida occurrence data. https://doi.org/10.15468/dl.h2kjnc.'
        citations[['Gingkoopsida']] <- 'GBIF.org (Date accessed: 24 September 2020) Gingkoopsida occurrence data. https://doi.org/10.15468/dl.da9wz8.'
        citations[['Lycopodiopsida']] <- 'GBIF.org (Date accessed: 24 September 2020) Lycopodiopsida occurrence data. https://doi.org/10.15468/dl.ydhyhz.'
        citations[['Anthocerotopsida']] <- 'GBIF.org (Date accessed: 24 September 2020) Anthocerotopsida occurrence data. https://doi.org/10.15468/dl.t9zenf.'
        citations[['Liliopsida']] <- 'GBIF.org (Date accessed: 24 September 2020) Liliopsida occurrence data. https://doi.org/10.15468/dl.axv3yd.'
        citations[['Magnoliopsida']] <- 'GBIF.org (Date accessed: 24 September 2020) Magnoliopsida occurrence data. https://doi.org/10.15468/dl.ra49dt.'
        citations[['Polypodiopsida']] <- 'GBIF.org (Date accessed: 24 September 2020) Polypodiopsida occurrence data. https://doi.org/10.15468/dl.87tbp6.'
        if (verbose)  cat('Please cite the following dataset(s):\n')
        for (class in list_of_classes)  {
            if (verbose)  cat(' -->  ', citations[[class]], '\n')
            tocite <- c(tocite, citations[[class]])
        }
    } else if (x$parameters$taxaType == 2) {
        tocite <- 'GBIF.org (Date accessed: 24 September 2020) Beetles occurrence data. https://doi.org/10.15468/dl.nteruy.'
        if (verbose)  cat('Please cite the following dataset: ', tocite, '\n')
        } else if (x$parameters$taxaType == 4) {
            tocite <- 'GBIF.org (Date accessed: 24 September 2020) Foraminifera occurrence data. https://doi.org/10.15468/dl.692yg6.'
            if (verbose)  cat('Please cite the following dataset: ', tocite, '\n')
            } else if (x$parameters$taxaType == 5) {
                tocite <- 'GBIF.org (Date accessed: 24 September 2020) Diatoms occurrence data. https://doi.org/10.15468/dl.vfr257.'
                if (verbose)  cat('Please cite the following dataset: ', tocite, '\n')
                } else if (x$parameters$taxaType == 3) {
                    tocite <- 'GBIF.org (Date accessed: 24 September 2020) Chironomids occurrence data. https://doi.org/10.15468/dl.jv3wsh.'
                    if (verbose)  cat('Please cite the following dataset: ', tocite, '\n')
                    } else if (x$parameters$taxaType == 6) {
                        tocite <- 'GBIF.org (Date accessed: 24 September 2020) Rodentia occurrence data. https://doi.org/10.15468/dl.fscw6q.'
                        if (verbose)  cat('Please cite the following dataset: ', tocite, '\n')
                        } else {
                            if (verbose)  cat('You did not use the gbif4crest dataset. No distribution data citations are required.\n')
                            tocite <- NULL
                        }
    invisible(tocite)
}


#' Returns the references associated with the climate data used to fit the \code{pdfs}.
#'
#' Returns the references associated with the climate data used to fit the \code{pdfs}.
#'
#' @inheritParams crest
#' @param x A \code{\link{crestObj}} produced by one of the \code{\link{crest}},
#'        \code{\link{crest.get_modern_data}}, \code{\link{crest.calibrate}},
#'        \code{\link{crest.reconstruct}} or \code{\link{loo}} functions.
#' @export
#'
cite_climate_data <- function(x, verbose=TRUE) {
    if (x$parameters$taxaType == 0) {
        if (verbose)  cat('You have not used the provided calibration dataset. No climate reference are required.\n')
        tocite <- NULL
    } else {
        ref1 <- "Fick, S.E. and Hijmans, R.J., 2017, WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas. International Journal of Climatology, 37, pp. 4302-4315."
        ref2 <- "Zomer, R.J., Trabucco, A., Bossio, D.A. and Verchot, L. V., 2008, Climate change mitigation: A spatial analysis of global land suitability for clean development mechanism afforestation and reforestation. Agriculture, Ecosystems \u0026 Environment, 126, pp. 67-80."
        ref3 <- "Locarnini, R.A., Mishonov, A.V., Antonov, J.I., Boyer, T.P., Garcia, H.E., Baranova, O.K., Zweng, M.M., Paver, C.R., Reagan, J.R., Johnson, D.R., Hamilton, M. and Seidov, D., 2013. In: Levitus, S. (Ed.), World Ocean Atlas 2013, Volume 1: Temperature. In: Mishonov, A. (Ed.), 73. NOAA Atlas NESDIS, pp. 40 Technical Ed."
        ref4 <- "Zweng, M.M., Reagan, J.R., Antonov, J.I., Locarnini, R.A., Mishonov, A.V., Boyer, T.P., Garcia, H.E., Baranova, O.K., Johnson, D.R., Seidov, D. and Biddle, M.M., 2013. In: Levitus, S. (Ed.), World Ocean Atlas 2013, Volume 2: Salinity. In: Technical, A.M. (Ed.), 74. NOAA Atlas NESDIS, pp. 39."
        ref5 <- "Reynolds, R.W., Smith, T.M., Liu, C., Chelton, D.B., Casey, K.S. and Schlax, M.G., 2007, Daily high-resolution-blended analyses for sea surface temperature. Journal of climate, 20(22), pp. 5473-5496."
        # 1 WorldClim
        # 2 AI
        # 3 & 4 SSTs, SSSs, and nutrients
        # 5 Ice concentration
        tocite <- c()
        if (sum(paste0('bio', 1:19) %in% x$parameters$climate) > 0) {
            tocite <- c(tocite, ref1)
        }
        if ('ai' %in% x$parameters$climate) {
            tocite <- c(tocite, ref2)
        }
        if (sum( paste(rep(c('sst', 'sss'), each=5), rep(c('ann', 'jfm', 'amj', 'jas', 'ond'), 2), sep='_') %in% x$parameters$climate ) > 0) {
            tocite <- c(tocite, ref3, ref4)
        }
        if (sum( c('diss_oxy', 'nitrate', 'phosphate', 'silicate') %in% x$parameters$climate ) > 0) {
            tocite <- c(tocite, ref3, ref4)
        }
        if (sum( paste(rep(c('icec'), each=5), rep(c('ann', 'jfm', 'amj', 'jas', 'ond'), 1), sep='_') %in% x$parameters$climate ) > 0) {
            tocite <- c(tocite, ref5)
        }                                                                                                                           

        if (verbose) {
            cat('Please cite the following dataset(s):\n')
            for (s in tocite)  {
                cat(' -->  ', s, '\n')
            }
        }
    }
    invisible(tocite)
}



#' Returns the references associated with the development of CREST.
#'
#' Returns the references associated with the development of CREST.
#'
#' @inheritParams crest
#' @param x A \code{\link{crestObj}} produced by one of the \code{\link{crest}},
#'        \code{\link{crest.get_modern_data}}, \code{\link{crest.calibrate}},
#'        \code{\link{crest.reconstruct}} or \code{\link{loo}} functions.
#' @export
#'
cite_method <- function(x, verbose=TRUE) {
    tocite <- 'Chevalier, M., Cheddadi, R. and Chase, B.M., 2014, CREST (Climate REconstruction SofTware): a probability density function (PDF)-based quantitative climate reconstruction method. Climate of the Past, 10, pp. 2081-2098. doi: 10.5194/cp-10-2081-2014'
    if (x$parameters$taxaType > 0) {
        tocite <- c(tocite, 'Chevalier, M., 2019, Enabling possibilities to quantify past climate from fossil assemblages at a global scale. Global and Planetary Change, 175, pp. 27-35. doi: 10.1016/j.gloplacha.2019.01.016')
    }
    if (verbose) {
        cat('Please cite the following papers(s):\n')
        cat(' -->  Method: ', tocite[1], '\n')
        if (length(tocite) > 1)  cat(' -->  Calibration dataset: ', tocite[2], '\n')
    }
    invisible(tocite)
}
