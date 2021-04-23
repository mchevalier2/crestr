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
cite_crest <- function(x, dbname = "gbif4crest_02", verbose=TRUE) {
    tocite <- list()
    tocite$distrib <- cite_distrib_data(x, dbname, verbose=FALSE)
    tocite$climate <- cite_climate_data(x, dbname, verbose=FALSE)
    tocite$method  <- cite_method(x, dbname, verbose=FALSE)

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
cite_distrib_data <- function(x, dbname = "gbif4crest_02", verbose=TRUE) {
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
                            if (verbose)  cat('You have used the example dataset. No distribution data citations are required.\n')
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
cite_climate_data <- function(x, dbname = "gbif4crest_02", verbose=TRUE) {
    if (x$parameters$taxaType == 0) {
        if (verbose)  cat('You have used the example dataset. No climate data citations are required.\n')
        tocite <- NULL
    } else {
        tocite <- "Fick, S.E. and Hijmans, R.J., 2017, WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas. International Journal of Climatology, 37, pp. 4302-4315."
        if ('ai' %in% x$parameters$climate) {
            tocite <- c(tocite, "Zomer, R.J., Trabucco, A., Bossio, D.A. and Verchot, L. V., 2008, Climate change mitigation: A spatial analysis of global land suitability for clean development mechanism afforestation and reforestation. Agriculture, Ecosystems \u0026 Environment, 126, pp. 67-80.")
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
cite_method <- function(x, dbname = "gbif4crest_02", verbose=TRUE) {
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