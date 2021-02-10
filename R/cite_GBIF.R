#' Returns the citations associated to the GBIF data used to fit the pdfs.
#'
#' Returns the citations associated to the GBIF data used to fit the pdfs.
#'
#' @inheritParams crest
#' @param x A crestObj produced by one of the crest functions.
#' @export

cite_GBIF <- function(x, dbname = "gbif4crest_02", verbose=TRUE) {
    if (x$parameters$taxaType == 1) {
        list_of_classes <- c()
        for (fam in unique(x$inputs$pse$Family[x$inputs$pse[,1]<4])) {
          list_of_classes <- c(list_of_classes, getTaxonomy(family=fam, depth.out=3)['class_name'])
        }
        list_of_classes <- unique(list_of_classes)
        citations = list()
        citations[['Pinopsida']] <- ' -->  GBIF.org (24 September 2020) Pinopsida occurrence data. https://doi.org/10.15468/dl.x2r7pa.\n'
        citations[['Cycadopsida']] <- ' -->  GBIF.org (24 September 2020) Cycadopsida occurrence data. https://doi.org/10.15468/dl.sfjzxu.\n'
        citations[['Gnetopsida']] <- ' -->  GBIF.org (24 September 2020) Gnetopsida occurrence data. https://doi.org/10.15468/dl.h2kjnc.\n'
        citations[['Gingkoopsida']] <- ' -->  GBIF.org (24 September 2020) Gingkoopsida occurrence data. https://doi.org/10.15468/dl.da9wz8.\n'
        citations[['Lycopodiopsida']] <- ' -->  GBIF.org (24 September 2020) Lycopodiopsida occurrence data. https://doi.org/10.15468/dl.ydhyhz.\n'
        citations[['Anthocerotopsida']] <- ' -->  GBIF.org (24 September 2020) Anthocerotopsida occurrence data. https://doi.org/10.15468/dl.t9zenf.\n'
        citations[['Liliopsida']] <- ' -->  GBIF.org (24 September 2020) Liliopsida occurrence data. https://doi.org/10.15468/dl.axv3yd.\n'
        citations[['Magnoliopsida']] <- ' -->  GBIF.org (24 September 2020) Magnoliopsida occurrence data. https://doi.org/10.15468/dl.ra49dt.\n'
        citations[['Polypodiopsida']] <- ' -->  GBIF.org (24 September 2020) Polypodiopsida occurrence data. https://doi.org/10.15468/dl.87tbp6.\n'
        cat('Please cite the following dataset(s):\n')
        for (class in list_of_classes)  cat(citations[[class]])
    } else if (x$parameters$taxaType == 2) {
        cat('Please cite the following dataset: GBIF.org (24 September 2020) Beetles occurrence data. https://doi.org/10.15468/dl.nteruy.\n')
        } else if (x$parameters$taxaType == 4) {
            cat('Please cite the following dataset: GBIF.org (24 September 2020) Foraminifera occurrence data. https://doi.org/10.15468/dl.692yg6.\n')
            } else if (x$parameters$taxaType == 5) {
                cat('Please cite the following dataset: GBIF.org (24 September 2020) Diatoms occurrence data. https://doi.org/10.15468/dl.vfr257.\n')
                } else if (x$parameters$taxaType == 3) {
                    cat('Please cite the following dataset: GBIF.org (24 September 2020) Chironomids occurrence data. https://doi.org/10.15468/dl.jv3wsh.\n')
                    } else if (x$parameters$taxaType == 6) {
                        cat('Please cite the following dataset: GBIF.org (24 September 2020) Rodentia occurrence data. https://doi.org/10.15468/dl.fscw6q.\n')
                        } else {
                            cat('You have used the example dataset. No data citations are required.\n')
                        }

    invisible(x)
}
