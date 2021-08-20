
#' Format a \code{crestObj} with private data.
#'
#' Format a \code{crestObj} with private data.
#'
#' @inheritParams crestObj
#' @inheritParams crest
#' @param climate_space A dataframe of climate values across the study area
#'        useful to correct for the imbalance of the sampling data (see
#'        '\code{\link{crest.calibrate}} for more details). Default is \code{NA}.
#' @param weight The records in the distributions can be weighted using the
#'        percentages by setting weight=TRUE. Include a column called 'weight'
#'        in the \code{distributions} table.
#' @param site_climate The climate values at the location of the dataset
#'        '(default \code{NA}).
#' @param verbose A boolean to print non-essential comments on the terminal
#'        (default \code{TRUE}).
#' @return A \code{\link{crestObj}} object containing the spatial distributions.
#' @export
#' @examples
#' #> Reformating the example dataset to fit this function
#' distributions <- cbind('ProxyName'= rep('Taxon1', nrow(reconstr$modelling$distributions[[1]])),
#'                        reconstr$modelling$distributions[[1]],
#'                        stringsAsFactors = FALSE)
#' for(tax in names(reconstr$modelling$distributions)[-1]) {
#'   distributions <- rbind(distributions,
#'                      cbind('ProxyName'= rep(tax, nrow(reconstr$modelling$distributions[[tax]])),
#'                            reconstr$modelling$distributions[[tax]],
#'                            stringsAsFactors = FALSE)
#'                      )
#' }
#' distributions <- distributions[, c(2,1,3:6)]
#' print(head(distributions))
#'
#' climate_space <- reconstr$modelling$climate_space
#' print(head(climate_space))
#'
#' x <- crest.set_modern_data(distributions, df=crest_ex,
#'                            climate = c("bio1", "bio12"))
#' x <- crest.set_modern_data(distributions, df=crest_ex,
#'                            climate_space=climate_space,
#'                            climate = c("bio1", "bio12"))
#'
#'
crest.set_modern_data <- function( distributions, climate,
                                   df = NA,
                                   climate_space = NA,
                                   weight = FALSE,
                                   minGridCells = 0,
                                   selectedTaxa = NA,
                                   site_info = c(NA, NA),
                                   site_name = NA,
                                   site_climate = rep(NA, length(climate)),
                                   verbose=TRUE) {

    if(base::missing(distributions)) distributions
    if(base::missing(climate)) climate

    if(verbose) cat('\n## Prepping data for database extraction\n')

    if(verbose) cat('  <> Checking distributions ................ ')

    if(weight) {
        if(! 'weight' %in% colnames(distributions) ) {
            cat("[FAILED]\n\n")
            stop("You selected 'weight=TRUE' but did not provide weights in your distribution data frame.\n")
            return()
        }
    }

    taxa.name <- unique(as.character(distributions[, 'ProxyName']))
    if (is.data.frame(df)) taxa.name <- unique(c(taxa.name, colnames(df)[-1]))

    taxa_to_ignore=c()
    # The taxa that are in the fossil but not the modern data files
    for(tax in taxa.name) {
        if (! tax %in% distributions[, 'ProxyName']) taxa_to_ignore=c(taxa_to_ignore, tax)
    }


    if(verbose) cat('[OK]\n  <> Checking/Defining selectedTaxa ........ ')
    if (is.na(as.vector(t(selectedTaxa))[1])) {
        selectedTaxa <- data.frame(matrix(rep(1, length(climate) * length(taxa.name)),
          ncol = length(climate)
        ))
        rownames(selectedTaxa) <- taxa.name
        colnames(selectedTaxa) <- climate
    }

    taxa_notes <- list()
    for (tax in taxa_to_ignore) {
        message <- 'Taxon not in the distribution table.'
        if (! message %in% names(taxa_notes)) {
            taxa_notes[[message]] <- c()
            warning(paste0("One or more taxa were are not in the distribution table and have been ignored. Check 'x$misc$taxa_notes' for details."))
        }
        taxa_notes[[message]] <- append(taxa_notes[[message]], tax)
        selectedTaxa[tax, climate] <- rep(-1, length(climate))
    }
    taxa.name <- taxa.name[taxa.name %in% rownames(selectedTaxa)[apply(selectedTaxa, 1, sum)>=0]]

    if(verbose) cat('[OK]\n  <> Checking the list of taxa ............. ')

    w <- !(taxa.name %in% rownames(selectedTaxa))
    if (sum(w) > 0) {
        for(w in which(!(taxa.name %in% rownames(selectedTaxa)))) {
            selectedTaxa <- rbind(selectedTaxa, rep(1, length(climate)))
            rownames(selectedTaxa)[nrow(selectedTaxa)] <- taxa.name[w]
            for (tax in taxa_to_ignore) {
                message <- 'Not present in the original selectedTaxa table. Added by default as 1s.'
                if (! message %in% names(taxa_notes)) {
                    taxa_notes[[message]] <- c()
                    warning(paste0("One or more taxa were are not in the selectedTaxa table. They have been added but are not selected for any variable. Check 'x$misc$taxa_notes' for details."))
                }
                taxa_notes[[message]] <- append(taxa_notes[[message]], tax)
                selectedTaxa[tax, climate] <- rep(0, length(climate))
            }
        }
    }

    taxa.name <- taxa.name[taxa.name %in% rownames(selectedTaxa)[apply(selectedTaxa, 1, sum)>=0]]


    if(verbose) cat('[OK]\n  <> Creating the crestObj ................. ')


    crest <- crestObj(taxa.name, taxaType=0, climate=climate,
        selectedTaxa = selectedTaxa,
        dbname=NA, year_min=NA, year_max=NA, nodate=NA,
        type_of_obs=NA
    )

    if(verbose) cat('[OK]\n  <> Inserting the fossil data ............. ')

    if (is.data.frame(df)) {
        crest$inputs$x <- df[, 1]
        crest$inputs$x.name <- colnames(df)[1]
        crest$inputs$taxa.name <- taxa.name
        crest$inputs$df <- df[, -1]

        if(unique(is.numeric(crest$inputs$x))) {
            crest$inputs$df <- crest$inputs$df[order(crest$inputs$x), ]
            crest$inputs$x  <- crest$inputs$x[order(crest$inputs$x)]
        }

        w <- (apply(crest$inputs$df, 2, sum) == 0)
        if (sum(w) > 0) {
            for (tax in colnames(crest$inputs$df)[w]) {
                crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                message <- "All percentages equal to 0."
                if (! message %in% names(crest$misc[['taxa_notes']])) {
                    crest$misc[['taxa_notes']][[message]] <- c()
                    warning(paste0("The percentages of one or more taxa were always 0 and have been removed accordingly. Check 'x$misc$taxa_notes' for details."))
                }
                crest$misc[['taxa_notes']][[message]] <- append(crest$misc[['taxa_notes']][[message]], tax)
            }
        }

        w <- (! taxa.name %in% colnames(df)[-1])
        if (sum(w) > 0) {
            for (tax in taxa.name[w]) {
                crest$inputs$selectedTaxa[tax, ] <- rep(-1, length(climate))
                message <- "Taxon not recorded in the data file."
                if (! message %in% names(crest$misc[['taxa_notes']])) {
                    crest$misc[['taxa_notes']][[message]] <- c()
                    warning(paste0("One or more taxa were are not recorded in the data file. Check 'x$misc$taxa_notes' for details."))
                }
                crest$misc[['taxa_notes']][[message]] <- append(crest$misc[['taxa_notes']][[message]], tax)
            }
        }

    } else {
        stop("'df' should be a data.frame.")
    }

    if(verbose) cat('[OK]\n  <> Formatting the modern distributions ... ')
    stats::na.omit(distributions)

    crest$modelling$distributions <- list()
    w <- which(colnames(distributions) %in% c('taxonid', 'longitude', 'latitude', climate))
    for (tax in crest$inputs$taxa.name) {
        if(weight) {
            tax.rows <- which(distributions[, 'ProxyName'] == tax)
            dupl.rows <- rep(tax.rows, times=base::ceiling(distributions[tax.rows, 'weight']))
            crest$modelling$distributions[[tax]] <- distributions[dupl.rows, w]
            crest$parameters$weightedPresences <- TRUE
        } else {
            crest$modelling$distributions[[tax]] <- distributions[distributions[, 'ProxyName'] == tax, w]
            crest$parameters$weightedPresences <- FALSE
        }
    }
    crest$modelling$distributions <- distributions

    if(verbose) cat('[OK]\n  <> Checking the climate space ............ ')
    if(is.data.frame(climate_space)) {
        stats::na.omit(climate_space)
        crest$modelling$climate_space <- unique(rbind(climate_space, distributions[, -which(colnames(distributions) %in% c('taxonid', 'ProxyName', 'weight'))]))
    } else {
        crest$modelling$climate_space <- unique(distributions[, -which(colnames(distributions) %in% c('taxonid', 'ProxyName', 'weight'))])
    }
    crest$modelling$climate_space <- cbind(crest$modelling$climate_space[, c(1,2)], crest$modelling$climate_space[, climate])

    resol <- sort(unique(diff(sort(unique(crest$modelling$climate_space[, 1])))))[1] / 2.0
    xx <- range(crest$modelling$climate_space[, 1])
    crest$parameters$xmn <- xx[1] - resol
    crest$parameters$xmx <- xx[2] + resol

    resol <- sort(unique(diff(sort(unique(crest$modelling$climate_space[, 2])))))[1] / 2.0
    yy <- range(crest$modelling$climate_space[, 2])
    crest$parameters$ymn <- yy[1] - resol
    crest$parameters$ymx <- yy[2] + resol

    crest$misc[['taxa_notes']] <- taxa_notes
    crest$misc$site_info                   <- list()
    crest$misc$site_info[['long']]         <- site_info[1]
    crest$misc$site_info[['lat']]          <- site_info[2]
    crest$misc$site_info[['site_name']]    <- site_name
    if(length(site_climate) != ncol(crest$modelling$climate_space) - 2) {
        cat("[FAILED]\n\n")
        stop(paste0("The number of modern climate values provided at the site's location differs from the number of variables in the climate space.\n\n"))
    } else {
        names(site_climate) <- colnames(crest$modelling$climate_space)[-(1:2)]
    }
    crest$misc$site_info[['climate']]      <- site_climate


    if(verbose) {
      cat('[OK]\n')
      cat(paste0('## Data insertion completed.\n\n'))
    }
    crest$misc$stage <- 'data_inserted'
    crest
}
