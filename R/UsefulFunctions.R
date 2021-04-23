#' Convert abundance data into percentage data.
#'
#' Convert abundance data into percentage data.
#'
#' @param df The dataframe containing the data to convert.
#' @param col2convert A vector of the columns to convert. Default is all the
#'    columns but the first, which contains an age, a depth or a sampleID.
#' @return A vector of unique taxonIDs.
#' @export
#' @examples
#' df <- data.frame(matrix(1:25, ncol = 5))
#' colnames(df) <- paste(rep("col", 5), 1:5, sep = "")
#' convert2percentages(df)
#' convert2percentages(df, col2convert = 3:5)
convert2percentages <- function(df, col2convert = 2:ncol(df)) {
    df2 <- cbind(
      df[, -col2convert],
      100 * df[, col2convert] / apply(df[, col2convert], 1, sum)
    )
    colnames(df2) <- colnames(df)
    rownames(df2) <- rownames(df)
    df2[is.na(df2)] <- 0
    df2
}


#' Convert data into presence/absence data.
#'
#' Convert data into presence/absence data.
#'
#' @param df The dataframe containing the data to convert.
#' @param threshold The threshold that defines presence (presence if >= threshold)
#' @param col2convert A vector of the columns to convert. Default is all the
#'    columns but the first, which contains an age, a depth or a sampleID.
#' @return A vector of unique taxonIDs.
#' @export
#' @examples
#' df <- data.frame(matrix(1:25, ncol = 5))
#' colnames(df) <- paste(rep("col", 5), 1:5, sep = "")
#' convert2presenceAbsence(df, threshold = 15)
#' convert2presenceAbsence(df, col2convert = 3:5)
convert2presenceAbsence <- function(df, threshold = 2, col2convert = 2:ncol(df)) {
    df2 <- cbind(
      df[, -col2convert],
      ifelse(df[, col2convert] >= threshold, 1, 0)
    )
    colnames(df2) <- colnames(df)
    rownames(df2) <- rownames(df)
    df2
}


#' Convert data into presence/absence data.
#'
#' Convert data into presence/absence data.
#'
#' @param df The dataframe containing the data to convert.
#' @param col2convert A vector of the columns to convert. Default is all the
#'    columns but the first, which contains an age, a depth or a sampleID.
#' @return A vector of unique taxonIDs.
#' @export
#' @examples
#' df <- data.frame(matrix(1:25, ncol = 5))
#' colnames(df) <- paste(rep("col", 5), 1:5, sep = "")
#' normalise(df)
#' normalise(df, col2convert = 3:5)
normalise <- function(df, col2convert = 2:ncol(df)) {
    df2 <- convert2percentages(df, col2convert)
    colweights <- apply(df2[, col2convert], 2, meanPositiveValues)
    for (i in 1:nrow(df2)) {
      df2[i, col2convert] <- df2[i, col2convert] / colweights
    }
    colnames(df2) <- colnames(df)
    rownames(df2) <- rownames(df)
    df2
}


#' Calculate the mean of all stricly positive values.
#'
#' Calculate the mean of all stricly positive values.
#'
#' @param x A vector of values.
#' @return The average of all the positive values. Returns \code{NaN} is no
#'         stricly positive values are found.
#' @export
#' @examples
#' meanPositiveValues(-10:10)
meanPositiveValues <- function(x) {
    base::mean(x[x > 0])
}



#' Copy crest data to the clipboard.
#'
#' Copy crest data to the clipboard for an easy extraction of the data from the
#' R environment.
#'
#' @inheritParams crest
#' @param x A \code{\link{crestObj}} produced by the \code{\link{crest.reconstruct}} or \code{\link{crest}} functions.
#' @param optima A boolean value to indicate if the optima should be copied to the clipboard.
#' @param mean A boolean value to indicate if the means should be copied to the clipboard.
#' @param uncertainties A boolean value to indicate if the uncertainties should be copied to the clipboard.
#' @export
#' @examples
#' \dontrun{
#'  if(requireNamespace('clipr', quietly=TRUE)) {
#'    reconstr <- crest(
#'     df = crest_ex, pse = crest_ex_pse, taxaType = 0,
#'     climate = c("bio1", "bio12"), bin_width = c(2, 20),
#'     shape = c("normal", "lognormal"),
#'     selectedTaxa = crest_ex_selection, dbname = "crest_example",
#'     leave_one_out = TRUE
#'    )
#'    copy_crest(reconstr, uncertainties=TRUE)
#'    ## You can now paste the values in a spreadsheet.
#'  }
#' }
#'
copy_crest <- function(x,  climate = x$parameters$climate, optima=TRUE, mean=FALSE, uncertainties=FALSE) {
    if(! requireNamespace('clipr', quietly=TRUE)) {
        cat('ERROR: copy_crest() requires the clipr package. You can install it using install.packages("clipr").\n')
        return(invisible(NULL))
    }
    if(optima + mean + uncertainties == 0) {
        cat('ERROR: optima, mean and uncertainties cannot all be set to FALSE.\n')
        return(invisible(NULL))
    }
    tbl <- list()
    tbl[[x$inputs$x.name]] <- x$inputs$x
    for (clim in climate) {
        if(optima) {
            lbl <- paste(clim, 'optima', sep='_')
            tbl[[lbl]] <- x$reconstructions[[clim]]$optima[, 2]
        }
        if(mean) {
            lbl <- paste(clim, 'mean', sep='_')
            tbl[[lbl]] <- x$reconstructions[[clim]]$optima[, 3]
        }
        if(uncertainties) {
            for(k in 2:ncol(x$reconstructions[[clim]][['uncertainties']])) {
                lbl <- paste(clim, colnames(x$reconstructions[[clim]][['uncertainties']])[k], sep='_')
                tbl[[lbl]] <- x$reconstructions[[clim]][['uncertainties']][, k]
            }
        }
    }
    tbl <- as.data.frame(tbl)
    clipr::write_clip(tbl)
    invisible(x)
}




#' Check if the coordinates are correct.
#'
#' Check if the coordinates are correct.
#'
#' @inheritParams crest
#' @export
#' @examples
#' check_coordinates(NA, NA, NA, NA)
#' check_coordinates(-200, 0, 0, 90)
#' check_coordinates(20, 0, 90, 0)
#'
check_coordinates <- function(xmn, xmx, ymn, ymx) {

    estimate_xlim <- estimate_ylim <- FALSE

    if (xmn < -180 | is.na(xmn) | xmx > 180 | is.na(xmx)) {
        xmn <- max(xmn, -180, na.rm=TRUE)
        xmx <- min(xmx, 180, na.rm=TRUE)
        estimate_xlim <- TRUE
        #cat("WARNING: [xmn; xmx] range larger than accepted values [-180; 180]. Adapting and continuing.\n")
    }
    if (xmn >= xmx) {
        #cat("WARNING: xmn is larger than xmx. Inverting the two values and continuing.\n")
        tmp <- xmn
        xmn <- xmx
        xmx <- tmp
    }

    if (ymn < -90| is.na(ymn)  | ymx > 90 | is.na(ymx) ) {
        ymn <- max(ymn, -90, na.rm=TRUE)
        ymx <- min(ymx, 90, na.rm=TRUE)
        estimate_ylim <- TRUE
        #cat("WARNING: [ymn; ymx] range larger than accepted values [-90; 90]. Adapting and continuing.\n")
    }
    if (ymn >= ymx) {
        #cat("WARNING: ymn is larger than ymx. Inverting the two values and continuing.\n")
        tmp <- ymn
        ymn <- ymx
        ymx <- tmp
    }
    c(xmn, xmx, ymn, ymx, estimate_xlim, estimate_ylim)
}
