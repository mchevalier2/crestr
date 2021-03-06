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
        stop("'copy_crest()' requires the 'clipr' package. You can install it using install.packages(\"clipr\").\n\n")
    }
    if(optima + mean + uncertainties == 0) {
        stop("'optima', 'mean' and 'uncertainties' cannot all be set to FALSE.\n\n")
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



#' Crop the dataset obtained from \code{\link{crest.get_modern_data}}
#'
#' Crop the dataset obtained from \code{\link{crest.get_modern_data}} according
#' to an object of the class \code{SpatialPolygonsDataFrame}.
#'
#' @inheritParams crest.calibrate
#' @param shp A shapefile to crop the data. Data points will be kept if their
#'        centroid is within the shape.
#' @export
#' @examples
#' data(M1)
#' ## We want only the data covering Nigeria
#' M2 <- M1[M1$COUNTRY == 'Nigeria', ]
#' data(reconstr)
#' reconstr.cropped <- crop(reconstr, M2)
#' data1 <- raster::rasterFromXYZ(reconstr$modelling$climate_space[, 1:3], crs=raster::crs(M1))
#' data2 <- raster::rasterFromXYZ(reconstr.cropped$modelling$climate_space[, 1:3], crs=raster::crs(M1))
#' layout(matrix(c(1,2,3,4), byrow=FALSE, ncol=2), width=1, height=c(0.2, 0.8))
#' plot_map_eqearth(data1, brks.pos=seq(13,29,2), colour_scale=TRUE,
#'                  title='Full dataset', zlim=c(13, 29))
#' plot_map_eqearth(data2, brks.pos=seq(13,29,2), colour_scale=TRUE,
#'                  title='Cropped dataset', zlim=c(13, 29))
#'
crop <- function(x, shp) {
    dat.x <- x$modelling$climate_space[, 1]
    dat.y <- x$modelling$climate_space[, 2]

    res <- cbind(dat.x, dat.y, rep(0, length(dat.x)))
    for(i in 1:length(shp)) {
        for(j in 1:length(shp@polygons[[i]]@Polygons)) {
            xy <- shp@polygons[[i]]@Polygons[[j]]@coords
            isin <- sp::point.in.polygon(dat.x, dat.y, xy[,1], xy[,2])
            res[ isin == 1, 3] <- 1
        }
    }
    if(sum(res[, 3]) > 0) {
        x$modelling$climate_space <- x$modelling$climate_space[res[, 3] == 1, ]
    } else {
        stop('\nNo overlap between the data and the selected shape.\n\n')
    }

    taxalist <- c()
    for(tax in names(x$modelling$distributions)) {
        dat.x <- x$modelling$distributions[[tax]][, 2]
        dat.y <- x$modelling$distributions[[tax]][, 3]

        res <- cbind(dat.x, dat.y, rep(0, length(dat.x)))
        for(i in 1:length(shp)) {
            for(j in 1:length(shp@polygons[[i]]@Polygons)) {
                xy <- shp@polygons[[i]]@Polygons[[j]]@coords
                isin <- sp::point.in.polygon(dat.x, dat.y, xy[,1], xy[,2])
                res[ isin == 1, 3] <- 1
            }
        }

        if(sum(res[, 3]) > 0) {
            x$modelling$distributions[[tax]] <- x$modelling$distributions[[tax]][res[, 3] == 1, ]
            if(max(table(x$modelling$distributions[[tax]][, 1])) < x$parameters$minGridCells) {
                x$modelling$distributions[[tax]]    <- NULL
                x$inputs$taxa.name                  <- x$inputs$taxa.name[!(x$inputs$taxa.name == tax)]
                x$inputs$selectedTaxa[tax, ]        <- rep(-1, length(x$parameters$climate))
                x$modelling$taxonID2proxy           <- x$modelling$taxonID2proxy[-(x$modelling$taxonID2proxy[, 'proxyName'] == tax), ]
                taxalist <- c(taxalist, tax)
            }
        } else {
            x$modelling$distributions[[tax]]    <- NULL
            x$inputs$taxa.name                  <- x$inputs$taxa.name[!(x$inputs$taxa.name == tax)]
            x$inputs$selectedTaxa[tax, ]        <- rep(-1, length(x$parameters$climate))
            x$modelling$taxonID2proxy           <- x$modelling$taxonID2proxy[-x$modelling$taxonID2proxy[, 'proxyName'] == tax, ]
            taxalist <- c(taxalist, tax)
        }
    }

    resol <- sort(unique(diff(sort(unique(x$modelling$climate_space[, 1])))))[1] / 2.0
    xx <- range(x$modelling$climate_space[, 1])
    x$parameters$xmn <- xx[1] - resol
    x$parameters$xmx <- xx[2] + resol

    resol <- sort(unique(diff(sort(unique(x$modelling$climate_space[, 2])))))[1] / 2.0
    yy <- range(x$modelling$climate_space[, 2])
    x$parameters$ymn <- yy[1] - resol
    x$parameters$ymx <- yy[2] + resol


    if( length(taxalist ) > 0) {
        warning(paste0("One or more taxa were were lost due to the cropping of the study area. Check 'x$misc$taxa_notes' for details."))
        message <- 'Taxon excluded by the crop function.'
        x$misc$taxa_notes[[message]] <- taxalist
    }
    x
}



#' Returns a vector of colours
#'
#' Returns a vector of colours
#'
#' @param n An idex to select the colour theme
#' @export
#' @examples
#' colour_theme(1)
#'
colour_theme <- function(n) {
    if(n == 1) {
        return(c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477", "#66aa00", "#b82e2e", "#316395", "#994499", "#22AA99", "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#651067", "#329262", "#5574A6", "#3B3EAC"))
    } else {
        warning("The selected colour theme does not exist.\n")
        return(NA)
    }
}
