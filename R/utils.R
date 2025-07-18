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
    if(base::missing(df)) df

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
    if(base::missing(df)) df

    df2 <- cbind(
      df[, -col2convert],
      ifelse(df[, col2convert] >= threshold & df[, col2convert] > 0, 1, 0)
    )
    colnames(df2) <- colnames(df)
    rownames(df2) <- rownames(df)
    df2
}


#' Normalises the percentages
#'
#' Normalises the percentages
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
    if(base::missing(df)) df

    df2 <- convert2percentages(df, col2convert)
    colweights <- apply(df2[, col2convert], 2, meanPositiveValues)
    for (i in 1:nrow(df2)) {
      df2[i, col2convert] <- df2[i, col2convert] / colweights
    }
    colnames(df2) <- colnames(df)
    rownames(df2) <- rownames(df)
    df2
}


#' Calculate the mean of all strictly positive values.
#'
#' Calculate the mean of all strictly positive values.
#'
#' @param x A vector of values.
#' @return The average of all the positive values. Returns \code{NaN} is no
#'         strictly positive values are found.
#' @export
#' @examples
#' meanPositiveValues(-10:10)
meanPositiveValues <- function(x) {
    if(base::missing(x)) x

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
#' @return No return value. This function is called to copy the crest data to the clipboard.
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
    if(base::missing(x)) x

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
#' @return Return a set of valid coordinates.
#' @export
#' @examples
#' check_coordinates(NA, NA, NA, NA)
#' check_coordinates(-200, 0, 0, 90)
#' check_coordinates(20, 0, 90, 0)
#'
check_coordinates <- function(xmn, xmx, ymn, ymx) {
    if(base::missing(xmn)) xmn
    if(base::missing(xmx)) xmx
    if(base::missing(ymn)) ymn
    if(base::missing(ymx)) ymx

    estimate_xlim <- estimate_ylim <- FALSE

    if (xmn < -180 | is.na(xmn) | xmx > 180 | is.na(xmx)) {
        if(!is.na(xmn) & !is.na(xmx)) {
            if (xmn < -180 | xmx > 180) {
                warning("[xmn; xmx] range larger than accepted values [-180; 180]. The limits were set to -180 and/or 180.\n")
            }
        }
        xmn <- max(xmn, -180, na.rm=TRUE)
        xmx <- min(xmx, 180, na.rm=TRUE)
        estimate_xlim <- TRUE
    }
    if (xmn >= xmx) {
        warning("xmn was larger than xmx. The two values were inverted.\n")
        tmp <- xmn
        xmn <- xmx
        xmx <- tmp
    }

    if (ymn < -90| is.na(ymn)  | ymx > 90 | is.na(ymx) ) {
        if(!is.na(ymn) & !is.na(ymx)) {
            if (ymn < -90 | ymn > 90) {
                warning("[ymn; ymx] range larger than accepted values [-90; 90]. The limits were set to -90 and/or 90.\n")
            }
        }
        ymn <- max(ymn, -90, na.rm=TRUE)
        ymx <- min(ymx, 90, na.rm=TRUE)
        estimate_ylim <- TRUE
    }
    if (ymn >= ymx) {
        warning("ymn was larger than ymx. The two values were inverted.\n")
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
#' @param shp A shapefile (spatVect) to crop the data. Data points will be kept
#'        if their centroid is within the shape.
#' @return An cropped version of the input \code{crestObj}.
#' @export
#' @examples
#' \dontrun{
#'   data(M1)
#'   M1 <- terra::unwrap(M1)
#'   ## We want only the data covering Nigeria
#'   M2 <- M1[M1$COUNTRY == 'Nigeria', ]
#'   data(reconstr)
#'   reconstr.cropped <- crop(reconstr, M2)
#'   data1 <- terra::rast(reconstr$modelling$climate_space[, 1:3],
#'                                  crs=terra::crs(M1), type='xyz')
#'   data2 <- terra::rast(reconstr.cropped$modelling$climate_space[, 1:3],
#'                                  crs=terra::crs(M1), type='xyz')
#'   layout(matrix(c(1,2,3,4), byrow=FALSE, ncol=2), width=1, height=c(0.2, 0.8))
#'   plot_map_eqearth(data1, brks.pos=seq(13,29,2), colour_scale=TRUE,
#'                    title='Full dataset', zlim=c(13, 29))
#'   plot_map_eqearth(data2, brks.pos=seq(13,29,2), colour_scale=TRUE,
#'                    title='Cropped dataset', zlim=c(13, 29))
#' }
#'
crop <- function(x, shp) {
    if(base::missing(x)) x
    if(base::missing(shp)) shp

    if (is.crestObj(x)) {
        dat.x <- x$modelling$climate_space[, 1]
        dat.y <- x$modelling$climate_space[, 2]

        res <- cbind(dat.x, dat.y, rep(0, length(dat.x)))
        pts <- terra::vect(res[, 1:2], crs="+proj=longlat")
        extracted <- terra::extract(shp, pts)
        res[extracted[!is.na(extracted[, 2]), 1], 3] <- 1
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
            pts <- terra::vect(res[, 1:2], crs="+proj=longlat")
            extracted <- terra::extract(shp, pts)
            res[extracted[!is.na(extracted[, 2]), 1], 3] <- 1

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

        resol <- getResol(x) / 2.0
        xx <- range(x$modelling$climate_space[, 1])
        x$parameters$xmn <- xx[1] - resol
        x$parameters$xmx <- xx[2] + resol

        yy <- range(x$modelling$climate_space[, 2])
        x$parameters$ymn <- yy[1] - resol
        x$parameters$ymx <- yy[2] + resol


        if( length(taxalist ) > 0) {
            name <- find.original.name(x)
            warning(paste0("One or more taxa were were lost due to the cropping of the study area. Use PSE_log() with the output of this function for details."))
            message <- 'Taxon excluded by the crop function.'
            x$misc$taxa_notes[[message]] <- taxalist
        }
        return(x)
    } else {
        cat('This function only works with a crestObj.\n\n')
    }
    return(invisible(NA))
}



#' Returns a vector of colours
#'
#' Returns a vector of colours
#'
#' @param n An index to select the colour theme
#' @return A vector of colours.
#' @export
#' @examples
#' colour_theme(1)
#'
colour_theme <- function(n) {
    if(base::missing(n)) n

    if(n == 1) {
        return(c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477", "#66aa00", "#b82e2e", "#316395", "#994499", "#22AA99", "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#651067", "#329262", "#5574A6", "#3B3EAC"))
    } else {
        warning("The selected colour theme does not exist.\n")
        return(NA)
    }
}



#' Returns the name of the function argument in the global environment
#'
#' Returns the name of the function argument in the global environment
#'
#' @param x The function argument
#' @return The name of the function argument in the global environment.
#' @export
#'
find.original.name <- function(x) {
    if(base::missing(x)) x

    objects <- ls(envir = .GlobalEnv)
    for (i in objects) {
        if (identical(x, get(i, envir = .GlobalEnv))) {
            return(i)
        }
    }
}



#' Returns the taxa type corresponding to the index.
#'
#' Returns the taxa type corresponding to the index.
#'
#' @param taxaType An integer between 0 and 6
#' @return Returns the taxa type corresponding to the index.
#' @export
#'
get_taxa_type <- function(taxaType) {
    if(base::missing(taxaType)) taxaType

    if(taxaType == 0) return('Example dataset')
    if(taxaType == 1) return('plant')
    if(taxaType == 2) return('beetle')
    if(taxaType == 3) return('chironomid')
    if(taxaType == 4) return('foraminifer')
    if(taxaType == 5) return('diatom')
    if(taxaType == 6) return('rodent')
}



#' Returns the taxa type corresponding to the taxID.
#'
#' Returns the taxa type corresponding to the taxID.
#'
#' @param taxID An integer between 0 and 6
#' @return Returns the taxa type ID corresponding to the taxon ID.
#' @export
#'
getTaxaTypeFromTaxID <- function(taxID) {
    return(taxID %/% 1000000)
}


#' Test if x is a crestObj.
#'
#' Test if x is a crestObj.
#'
#' @param x The object to be tested
#' @return \code{TRUE} (x is a crestObj) or \code{FALSE} (not a crestObj).
#' @export
#'
is.crestObj <- function(x) {
    return(methods::is(x, "crestObj"))
}


#' Simplify a crestObj into a dataframe.
#'
#' Simplify a crestObj with reconstructed values into a dataframe.
#'
#' @inheritParams plot.crestObj
#' @return A dataframe with the age/depth of each sample and all the best
#' reconstructed values.
#' @export
#' @examples
#' head(crest.simplify(reconstr))
#'
crest.simplify <- function(x, optima=TRUE) {
    if(base::missing(x)) x

    if(!is.crestObj(x)) {
        cat('\nx should be a crestObj.\n\n')
        return(invisible(NA))
    }

    if(!x$misc$stage %in% c('climate_reconstructed', 'leave_one_out')) {
        cat('\nReconstruct a climate variable before using crest.simplify().\n\n')
        return(invisible(NA))
    }

    df <- x$inputs$x
    for(clim in x$parameters$climate){
        df <- cbind(df, x$reconstructions[[clim]]$optima[, ifelse(optima, 2, 3)])
    }
    colnames(df) <- c(x$inputs$x.name, x$parameters$climate)
    return(df)
}

#.onAttach <- function(libname, pkgname) {
#  packageStartupMessage('\nIMPORTANT: From May 2024, the cloud-based gbif4crest calibration\ndatabase will be taken down. The database will remain accessible\nthrough the dbDownload() function.\n\n')
#}

#.onLoad <- function(libname, pkgname){
#    msg <-
#    cat(msg)
#}



#' Calculates a unique identifier from coordinates.
#'
#' Calculates a unique identifier from coordinates.
#'
#' @param long The longitude of the grid cell of interest
#' @param lat The latitude of the grid cell of interest
#' @param resol The spatial resolution of the database to target (1/12 for gbif4crest_03)
#' @return A unique integer describing the cell.
#' @export
#' @examples
#' f_locid(0, 0, 1/12)
#'
f_locid <- function(long, lat, resol) {
    if(long >= 180) long = 180 - resol/2
    if(lat >= 90) lat = 90 - resol/2
    return(((180 + long) %/% resol)%/%1 + 360 / resol * ((90 + lat) %/% resol)%/%1)
}


#' Calculates a unique identifier from coordinates.
#'
#' Calculates a unique identifier from coordinates.
#'
#' @param crest A crestObj object.
#' @return A unique integer describing the cell.
#' @export
#' @examples
#' getResol(reconstr)
#'
getResol <- function(crest) {
    if(!is.na(crest$misc$resol)) return(crest$misc$resol)
    if(.ifExampleDB(crest$misc$dbname)) return(0.5)
    if(crest$misc$dbname == "private-database") return(1)
    res = dbRequest("SELECT * FROM sqlite_master WHERE type='table'", dbname=crest$misc$dbname)
    if( 'params' %in% res[,2] ) return(1/12)
    if(grepl('gbif4crest_02-5m', crest$misc$dbname, fixed = TRUE)) return(1/12)
    return(1/4)
}


#' Identify the calibration database used.
#'
#' Identify the calibration database used.
#'
#' @param dbname A functional crestObj or a database name.
#' @return A string uniquely characterising the database used. Possible values
#'         are 'privateDB', 'exampleDB', 'gbif4crest_02', and 'gbif4crest_03'.
#' @export
#' @examples
#' identifyDatabase(reconstr)
#'
identifyDatabase <- function(dbname) {
    if(is.crestObj(dbname)) dbname <- dbname$misc$dbname
    if(dbname == 'private-database') return('privateDB')
    if(grepl("exampleDB",dbname, fixed=TRUE)) return('exampleDB')
    params <- tryCatch(
                {
                    dbRequest("SELECT  * FROM params", dbname=dbname)
                },
                error = function(cond) {
                    # Choose a return value in case of error
                    NA
                }
    )
    if(is.na(params)[1]) return('gbif4crest_02')
    return('gbif4crest_03')
}

#' Check if the database is one of mine.
#'
#' Check if the database is one of mine.
#'
#' @param dbname A functional crestObj or a database name.
#' @return A boolean indicating if the database if of the gbif4crest Family
#'         or the example database (TRUE). FALSE otherwise.
#' @export
#' @examples
#' is_database_gbif(reconstr$misc$dbname)
#'
is_database_gbif <- function(dbname) {
    dbname=identifyDatabase(dbname)
    if(dbname %in% c('gbif4crest_02', 'gbif4crest_03', 'exampleDB')) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
