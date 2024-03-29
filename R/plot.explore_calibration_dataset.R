
#' Extract distributions from the database
#'
#' This function will extract the distributions of all the studied climate proxy and plot the data on a map.
#'
#' @inheritParams crest
#' @inheritParams plot.crestObj
#' @inheritParams plot_climateSpace
#' @inheritParams plot_map_eqearth
#' @param width,height The dimensions of the pdf file (default 7.48in ~19cm).
#' @return The distribution data
#' @export
#' @examples
#' \dontrun{
#'   #> Replace 'tempdir()' by the location where to save the sample (e.g. 'getwd()')
#'   d = explore_calibration_dataset(2, xmn=-85, xmx=-30, ymn=-60, ymx=15,
#'                                   save=TRUE, width = 4, height = 7.5,
#'                                   filename=file.path(tempdir(), 'calibrationDataset.pdf')
#'   )
#'   head(d)
#' }
#'
explore_calibration_dataset <- function( taxaType,
                                         save = FALSE, filename = 'calibrationDataset.pdf',
                                         col = viridis::viridis(22)[3:22],
                                         width = 7.48, height = 7.48,
                                         as.png = FALSE, png.res=300,
                                         xmn = NA, xmx = NA, ymn = NA, ymx = NA,
                                         continents = NA, countries = NA,
                                         realms = NA, biomes = NA, ecoregions = NA,
                                         dbname = "gbif4crest_02") {

    if(base::missing(taxaType)) taxaType

    if(!testConnection(dbname)) return(NA)

    coords        <- check_coordinates(xmn, xmx, ymn, ymx)
    xmn           <- coords[1]
    xmx           <- coords[2]
    ymn           <- coords[3]
    ymx           <- coords[4]
    estimate_xlim <- coords[5]
    estimate_ylim <- coords[6]

    taxonIDs <- getTaxonID( taxaType = taxaType, dbname = dbname )
    distributions <- getDistribTaxa( taxonIDs,
                                     xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx,
                                     continents = continents, countries = countries,
                                     realms = realms, biomes = biomes, ecoregions = ecoregions,
                                     dbname = dbname)

    plot.distrib <- ifelse(nrow(distributions) == 0, FALSE, TRUE)

    # Climate_space useful to estimate the extent of selected region
    climate_space <- getClimateSpace(
      ifelse(taxaType %in% c(1, 2, 3, 6), 'bio1', 'sst_ann'), # Get any random climate variable, but this depends on terrestrial or marine.
      xmn, xmx, ymn, ymx,
      continents, countries,
      realms, biomes, ecoregions,
      dbname
    )
    if(nrow(climate_space) > 0) {
        resol <- sort(unique(diff(sort(unique(climate_space[, 1])))))[1] / 2.0
        xx <- range(climate_space[, 1])
        if (estimate_xlim) {
            xmn <- xx[1] - resol
            xmx <- xx[2] + resol
        } else {
            if (xmn > xx[1] - resol) xmn <- xx[1] - resol
            if (xmx < xx[2] + resol) xmx <- xx[2] + resol
        }

        resol <- sort(unique(diff(sort(unique(climate_space[, 2])))))[1] / 2.0
        yy <- range(climate_space[, 2])
        if (estimate_ylim) {
            ymn <- yy[1] - resol
            ymx <- yy[2] + resol
        }else {
            if (ymn > yy[1] - resol) ymn <- yy[1] - resol
            if (ymx < yy[2] + resol) ymx <- yy[2] + resol
        }
    }

    ext <- c(xmn, xmx, ymn, ymx)
    ext_eqearth <- eqearth_get_ext(ext)
    xy_ratio <- diff(ext_eqearth[1:2]) / diff(ext_eqearth[3:4])


    if(save) {
        if(as.png) {
            grDevices::png(paste0(strsplit(filename, '.png')[[1]], '.png'), width = width, height = height, units='in', res=png.res)
        } else {
            grDevices::pdf(paste0(strsplit(filename, '.pdf')[[1]], '.pdf'), width=width, height=height)
        }
    } else {
        par_usr <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(par_usr))
    }

    if (plot.distrib) {
        veg_space      <- distributions[, c('longitude', 'latitude')]
        veg_space      <- plyr::count(veg_space)
        veg_space      <- veg_space[!is.na(veg_space[, 1]), ]
        veg_space[, 3] <- base::log10(veg_space[, 3])
        veg_space      <- terra::rast(veg_space, type='xyz', crs=terra::crs("+proj=longlat +datum=WGS84 +no_defs"))
    } else {
        veg_space <- NA
    }


    ## Plot species abundance --------------------------------------------------
    zlab=c(0, ifelse(plot.distrib, ceiling(max(terra::values(veg_space), na.rm=TRUE)), 1))

    clab=c()
    i <- 0
    while(i <= max(zlab)){
        clab <- c( clab, c(1,2,5)*10**i )
        i <- i+1
    }
    if (plot.distrib)  clab <- c(clab[log10(clab) <= max(terra::values(veg_space), na.rm=TRUE)], clab[log10(clab) > max(terra::values(veg_space), na.rm=TRUE)][1])
    zlab[2] <- log10(clab[length(clab)])

    graphics::layout(c(1,2), height = c(0.5, height-0.5))
    graphics::par(mar = c(0, 0, 0, 0), ps=8)

    ext <- plot_map_eqearth(veg_space, ext, zlim = zlab,
                     brks.pos=log10(clab), brks.lab=clab,
                     col=col,
                     title='Number of unique species occurences per grid cell')

    if(save) {
        if(((ext[4]-ext[3])/(ext[2]-ext[1]) - (height-0.5)/width > 0.05) | ((ext[4]-ext[3])/(ext[2]-ext[1]) - (height-0.5)/width < -0.05 )) {
            cat('SUGGEST: Using height =', round(width*(ext[4]-ext[3])/(ext[2]-ext[1])+0.5, 2), 'would get rid of all the white spaces.\n')
        }
        grDevices::dev.off()
    }

    if (!plot.distrib)  {
        warning('No data were available for plotting.\n')
    }

    invisible(distributions)
}
