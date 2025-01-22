library(crestr)
library(raster)


FINAL <- TRUE




# Figure projected in eqearthson (rounded sides)
ext <- c(-180, 180, -90, 90)
nlines <- 9
npoints <- 30
PROJ=sp::CRS("+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

utils::data(M1, package='crestr', envir = environment())
M1 <- terra::unwrap(M1)
M1 <- terra::crop(M1, ext)
M2 <- terra::project(M1, PROJ)


idx=1
verticals <- cbind(id = NA, part = NA, lon = NA, lat = NA)
for(i in seq(ext[1], ext[2], length.out=nlines)){
    verticals <- rbind(verticals,
                       cbind(id = 1, part = idx,
                             lon = rep(i, npoints),
                             lat = seq(ext[3], ext[4], length.out=npoints))
                       )
    idx = idx + 1
}
verticals <- verticals[-1, ]
verticals <- terra::vect(verticals, type="lines", crs="+proj=longlat +datum=WGS84 +no_defs")
verticals.eqearth <- terra::project(verticals, PROJ)
verticals.eqearth.x <- sort((terra::crds(verticals.eqearth, df=TRUE)$x)[seq(1, npoints*nlines, by=npoints) ])

idx=1
horizontals <- cbind(id = NA, part = NA, lon = NA, lat = NA)
for(i in seq(ext[3], ext[4], length.out=nlines)){
    horizontals <- rbind(horizontals,
                         cbind(id = 1, part = idx,
                               lon = seq(ext[1], ext[2], length.out=npoints),
                               lat = rep(i, npoints))
                         )
    idx = idx + 1
}
horizontals <- horizontals[-1, ]
horizontals <- terra::vect(horizontals, type="lines", crs="+proj=longlat +datum=WGS84 +no_defs")
horizontals.eqearth <- terra::project(horizontals, PROJ)
horizontals.eqearth.y <- sort((terra::crds(horizontals.eqearth, df=TRUE)$y)[seq(1, npoints*nlines, by=npoints)])
horizontals.eqearth.xy <- (terra::crds(horizontals.eqearth, df=TRUE))[seq(1, npoints*nlines, by=npoints), ]


bckg <- cbind(id = 1, part = 1,
              lon = c(rep(ext[1], npoints), rep(ext[2], npoints)),
              lat = c(seq(ext[3], ext[4], length.out=npoints), seq(ext[4], ext[3], length.out=npoints)))
bckg <- terra::vect(bckg, type="polygons", crs="+proj=longlat +datum=WGS84 +no_defs")
bckg.eqearth <- terra::project(bckg, PROJ)



TAXA_DENSITIES <- list()
TAXA_NAMES <- c('Plants', '', '', 'Foraminifers', 'Diatoms', 'Mammals')
TAXA_SIZES <- rep(0, 6)
for(i in c(1, 6, 4, 5)) {
    print(c(i, TAXA_NAMES[i]))
    dat <- dbRequest(paste0("
                        WITH t_locids AS (
                            SELECT locid, count(*) as total
                            FROM distrib_qdgc
                            WHERE taxonid < ",i+1,"000000 AND taxonid > ",i,"000000
                            GROUP BY locid
                        )

                        SELECT t_locids.locid, longitude, latitude, total
                        FROM t_locids
                        LEFT JOIN data_qdgc USING (locid)"
                    ),
                    dbname='/Users/palaeosaurus/Research/GBIF/gbif4crest_03.sqlite3')
    #dat[, 3] <- as.numeric(dat[, 3])
    TAXA_SIZES[i] <- sum(dat[, 4], na.rm=TRUE)
    ##dat2 <- raster::rasterFromXYZ(dat[, -1], res=c(1/12, 1/12), crs=sp::CRS("+init=epsg:4326"))
    dat2 <- terra::rast(dat[, -1], type="xyz")
    terra::crs(dat2) = "+proj=longlat +datum=WGS84 +no_defs"
    dat2 <- terra::aggregate(dat2, fun="sum", fact=12, na.rm=TRUE)
    if(i==1)  terra::values(dat2)[terra::values(dat2) <= 2] <- NA ## Just for plants
    if(i==6)  terra::values(dat2)[terra::values(dat2) <= 2] <- NA ## Just for mammals
    terra::values(dat2) <- log10(terra::values(dat2))
    dat2 <- terra::project(dat2, PROJ)
    TAXA_DENSITIES[[TAXA_NAMES[i]]] <- terra::mask(dat2, bckg.eqearth )
}
#TAXA_DENSITIES[['Rodents']] <- TAXA_DENSITIES[[5]]


ext <- c(-17500000, 17500000, -8400000,  8400000)

for(loc in "/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/fig-gbif_v3.png") {
    png(loc, width=17.5, height=11.5, units='cm', res=600) ; {
        layout(matrix(c(13,1:6,13, 7:12),
                    ncol=2, byrow=FALSE), width=1, height=c(0.3, rep(c(0.15, 1.75, 0.4), 3)))

        par(mar=c(0,0,0,0), ps=8*3/2)

        max_densities <- lapply(TAXA_DENSITIES, function(x) return(ceiling(10*max(raster::values(x), na.rm=TRUE))/10))

        for(i in c(1,4,6,5)) {
            max_dens = max_densities[[TAXA_NAMES[i]]]

            plot(0,0, type='n', xlim=c(0,1), ylim=c(0,1),
                main='', xlab='', ylab='', xaxs='i', yaxs='i',
                frame=FALSE, axes=FALSE)
            text(0.5, 0.45, paste0(TAXA_NAMES[i], ' (N=', prettyNum(TAXA_SIZES[i], big.mark = ","),')'), adj=c(0.5, 0.5), font=2, cex=1)

            colour_scale <- viridis::inferno(23)[4:23]

            plot(0, 0, type='n',
                 xlim=c(ext[1], ext[2]), ylim=c(ext[3], ext[4]),
                 main='', ylab='', xlab='', xaxs='i', yaxs='i',
                 asp=1, frame=FALSE, axes=FALSE)  ;  {

                terra::plot(bckg.eqearth, col='grey80', border=NA, cex=0.2, add=TRUE)
                terra::plot(horizontals.eqearth,col='white',lwd=0.5, add=TRUE )
                terra::plot(verticals.eqearth,col='white',lwd=0.5, add=TRUE )

                terra::plot(M2, col='black', border=NA, add=TRUE)
                terra::plot(TAXA_DENSITIES[[TAXA_NAMES[i]]], add=TRUE, zlim=c(0, max_dens), col=colour_scale, border=colour_scale, legend=FALSE)

                sp::plot(bckg.eqearth, col=NA, border='black', cex=0.2, add=TRUE)
            }

            plot(0,0, type='n', xlim=c(-0.37, 1.37), ylim=c(0,1),
                main='', xlab='', ylab='', xaxs='i', yaxs='i',
                frame=FALSE, axes=FALSE)

            for(j in 1:100) {
                rect((j-1)/100, 0.95, j/100, 0.65, col=viridis::inferno(115)[j+15], border=viridis::inferno(115)[j+15])
            }
            rect(0, 0.95, 1, 0.65, lwd=0.5)

            axs_lab <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000)
            axs_lab <- axs_lab[axs_lab <= 10**max_dens]
            plotted_lab <- rep(FALSE, length(axs_lab))
            for(j in 1:length(axs_lab)){
                segments(log10(axs_lab[j])/max_dens, 0.65, log10(axs_lab[j])/max_dens, 0.6, lwd=0.5)
                if(j>6 & !plotted_lab[max(1, j-1)]) {
                    if(log10(axs_lab[j-1])/max_dens + strwidth(axs_lab[j-1], cex=6/8)/2 + 0.01 < log10(axs_lab[j])/max_dens - strwidth(axs_lab[j], cex=6/8)/2) {
                        text(log10(axs_lab[j])/max_dens, 0.55, axs_lab[j], cex=6/8, adj=c(0.5, 1))
                    } else {
                        plotted_lab[j] <- TRUE
                    }
                } else {
                    text(log10(axs_lab[j])/max_dens, 0.55, axs_lab[j], cex=6/8, adj=c(0.5, 1))
                }
            }

        }

        plot(0,0, type='n', xlim=c(0,1), ylim=c(0,1),
            main='', xlab='', ylab='', xaxs='i', yaxs='i',
            frame=FALSE, axes=FALSE)
        text(0.5, 0.65, 'Number of occurrence data per grid cell for four common biological climate proxies', adj=c(0.5, 0.5), font=2, cex=9/8)

    }  ;  dev.off()
}
