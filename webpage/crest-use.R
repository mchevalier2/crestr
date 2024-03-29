library(crestr)


dat <- rio::import("/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/crest-use.csv")


dat$Continent <- as.factor(dat$Continent)

dat.unique.site <- unique(dat[, -c(1, 2)])
dat.unique.study <- unique(dat[, c(1, 2)])

timecov <- rep(0, max(dat.unique.site[, 'End']) / 100)
pos <- 1
for(t in seq(50, max(dat.unique.site[, 'End']), by=100)) {
    timecov[pos] <- sum((dat.unique.site[, 'End'] > t) & (dat.unique.site[, 'Start'] < t))
    pos <- pos + 1
}

timecov <- cbind(seq(1, max(dat.unique.site[, 'End']), by=100), timecov)


sites <- sp::SpatialPointsDataFrame(coords = dat[, c('Longitude', 'Latitude')], data = dat,
                               proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
sites.eqearth=sp::spTransform(sites, sp::CRS("+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))



max_year=2023
png("/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/crest-use-01.png", width=8, height=2.5, units='in', res=150)  ;  {
    layout(matrix(c(1,2), ncol=2, byrow=TRUE), width=1, height=1)

    par(mar=c(2,4,2,1), cex=1, ps=8, xaxs='i', yaxs='i', cex.axis=0.9)
    plot(0, 0, type='n', frame=FALSE, axes=FALSE, xlim=c(2013, max_year+1.1), ylim=c(0, max(table(dat.unique.study[, 'Year'])))*1.05, main='# Studies / Year', cex.main=1.5, xlab='', ylab='')
    pos <- 2013
    for(r in 2013:max_year) {
        rect(pos+0.1, 0, pos+0.9, sum(dat.unique.study[, 'Year'] == r), col='cornflowerblue')
        pos <- pos + 1
        print(c(r, sum(dat.unique.study[, 'Year'] == r)))
    }
    par(mgp=c(0,0.5,0))
    axis(1, at=c(2013, max_year+1), labels=FALSE, tck=0)
    axis(1, at=seq(2013.5, max_year+0.5, 1), labels=2013:max_year)

    par(mgp=c(0.5,0.7,0))
    axis(2, at=seq(0, max(table(dat.unique.study[, 'Year'])), 1), las=2)
    axis(2, at=c(0, max(table(dat.unique.study[, 'Year']))), labels=FALSE, tck=0)



    par(mar=c(2,1,2,4), cex=1, ps=8, xaxs='i', yaxs='i', cex.axis=0.9)
    plot(0, 0, type='n', frame=FALSE, axes=FALSE, xlim=c(0, 6), ylim=c(0, max(table(dat.unique.site[, 'Continent']))), main='# Sites / Continent', cex.main=1.5, xlab='', ylab='')
    pos <- 0
    for(r in c('Africa', 'Asia', 'Europe', 'N. America', 'S. America', 'Oceania')) {
        rect(pos+0.1, 0, pos+0.9, sum(dat.unique.site[, 'Continent'] == r), col='goldenrod3')
        pos <- pos + 1
        print(c(r, sum(dat.unique.site[, 'Continent'] == r)))

    }
    par(mgp=c(0,0.5,0))
    axis(1, at=c(0,6), labels=FALSE, tck=0)
    axis(1, at=seq(0.5, 5.5, 1), labels=c('Africa', 'Asia', 'Europe', 'N. Amer.', 'S. Amer.', 'Oceania'))
    par(mgp=c(0.5,0.7,0))
    axis(4, at=seq(0, max(table(dat.unique.site[, 'Continent'])), 3), las=2)
    axis(4, at=c(0, max(table(dat.unique.site[, 'Continent']))), labels=FALSE, tck=0)

}  ;  dev.off()



png("/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/crest-use-02.png", width=10, height=4.5, units='in', res=150)  ;  {
    layout(matrix(c(2,1,3), ncol=3), width=c(0.95,10,1.05))
    par(mar=c(0,0,0,0), ps=8*2)
    plot_map_eqearth(NA, c(-180,180,-90,90), colour_scale=FALSE, npoints=30)
    points(sites.eqearth, pch=23, col='red', bg='beige', cex=2, lwd=1.2)
}  ;  dev.off()



png("/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/crest-use-03.png", width=8, height=4, units='in', res=150)  ;  {
    par(mar=c(2,4,2,4), cex=1, ps=8, xaxs='i', yaxs='i', cex.axis=0.9)
    layout(matrix(c(1,2), ncol=1, byrow=TRUE), width=1, height=1)

    w <- timecov[, 1] <= 50000
    plot(1,0, type='n', frame=FALSE, axes=FALSE, ylim=c(0, max(timecov[w, 2]))*1.05, xlim=c(0, 50000), main='\n# Reconstructions across time (years BP)', cex.main=1.5, xlab='', ylab='')
    polygon(c(1, timecov[w, 1],max(timecov[w, 1])), c(0,timecov[w, 2], 0))
    par(mgp=c(0,0.5,0))
    axis(1, at=seq(0, 50000, 2500), labels=seq(0, 50000, 2500))
    par(mgp=c(0.5,0.7,0))
    axis(2, at=seq(0, max(timecov[w, 2]), 2), las=2)
    axis(2, at=c(0, max(timecov[w, 2])), labels=FALSE, tck=0)


    w <- timecov[, 1] >= 1000
    plot(1,0, type='n', frame=FALSE, axes=FALSE, ylim=c(0, max(timecov[w, 2]))*1.05, xlim=range(timecov[w, 1]), log='x', main='\n# Reconstructions across time (years BP)', cex.main=1.5, xlab='', ylab='')
    polygon(c(1000, timecov[w, 1], max(timecov[w, 1])), c(0, timecov[w, 2], 0))
    par(mgp=c(0,0.5,0))
    axis(1, at=c(1001, 2000, 5000, 10000, 20000, 50000, 100000, 200000, 500000, 1000000, 2000000, 5000000, 10000000, 20000000), labels=c('1000', '2000', '5000', '10,000', '20,000', '50,000', '100,000', '200,000', '500,000', '1,000,000', '2,000,000', '5,000,000', '10,000,000', '20,000,000'))
    par(mgp=c(0.5,0.7,0))
    axis(2, at=seq(0, max(timecov[w, 2]), 2), las=2)
    axis(2, at=c(0, max(timecov[w, 2])), labels=FALSE, tck=0)

}  ;  dev.off()


png("/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/cheatsheet01.png", width=5.2, height=4.5, units='in', res=150)  ;  {
    temp_col <- pals::coolwarm(20)[(climate$Temperature-10) %/% 1]
    prec_col <- pals::kovesi.linear_bgyw_15_100_c68(25)[6:25][(climate$Precipitation-2) %/% 50 + 1]

    climate <- read.csv('/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/clim.csv')
    distribs <- read.csv('/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/distrib.csv')
    par(mfrow = c(3, 2), bg=NA)
    par(mar=c(0,0.7,1,0.7))

    for (tax in 1:6) {
        w <- which(distribs[, 'CODE'] == tax)
        plot(0, 0, type='n', axes=FALSE, frame=FALSE, xlim=c(0, 61), ylim=c(0, 30), asp=1, xaxs='i', yaxs='i')
        for (i in 1:nrow(climate)) {
          rect(climate[i, 'Lon'] - 1, climate[i, 'Lat'] - 1, climate[i, 'Lon'], climate[i, 'Lat'], col=temp_col[i], border=temp_col[i], lwd=0.2)
        }
        points(distribs[w, 1:2] - 0.5, pch=20, cex=0.7)
        rect(0,0,30,30, lwd=0.5)

        for (i in 1:nrow(climate)) {
          rect(climate[i, 'Lon'] + 30, climate[i, 'Lat'] - 1, climate[i, 'Lon'] + 31, climate[i, 'Lat'], col=prec_col[i], border=prec_col[i], lwd=0.2)
        }
        mtext(paste0('Taxon', tax), 3, line=-.3, font=2)
        points(distribs[w, 1] + 30.5, distribs[w, 2] - 0.5, pch=20, cex=0.7)
        rect(31,0, 61, 30, lwd=0.5)
    }

}  ;  dev.off()



png("/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/cheatsheet02.png", width=3, height=6, units='in', res=150)  ;  {
    par(bg=NA)
    crestr::plot_diagram(crestr::crest_ex, bars=TRUE, bar_width=0.8, xlim=c(0,20))
}  ;  dev.off()


png("/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/cheatsheet03.png", width=5, height=3, units='in', res=150)  ;  {
    par(bg=NA)
    plot(reconstr, climate = 'bio1')
}  ;  dev.off()


png("/Users/palaeosaurus/GitHub/Rpackages/crestr/webpage/cheatsheet04.png", width=5, height=2.7, units='in', res=150)  ;  {
    par(bg=NA)
    plot(reconstr, climate = 'bio12', simplify=TRUE,
            uncertainties=c(0.4, 0.6, 0.8), as.anomaly=TRUE)
}  ;  dev.off()


#-;
