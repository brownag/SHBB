# growing degree day comparison
library(sp)
library(raster)
library(rnoaa)
library(stringdist)
setwd("E:/workspace/SHBB")

# load spatial lcoation of stations from ghcnd metadata 
# (ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt)
all.stations <- read.csv("ghcnd-stations.csv", stringsAsFactors = F)

# ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/supplemental/products/agricultural/
gddbase50 <- read.table('ann-grdd-base50.txt')
gddbase50_86 <- read.table('ann-grdd-tb5086.txt')

# several stations in here
noaa.isd.stations <- rnoaa::isd_stations()

# R package source of ghcnd
noaa.ghncd.stations <- rnoaa::ghcnd_stations()
noaa.ghncd.stations$gddbase50 <- as.numeric(gsub("[A-Z]","",gddbase50$V2[match(noaa.ghncd.stations$id, gddbase50$V1)]))
noaa.ghncd.stations$gddbase50_86 <- as.numeric(gsub("[A-Z]","",gddbase50_86$V2[match(noaa.ghncd.stations$id, gddbase50_86$V1)]))

noaa.ncdc.stations <- rnoaa::ncdc_stations()
noaa.noaa.stations <- rnoaa::noaa_stations()
noaa.buoy.stations <- rnoaa::buoy_stations()

# load bob's GDD calibration dataset
dobos.stations <- read.csv("dobos_updated_ffd_maat_gdd.csv", stringsAsFactors = F)

# get station names out of respective files
all.stations$station_nam <- as.character(trimws(all.stations$station_nam))
dobos.stations$STATION_NA <- as.character(trimws(dobos.stations$STATION_NA))

chunks <- soilDB::makeChunks(dobos.stations$STATION_NA)
res <- character()
for(i in 1:max(chunks)) {
  idx <- which(chunks == i)
  c.stations <- amatch(dobos.stations$STATION_NA[idx], noaa.ghncd.stations$name, maxDist = 20, )
  print(i)
  print(data.frame(dobos.stations$STATION_NA[idx], noaa.ghncd.stations$name[c.stations]))
  res <- c(res, c.stations)
}
dobos.stations$STATION_NA <- noaa.ghncd.stations$name[as.numeric(res)]
dobos.stations$ANN_GDD50_new <- noaa.ghncd.stations$gddbase50[as.numeric(res)]
dobos.stations$ANN_GDD5086 <- noaa.ghncd.stations$gddbase50_86[as.numeric(res)]

# determine stations with exact match to name in Bob's file (there are some discrepancies...)
idx.in <- match(dobos.stations$STATION_NA, all.stations$station_nam)
stations.sub <- all.stations[na.omit(idx.in),]
in.idx <- match(stations.sub$station_nam, dobos.stations$STATION_NA)
dobos.sub <- dobos.stations[na.omit(in.idx),]

# transfer lat/lng from metadata to Bob's calibration dataset
dobos.sub$lat <- stations.sub[stations.sub$station_nam %in% dobos.sub$STATION_NA,]$lat
dobos.sub$long <- stations.sub[stations.sub$station_nam %in% dobos.sub$STATION_NA,]$long

# promote to spdf
coordinates(dobos.sub) <- ~ long + lat

# visually inspect station locations in calibrationd dataset
maps::map(database="usa")
points(dobos.sub)

# extract gdd raster values at station locations
gdd_raster <- raster::raster("C:/Geodata/project_data/MUSum_PRISM/gdd_mean_800m.tif")
dobos.sub$raster_gdd <- extract(gdd_raster, dobos.sub)

# note that GDD raster is in FARENHEIT days
# slight differences in slope may be result of differences in determination of growing thresholds at high and low temperatures
dobos.sub$raster_gdd_Fday <- dobos.sub$raster_gdd * 9/5

# compare observed versus NASIS calculation
fm <- dobos.sub$ANN_GDD50 ~ dobos.sub$Estimated_GDD
plot(fm, xlim=c(0,10000), ylim=c(0,10000), xlab="Observed GDD (F-days)", ylab="Estimated GDD (F-days)")
m <- lm(fm)
text(1000, 8000, paste("R^2:", round(summary(m)$r.squared, 2)))
abline(m, col="BLUE")
abline(0, 1, col="RED")

# compare raster (F days) versus observed
fm <- dobos.sub$raster_gdd_Fday ~ dobos.sub$ANN_GDD50
plot(fm, xlim=c(0,10000), ylim=c(0,10000), xlab="Raster GDD (F-days)", ylab="Observed GDD (F-days)")
m <- lm(fm)
text(1000, 8000, paste("R^2:", round(summary(m)$r.squared, 2)))
abline(m, col="BLUE")
abline(0, 1, col="RED")

# compare raster (F days) versus NASIS calculation
fm <- dobos.sub$raster_gdd_Fday ~ dobos.sub$Estimated_GDD
plot(fm, xlim=c(0,10000), ylim=c(0,10000), xlab="Raster GDD (F-days)", ylab="Estimated GDD (F-days)")
m <- lm(fm)
text(1000, 8000, paste("R^2:", round(summary(m)$r.squared, 2)))
abline(m, col="BLUE")
abline(0, 1, col="RED")

offset <- dobos.sub$raster_gdd_Fday - predict(m, data.frame(ANN_GDD50=dobos.sub$ANN_GDD50))
idx <- which(offset > 2000)
plot(density(offset[idx], na.rm=T))
plot(dobos.sub[idx,])

idx <- which(offset < -500)
plot(density(offset[idx], na.rm=T))
plot(dobos.sub[idx,])

offset <- dobos.sub$raster_gdd_Fday - predict(m, data.frame(ANN_GDD50=dobos.sub$ANN_GDD5086))
idx <- which(offset > 2000)
plot(density(offset[idx], na.rm=T))
plot(dobos.sub[idx,])

idx <- which(offset < -500)
plot(density(offset[idx], na.rm=T))
plot(dobos.sub[idx,])

fm <- dobos.sub$ANN_GDD50_new ~ dobos.sub$ANN_GDD50
plot(fm, xlim=c(0,10000), ylim=c(0,10000), xlab="From File (F-days)", ylab="New From NOAA (F-days)")
m <- lm(fm)
text(1000, 8000, paste("R^2:", round(summary(m)$r.squared, 2)))
abline(m, col="BLUE")
abline(0, 1, col="RED")

fm <- dobos.sub$raster_gdd_Fday ~ dobos.sub$ANN_GDD5086
plot(fm, xlim=c(0,10000), ylim=c(0,10000), xlab="Raster GDD (F-days)", ylab="Estimated GDD (F-days)")
m <- lm(fm)
text(1000, 8000, paste("R^2:", round(summary(m)$r.squared, 2)))
abline(m, col="BLUE")
abline(0, 1, col="RED")