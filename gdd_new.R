#gdd new
# growing degree day comparison
library(sp)
library(raster)
library(stringdist)

# load spatial lcoation of stations from ghcnd metadata 
# (ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt)
# see also rnoaa package
all.stations <- read.csv("ghcnd-stations.csv", stringsAsFactors = F)
all.stations[,1] <- toupper(all.stations[,1])
all.stations$id <- all.stations$ï..siteid

# get recent estimates of gdd for two common methods (BASE50, BASE50)
# ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/supplemental/products/agricultural/
gddbase50 <- read.table('ann-grdd-base50.txt')
gddbase50_86 <- read.table('ann-grdd-tb5086.txt')
all.stations$gddbase50 <- as.numeric(gsub("[A-Z]", "", 
                                          gddbase50$V2[match(all.stations$id, gddbase50$V1)]))
all.stations$gddbase50_86 <- as.numeric(gsub("[A-Z]", "", 
                                             gddbase50_86$V2[match(all.stations$id, gddbase50_86$V1)]))

coordinates(all.stations) <- ~long+lat
proj4string(all.stations) <- "+proj=longlat +datum=WGS84"
plot(all.stations[!is.na(all.stations$gddbase50) & !is.na(all.stations$gddbase50_86),])

gdd_raster <- raster::raster("C:/Geodata/project_data/MUSum_PRISM/gdd_mean_800m.tif")
all.stations$raster_gdd5086 <- extract(gdd_raster, all.stations) * 9/5

fm <- raster_gdd5086 ~ gddbase50_86
plot(data=all.stations, fm, xlim = c(0,10000), ylim = c(0,10000), pch=19, cex=0.3,
     ylab="Raster GDD (BASE 50-86, F-days)", xlab="Observed GDD (BASE 50-86 F-days)")
m <- lm(fm, data=all.stations)
rss <- c(crossprod(m$residuals))
mse <- rss / length(m$residuals)
text(1000, 8000, paste("R^2:", round(summary(m)$r.squared, 3), 
                       "\nn:", sum(!is.na(m$residuals)),
                       #"\nRSS:", round(rss),
                       #"\nMSE:", round(mse),
                       "\nRMSE:", round(sqrt(mse))))#,
                       #"\nsig2:", round(rss / m$df.residual)))
abline(0,1, col="RED", lwd=2)
abline(m, col="BLUE")

fm <- raster_gdd5086 ~ gddbase50
plot(data=all.stations, fm, xlim = c(0,10000), ylim = c(0,10000), pch=19, cex=0.3,
     ylab="Raster GDD (BASE 50-86, F-days)", xlab="Observed GDD (BASE 50 F-days)")
m <- lm(fm, data=all.stations)
rss <- c(crossprod(m$residuals))
mse <- rss / length(m$residuals)
text(1000, 8000, paste("R^2:", round(summary(m)$r.squared, 3), 
                       "\nn:", sum(!is.na(m$residuals)),
                       #"\nRSS:", round(rss),
                       #"\nMSE:", round(mse),
                       "\nRMSE:", round(sqrt(mse))))#,
#"\nsig2:", round(rss / m$df.residual)))
abline(0,1, col="RED", lwd=2)
abline(m, col="BLUE")

## OK, now try to line these results up with Bob's dataset

# load bob's GDD calibration dataset
dobos.stations <- read.csv("dobos_updated_ffd_maat_gdd.csv", stringsAsFactors = F)

# get station names out of respective files
all.stations$station_nam <- as.character(trimws(all.stations$station_nam))
dobos.stations$STATION_NA <- as.character(trimws(dobos.stations$STATION_NA))

chunks <- soilDB::makeChunks(dobos.stations$STATION_NA)
res <- character()
for(i in 1:max(chunks)) {
  idx <- which(chunks == i)
  c.stations <- amatch(dobos.stations$STATION_NA[idx], all.stations$station_nam, maxDist = 20, )
  print(i)
  print(data.frame(dobos.stations$STATION_NA[idx], all.stations$station_nam[c.stations]))
  res <- c(res, c.stations)
}
res <- as.numeric(res)
dobos.stations$new_station_name <- all.stations$station_nam[res]
dobos.stations$ANN_GDD50_new <- all.stations$gddbase50[res]
dobos.stations$ANN_GDD5086 <- all.stations$gddbase50_86[res]

dobos.anomalies <- dobos.stations[dobos.stations$STATION_NA %in% all.stations$station_nam,]
dobos.anomalies$long <- all.stations[match(dobos.anomalies$STATION_NA, all.stations$station_nam),]$long
dobos.anomalies$lat <- all.stations[match(dobos.anomalies$STATION_NA, all.stations$station_nam),]$lat

coordinates(dobos.anomalies) <- ~ long + lat
is.above <- which((dobos.anomalies$ANN_GDD50_new - dobos.anomalies$ANN_GDD50) > sd(m$residuals))
is.below <- which((dobos.anomalies$ANN_GDD50_new - dobos.anomalies$ANN_GDD50) < -sd(m$residuals))

fm <- ANN_GDD50 ~ ANN_GDD50_new
plot(data=dobos.anomalies, fm)
m <- lm(fm, data=dobos.anomalies)
# identify points where deviation is greater than 1 standard deviation of linear model residuals
idx <-  which((dobos.anomalies$ANN_GDD50_new - dobos.anomalies$ANN_GDD50) > sd(m$residuals))
points(data=dobos.anomalies[idx,], fm, xlim=c(0,10000), ylim=c(0,10000), col="RED")
idx <-  which((dobos.anomalies$ANN_GDD50_new - dobos.anomalies$ANN_GDD50) < -sd(m$residuals))
points(data=dobos.anomalies[idx,], fm, xlim=c(0,10000), ylim=c(0,10000), col="BLUE")

maps::map("usa")
points(dobos.anomalies[is.above,], pch=19, cex=0.5, col="RED")
points(dobos.anomalies[is.below,], pch=19, cex=0.5, col="BLUE")

write.csv(dobos.anomalies, file = "dobos_stations_anomaly.csv")
