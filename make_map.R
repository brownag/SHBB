library(rgdal)
library(soilDB)

df1 <- read.table("GA_SHBB_Suitability.txt", sep = "|", header = T)
df2 <- read.table("FL_SHBB_Suitability.txt", sep = "|", header = T)
#df <- (rbind(df1, df2))
df <- read.table("GA_SHBB_Suitability.txt",sep="|",header=T)
df$mukey_char <- as.character(df$Mukey)
df.na <- df[1,]
df.na[,1:8] <- NA
df.na$mukey_char <- "a"
df <- rbind(df.na, df)
write.csv(df, file='GA-FL_SHBB_Suitability.csv')
          
df.sub <- df[df$Area_Symbol %in% c("GA602"),]
spatial <- readOGR("GA602.shp")
spatial@data <- spatial@data[,1:3]
#spatial <- fetchSDA_spatial(df.sub$Mukey, add.fields = 'muname', chunk.size = 1)
#spatial.c <- spatial
spatial@data <- merge(spatial@data, df.sub, by.x="mukey", by.y="Mukey", sort=F)

spatial$shbb_suit <- "Poorly Suited"
spatial$shbb_suit[spatial$Overall_Index > 0.01 & spatial$Overall_Index < 1] <- "Suited"
spatial$shbb_suit[spatial$Overall_Index == 1] <- "Well Suited"
spatial$shbb_suit[spatial$Overall_Index == 50] <- "Water"

rgdal::writeOGR(spatial, dsn=".", layer="GA602", driver="ESRI Shapefile", overwrite_layer = T)


all.spatial <- readOGR("soils_GSSURGO_ga_3801232_01/soils/gssurgo_g_ga/gSSURGO_GA.gdb", layer = "MUPOLYGON")
all.spatial <- rbind(all.spatial, readOGR("soils_GSSURGO_fl_3801232_01/soils/gssurgo_g_fl/gSSURGO_FL.gdb", layer = "MUPOLYGON"))
all.spatial.c <- all.spatial
all.spatial@data <- merge(all.spatial@data, df, by.x="MUKEY", by.y="Mukey", all.x=TRUE, sort=F)

rgdal::writeOGR(all.spatial, dsn=".", layer="GA", driver="ESRI Shapefile", overwrite_layer = T)
