library(soilDB)

df1 <- read.table("GA_SHBB_Suitability.txt", sep = "|", header = T)
df2 <- read.table("FL_SHBB_Suitability.txt", sep = "|", header = T)


df <- rbind(df1, df2)
df.sub <- df[df$Area_Symbol %in% c("GA601","GA299",'GA611',"GA616","GA602","FL003"),]

spatial <- fetchSDA_spatial(df.sub$Mukey, add.fields = 'muname')
