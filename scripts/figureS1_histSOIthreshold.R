library(raster)
library(rgdal)

eucl_raster<-raster("results/resW_Raster/raster_W_Min..tif")


oldcoorData<-read.csv("scripts/allSpeciesSubAntPointsStatus.csv")
newcoorData<-read.csv("scripts/GrantSpecies.csv")
coorData<-rbind(oldcoorData, newcoorData)
dim(coorData)
head(coorData)
coordinates(coorData)<-~Long+Lat
head(coorData)
proj4string(coorData)  <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

Ant_eucl_Dist<-extract(eucl_raster,coorData)
coorData$EuclDist<-Ant_eucl_Dist
coorData<-coorData[which(!is.na(coorData$status)),]


library(plyr)
X2u <- ddply(coorData@data, ~ Species, summarize,
             Min=min(EuclDist, na.rm=TRUE), 
             Max=max(EuclDist, na.rm=TRUE))
X2u$diff=X2u$Max-X2u$Min
summary(X2u$diff)
hist(X2u$diff)
library(ggplot2)
soi_insects_hist=ggplot(X2u, aes(diff))
soi_insects_hist+
  #geom_freqpoly()+
  #geom_density(lwd = 2,color="blue")+
  geom_histogram(fill="#bdbdbd", color="#bdbdbd",binwidth = 0.5)+
  #geom_vline(xintercept=mean(X2u$diff),linetype="dashed", show.legend = TRUE)+
  geom_vline(xintercept=median(X2u$diff),
             color="red", show.legend = TRUE)+
  #geom_vline(xintercept=quantile(X2u$diff,0.25),color="red",show.legend = TRUE)+
  labs(title="Difference of Euclidean distance, \ninsects introduced to SOI",x="Euclidean distance range", y = "Count")+
  #scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))+
  theme_classic()+
  coord_fixed()
ggsave(filename = "figures/diff SOI insects threshold.tiff",dpi=300,width=16.8,height= 16.8, units = "cm")
