
library(ggplot2)
library(dplyr)
library(rgdal)

setwd("C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/rData")
Antarctica_shp =readOGR(dsn = ".", layer = "Antarctica_land_wgs84")
Antarctica <- fortify(Antarctica_shp, region="gid")
head(Antarctica )


ggplot(Antarctica)+
aes(long, lat, group=group) +
geom_polygon(fill="white")+
scale_y_continuous(breaks=c(-45,-60,-75,-90))+
scale_x_continuous(breaks=seq(-180,180,30))+
theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+
theme(panel.background = element_rect(fill = "#E1E1E1",
                                colour = "#E1E1E1",
                                size = 0.5, linetype = "solid"), panel.border = element_blank(),panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"))+
coord_polar(start=pi)

ggsave("antarctica.tiff",dpi=300,width = 3,height=3,units = "cm")


soi=readOGR(dsn = ".", layer = "SOI_land_wgs84")
soi<- fortify(soi, region="LOCATION")
ggplot(soi)+
aes(long, lat, group=group) +
geom_polygon(colour="black")+
geom_polygon(data=Antarctica, fill="white")+
scale_y_continuous(breaks=c(-30,-45,-60,-75,-90))+
scale_x_continuous(breaks=seq(-180,180,30))+
theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+
theme(panel.background = element_rect(fill = "#E1E1E1",
                                colour = "#E1E1E1",
                                size = 0.5, linetype = "solid"), panel.border = element_blank(),panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"))+
coord_polar(start=pi)


ggsave("soi_antarctica.tiff",dpi=300,width = 8,height=8,units = "cm")


 
