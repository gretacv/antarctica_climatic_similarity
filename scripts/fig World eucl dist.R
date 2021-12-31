### figure for CED of the world using as threshold the median of CED in SOI species
library(raster)
library(ggplot2)


w_eucl=raster("results/resW_Raster/raster_W_Min..tif")
dfResW=read.csv("results/dfResW.csv")
head(dfResW)

algoPal <- colorRampPalette(c("#b2182b","#f7f7f7","#2166ac"))
g=ggplot(data=dfResW, aes(x=x.world,y=y.world))
g+
  geom_raster(aes(fill=W_Min.))+
  # scale_fill_gradient(low = "#b2182b", high = "#2166ac")
  #scale_fill_gradientn(colours=algoPal(3), breaks=c(0,2.4256,max(dfResW$W_Min.)))
  # 
  # scale_fill_manual(values=c("#b2182b","#f7f7f7", "#2166ac"), breaks=c(0, 2.4256, max(dfResW$W_Min.))
  # scale_fill_gradientn(colours = terrain.colors(10))
  scale_fill_gradient2(low="#b2182b", mid="#b2182b", high="#2166ac", midpoint = 2.4256, limits = c(0, max(dfResW$W_Min.)), breaks=c(0,2.4256,max(dfResW$W_Min.)))


gg=ggplot(transform(dfResW, rasterColor=cut(W_Min., c(0,2.4256,max(dfResW$W_Min.)))), aes(x=x.world,y=y.world))
gg+
  geom_raster(aes(fill=rasterColor))+
  scale_fill_manual(values=c("#b2182b","#67a9cf"), name="Euclidean Distance", label=c("< Threshold", "> Threshold"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_fixed(ratio=1.3)+
  ggtitle("Minimum Climatic Euclidean Distance to Antarctica")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(-120, 120, 60), name = "longitude", limits=c(-180,180))+
  scale_y_continuous(breaks=seq(-60, 90, 30), name = "latitude", limits=c(-60,90))
ggsave("figures/WorldEuclideanDistance.tiff",dpi=300,width = 29.7,height=21,units = "cm")


gg2=ggplot(transform(dfResW, rasterColor=cut(W_Min., c(0,0.5,1,1.5,2,2.4256,max(dfResW$W_Min.)))), aes(x=x.world,y=y.world))
gg2+
  geom_raster(aes(fill=rasterColor))+
  scale_fill_manual(values=c("#67001f","#b2182b", "#d6604d", "#f4a582", "#fddbc7","#E1E1E1"), name="Euclidean Distance", label=c("0 - 0.5", "0.5 - 1", "1 - 1.5", "1.5 - 2", "2 - Threshold", "> Threshold"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_fixed(ratio=1.3)+
  ggtitle("Minimum Climatic Euclidean Distance to Antarctica")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(-120, 120, 60), name = "longitude", limits=c(-180,180))+
  scale_y_continuous(breaks=seq(-60, 90, 30), name = "latitude", limits=c(-60,90))


ggsave("figures/WorldEuclideanDistance_scale.tiff",dpi=300,width = 29.7,height=21,units = "cm")


