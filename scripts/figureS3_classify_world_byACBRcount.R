###script to extract the data from ResW and get the number of sinks that each cell in the world has in Antarctica. Also by ACBR, how many sinks in each ACBR

#So then the world can be classified by number of ACBRs with sinks. 
# figure S3

library(raster)
library(foreign)

acbr_id=read.dbf("C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/rData/cell_id_acbr_id.dbf")


acbr_id=acbr_id[,c("grid_code", "acbr_ID")]
names(acbr_id)[1]="A_id"
head(acbr_id)
tail(acbr_id)

objectsPath="C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/results/resW_rData"
listofWres=list.files(path=objectsPath, pattern = "res")

resCountWorld=list()
thr=2.4256


load(file = paste(objectsPath, listofWres[1], sep="/"), verbose = TRUE) #loads resW_for
for (j in 1:16)
{
  selCells=acbr_id[acbr_id$acbr_ID==j,1]
  subres_count = apply(resW_for[,selCells], 1, function(x)
    sum(x < thr))
  resCountWorld[[j]]=subres_count
  
}



for (i in 2:length(listofWres))
{
  load(file = paste(objectsPath, listofWres[i], sep="/"), verbose = TRUE) #loads resW_for
  for (j in 1:16)
  {
    selCells=acbr_id[acbr_id$acbr_ID==j,1]
    subres_count = apply(resW_for[,selCells], 1, function(x)
      sum(x < thr))
    resCountWorld[[j]]=c(resCountWorld[[j]],subres_count)
    
    
    
  }
  
}


str(resCountWorld)

names(resCountWorld)=paste("ACBR",1:16,sep="_")
df_CountWorld=data.frame(resCountWorld)
head(df_CountWorld)

acbrCount=apply(df_CountWorld, 1, function(x) sum(x > 1))
table(acbrCount)
hist(acbrCount)
total_count=apply(df_CountWorld, 1, sum)

df_CountWorld$acbrCount=acbrCount
df_CountWorld$total=total_count

head(df_CountWorld)
dim(df_CountWorld)
write.csv(df_CountWorld, "C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/results/World_ACBRcount_class.csv", row.names = TRUE)

df_CountWorld=read.csv( "C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/results/World_ACBRcount_class.csv")
dfResW=read.csv("C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/results/dfResW.csv")
head(dfResW)
dfWorldACBR=cbind(dfResW[,1:3], df_CountWorld)

head(dfWorldACBR)
names(dfWorldACBR)[1]="XX"

library(ggplot2)
library(colorspace)

pal <-choose_palette()
pal(17) 

col2rgb(rev(c("black", "#8A1923", "#91272F", "#97343A", "#9E3F44", "#A54B4F", "#AC565A", "#B36266", "#BB6E71", "#C27B7E", "#CA888B", "#D29698", "#DBA6A8", "#E4B7B9", "#EECCCD", "#FEEDED","snow2")))

ggplot(data=dfWorldACBR, aes(x=x.world,y=y.world))+
  geom_raster(aes(fill=as.factor(acbrCount)))+
  scale_fill_manual(values=rev(c("black", "#8A1923", "#91272F", "#97343A", "#9E3F44", "#A54B4F", "#AC565A", "#B36266", "#BB6E71", "#C27B7E", "#CA888B", "#D29698", "#DBA6A8", "#E4B7B9", "#EECCCD", "#FEEDED","snow2")), name="Number of \nACBRs with CAs")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_fixed(ratio=1.3)+
  guides(fill=guide_legend(ncol=17))+
  theme(legend.position="bottom")+
  scale_x_continuous(breaks=seq(-120, 120, 60), name = "longitude", limits=c(-180,180))+
  scale_y_continuous(breaks=seq(-60, 90, 30), name = "latitude", limits=c(-60,90))

ggsave("C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/figures/world_source_Class.tiff",dpi=300,width = 29.7,height=21,units = "cm")


ggplot(data=dfWorldACBR, aes(x=x.world,y=y.world))+
  geom_raster(aes(fill=as.factor(acbrCount)))+
  scale_fill_manual(values=rev(c("black", "#8A1923", "#91272F", "#97343A", "#9E3F44", "#A54B4F", "#AC565A", "#B36266", "#BB6E71", "#C27B7E", "#CA888B", "#D29698", "#DBA6A8", "#E4B7B9", "#EECCCD", "#FEEDED","snow2")), name="Number of \nACBRs with CAs")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_fixed(ratio=1.3)+
  #guides(fill=guide_legend(ncol=11))+
  theme(legend.position="right")+
  scale_x_continuous(breaks=seq(-120, 120, 60), name = "", limits=c(-180,180))+
  scale_y_continuous(breaks=seq(-60, 90, 30), name = "", limits=c(-60,90))

ggsave("C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/figures/world_source_Class_ddi_legend.tiff",dpi=300,width = 230,height=168,units = "mm")



ggplot(data=dfWorldACBR, aes(x=x.world,y=y.world))+
  geom_raster(aes(fill=as.factor(acbrCount)))+
  scale_fill_manual(values=rev(pal(17)), name="Number of \nACBRs with CAs")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_fixed(ratio=1.3)+
  guides(fill=guide_legend(ncol=11))+
  theme(legend.position="bottom")+
  scale_x_continuous(breaks=seq(-120, 120, 60), name = "", limits=c(-180,180))+
  scale_y_continuous(breaks=seq(-60, 90, 30), name = "", limits=c(-60,90))

ggsave("C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/figures/world_source_Class_ddi_fig1_bw.tiff",dpi=300,width = 168,height=230,units = "mm")



w_eucl=raster("C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/results/resW_Raster/raster_W_Min..tif")

world_count_raster=rasterize(x=dfWorldACBR[,2:3], y=w_eucl, field=dfWorldACBR$acbrCount)

writeRaster(world_count_raster, "C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/results/world_count_ACBR.tiff", format="GTiff")

world_count_raster=raster("C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/results/world_count_ACBR.tif")

w_raster=stack(world_count_raster,w_eucl)

countMin=as.data.frame(rasterToPoints(w_raster))

head(countMin)
names(countMin)[3:4]=c("acbrCount","eucl_min")
ggplot(data=countMin[countMin$acbrCount>=1,], aes( x=eucl_min))+
  geom_boxplot(aes(y=as.factor(acbrCount),fill=as.factor(acbrCount), outlier.fill=as.factor(acbrCount)),outlier.shape = 21, outlier.alpha=0.5)+
  scale_fill_manual(values=rev(c("black", "#8A1923", "#91272F", "#97343A", "#9E3F44", "#A54B4F", "#AC565A", "#B36266", "#BB6E71", "#C27B7E", "#CA888B", "#D29698", "#DBA6A8", "#E4B7B9", "#EECCCD", "#FEEDED")), name="Number of \nACBRs as sink")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  stat_smooth(data=countMin[countMin$acbrCount>=1,],aes(y=acbrCount), method = "lm",  fullrange = TRUE, colour="red", size=1)+
  labs(title="Relationship between classification and Euclidean distance",y="Count of ACBR with Climate Analogue under threshold", x="Minimum Euclidean distance with Antarctica")

ggsave("results/bxplot_min_Class.tiff",dpi=300,width = 29.7,height=21,units = "cm")

mod=lm(countMin[countMin$acbrCount>=1,"acbrCount"]~countMin[countMin$acbrCount>=1,"eucl_min"])
summary(mod)

# Call:
#   lm(formula = countMin[countMin$acbrCount >= 1, "acbrCount"] ~ 
#        countMin[countMin$acbrCount >= 1, "eucl_min"])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.1345  -2.0837  -0.8856   1.7977   9.5323 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                   18.37638    0.02314   794.1   <2e-16 ***
#   countMin[countMin$acbrCount >= 1, "eucl_min"] -6.61998    0.01445  -458.1   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.945 on 66572 degrees of freedom
# Multiple R-squared:  0.7592,	Adjusted R-squared:  0.7592 
# F-statistic: 2.099e+05 on 1 and 66572 DF,  p-value: < 2.2e-16


