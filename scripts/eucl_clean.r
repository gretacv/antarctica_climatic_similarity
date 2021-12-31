#0 load libraries
library(fields)
library(plyr)
library(raster)
library(rgdal)

#1 prepare rasters

#make sure -60 is the limit
AntPCApath<-"IceFreePCA"
pcaAnt<-list.files(AntPCApath, pattern=".tif$")
sAnt<-stack(paste(AntPCApath,pcaAnt,sep="/"))
antCrop<-c(-180, 180.0658, -90, -60)
sAnt<-crop(sAnt,antCrop)

worldPCApath<-"PCA_Variables Ambientales"
pcaWorld<-list.files(worldPCApath, pattern=".asc$")
sWorld<-stack(paste(worldPCApath, pcaWorld,sep="/"))
eCrop<-c(-179.75, 179.816, -60, 83.78404)
sWorld<-crop(sWorld,eCrop)
crs(sWorld)<-crs(sAnt)


#2 create dfW and dfA
dfW=as.data.frame(rasterToPoints(sWorld))
dfA=as.data.frame(rasterToPoints(sAnt))

row.names(dfW)=paste("W",row.names(dfW), sep="_")
names(dfW)[1:2]=c("x.world","y.world")

row.names(dfA)=paste("A",row.names(dfA), sep="_")
names(dfA)[1:2]=c("x.ant","y.ant")

Wcells=nrow(dfW)
Acells=nrow(dfA)

rm(sWorld)
gc()

#3 create Wa, which is a list that contains in each element the Antarctic Cells
Wa=list()

 
for (i in 1:Wcells)
{
  Wa[[i]]=dfA[,-c(1,2)]
  
}

#4 Calculate the euclidean distance by batches
seqStart=seq(1,Wcells,100000)
seqEnd=c(seq(100000,Wcells,100000),Wcells)

setwd("results")


for (i in 1:length(seqStart))
{
  sequenceNumbers=seq(seqStart[i], seqEnd[i], 1)
  #here we calculate the euclidean distance
  resW_for = ldply(sequenceNumbers, function(i)
    as.vector(rdist(dfW[i, -c(1, 2)], Wa[[i]])))
  
  #we name the dataframe's rows and columns
  namesA=paste("A",seq(1, Acells,1), sep="_")
  namesW=paste("W",seq(seqStart[i], seqEnd[i], 1), sep="_")
  names(resW_for)=namesA
  row.names(resW_for)=namesW
  
  #we save the portion
  rDataName=paste("resW",seqStart[i],"_",seqEnd[i],".Rdata",sep="")
  save(resW_for, file = rDataName)
  
  #we identify the smallest Euclidean distance and get the summary
  subres_index_min=apply(resW_for,1,which.min)
  subres_sum=apply(resW_for,1,summary)
  subResWbind=cbind(subres_index_min, t(subres_sum))
  
  #we save the portion
  subResName=paste("subResWbind",seqStart[i],"_",seqEnd[i],".Rdata",sep="")
  save(subResWbind, file = subResName)
  
}

rm(Wa)
gc()

#5 create Aw, which is a list that contains in each element the World Cells

Aw=list()
for (i in 1:Acells)
{
  Aw[[i]]=dfW[,-c(1,2)]
  
}


#6 Calculate the euclidean distance by batches
seqStart=seq(1,Acells,1700)
seqEnd=c(seq(1700,Acells,1700),Acells)

# setwd()

thr=4.99

for (i in 1:length(seqStart))
{
  sequenceNumbers=seq(seqStart[i], seqEnd[i], 1)
  #here we calculate the euclidean distance
  
  
  resA_for = ldply(sequenceNumbers, function(i)
    as.vector(rdist(dfA[i, -c(1, 2)], Aw[[i]])))
  
  #we name the dataframe's rows and columns
  namesA=paste("A",seq(seqStart[i], seqEnd[i],1), sep="_")
  namesW=paste("W",seq(1, Wcells, 1), sep="_")
  names(resA_for)=namesW
  row.names(resA_for)=namesA
  
  #we save the portion
  rDataName=paste("resA",seqStart[i],"_",seqEnd[i],".Rdata",sep="")
  save(resA_for, file = rDataName)
  
  #load resA_for
  
  #we count how many cells are under the threshold and their summary, also the summary for all the cells
  subres_count=apply(resA_for, 1, function(x) sum(x<thr))
  subres_sum_thr=apply(resA_for,1,function(x) summary(x[x<thr]))
  subres_sum=apply(resA_for,1,summary)
  subResbind=cbind(subres_count, t(subres_sum_thr), t(subres_sum))
  names(subResbind)=c("count_CAs",
                      "thr_min","thr_1Qu","thr_median","thr_mean","thr_3Qu","thr_max",
                      "all_min","all_1Qu","all_median","all_mean","all_3Qu","all_max")
  
  #we save the portion
  subResName=paste("subResAbind",seqStart[i],"_",seqEnd[i],".Rdata",sep="")
  save(subResbind, file = subResName)
  
}

rm(Aw)
gc()

##getting subrResbind with different threshold.
setwd("results")


listOfRobjects=list.files(pattern="Rdata$")
listOfRobjects=listOfRobjects[c(1,3,4,5,6,7,2)]
#check order of files

thr=2.4256 #thr=2

for (i in 1:length(listOfRobjects))
{
  #load resA_for
  load(listOfRobjects[i])
  #we count how many cells are under the threshold and their summary, also the summary for all the cells
  subres_count = apply(resA_for, 1, function(x)
    sum(x < thr))
  subres_sum_thr = apply(resA_for, 1, function(x)
    summary(x[x < thr]))
  subres_sum = apply(resA_for, 1, summary)
  subResbind = cbind(subres_count, t(subres_sum_thr), t(subres_sum))
  names(subResbind) = c(
    "count_CAs",
    "thr_min",
    "thr_1Qu",
    "thr_median",
    "thr_mean",
    "thr_3Qu",
    "thr_max",
    "all_min",
    "all_1Qu",
    "all_median",
    "all_mean",
    "all_3Qu",
    "all_max"
  )
  
  #we save the portion
  fname=gsub(x =listOfRobjects[i],pattern = ".Rdata",replacement = "" )
  subResName = paste("subResAbind_", fname, "_", thr, ".Rdata", sep = "")
  save(subResbind, file = subResName)
  
  
}




#######TREATING THE RESULTS
#World
#Load results 
setwd("results")
library(raster)
rWdata=list.files(pattern="subResWbind")
rWdata
load(rWdata[1], verbose=TRUE)
rWdata=rWdata[-1]
rWdata
subresWmerge=subResWbind
for (i in rWdata)
{
  print(dim(subresWmerge))
  load(i)
  subresWmerge=rbind(subresWmerge,subResWbind)
  
}
dim(subresWmerge)
object.size(subresWmerge)

#Build the data frame with all the information
#We provide information from dfW to the results, e.g, the coordinates of each point
names(dfW)
dfResW=cbind(dfW[,1:2],subresWmerge)
row.names(dfResW)[1:10]
names(dfResW)=c("x.world","y.world", "ACell_Eucl_min", "W_Min.", "W_1st Qu.", "W_Median", "W_Mean", "W_3rd Qu.", "W_Max." )
dfResW$A_ID=paste("A", dfResW$ACell_Eucl_min, sep="_")

write.csv(dfResW, "results/dfResW.csv")

#create sink cells using table() to compare the results with ResA
dfA2=dfA[,1:2]
dfA2$A_ID=row.names(dfA2)
class(dfA2$A_ID)

sinkFromW=as.data.frame(table(dfResW$A_ID))
names(sinkFromW)=c("A_ID","Count_CAs")
sinkFromW$A_ID=as.character(sinkFromW$A_ID)

sinkFromW_complete=merge(sinkFromW,dfA2)
write.csv(sinkFromW_complete,"results/sinkFromW_complete.csv")

#create a raster for each columm
worldRaster=raster("PCA_Variables Ambientales/Comp1.asc")
eCrop<-c(-179.75, 179.816, -60, 83.78404)
eWorldRaster<-crop(worldRaster,eCrop)
crs(worldRaster)<-crs(sAnt)

setwd("results")
for(field in 3:9)
{
r=rasterize(x = dfResW[,1:2], y=worldRaster, field=dfResW[,field])
names(r)=names(dfResW)[field]
rasterName=paste("raster_", names(r), ".tif", sep="")
writeRaster(r, file=rasterName,format="GTiff")
}

#Antarctica
#Load results 
setwd("results")
library(raster)
rAdata=list.files(pattern="subResAbind")
rAdata=rAdata[c(1,3,4,5,6,7,2)]
load(rAdata[1], verbose=TRUE)
rAdata=rAdata[-1]
rAdata
subresAmerge=subResbind
for (i in rAdata)
{
  print(dim(subresAmerge))
  load(i)
  subresAmerge=rbind(subresAmerge,subResbind)
  
}
dim(subresAmerge)
object.size(subresAmerge)

#Build the data frame with all the information
#We provide information from dfA to the results, e.g, the coordinates of each point
names(dfA)
names(subresAmerge)
dfResA=cbind(dfA[,1:2],subresAmerge)

row.names(dfResA)[1:10]
head(dfResA)
names(dfResA)[-c(1,2)]=c("count_CAs_thr",
                      "thr_min","thr_1Qu","thr_median","thr_mean","thr_3Qu","thr_max",
                      "all_min","all_1Qu","all_median","all_mean","all_3Qu","all_max")



write.csv(dfResA, "results/dfResA_2.4256.csv")


#create a raster for each column
AntRaster=raster("IceFreePCA/IceFreeComp1.tif")
antCrop<-c(-180, 180.0658, -90, -60)
AntRaster<-crop(AntRaster,antCrop)

setwd("results")

dfResA=read.csv("results/dfResA_2.csv")
names(dfResA)
for(field in 4:10)
{
  keepNA=which(!is.na(dfResA[,field]))
  r=rasterize(x = dfResA[keepNA,2:3], y=AntRaster, field=dfResA[keepNA,field])
  names(r)=names(dfResA)[field]
  print(names(r))
  rasterName=paste("rasterA_", names(r), "_thr06.tif", sep="")
  writeRaster(r, file=rasterName,format="GTiff")
}

#Creating a raster with the index of A_ID
r=rasterize(x = dfResA[,1:2], y=AntRaster, field=1:Acells)
writeRaster(r, file="raster_idA.tif",format="GTiff")
r_ID=raster("results/raster_idA.tif")


aspas_coordinates=read.csv("rData/ASPAs Coordinates.csv")
coordinates(aspas_coordinates)=~long_dd + lat_dd
crs(aspas_coordinates)=CRS("+init=epsg:4326")



#for each spatial element we extract the A_ID
# for aspas, they are points, it is easy to get the value from the raster at that point
aspas_cell_id=extract(raster_ID, aspas_coordinates)
aspas_coordinates@data$A_ID=aspas_cell_id
names(aspas_coordinates)[1]="number"
write.csv(aspas_coordinates@data,"results/aspas_a_id.csv")

#for ACBRs the raster has to be transformed into points. To avoid crashing the computer it is better to go acbr by acbr
library(foreign)
#grid_code is the cell id
acbr_id=read.dbf("rData/cell_id_acbr_id.dbf")

#give this info to dfResA
head(acbr_id)
acbr_id$X=paste("A",acbr_id$grid_code,sep="_")

dfResA=read.csv("results/dfResA_499.csv")
names(dfResA)
dfResA$X=row.names(dfResA)
dfResA_acbrID=merge(dfResA,acbr_id, sort=FALSE)
dfResA_acbrID$acbr_ID=as.factor(dfResA_acbrID$acbr_ID)
head(dfResA_acbrID)
dim(dfResA_acbrID)

aspas_cellA_id=read.csv("results/aspas_a_id.csv")
dim(aspas_cellA_id)
head(aspas_cellA_id)
aspas_cellA_id$X=paste("A",aspas_cellA_id$A_ID,sep="_")

dfResA_acbr_ASPAS_ID=merge(dfResA_acbrID,aspas_cellA_id,all.x=TRUE, sort=FALSE)
dim(dfResA_acbr_ASPAS_ID)
head(dfResA_acbr_ASPAS_ID)
aspas_AcbrID=dfResA_acbr_ASPAS_ID[!is.na(dfResA_acbr_ASPAS_ID$number),]
 
dim(aspas_AcbrID)

#write dataframe with all the information of ACBRs and ASPAS
write.csv(dfResA_acbr_ASPAS_ID, "results/dfResA_acbr_ASPAS_ID_thr_2-4256.csv")
#plotting the distribution of euclidean distance by ACBRs, using violin plot. 
eucl_acbr_plot=ggplot(data=dfResA_acbr_ASPAS_ID, aes(x=acbr_ID,y=count_CAs_thr, fill=acbr_ID))

eucl_acbr_plot+
  geom_violin()+
  geom_point(data=aspas_AcbrID, mapping=aes(x=acbr_ID, y=count_CAs_thr), pch=21, fill="white")+
  theme_bw()+
  guides(fill=FALSE)+
  labs(title="Distribution of count of CAs by ACBR, threshold=4.99",x="ACBR ID", y = "Count of CAs under threshold")

ggsave("violin_2.tiff",dpi=300,width = 29.7,height=21,units = "cm")




#compare the sink results with ResW
head(sinkFromW_complete) #here we have all the sink cells, not just the ones above the threshold
sinkFromA=as.data.frame(cbind(dfA2$A_ID,dfResA$x.ant,dfResA$y.ant,dfResA$count_CAs_thr))
names(sinkFromA)=c("A_ID", "x.ant", "y.ant", "count_CAs_thr")
dim(sinkFromA)
dim(sinkFromW_complete)
comp_sink=merge(sinkFromA, sinkFromW_complete, all.x=TRUE)
names(comp_sink)[5]="Count_CAs_inW"
dim(comp_sink)

#compare the proportions of the last two columns
comp_sink$propThr=as.numeric(as.character(comp_sink$count_CAs_thr))/Wcells
comp_sink$propMin=as.numeric(comp_sink$Count_CAs_inW)/Acells
head(comp_sink[order(comp_sink$propThr),])

#use ggplot to show the difference

### Extract the euclidean value for the SOI species, try to figure out a new threshold with more biological meaning. 
#What is the range of euclidean distance for each species?


library(raster)
library(rgdal)

eucl_raster<-raster("results/raster_W_Min..tif")

oldcoorData<-read.csv("scripts/indices sept2017/allSpeciesSubAntPointsStatus.csv")
newcoorData<-read.csv("scripts/indices sept2017/GrantSpecies.csv")
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
soi_insects_hist=ggplot(X2u, aes(diff))
soi_insects_hist+
  geom_histogram(fill="white", color="black", binwidth = 0.5)+
  geom_vline(xintercept=mean(X2u$diff),
           linetype="dashed", show.legend = TRUE)+
  geom_vline(xintercept=median(X2u$diff),
             linetype="dotted", show.legend = TRUE)+
  geom_vline(xintercept=quantile(X2u$diff,0.25),color="red",
              show.legend = TRUE)+
  labs(title="Difference of Euclidean distance, SOI introduced insects",x="Euclidean distance", y = "Count")+
  #scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))+
  theme_classic()


library(RColorBrewer)
YlOrRd
plot(eucl_raster, breaks=c(0,0.6,2,4.99,11.81253),col=rev(brewer.pal(4, "YlOrRd")))
SOI_extent=extent(eucl_raster)
SOI_extent@ymin=-60
SOI_extent@ymax=-45
SOI_eucl_raster=crop(eucl_raster, SOI_extent)
plot(SOI_eucl_raster, breaks=c(0,0.6,2,4.99,11.81253),col=rev(brewer.pal(4, "YlOrRd")), ylim=c(-60,-45))


setwd("rdata")
poly_soi=readOGR(dsn = ".", layer = "SOI_outline_dis")
soi_poly1984=spTransform(poly_soi, CRS("+init=epsg:4326"))
#for each soi we extract the bit of raster and put it in a list after rasterToPoint
soi_eucl=list()
for (i in 1:length(soi_poly1984@data$LOCATION))
{
  island=soi_poly1984[i,]
  raster_island=crop(eucl_raster, extent(island))
  soi_eucl[[i]]=as.data.frame(rasterToPoints(raster_island))
  
}

names(soi_eucl)=soi_poly1984@data$LOCATION

library(ggplot2)
library(viridis)  # better colors for everyone
library(ggthemes)

i=24
ggplot() +  
  geom_tile(data=soi_eucl[[i]], aes(x=x, y=y, fill=raster_W_Min.), alpha=0.8) + 
  geom_polygon(data=soi_poly1984[i,], aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=0.25) +
  scale_fill_distiller(palette = "Spectral", breaks=c(0,0.6,2,4.99,11.81253))+
  # scale_fill_brewer(palette="YlOrRd", breaks=c(0,0.6,2,4.99,11.81253)) +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))



#working with dfResA to create weighted values for sink area

dfResA=read.csv("dfResA.csv")
head(dfResA)

summary(dfResA$count_CAs_thr)
hist(dfResA$count_CAs_thr)
prop_count_CAs_thr=dfResA$count_CAs_thr/605854
summary(prop_count_CAs_thr)
quantile(prop_count_CAs_thr)
hist(prop_count_CAs_thr)

# figure looking at the distribution of number of CAs in function of threshold

dfRes499=read.csv("results/dfResA_acbr_ASPAS_ID_thr499.csv")
dfRes06=read.csv("results/dfResA_acbr_ASPAS_ID_thr06.csv")
dfRes2=read.csv("results/dfResA_acbr_ASPAS_ID_thr2.csv")

library(ggplot2)

head(dfRes499)
dfRes499_order=dfRes499[order(dfRes499$acbr_ID),]
head(dfRes499_order)
dfRes499_order$plotOrder=seq_len(nrow(dfRes499_order))
dfRes499_order$acbr_ID=as.factor(dfRes499_order$acbr_ID)

dfRes2_order=dfRes2[order(dfRes499$acbr_ID),]
head(dfRes2_order)
dfRes2_order$plotOrder=seq_len(nrow(dfRes2_order))
dfRes2_order$acbr_ID=as.factor(dfRes2_order$acbr_ID)
dfRes2_order$threshold=2

dfRes06_order=dfRes06[order(dfRes499$acbr_ID),]
head(dfRes06_order)
dfRes06_order$plotOrder=seq_len(nrow(dfRes06_order))
dfRes06_order$acbr_ID=as.factor(dfRes06_order$acbr_ID)
dfRes06_order$threshold=0.6

plot499=ggplot(data=dfRes499_order, aes(x=plotOrder, y=count_CAs_thr, colour=acbr_ID))
plot499+
  geom_line()+
  geom_line(data=dfRes2_order, aes(x=plotOrder, y=count_CAs_thr, colour=acbr_ID))+
  geom_line(data=dfRes06_order, aes(x=plotOrder, y=count_CAs_thr, colour=acbr_ID))+
  facet_wrap(~acbr_ID)

plot2=ggplot(data=dfRes2_order, aes(x=plotOrder, y=count_CAs_thr))
plot2+
  geom_line(colour="blue")+
  #geom_line(data=dfRes06_order, aes(x=plotOrder, y=count_CAs_thr), colour="red")+
  facet_wrap(~acbr_ID)


plot06=ggplot(data=dfRes06_order, aes(x=plotOrder, y=count_CAs_thr, colour=acbr_ID))
plot06+
  geom_line()

names(dfRes06_order)
names(dfRes2_order)
dfres_06_2=rbind(dfRes06_order, dfRes2_order)
dfres_06_2$threshold=as.factor(dfres_06_2$threshold)

head(dfres_06_2)


head(dfRes06_order)
sum(dfRes2_order$X!=dfRes06_order$X)



threshold_plot=ggplot(data=dfres_06_2, aes(x=threshold,y=count_CAs_thr,  fill=acbr_ID, colour=acbr_ID))

threshold_plot+
  geom_violin(aes(alpha=threshold))+
  scale_alpha_discrete(range = c(0.5, 1))+
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.1,1)),
                                              colour=NA)))+
  facet_wrap(~acbr_ID, ncol=2)
ggsave("figures/violin_2_06.tiff",dpi=300,width = 21,height=29.7,units = "cm")

threshold_plot+
  geom_boxplot(aes(alpha=threshold), colour="black")+
  scale_alpha_discrete(range = c(0.5, 1))+
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.1,1)),
                                              colour=NA)))+
  facet_wrap(~acbr_ID, ncol=2)

ggsave("figures/boxplot_2_06.tiff",dpi=300,width = 21,height=29.7,units = "cm")





dfres_06_2_wide=cbind(dfRes2_order, dfRes06_order)
names(dfres_06_2_wide)

dfres_06_2_wide=dfres_06_2_wide[,-c(1,21,22,28,29,30,31,32,33,41:58)]
names(dfres_06_2_wide)[25:31]=paste(names(dfres_06_2_wide)[25:31],"06", sep=".")
names(dfres_06_2_wide)[4:10]=paste(names(dfres_06_2_wide)[4:10],"2", sep=".")

write.csv(dfres_06_2_wide, "results/dfres_06_2_wide.csv")


cas_map2=ggplot(data=dfres_06_2_wide, aes(x=x.ant, y=y.ant, colour=thr_min.2, size=count_CAs_thr.2))
cas_map2+
  geom_point()+
  coord_polar(start = pi)


cas_map06=ggplot(data=dfres_06_2_wide, aes(x=x.ant, y=y.ant, colour=thr_min.1.06, size=count_CAs_thr.1.06))
cas_map06+
  geom_point()+
  scale_colour_gradient(low = "red", high = "yellow")+
  coord_polar(start = pi)

