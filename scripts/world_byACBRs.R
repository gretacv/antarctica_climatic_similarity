
library(raster)
library(foreign)

acbr_id=read.dbf("rData/cell_id_acbr_id.dbf")


acbr_id=acbr_id[,c("grid_code", "acbr_ID")]
names(acbr_id)[1]="A_id"
head(acbr_id)
tail(acbr_id)
acbr_id$conc_Aid=paste("A_",acbr_id$A_id, sep="")
summary(acbr_id)

objectsPath="results/resW_rData"
listofWres=list.files(path=objectsPath, pattern = "res")

resEuclMinACBR=list()

load(file = paste(objectsPath, listofWres[1], sep="/"), verbose = TRUE) #loads resW_for

for (i in 1:16)
{
  resFor_Acbr=resW_for[,acbr_id[acbr_id$acbr_ID==i,1]]
  resEuclMinACBR[[i]]=apply(resFor_Acbr,1,min)
  
}


for (i in 2:length(listofWres))
{
  load(file = paste(objectsPath, listofWres[i], sep="/"), verbose = TRUE) #loads resW_for
  for (i in 1:16)
  {
    resFor_Acbr=resW_for[,acbr_id[acbr_id$acbr_ID==i,1]]
    resEuclMinACBR[[i]]=c(resEuclMinACBR[[i]],apply(resFor_Acbr,1,min))
    
  }
}



str(resEuclMinACBR)
save(resEuclMinACBR,file="listresEuclMinACBR.Rdata")
load("results/listresEuclMinACBR.Rdata")

all_res=as.data.frame(resEuclMinACBR)
names(all_res)=paste("acbr_",1:16,sep="")
head(all_res)

dfResW=read.csv("results/dfResW.csv")
head(dfResW)


all_res=cbind(dfResW[,2:3],all_res)
head(all_res)
summary(all_res)
dim(all_res)
breakslm=c(0,
           0.7226667,
           0.837347,
           0.9520273,
           1.0667076,
           1.1813879,
           1.2960682,
           1.4107485,
           1.5254288,
           1.6401091,
           1.7547894,
           1.8694697,
           1.98415,
           2.0988303,
           2.2135106,
           2.3281909,
           2.4256,
           14)
all_res_fct=apply(all_res[,3:18],2,cut,breakslm)
head(all_res_fct)
all_res_fct=cbind(all_res[,1:2],all_res_fct)   
summary(all_res_fct)

library(ggplot2)

# byACBR=ggplot(data=all_res_fct, aes(x=x.world, y=y.world))
# byACBR+
#   geom_raster(aes(fill=acbr_1))+
#   theme_bw()+
#   scale_fill_manual(values=c("black", "#8A1923", "#91272F", "#97343A", "#9E3F44", "#A54B4F", "#AC565A", "#B36266", "#BB6E71", "#C27B7E", "#CA888B", "#D29698", "#DBA6A8", "#E4B7B9", "#EECCCD", "#FEEDED","snow2"), name="Number of \nACBRs as sink")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   coord_fixed(ratio=1.3)+
#   #ggtitle("Source Points ranked by sink 125")+
#   #theme(plot.title = element_text(hjust = 0.5))+
#   scale_x_continuous(breaks=seq(-120, 120, 60), name = "longitude", limits=c(-180,180))+
#   scale_y_continuous(breaks=seq(-60, 90, 30), name = "latitude", limits=c(-60,90))


library(reshape2)
all_res_fctmelt=melt(all_res_fct,id.vars =c("x.world","y.world"),value.name="eucl.min")
summary(all_res_fctmelt)

ggplot(data=all_res_fctmelt, aes(x=x.world, y=y.world))+
  geom_raster(aes(fill= eucl.min))+
  theme_bw()+
  scale_fill_manual(values=c("black", "#8A1923", "#91272F", "#97343A", "#9E3F44", "#A54B4F", "#AC565A", "#B36266", "#BB6E71", "#C27B7E", "#CA888B", "#D29698", "#DBA6A8", "#E4B7B9", "#EECCCD", "#FEEDED","snow2"), name="Minimum Climatic \nEuclidean Distance")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_fixed(ratio=1.3)+
  #ggtitle("Source Points ranked by sink 125")+
  #theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(-120, 120, 60), name = "longitude", limits=c(-180,180))+
  scale_y_continuous(breaks=seq(-60, 90, 30), name = "latitude", limits=c(-60,90))+
  facet_wrap(~variable, ncol=4)
ggsave( "figures/euclMin_byACBR2.tiff",dpi=300,width = 29.7,height=21,units = "cm")



#creating maps of the world for each acbr

acbr_id=read.dbf("rData/cell_id_acbr_id.dbf")
acbr_id=acbr_id[,c("grid_code", "acbr_ID")]
names(acbr_id)[1]="A_id"
head(acbr_id)
tail(acbr_id)
acbr_id$conc_Aid=paste("A_",acbr_id$A_id, sep="")
summary(acbr_id)

objectsPath="results/resW_rData"
listofWres=list.files(path=objectsPath, pattern = "res")

resCaACBR=list()
thr=2.4256
load(file = paste(objectsPath, listofWres[1], sep="/"), verbose = TRUE) #loads resW_for

for (i in 1:16)
{
  resFor_Acbr=resW_for[,acbr_id[acbr_id$acbr_ID==i,1]]
  resCaACBR[[i]]=apply(resFor_Acbr, 1, function(x)
    sum(x < thr))
  
  
}


for (i in 2:length(listofWres))
{
  load(file = paste(objectsPath, listofWres[i], sep="/"), verbose = TRUE) #loads resW_for
  for (i in 1:16)
  {
    resFor_Acbr=resW_for[,acbr_id[acbr_id$acbr_ID==i,1]]
    resCaACBR[[i]]=c(resCaACBR[[i]],apply(resFor_Acbr, 1, function(x)
      sum(x < thr)))
    
  }
}


str(resCaACBR)
save(resCaACBR,file="listresCaACBR.Rdata")
load("results/listresCaACBR.Rdata")

all_res_CA=as.data.frame(resCaACBR)
names(all_res_CA)=paste("acbr_",1:16,sep="")
head(all_res_CA)


totalCells=as.data.frame(table(acbr_id$acbr_ID))[,2]
class(totalCells)
all_res_CA_prop=t(t(all_res_CA)/totalCells)






dfResW=read.csv("results/dfResW.csv")
head(dfResW)


all_res_CA_prop=cbind(dfResW[,2:3],all_res_CA_prop)
head(all_res_CA_prop)

all_res_CA_prop_fct=apply(all_res_CA_prop[,3:18],2,cut,10)
head(all_res_CA_prop_fct)
all_res_CA_prop_fct=cbind(all_res_CA_prop[,1:2],all_res_CA_prop_fct)   


library(reshape2)
all_res_CA_propmelt=melt(all_res_CA_prop_fct,id.vars =c("x.world","y.world"),value.name="prop")
head(all_res_CA_propmelt)
summary(all_res_CA_propmelt)


greenCols=c("snow2", "#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B")


library(ggplot2)

ggplot(data=all_res_CA_propmelt, aes(x=x.world, y=y.world))+
  geom_raster(aes(fill= prop))+
  theme_bw()+
  scale_fill_manual(values=greenCols, name="Proportion \nof Climate Analogues")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_fixed(ratio=1.3)+
  scale_x_continuous(breaks=seq(-120, 120, 60), name = "longitude", limits=c(-180,180))+
  scale_y_continuous(breaks=seq(-60, 90, 30), name = "latitude", limits=c(-60,90))+
  facet_wrap(~variable, ncol=4)

ggsave( "figures/propCA_byACBR.tiff",dpi=300,width = 29.7,height=21,units = "cm")


