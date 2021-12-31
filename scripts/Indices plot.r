#figure with the indices for the SOI species

library(raster)
library(rgdal)
library(ggplot2)

thr=2.4256


eucl_raster<-raster("results/resW_Raster/raster_W_Min..tif")
eucl=as.data.frame(rasterToPoints(eucl_raster))
ggplot(data=eucl, aes(x=x,y=y, fill=raster_W_Min. ))+
  geom_raster()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_fixed(ratio=1.3)+
  scale_fill_gradient(low="black", high="snow2")+
  ggtitle("Minimum Climatic Euclidean Distance to Antarctica")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(-120, 120, 60), name = "longitude", limits=c(-180,180))+
  scale_y_continuous(breaks=seq(-60, 90, 30), name = "latitude", limits=c(-60,90))

ggsave("figures/WorldEuclideanDistance_initial.tiff",dpi=300,width = 29.7,height=21,units = "cm")



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
sp<-levels(coorData$Species)
length(sp)
spc<-rep("z", length(sp))
uPoints<-rep(0, length(sp))
iPoints<-rep(0, length(sp))

setwd("results/EuclHistIntroduced_2-4256")

for(i in 1:length(sp))
{
  s<-sp[i]
  subSp<-coorData[coorData$Species==s,]
  csubSp<-remove.duplicates(subSp, zero=0.16662) #resolution of the raster
  nsubSp<-as.data.frame(csubSp)
  x.range <-range(c(nsubSp[nsubSp$status=="unknown","EuclDist"], nsubSp[nsubSp$status=="introduced","EuclDist"]), na.rm=TRUE)+c(-0.2,0.2)
  cat(paste(s, "unknown:",length(nsubSp[nsubSp$status=="unknown","EuclDist"]), "introduced:", length(nsubSp[nsubSp$status=="introduced","EuclDist"]),"\n"))
  
  spc[i]<-s
  uPoints[i]<-length(nsubSp[nsubSp$status=="unknown","EuclDist"])
  iPoints[i]<-length(nsubSp[nsubSp$status=="introduced","EuclDist"])
  
  if (iPoints[i]>1)
  {
    h<-hist(nsubSp[nsubSp$status=="unknown","EuclDist"], plot=F) #blue
    b<-h$breaks[2]-h$breaks[1]
    h2<-hist(nsubSp[nsubSp$status=="introduced","EuclDist"], breaks=seq(0,max(c(h$breaks,x.range)),b),plot=F) # red
    
    y.range <-range(c(h$counts, h2$counts))
    
    png(filename=paste(s,"_AntIceFree_Euclidean_hist.png", sep=""), width=480*2,height=480*2.25)
    par(mfrow=c(2, 1))
    h<-hist(nsubSp[nsubSp$status=="unknown","EuclDist"], xlim=c(0, x.range[2]), col=rgb(0,0,1,1/4), border =rgb(0,0,1,1/4), axes=FALSE, main="", xlab="", ylim=y.range) #blue
    axis(2, ylim=c(0,260), las=1)
    par(new=TRUE)
    
    hist(nsubSp[nsubSp$status=="introduced","EuclDist"], xlim=c(0, x.range[2]),  col=rgb(1,0,0,1/4),border="red", breaks=seq(0,max(c(h$breaks,x.range)),b), axes=FALSE, main="", xlab="", ylim=y.range) # red
    title(main=s)
    axis(1, mgp=c(0,1,0), line=-1, lwd=1,xlim=c(2, 4))
    mtext("Antarctic Euclidean distance",side=1,col="black",line=2.5, cex=2)
    
    abline(v=c(1, 2, 3, 4,5), lty=2)
    plot(eucl_raster, col="lightgrey", legend=FALSE)
    points(csubSp, col=csubSp$status, pch=19, cex=1)
    legend("bottomleft", legend=c("introduced","unknown"), title="Status", pch=19, col=c(1,2))
    title("Distribution")
    
    mtext(paste("unknown:",length(nsubSp[nsubSp$status=="unknown","EuclDist"]), "introduced:", length(nsubSp[nsubSp$status=="introduced","EuclDist"])), side=1, outer=FALSE, line=3)
    dev.off()
  }
}

resHist<-data.frame(spc, uPoints,  iPoints)
summary(resHist)
resHist<-resHist[resHist$iPoints>1,]


sp<-as.character(resHist$spc)

sp=as.character(sp)

length(sp)
porc<-rep(0, length(sp))
porc_all<-rep(0, length(sp))
dgeom<-rep(0, length(sp))
nshift<-rep(0, length(sp))
unknown <-rep(0, length(sp))
introduced <-rep(0, length(sp))
spc<-rep("z", length(sp))
min_nat_eucl<-rep(0, length(sp))
min_intr_eucl<-rep(0, length(sp))
dThr<-rep(0, length(sp))


thr=2.4256

for(i in 1:length(sp))
{
  s<-sp[i]
  subSp<-coorData[coorData$Species==s,]
  csubSp<-remove.duplicates(subSp, zero=0.16662) 
  nsubSp<-as.data.frame(csubSp)
  
  unk<-nsubSp[nsubSp$status=="unknown",5]
  int<-nsubSp[nsubSp$status=="introduced",5]
  
  
  
  underThreshold<-which(unk <= thr)
  overThreshold<-which(unk> thr)
  
  porc[i] <- length(underThreshold)*100/length(unk) #establishment index
  
  porc_all[i]=length(which(c(unk,int)<= thr))*100/(length(unk)+ length(int)) #establishment index using all range
  
  dgeom[i] <- (max(unk, na.rm=TRUE)-min(unk, na.rm=TRUE))#cambiar abs((thr-min(unk, na.rm=TRUE))/ #marginality index
  dThr[i] <- thr-min(unk, na.rm=TRUE)
  nshift[i] <- min(unk, na.rm=TRUE)-min(int, na.rm=TRUE) #plasticity index
  unknown[i] <- length(unk)
  introduced[i] <- length(int)
  spc[i]<-as.character(s)
  
  min_nat_eucl[i]<-min(unk, na.rm=TRUE)
  min_intr_eucl[i]<-min(int, na.rm=TRUE)
  
}

indices<-data.frame(sp,porc, porc_all, dThr, dgeom, nshift, unknown, introduced, min_nat_eucl, min_intr_eucl)
row.names(indices)<-sp

pIndex<-indices[indices$introduced>1,] #species with at least one introduced occurrence

pIndex<-pIndex[pIndex$min_intr_eucl!=Inf,] #this removes two species for which the introduced record doesn't land on a raster pixel. 


library(ggplot2)

rbPal <- colorRampPalette(c("red","orange","beige","blue"))


indPlot=ggplot(data=pIndex, aes(x=porc,y=nshift,fill=dgeom))

indPlot+
  geom_point(aes(size= unknown+introduced),alpha=0.8, pch=21)+
  scale_x_sqrt(limits=c(0,100))+
  scale_y_continuous(limits=c(-2.25,2.25))+
  scale_fill_gradientn(colours = rbPal(5))+
  theme_bw()+
  coord_fixed(ratio=2.5)+
  scale_size_continuous(breaks  = c(10,50,100, 500, 1000,9000), trans="log2")+
  labs(fill="Niche Marginality", size="Number ocurrences",x="Establishment", y="Phenotypic plasticity")

ggsave("figures/Indices Plot.tiff",dpi=300,width = 29.7,height=21,units = "cm")



##Plotting only the species in Grant's paper

grant<-c("Fucellia tergina",
         "Fannia canicularis",
         "Aulacorthum solani",
         "Brachycaudus helichrysi",
         "Myzus persicae",
         "Neomyzus circumflexus",
         "Rhopalosiphum padi",
         "Agrotis ipsilon",
         "Plutella xylostella")

grant_similarity=which(row.names(pIndex)%in%grant)
grant_dissimilarity=which(!(grant%in%row.names(pIndex)))
pIndex$duffy2017[grant_similarity]="duffy2017"
pIndex$duffy2017[-grant_similarity]="c&c2017"
pIndex$duffy2017=as.factor(pIndex$duffy2017)

indPlot=ggplot(data=pIndex, aes(x=porc,y=nshift,fill=dgeom))

indPlot+
  geom_point(aes(size= unknown+introduced),alpha=0.8, pch=21)+
  scale_x_sqrt(limits=c(0,100))+
  scale_y_continuous(limits=c(-2.25,2.25))+
  scale_fill_gradientn(colours = rbPal(5))+
  theme_bw()+
  coord_fixed(ratio=2.5)+
  scale_size_continuous(breaks  = c(10,50,100, 500, 1000,9000), trans="log2")+
  labs(fill="Niche Marginality", size="Number ocurrences",x="Establishment", y="Phenotypic plasticity")+
  facet_wrap(~duffy2017)
ggsave("figures/Indices Plot_duffy2017.tiff",dpi=300,width = 29.7,height=21,units = "cm")




pIndex$superIndex=(pIndex$nshift*(pIndex$porc_all/100)/pIndex$dgeom)

pIndex[rev(order(pIndex$superIndex)),c(2,3,5,6,12)]


indPlot=ggplot(data=pIndex, aes(x=porc_all,y=nshift,fill=dgeom))

indPlot+
  geom_point(data=pIndex[rev(order(pIndex$superIndex))[1:10],], pch=3, size=4)+
  geom_point(aes(size= unknown+introduced),alpha=0.8, pch=21)+
  #geom_text(data=head(pIndex[rev(order(pIndex$superIndex)),]), aes(label=sp_name),check_overlap = TRUE, size=2)+
  geom_text(data=pIndex[rev(order(pIndex$superIndex))[1:10],], aes(label=1:10),check_overlap = FALSE, size=3,nudge_x=-0.2, nudge_y = 0.15)+
  #geom_text( aes(label=sp_name),check_overlap = TRUE, size=2)+
  scale_x_sqrt(limits=c(0,100))+
  scale_y_continuous(limits=c(-2.25,2.25))+
  scale_fill_gradientn(colours = rbPal(5))+
  theme_bw()+
  coord_fixed(ratio=2.5)+
  scale_size_continuous(breaks  = c(10,50,100, 500, 1000,9000), trans="log2")+
  labs(fill="Niche Marginality", size="Number ocurrences",x="Establishment", y="Phenotypic plasticity")+
  facet_wrap(~duffy2017)

ggsave("C:/Users/greta.vega/Dropbox/Brasil Alien SubAntartic/ClimateSimilarity/_Review DDI/figures/Indices Plot_duffy2017_ranked.tiff",dpi=300,width = 29.7,height=21,units = "cm")

pIndex$shape=NA
pIndex$shape[pIndex$duffy2017=="duffy2017"]=4
pIndex$shape[rev(order(pIndex$superIndex))[1:10]]=3

pIndex$shape[which(pIndex$shape==3 &pIndex$duffy2017=="duffy2017")]=8

indPlot+
  geom_point(aes(size= unknown+introduced),alpha=0.8, shape=21)+
  geom_point(data=pIndex[pIndex$shape>1,], aes(shape=as.factor(shape)), size=4, na.rm = TRUE)+
  #geom_text(data=head(pIndex[rev(order(pIndex$superIndex)),]), aes(label=sp_name),check_overlap = TRUE, size=2)+
  geom_text(data=pIndex[rev(order(pIndex$superIndex))[1:10],], aes(label=1:10),check_overlap = FALSE, size=3,nudge_x=-0.2, nudge_y = 0.15)+
  #geom_text( aes(label=sp_name),check_overlap = TRUE, size=2)+
  scale_x_sqrt(limits=c(0,100))+
  scale_y_continuous(limits=c(-2.25,2.25))+
  scale_fill_gradientn(colours = rbPal(5), guide = guide_legend(
    direction = "horizontal",
    title.position = "top"
  ))+
  scale_shape_manual(values=c(3,4,8),labels = c("Top 10 species of interest", "SDM done by Duffy et al. 2017", "Top 10 and SDM done by Duffy et al. 2017", ""), guide = guide_legend(
    direction = "horizontal",
    title.position = "top"
  ))+
  theme_bw()+
  coord_fixed(ratio=2.5)+
  scale_size_continuous(breaks  = c(10,50,100, 500, 1000,9000), trans="log2")+
  labs(fill="Marginality Index", size="Number ocurrences",x="Establishment Index", y="Shift Index", shape="Species ranking")+
  guides(fill = guide_colorbar(order = 3, nrow=2, byrow=TRUE),size = guide_legend(order = 0, nrow=3, byrow=TRUE), shape=guide_legend(order=2, nrow=4,byrow=TRUE))+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.box = "horizontal",  legend.title.align = 0, legend.text=element_text(size=10))
  


ggsave("figures/Indices Plot_duffy2017_ranked_legend.tiff",dpi=300,width = 29.7,height=21,units = "cm")


pIndex2=read.csv("results/soi_species_indices.csv")
head(pIndex2)
row.names(pIndex2)=pIndex2$species


grant<-c("Fucellia tergina",
         "Fannia canicularis",
         "Aulacorthum solani",
         "Brachycaudus helichrysi",
         "Myzus persicae",
         "Neomyzus circumflexus",
         "Rhopalosiphum padi",
         "Agrotis ipsilon",
         "Plutella xylostella")

grant_similarity=which(row.names(pIndex2)%in%grant)
grant_dissimilarity=which(!(grant%in%row.names(pIndex2)))
pIndex2$duffy2017="duffy2017"
pIndex2$duffy2017[-grant_similarity]="c&c2017"
pIndex2$duffy2017=as.factor(pIndex2$duffy2017)


indPlot=ggplot(data=pIndex2, aes(x=IE,y=Ipheno,fill=Imarginality))

indPlot+
  geom_point(alpha=0.8, pch=21)+
  geom_point(data=pIndex[pIndex$duffy2017=="duffy2017",], pch=4, size=4)+
  geom_point(data=pIndex[rev(order(pIndex$superIndex))[1:10],], pch=3, size=4)+
  geom_text(data=pIndex[rev(order(pIndex$superIndex))[1:10],], aes(label=1:10),check_overlap = FALSE, size=3,nudge_x=-0.2, nudge_y = 0.15)+
  #geom_text( aes(label=sp_name),check_overlap = TRUE, size=2)+
  scale_x_sqrt(limits=c(0,100))+
  scale_y_continuous(limits=c(-2.25,2.25))+
  scale_fill_gradientn(colours = rbPal(5))+
  theme_bw()+
  coord_fixed(ratio=2.5)+
  scale_size_continuous(breaks  = c(10,50,100, 500, 1000,9000), trans="log2")+
  labs(fill="Niche Marginality", size="Number ocurrences",x="Establishment", y="Phenotypic plasticity")

+
  facet_wrap(~duffy2017)




scaled.pIndex <- scale(pIndex[,3:5])
scaled.pIndex=cbind(pIndex,scaled.pIndex)
names(scaled.pIndex)[7:9]=c("IE.sc","Imarginality.sc", "Ipheno.sc")
scaled.pIndex$sc.superIndex=scaled.pIndex$IE.sc*scaled.pIndex$Ipheno.sc/scaled.pIndex$Imarginality.sc
scaled.pIndex
scale_plot=ggplot(data=scaled.pIndex, aes(x=IE.sc,y=Ipheno.sc,fill=Imarginality.sc))
scale_plot+
  geom_point(alpha=0.8, pch=21)+
  # scale_x_sqrt(limits=c(0,100))+
  # scale_y_continuous(limits=c(-2.25,2.25))+
  scale_fill_gradientn(colours = rbPal(5))+
  theme_bw()+
  coord_fixed(ratio=2.5)+
  scale_size_continuous( trans="log2")+
  labs(fill="Niche Marginality", size="Number ocurrences",x="Establishment", y="Phenotypic plasticity")


indPlot+
  geom_point(aes(size= unknown+introduced),alpha=0.8, shape=21)+
  #geom_point(data=pIndex[pIndex$shape>1,], aes(shape=as.factor(shape)), size=4, na.rm = TRUE)+
  #geom_text(data=head(pIndex[rev(order(pIndex$superIndex)),]), aes(label=sp_name),check_overlap = TRUE, size=2)+
  geom_text(data=pIndex[rev(order(pIndex$superIndex))[1:10],], aes(label=1:10),check_overlap = FALSE, size=3,nudge_x=-0.2, nudge_y = 0.15)+
  #geom_text( aes(label=sp_name),check_overlap = TRUE, size=2)+
  scale_x_sqrt(limits=c(0,100))+
  scale_y_continuous(limits=c(-2.25,2.25))+
  scale_fill_gradientn(colours = rbPal(5), guide = guide_legend(
    direction = "horizontal",
    title.position = "top"
  ))+
  scale_shape_manual(values=c(3,4,8),labels = c("Top 10 species of interest", "SDM done by Duffy et al. 2017", "Top 10 and SDM done by Duffy et al. 2017", ""), guide = guide_legend(
    direction = "horizontal",
    title.position = "top"
  ))+
  theme_bw()+
  coord_fixed(ratio=2.5)+
  scale_size_continuous(breaks  = c(10,50,100, 500, 1000,9000), trans="log2")+
  labs(fill="Marginality Index", size="Number ocurrences",x="Establishment Index", y="Shift Index", shape="Species ranking")+
  guides(fill = guide_colorbar(order = 3, nrow=2, byrow=TRUE),size = guide_legend(order = 0, nrow=3, byrow=TRUE))+
  theme(legend.direction = "horizontal", legend.position = "right", legend.box = "vertical",  legend.title.align = 0, legend.text=element_text(size=10))

ggsave("figures/Indices Plot_NOduffy2017_ranked_legend.tiff",dpi=300,width = 29.7,height=21,units = "cm")













