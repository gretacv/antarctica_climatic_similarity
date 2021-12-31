#Maping Sinks in Antarctica
sinkData=read.csv("results/dfResA_2.4256.csv", stringsAsFactors=FALSE)

head(sinkData)
sinkData$sinkIndex=(sinkData$count_CAs_thr/605854)/sinkData$thr_min

sinkInfo=sinkData[order(sinkData$sinkIndex,decreasing = FALSE), c(1:5,17)]
head(sinkInfo)
summary(sinkInfo)
h=hist(sinkInfo$sinkIndex, breaks=25)
h
sum(h$counts[5:25])/sum(h$counts)

library(rgdal)
library(scales)
setwd("rData")
Antarctica_shp =readOGR(dsn = ".", layer = "Antarctica_land_wgs84")
Antarctica <- fortify(Antarctica_shp, region="gid")
head(Antarctica )


ggplot(data=sinkInfo, aes(x=x.ant, y=y.ant, fill=sinkIndex), colour="transparent")+
  geom_polygon(data=Antarctica, aes(long, lat, group=group),fill="white")+
  geom_point(pch=21)+
  scale_y_continuous(breaks=c(-60,-75,-90))+
  scale_x_continuous(breaks=seq(-180,180,30))+
  scale_fill_gradient(high = "red", low = "white")
  # coord_polar(start=pi)



h=hist(sinkInfo$sinkIndex, breaks=25)
h
sum(h$counts[5:25])/sum(h$counts)



ggplot(data=sinkInfo[sinkInfo$sinkIndex>0.5,], aes(x=x.ant, y=y.ant, fill=sinkIndex), colour="transparent")+
  geom_polygon(data=Antarctica, aes(long, lat, group=group),fill="white")+
  geom_point(pch=21)+
  scale_y_continuous(breaks=c(-60,-75,-90))+
  scale_x_continuous(breaks=seq(-180,180,30))+
  scale_fill_gradient(high = "red", low = "white")



sinkInfo$fct=cut(sinkInfo$sinkIndex, breaks=h$breaks, labels=seq(1,25,1))
table(sinkInfo$fct)

library(BAMMtools)
jbreaks=getJenksBreaks(sinkInfo$sinkIndex,6)

sinkInfo$fct2=cut(sinkInfo$sinkIndex, breaks=jbreaks, labels=seq(1,5,1))
sinkInfo[1,"fct2"]=1
head(sinkInfo)

ggplot(data=sinkInfo[sinkInfo$sinkIndex>0.2,], aes(x=x.ant, y=y.ant, fill=sinkIndex), colour="transparent")+
  geom_polygon(data=Antarctica, aes(long, lat, group=group),fill="white")+
  geom_point(pch=21)+
  scale_y_continuous(breaks=c(-60,-75,-90))+
  scale_x_continuous(breaks=seq(-180,180,30))+
  scale_fill_gradient(high = "red", low = "white")+
  facet_wrap(~fct2)

ggplot(data=sinkInfo[sinkInfo$sinkIndex>0.2,], aes(x=count_CAs_thr, y=thr_min, fill=fct2))+
  geom_point(pch=21)


library(foreign)
acbr_id=read.dbf("rData/cell_id_acbr_id.dbf")
head(acbr_id)
acbr_id$X=paste("A",acbr_id$grid_code,sep="_")

sinkInfo_acbr=merge.data.frame(acbr_id[,c(7,6)],sinkInfo, by="X", sort=FALSE)
head(sinkInfo_acbr)
sinkInfo_acbr=sinkInfo_acbr[order(sinkInfo_acbr$sinkIndex,decreasing = FALSE),]
sinkInfo_acbr$ca_prop=sinkInfo_acbr$count_CAs_thr/605854
sinkInfo_acbr$sinkIndex2=sinkInfo_acbr$ca_prop/sinkInfo_acbr$thr_min

ggplot(data=sinkInfo_acbr[sinkInfo_acbr$sinkIndex>0.2,], aes(x=count_CAs_thr, y=thr_min, fill=fct2))+
  geom_point(pch=21)+
  facet_wrap(~as.factor(acbr_ID))



ggplot(data=sinkInfo_acbr[sinkInfo_acbr$fct2%in%(3:4),], aes(x=count_CAs_thr, y=thr_min, fill=sinkIndex))+
  geom_point(pch=21)+
  facet_wrap(~as.factor(acbr_ID))+
  scale_fill_gradientn(colours = rev(c("#023FA5", "#4D60A9", "#727EB5", "#9299C2", "#AEB2CD", "#C6C8D7", "#D8D9DE", "#E2E2E2")))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  


ggplot(data=sinkInfo_acbr[sinkInfo_acbr$fct2%in%(3:4),], aes(x=count_CAs_thr, y=thr_min, fill=fct2))+
  geom_point(pch=21)+
  facet_wrap(~as.factor(acbr_ID))+
  scale_fill_brewer(palette = "Blues")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

contour_data=data.frame()
x=seq(0,0.1,0.001)
for (i in 1:length(jbreaks))
{
  jb=jbreaks[i]
  y=x*jb
  group=rep(paste("jb",i,sep=""),length(x))
  d=cbind(x,y, group)
  contour_data=rbind(contour_data,d)
}
head(contour_data)
contour_data$x=as.numeric(as.character(contour_data$x))
contour_data$y=as.numeric(as.character(contour_data$y))
summary(contour_data)


ggplot(data=sinkInfo_acbr, aes(x=ca_prop, y=thr_min, colour=fct2))+
  geom_point()+
  scale_colour_manual(values=c("#bdd7e7", "#6baed6","#3182bd", "#08519c","black"))+
  facet_wrap(~as.factor(acbr_ID))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


jbreaks2=getJenksBreaks(sinkInfo_acbr$sinkIndex,11)
jbreaks2[1]=0

ggplot(data=sinkInfo_acbr, aes(x=ca_prop, y=thr_min, colour=sinkIndex))+
  geom_point()+
  scale_colour_gradientn(colours=c("snow2","#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b","black"), values=jbreaks2)+
  facet_wrap(~as.factor(acbr_ID))+
  labs(x="Proportion of Climate Analogues",y="Minimum Euclidean Distance", colour="Sink Index = \nProp CA\n---------------\nmin Eucl. Dist")+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
ggsave("figures/sinkScatter_jb.tiff",dpi=300,width = 29.7,height=21,units = "cm")


ggplot(data=sinkInfo_acbr, aes(x=sinkIndex, fill=..x..))+
  geom_histogram(bins=75)+
  scale_fill_gradientn(colours=c("snow2","#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b","black"), values=jbreaks2)+
  labs(y="Count",x="Sink Index",fill="")+#"Sink Index = \nProp CA\n---------------\nmin Eucl. Dist")+
  facet_wrap(~as.factor(acbr_ID), scales="free_y")+
  guides(fill = guide_colorbar(barwidth = 1, barheight = 36))+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("figures/sinkHist_lg.tiff",dpi=300,width = 29.7,height=21,units = "cm")


ggplot(data=sinkInfo_acbr, aes(y=sinkIndex, x=as.factor(acbr_ID)))+
  geom_violin()+
  labs(x="ACBR",y="Sink Index")+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("figures/sinkViolin.tiff",dpi=300,width = 29.7,height=21,units = "cm")


textCoord=read.dbf("rData/acbr_labelCoord.dbf")

setwd("rData/")
ACBRsPaek=readOGR(dsn = ".", layer = "ACBRs_paek_topo")
ACBRsPaek@data
ACBRsPaekMap=map_data(ACBRsPaek)


ggplot()+
  geom_polygon(data=Antarctica, aes(long, lat, group=group),fill="darkgray")+
  geom_point(data=sinkInfo_acbr, aes(x=x.ant, y=y.ant, colour=sinkIndex))+
  scale_colour_gradientn(colours=c("snow2","#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b","black"), values=jbreaks2)+
  labs(colour="Sink Index = \nProp CA\n---------------\nmin Eucl. Dist")+
  geom_polygon(data = ACBRsPaekMap, aes(x=long, y = lat, group = group), linetype="dotted",color="black", fill=NA)+
  geom_label(data=textCoord,aes(x = x, y = y, label=TextString), size=2, fontface = "bold", label.padding=unit(0.1, "lines") ) +
  theme_bw()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  scale_y_continuous(breaks=c(-60,-65,-70,-75,-80,-85,-90))+
  scale_x_continuous(breaks=seq(-180,180,30))+
  coord_polar(start=pi)
  
ggsave("figures/sinkMap_acbr_jb.tiff",dpi=300,width = 29.7,height=21,units = "cm")






ggplot()+
  geom_polygon(data=Antarctica, aes(long, lat, group=group),fill="white", color="black")+
  geom_point(data=sinkInfo_acbr, aes(x=x.ant, y=y.ant, colour=sinkIndex))+
  scale_colour_gradientn(colours=rev(c("black",gray.colors(9, start=0.1, end=0.9))), values=jbreaks2)+
  labs(colour="Sink Index = \nProp CA\n---------------\nmin Eucl. Dist")+
  geom_polygon(data = ACBRsPaekMap, aes(x=long, y = lat, group = group), linetype="dotted",color="black", fill=NA)+
  geom_label(data=textCoord,aes(x = x, y = y, label=TextString), size=2, fontface = "bold", label.padding=unit(0.1, "lines") ) +
  theme_bw()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  scale_y_continuous(breaks=c(-60,-65,-70,-75,-80,-85,-90))+
  scale_x_continuous(breaks=seq(-180,180,30))+
  coord_polar(start=pi)

ggsave("figures/sinkMap_acbr_jb_fig3_bw.tiff",dpi=300,width = 29.7,height=21,units = "cm")










