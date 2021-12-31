library(raster)
rdata=list.files(pattern=".Rdata$")
rdata=rdata[-1]
load("subResbind1_100000.Rdata")
subresmerge=subResbind
for (i in rdata)
{
  print(dim(subresmerge))
  load(i)
  subresmerge=rbind(subresmerge,subResbind)
  
}
rm(subResbind)
dim(subresmerge)
class(subresmerge)
str(dimnames(subresmerge))
dfResW=cbind(dfW,subresmerge)
dfResW=dfResW[,-3]
head(dfResW)
names(dfResW)=c("x.world","y.world","min_Eucl_dist","Acell_min_Eucl_dist","summary.Min","summary.1stQu", "summary.Median", "summary.Mean", "summary.3rdQu", "summary.Max")
dfResW$Acell_min_Eucl_dist=paste("A", dfResW$Acell_min_Eucl_dist,sep="_")
dfResW$W_ID=gsub("W_",row.names(dfResW),replacement = "")
head(dfA)
coordA=dfA[,1:2]
coordA$Acell_min_Eucl_dist=row.names(coordA)
head(coordA)
xx=merge(dfResW,coordA)
head(xx)
row.names(xx)=xx$W_ID
dfResW_complete=xx[order(as.numeric(xx$W_ID)),]
head(dfResW_complete)
dfResW_complete$W_ID=paste("W",dfResW_complete$W_ID,sep="_")
head(dfResW_complete)

write.csv(dfResW_complete,"results/dfResW_complete.csv")
unique(dfResW_complete$Acell_min_Eucl_dist)

sinkCells_min=as.data.frame(table(dfResW_complete$Acell_min_Eucl_dist))
names(dfA)
dfA$A_ID=row.names(dfA)
head(dfA)
names(sinkCells_min)=c("A_ID","Count_min")
sinkCells_min$A_ID=as.character(sinkCells_min$A_ID)
head(sinkCells_min)
xx=merge(sinkCells_min, dfA)
head(xx)
dim(xx)
write.csv(xx,"results/Sink_minEucl_res.csv")
plot(sAnt)
points(xx$x.ant,xx$y.ant, col="red", pch=19,cex=0.25)
plot(sWorld)

W_eucl_raster=rasterize(x = dfResW[,1:2], y=sWorld, field=dfResW$subres_min)
writeRaster(W_eucl_raster, file="results/W_eucl_raster.tif",format="GTiff")
plot(W_eucl_raster)
