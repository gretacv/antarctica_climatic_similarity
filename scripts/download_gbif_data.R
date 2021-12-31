
#species es la tabla donde la primera columna contiene las especies para las cuales se quieren buscar los puntos
species<-read.csv("chown_alien.csv") #cambiar seg?n donde se encuentre el archivo con los nombres de las especies. 
f<-factor(species[,1])
lookupSpecies<-as.character(f)


install.packages("spocc")
library("spocc")

##la funci?n occ() es la que busca las especies. Hay que tener en cuenta que el argumento "limit" limita el numero de presencias por especie que se descargan. Este argumento sirve para que la descarga no sea enorme la primera vez que se lanza el c?digo. El l?mite est? puesto a 10 presencias para que no tarde mucho. Una vez se haya ejecutado con este l?mite hay que poner uno muy elevado (1 mill?n, por ejemplo) para asegurarse de que todas las presencias disponible se descargan

res <- occ(query = lookupSpecies, from = 'gbif', limit = 100000)
locs<-occ2df(res)


names(locs) <- c('Especie', 'x', 'y', 'prov', 'date','key') ##Criar tabela com a especie e as coordenadas
a <- table(locs$Especie) #### Freq. Table
b <- as.data.frame(a) ##transformar a tabela em dataframe
names(b) <- c('Especie','N') ###Nomear as colunas



###select species with > 10 localities #####

locs.sub <- locs[locs$Especie %in% b$Especie,]
locs.sub$Especie <- factor(locs.sub$Especie)
occ.table <- as.data.frame(table(locs.sub$Especie))
names(occ.table) <- c('Especie','N') ### table with number of points per species


##Loop para separar cada especies em um archivo
# En el loop hay que cambiar el path donde se van a guardar los archivos, habr? un archivo por especie. 
for (i in 66:length(unique(locs.sub$Especie))) {
  occ.file <- locs.sub[which(locs.sub[,1]==paste(unique(locs.sub$Especie)[i])),]
  occ <- occ.file[,2:3]
  goodLines<-which((occ[,1]!=0 & occ[,2]!=0))
  if(length(goodLines)>0)
  {occ<- occ[goodLines,]
  goodLines<-which(occ[,1]!='NA')
  if(length(goodLines)>0)
  {occ<- occ[goodLines,]
  write.csv(occ, file=paste("SubAntarctic alien species points/",unique(locs.sub$Especie)[i],".csv",sep=""),row.names=F)
  }}
  
}

