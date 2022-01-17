library(raster)
library(rgdal)
library(dismo)
library(usdm)
library(sdm)
installAll()
###Para Seagrass
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Shapefiles')
a<-list.files(pattern = '.shp$')
seagrass<-shapefile('SeaGrass_Miller.shp')
seagrass$Species<-1
seagrass@data<-seagrass@data[,"Species",drop = F]

setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Preds/Preds_SeaGrass")
a<-list.files(pattern = '.img$')
preds<-stack(a)

sdmData<-sdmData(Species~.,train=seagrass,predictors=preds,bg=list(n=157,method='gRandom',remove=T))

modelo<-sdm(Species~.,data=sdmData,methods=c('brt','glm','maxent','svm','rbf'),
    replication='boot',n = 100, parallelSettings = list(ncore=4))

write.sdm(modelo,filename = 'modelo.seagrass.sdm')
sts<-getEvaluation(modelo,west='test.dep',stat = c('sensitivity','specificity','TSS','AUC','threshold'),opt = 2)

ids<-sts$modelID[which(sts$TSS>=0.400)]

en <- ensemble(modelo, preds, filename='ensemble_sdm_seagrass.img',
               setting=list(id = ids,method='weighted',stat='TSS',opt=2))
plot(en)
mediathrs<-mean(sts$threshold)

binario<-en

binario[]<-ifelse(en[]>=mediathrs,1,0)
plot(binario)
writeRaster(binario,file = 'binario_seagrass.img')
#############
#Para CoralReef
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Preds/Preds_CoralReef')
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Shapefiles')
a<-list.files(pattern = '.shp$')
coral<-shapefile('CoralReef_Miller.shp')
coral$Species<-1
coral@data<-coral@data[,"Species",drop = F]
b<-list.files(pattern = '.img')
preds<-stack(b)
sdmData<-sdmData(Species~.,train=coral,predictors=preds,bg=list(n=311,method='gRandom',remove=T))
modelo<-sdm(Species~.,data=sdmData,methods=c('brt','glm','maxent','svm','rbf'),
            replication='boot',n = 100, parallelSettings = list(ncore=4))

write.sdm(modelo,filename = 'modelo.coralreef.sdm')
sts<-getEvaluation(modelo,west='test.dep',stat = c('sensitivity','specificity','TSS','AUC','threshold'),opt = 2)

ids<-sts$modelID[which(sts$TSS>=0.400)]

en <- ensemble(modelo, preds, filename='ensemble_sdm_coralreef.img',
               setting=list(id = ids,method='weighted',stat='TSS',opt=2))
plot(en)
mediathrs<-mean(sts$threshold)
binario<-en

binario[]<-ifelse(en[]>=mediathrs,1,0)
plot(binario)
writeRaster(binario,file = 'binario_coralreef.img')
####
#Para RockyReef
a<-list.files(pattern = '.shp$')
rocky<-shapefile(a[[5]])
rocky$Species<-1
rocky@data<-rocky@data[,"Species",drop = F]
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Preds/Preds_RockyReef")
b<-list.files(pattern = '.img$')
preds<-stack(b)
sdmData<-sdmData(Species~.,train=rocky,predictors=preds,bg=list(n=2675,method='gRandom',remove=T))
modelo<-sdm(Species~.,data=sdmData,methods=c('brt','glm','maxent','svm','rbf'),
            replication='boot',n = 100, parallelSettings = list(ncore=4))

write.sdm(modelo,filename = 'modelo.rockyreef.sdm')
sts<-getEvaluation(modelo,west='test.dep',stat = c('sensitivity','specificity','TSS','AUC','threshold'),opt = 2)

ids<-sts$modelID[which(sts$TSS>=0.400)]

en <- ensemble(modelo, preds, filename='ensemble_sdm_rockyreef.img',
               setting=list(id = ids,method='weighted',stat='TSS',opt=2))
plot(en)
mediathrs<-mean(sts$threshold)
binario<-en

binario[]<-ifelse(en[]>=mediathrs,1,0)
plot(binario)
writeRaster(binario,file = 'binario_rockyreef.img')
#####
##Para Mangrove
mangrove<-shapefile(a[[3]])
mangrove$Species<-1
mangrove@data<-mangrove@data[,"Species",drop = F]
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Preds/Preds_Mangrove')
b<-list.files(pattern = '.img$')
preds<-stack(b)
plot(preds)

sdmData<-sdmData(Species~.,train=mangrove,predictors=preds,bg=list(n=3923,method='gRandom',remove=T))
modelo<-sdm(Species~.,data=sdmData,methods=c('brt','glm','maxent','svm','rbf'),
            replication='boot',n = 100, parallelSettings = list(ncore=4))
write.sdm(modelo,filename = 'modelo.mangrove.sdm')
sts<-getEvaluation(modelo,west='test.dep',stat = c('sensitivity','specificity','TSS','AUC','threshold'),opt = 2)

ids<-sts$modelID[which(sts$TSS>=0.400)]

en <- ensemble(modelo, preds, filename='ensemble_sdm_rockyreef.img',
               setting=list(id = ids,method='weighted',stat='TSS',opt=2))
plot(en)
mediathrs<-mean(sts$threshold)
binario<-en

binario[]<-ifelse(en[]>=mediathrs,1,0)
plot(binario)
writeRaster(binario,file = 'binario_mangrove.img')
