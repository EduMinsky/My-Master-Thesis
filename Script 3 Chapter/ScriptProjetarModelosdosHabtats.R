library(raster)
library(rgdal)
library(dismo)
library(usdm)
library(sdm)
installAll()
###Modelos de nicho dos haitats no futuro
#Para Seagrass RCP 8.5
setwd('D:/Pesquisa/Mestrado/TERCEIRO_CAPITULO/VariavelDist_To_SeaGrass')
modelo.seagrass<-read.sdm('modelo.seagrass.sdm')
setwd("D:/Pesquisa/Mestrado/TERCEIRO_CAPITULO/VariavelDist_To_SeaGrass/Preds")
a<-list.files(pattern = '.img$')
preds<-stack(a)

sts<-getEvaluation(modelo.seagrass,west='test.dep',stat = c('sensitivity','specificity','TSS','AUC','threshold'),opt = 2)

ids<-sts$modelID[which(sts$TSS>=0.400)]

en <- ensemble(modelo.seagrass, preds, filename='ensemble_sdm_seagrass_rcp85.img',
               setting=list(id = ids,method='weighted',stat='TSS',opt=2))
plot(en)
en<-stack("ensemble_sdm_seagrass_rcp85.img")
mediathrs<-mean(sts$threshold)

binario<-en

binario[]<-ifelse(en[]>=mediathrs,1,0)
plot(binario)
writeRaster(binario,file = 'binario_seagrass_rcp85.img')
###
#Para RockyReef rcp8.5
setwd('D:/Pesquisa/Mestrado/TERCEIRO_CAPITULO/VariavelDist_To_RockyReef/Preds')
a<-list.files(pattern = '.img$')
preds<-stack(a)
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Preds/Preds_RockyReef')
m<-read.sdm('modelo.rockyreef.sdm')
sts<-getEvaluation(m,west='test.dep',stat = c('sensitivity','specificity','TSS','AUC','threshold'),opt = 2)

ids<-sts$modelID[which(sts$TSS>=0.400)]

en <- ensemble(m, preds, filename='ensemble_sdm_rockyreef_rcp85.img',
               setting=list(id = ids,method='weighted',stat='TSS',opt=2))

plot(en)
mediathrs<-mean(sts$threshold)
binario<-en

binario[]<-ifelse(en[]>=mediathrs,1,0)
plot(binario)
writeRaster(binario,file = 'binario_rockyreef_rcp85.img')
#ParaMangrove RCP85
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Preds/Preds_Mangrove')
m<-read.sdm('modelo.mangrove.sdm')
setwd('D:/Pesquisa/Mestrado/TERCEIRO_CAPITULO/VariavelDistToMangrove/Pred')
a<-list.files(pattern = '.img$')
preds<-stack(a)

sts<-getEvaluation(m,west='test.dep',stat = c('sensitivity','specificity','TSS','AUC','threshold'),opt = 2)

ids<-sts$modelID[which(sts$TSS>=0.400)]

en <- ensemble(m, preds, filename='ensemble_sdm_mangrovercp85.img',
               setting=list(id = ids,method='weighted',stat='TSS',opt=2))
plot(en)
mediathrs<-mean(sts$threshold)
binario<-en

binario[]<-ifelse(en[]>=mediathrs,1,0)
plot(binario)
writeRaster(binario,file = 'binario_mangrove_rcp85.img')
