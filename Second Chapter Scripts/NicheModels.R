library(raster)
library(rgdal)
library(dismo)
library(usdm)
library(sdm)
installAll()
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/Presente_EnvOnly')
m1<-read.sdm('modelo1x_90T_10V.sdm')
sts<-getEvaluation(m1,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
##selecionando Ids com tss>= 0.700
ids<-sts$modelID[which(sts$TSS>= 0.700)]
###Fazer predict e ensemble
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Shapefiles')
meroTrain<-shapefile('Mero_Miller_Rarefeito.shp')
meroTrain$Species<-1###Criar uma coluna chamada Species e com valor 1
meroTrain@data<-meroTrain@data[,'Species',drop=F]###Fazer com que somente a coluna Species seja "ativada"

setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Preds/Preds_Usadas_Mero')
a<-list.files(pattern = '.img$')
preds<-stack(a)

p1<-predict(m1, preds,w = ids,filename='predicts_presente_EnvOnly.img')
en<-ensemble(m1, preds, filename='ensemble_presente_EnvOnly.img',
             setting=list(id =ids,method='weighted',stat='TSS',opt=2))
plot(en)
###Binarizar
thrs<-mean(sts$threshold)
PA<-en
PA[]<-ifelse(en[]>=thrs,1,0)
writeRaster(PA,filename = 'PA_Presente_EnvOnly.img')
