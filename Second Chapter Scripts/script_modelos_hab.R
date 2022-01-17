library(raster)
library(rgdal)
library(dismo)
library(usdm)
library(sdm)
installAll()

setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Shapefiles')
meroTrain<-shapefile('Mero_Miller_Rarefeito.shp')
meroTrain$Species<-1###Criar uma coluna chamada Species e com valor 1
meroTrain@data<-meroTrain@data[,'Species',drop=F]###Fazer com que somente a coluna Species seja "ativada"

setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/Hab_Only/Preds")
a<-list.files(pattern = '.img$')
preds<-stack(a)

data1x<-sdmData(Species~.,train=meroTrain,predictors=preds,bg=list(n=92,method='gRandom',remove=T))
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/Hab_Only")
write.sdm(data1x,file = 'data_habonly.sdd')

modelo_Sub<-sdm(Species~.,data=data1x,methods=c('brt','glm','maxent','svm','rbf'),
                  replication='sub',test.percent= 10,n = 100, parallelSettings = list(ncore=4))

write.sdm(modelo_Sub,filename = 'modelo_habOnly.sdm')
modelo_Sub_habonly<-read.sdm('modelo_habOnly.sdm')
sts<-getEvaluation(modelo_Sub,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
ids<-sts$modelID[which(sts$TSS>=0.700)]
en<-ensemble(modelo_Sub, preds, filename='ensemble_presente_HabOnly.img',
              setting=list(id =ids,method='weighted',stat='TSS',opt=2))

plot(en)
###Binarizar
thrs<-mean(sts$threshold)
PA<-en
PA[]<-ifelse(en[]>=thrs,1,0)
writeRaster(PA,filename = 'PA_Presente_HabOnly.img')
####
##Para Env+SomaHab
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/DistToAnyhab+Env/Preds")
a<-list.files(pattern=".img$")
preds<-stack(a)
data1x<-sdmData(Species~.,train=meroTrain,predictors=preds,bg=list(n=92,method='gRandom',remove=T))
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/DistToAnyhab+Env")
write.sdm(data1x,file = 'data_Env_AllHab.sdd')

modelo_Sub<-sdm(Species~.,data=data1x,methods=c('brt','glm','maxent','svm','rbf'),
                replication='sub',test.percent= 10,n = 100, parallelSettings = list(ncore=4))

write.sdm(modelo_Sub,filename = 'modelo_Env_AllHab.sdm')
modelo_EnvAnyHab<-read.sdm('modelo_Env_AllHab.sdm')
sts<-getEvaluation(modelo_Sub,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
ids<-sts$modelID[which(sts$TSS>=0.700)]
en<-ensemble(modelo_Sub, preds, filename='ensemble_presente_Env_AllHab.img',
             setting=list(id =ids,method='weighted',stat='TSS',opt=2))

plot(en)
###Binarizar
thrs<-mean(sts$threshold)
PA<-en
PA[]<-ifelse(en[]>=thrs,1,0)
writeRaster(PA,filename = 'PA_Presente_Env_AllHab.img')
plot(PA)
#######
#Modelo Env_Hab_Separado Sem correlacao
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/Env_Hab_Separado/Preds")
a<-list.files(pattern = '.img$')
b<-c(a[[2]],a[[3]],a[[4]],a[[5]],a[[7]])
preds<-stack(b)
data1x_semcorrel<-sdmData(Species~.,train = meroTrain,predictors = preds,bg=list(n=92,method='gRandom',remove=T))
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/Env_Hab_Separado/Modelo_SemVar_Correl")
write.sdm(data1x_semcorrel, file = 'data_Env_Hab_Separados_semcorrel.sdd')

m<-sdm(Species~.,data=data1x_semcorrel,methods=c('brt','glm','maxent','svm','rbf'),
       replication='sub',test.percent= 10,n = 100, parallelSettings = list(ncore=4))
write.sdm(m,file = 'modelo_Env_Hab_Separados_semCorrel.sdm')

sts<-getEvaluation(m,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
ids<-sts$modelID[which(sts$TSS>=0.700)]
en<-ensemble(m, preds, filename='ensemble_presente_Env_Hab_Separado_SemCorrel.img',
             setting=list(id =ids,method='weighted',stat='TSS',opt=2))

plot(en)
m<-read.sdm('modelo_Env_Hab_Separados_semCorrel.sdm')
en<-raster('ensemble_presente_Env_Hab_Separado_SemCorrel.img')
thrs<-mean(sts$threshold)
PA<-en
PA[]<-ifelse(en[]>=thrs,1,0)
plot(PA)
writeRaster(PA,file = 'PA_Presente_Env_HabSeparado.img')
#####
#Fazer a binarização pelo valor fixo de 0.5 de adequabilidade
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/Hab_Only/Hab_Only_SemCoralReef")
mHabOnly<-read.sdm('modelo_habOnly_semCoral.sdm')

setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/DistToAnyhab+Env/Modelo_AnyHab+EnvSemTempLtMax")
mEnvDistToAny<-read.sdm('modelo_Env_AllHab_SemtempLtMax.sdm')

setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/Env_Hab_Separado/Modelo_SemVar_Correl")
mEnv_HabSeparado<-read.sdm("modelo_Env_Hab_Separados_semCorrel.sdm")

setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/Presente_EnvOnly")
mclimatico<-read.sdm("modelo1x_90T_10V.sdm")
#
en_HabOnly<-raster('ensemble_presente_HabOnly_SemCoralreef.img')
en_EnvDistToAny<-raster("ensemble_presente_Env_AllHab_SemTempLtMax.img")
en_HabSeparado<-raster('ensemble_presente_Env_Hab_Separado_SemCorrel.img')
en_climatico<-raster('ensemble_presente_EnvOnly.img')

PA_fix1<-en_HabOnly
PA_fix1[]<-ifelse(en_HabOnly[]>=0.5,1,0)
plot(PA_fix1)

PA_fix2<-en_EnvDistToAny
PA_fix2[]<-ifelse(en_EnvDistToAny[]>=0.5,1,0)
plot(PA_fix2)

PA_fix3<-en_HabSeparado
PA_fix3[]<-ifelse(en_HabSeparado[]>=0.5,1,0)
plot(PA_fix3)

PA_fix4<-en_climatico
PA_fix4[]<-ifelse(en_climatico[]>=0.5,1,0)
plot(PA_fix4)

setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/Geoprocessamentos_E_analises")
writeRaster(PA_fix1,filename = 'PA_HabOnlyFixValue.img')
writeRaster(PA_fix2,filename = 'PA_EnvDistToAnyFixValue.img')
writeRaster(PA_fix3,filename = "PA_EnvHabSeparadoFixValue.img")
writeRaster(PA_fix4,filename = "PA_ClimaticoFixValue.img")
#####
#Calcular Estatisticas
sts_HabOnly<-getEvaluation(mHabOnly,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
sts_EnvDistToAny<-getEvaluation(mEnvDistToAny,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
sts_mEnv_HabSeparado<-getEvaluation(mEnv_HabSeparado,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
sts_mClimatico<-getEvaluation(mclimatico,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)

write.table(sts_HabOnly,sep = ';',file = 'sts_habOnly.txt',row.names = F)

write.table(sts_EnvDistToAny,sep =';',file = 'sts_EnvDistToAny.txt',row.names = F)

write.table(sts_mEnv_HabSeparado,sep = ';',file = 'sts_mEnv_HabSeparado.txt',row.names = F)

write.table(sts_mClimatico,sep = ';',file = 'sts_mClimatico.txt',row.names = F)
