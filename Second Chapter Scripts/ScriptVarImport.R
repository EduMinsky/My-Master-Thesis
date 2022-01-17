library(raster)
library(rgdal)
library(dismo)
library(usdm)
library(sdm)
installAll()
####Seleção das variaveis para os modelos de nicho####
#Setting working director para as variaveis e bota-las na memoria do R#
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Preds/Var_Clip')
a<-list.files(pattern = ".img$")
preds<-stack(a)
###Botar na memoria os shapefiles de presença da especie focal###
#Setting working director para os shapefiles#
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Shapefiles')
meroTrain<-shapefile('Mero_Train_Miller.shp')
meroTrain$Species<-1###Criar uma coluna chamada Species e com valor 1
meroTrain@data<-meroTrain@data[,'Species',drop=F]###Fazer com que somente a coluna Species seja "ativada"
#Mesmo procedimento para o shapefile de test
meroTest<-shapefile('Mero_Test_Miller.shp')
meroTest$Species<-1
meroTest@data<-meroTest@data[,'Species',drop=F]
###Criar sdmData com todas as variaveis e com uma###
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/VarImport")
#Para Salinity_LtMax
variavel<-preds[[-1]]

modelo_salinity_LtMax_only<-sdmData(Species~Present.Salinity.Lt.max_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                                    ,bg=list(n=169,method='vRandom',remove=T))
modelo_AllMenosSalLtMax<-sdmData(Species~.,train = meroTrain,test = meroTest,
                                 predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche_Salinity_LtMax_only<-sdm(Species~.,data = modelo_salinity_LtMax_only, methods=c('glm','brt','maxent','svm','rbf'),
                               replication='boot',n = 1, parallelSettings = list(ncore=4))

niche_Salinity_SemLtMax<-sdm(Species~., data = modelo_AllMenosSalLtMax,methods=c('glm','brt','maxent','svm','rbf'),
                             replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche_Salinity_LtMax_only,filename = 'nicho_SalinidadeLtMax.sdm')
write.sdm(niche_Salinity_SemLtMax, filename = 'nicho_SemSalinidadeLtMax.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = nicho_SalinidadeLtMax,id = i,wtest = 'test.indep')
}

impcomsal<-list()
i = 1
for(i in 1:5){
  impcomsal[[i]]<-getVarImp(x = nicho_SemSalinidadeLtMax , id = i,wtest = 'test.indep')
}
######
#Para Present.Salinity.Lt.min
variavel<-preds[[-2]]
sdmDataSalinityLtMin<-sdmData(Species~Present.Salinity.Lt.min_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                              ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemSalinityLtMin<-sdmData(Species~.,train = meroTrain,test = meroTest,
                                   predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.salinity.only<-sdm(Species~.,data = sdmDataSalinityLtMin, methods=c('glm','brt','maxent','svm','rbf'),
                         replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.salinity.semEla<-sdm(Species~.,data = sdmDataSemSalinityLtMin, methods=c('glm','brt','maxent','svm','rbf'),
                           replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.salinity.only,filename = 'nicho_SalinidadeLtMin.sdm')
write.sdm(niche.salinity.semEla, filename = 'nicho_SemSalinidadeLtMin.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.salinity.only,id = i,wtest = 'test.indep')
}

impcomsal<-list()
i = 1
for(i in 1:5){
  impcomsal[[i]]<-getVarImp(x = niche.salinity.semEla , id = i,wtest = 'test.indep')
}
######
##Para "Present.Salinity.max_Miller_Clip##
names(preds)
variavel<-preds[[-3]]
names(variavel)

sdmDataSalinityMax<-sdmData(Species~Present.Salinity.max_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                              ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemSalinityMax<-sdmData(Species~.,train = meroTrain,test = meroTest,
                                 predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.salinity.only<-sdm(Species~.,data = sdmDataSalinityMax, methods=c('glm','brt','maxent','svm','rbf'),
                         replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.salinity.semEla<-sdm(Species~.,data = sdmDataSemSalinityMax, methods=c('glm','brt','maxent','svm','rbf'),
                           replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.salinity.only,filename = 'nicho_SalinidadeMAX.sdm')
write.sdm(niche.salinity.semEla, filename = 'nicho_SemSalinidadeMAX.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.salinity.only,id = i,wtest = 'test.indep')
}

impcomsal<-list()
i = 1
for(i in 1:5){
  impcomsal[[i]]<-getVarImp(x = niche.salinity.semEla , id = i,wtest = 'test.indep')
}
###
##Para Present.Salinity.mean##
variavel<- preds[[-4]]
preds[[4]]
sdmDataSalinityMean<-sdmData(Species~Present.Salinity.mean_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                            ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemSalinityMean<-sdmData(Species~.,train = meroTrain,test = meroTest,
                               predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.salinity.only<-sdm(Species~.,data = sdmDataSalinityMean, methods=c('glm','brt','maxent','svm','rbf'),
                         replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.salinity.semEla<-sdm(Species~.,data = sdmDataSemSalinityMean, methods=c('glm','brt','maxent','svm','rbf'),
                           replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.salinity.only,filename = 'nicho_SalinidadeMEAN.sdm')
write.sdm(niche.salinity.semEla, filename = 'nicho_SemSalinidadeMEAN.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.salinity.only,id = i,wtest = 'test.indep')
}

impcomsal<-list()
i = 1
for(i in 1:5){
  impcomsal[[i]]<-getVarImp(x = niche.salinity.semEla , id = i,wtest = 'test.indep')
}
####Para Present.Salinity.min_Miller_Clip
variavel<-preds[[-5]]
preds[[5]]

sdmDataSalinityMin<-sdmData(Species~Present.Salinity.min_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                             ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemSalinityMin<-sdmData(Species~.,train = meroTrain,test = meroTest,
                                predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.salinity.only<-sdm(Species~.,data = sdmDataSalinityMin, methods=c('glm','brt','maxent','svm','rbf'),
                         replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.salinity.semEla<-sdm(Species~.,data = sdmDataSemSalinityMin, methods=c('glm','brt','maxent','svm','rbf'),
                           replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.salinity.only,filename = 'nicho_SalinidadeMIN.sdm')
write.sdm(niche.salinity.semEla, filename = 'nicho_SemSalinidadeMIN.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.salinity.only,id = i,wtest = 'test.indep')
}

impcomsal<-list()
i = 1
for(i in 1:5){
  impcomsal[[i]]<-getVarImp(x = niche.salinity.semEla , id = i,wtest = 'test.indep')
}
##Para Present.Salinity.range_Miller_Clip
variavel<-preds[[-6]]

sdmDataSalinityRange<-sdmData(Species~Present.Salinity.range_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                            ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemSalinityRange<-sdmData(Species~.,train = meroTrain,test = meroTest,
                               predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.salinity.only<-sdm(Species~.,data = sdmDataSalinityRange, methods=c('glm','brt','maxent','svm','rbf'),
                         replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.salinity.semEla<-sdm(Species~.,data = sdmDataSemSalinityRange, methods=c('glm','brt','maxent','svm','rbf'),
                           replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.salinity.only,filename = 'nicho_SalinidadeRANGE.sdm')
write.sdm(niche.salinity.semEla, filename = 'nicho_SemSalinidadeRANGE.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.salinity.only,id = i,wtest = 'test.indep')
}

impcomsal<-list()
i = 1
for(i in 1:5){
  impcomsal[[i]]<-getVarImp(x = niche.salinity.semEla , id = i,wtest = 'test.indep')
}
###
#Para Present.Temperature.Lt.max_Miller_Clip
variavel<-preds[[-7]]

sdmDataTemperatureLtMax<-sdmData(Species~Present.Temperature.Lt.max_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                              ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemTemperatureLtMax<-sdmData(Species~.,train = meroTrain,test = meroTest,
                                 predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.temperature.only<-sdm(Species~.,data = sdmDataTemperatureLtMax, methods=c('glm','brt','maxent','svm','rbf'),
                         replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.temperature.semEla<-sdm(Species~.,data = sdmDataSemTemperatureLtMax, methods=c('glm','brt','maxent','svm','rbf'),
                           replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.temperature.only,filename = 'nicho_TemperaturaLtMAX.sdm')
write.sdm(niche.temperature.semEla, filename = 'nicho_SemTemperaturaLtMax.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.temperature.only,id = i,wtest = 'test.indep')
}

impcomtemp<-list()
i = 1
for(i in 1:5){
  impcomtemp[[i]]<-getVarImp(x = niche.temperature.semEla , id = i,wtest = 'test.indep')
}
###
#Para Present.Temperature.Lt.min_Miller_Clip
names(preds)
variavel<-preds[[-8]]

sdmDataTemperatureLtMin<-sdmData(Species~Present.Temperature.Lt.min_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                                 ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemTemperatureLtMin<-sdmData(Species~.,train = meroTrain,test = meroTest,
                                    predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.temperature.only<-sdm(Species~.,data = sdmDataTemperatureLtMin, methods=c('glm','brt','maxent','svm','rbf'),
                            replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.temperature.semEla<-sdm(Species~.,data = sdmDataSemTemperatureLtMin, methods=c('glm','brt','maxent','svm','rbf'),
                              replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.temperature.only,filename = 'nicho_TemperaturaLtMin.sdm')
write.sdm(niche.temperature.semEla, filename = 'nicho_SemTemperaturaLtMin.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.temperature.only,id = i,wtest = 'test.indep')
}

impcomtemp<-list()
i = 1
for(i in 1:5){
  impcomtemp[[i]]<-getVarImp(x = niche.temperature.semEla , id = i,wtest = 'test.indep')
}
##
#Para Present.Temperature.max_Miller_Clip
variavel<-preds[[-9]]

sdmDataTemperaturemax<-sdmData(Species~Present.Temperature.max_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                                 ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemTemperaturemax<-sdmData(Species~.,train = meroTrain,test = meroTest,
                                    predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.temperature.only<-sdm(Species~.,data = sdmDataTemperaturemax, methods=c('glm','brt','maxent','svm','rbf'),
                            replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.temperature.semEla<-sdm(Species~.,data = sdmDataSemTemperaturemax, methods=c('glm','brt','maxent','svm','rbf'),
                              replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.temperature.only,filename = 'nicho_Temperaturamax.sdm')
write.sdm(niche.temperature.semEla, filename = 'nicho_SemTemperaturamax.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.temperature.only,id = i,wtest = 'test.indep')
}

impcomtemp<-list()
i = 1
for(i in 1:5){
  impcomtemp[[i]]<-getVarImp(x = niche.temperature.semEla , id = i,wtest = 'test.indep')
}
##
#Para Present.Temperature.mean_Miller_Clip
variavel<-preds[[-10]]

sdmDataTemperaturemean<-sdmData(Species~Present.Temperature.mean_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                               ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemTemperaturemean<-sdmData(Species~.,train = meroTrain,test = meroTest,
                                  predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.temperature.only<-sdm(Species~.,data = sdmDataTemperaturemean, methods=c('glm','brt','maxent','svm','rbf'),
                            replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.temperature.semEla<-sdm(Species~.,data = sdmDataSemTemperaturemean, methods=c('glm','brt','maxent','svm','rbf'),
                              replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.temperature.only,filename = 'nicho_Temperaturamean.sdm')
write.sdm(niche.temperature.semEla, filename = 'nicho_SemTemperaturamean.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.temperature.only,id = i,wtest = 'test.indep')
}

impcomtemp<-list()
i = 1
for(i in 1:5){
  impcomtemp[[i]]<-getVarImp(x = niche.temperature.semEla , id = i,wtest = 'test.indep')
}
##
#Para Present.Temperature.min_Miller_Clip
variavel<-preds[[-11]]

sdmDataTemperaturemin<-sdmData(Species~Present.Temperature.min_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                                ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemTemperaturemin<-sdmData(Species~.,train = meroTrain,test = meroTest,
                                   predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.temperature.only<-sdm(Species~.,data = sdmDataTemperaturemin, methods=c('glm','brt','maxent','svm','rbf'),
                            replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.temperature.semEla<-sdm(Species~.,data = sdmDataSemTemperaturemin, methods=c('glm','brt','maxent','svm','rbf'),
                              replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.temperature.only,filename = 'nicho_Temperaturamin.sdm')
write.sdm(niche.temperature.semEla, filename = 'nicho_SemTemperaturamin.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.temperature.only,id = i,wtest = 'test.indep')
}

impcomtemp<-list()
i = 1
for(i in 1:5){
  impcomtemp[[i]]<-getVarImp(x = niche.temperature.semEla , id = i,wtest = 'test.indep')
}
###
#Para Present.Temperature.range_Miller_Clip
variavel<-preds[[-12]]

sdmDataTemperatureRange<-sdmData(Species~Present.Temperature.range_Miller_Clip,train=meroTrain,test = meroTest,predictors=preds
                               ,bg=list(n=169,method='vRandom',remove=T))
sdmDataSemTemperatureRange<-sdmData(Species~.,train = meroTrain,test = meroTest,
                                  predictors = variavel,bg = list(n = 169, method = 'vRandom', remove = T))

niche.temperature.only<-sdm(Species~.,data = sdmDataTemperatureRange, methods=c('glm','brt','maxent','svm','rbf'),
                            replication='boot',n = 1, parallelSettings = list(ncore=4))
niche.temperature.semEla<-sdm(Species~.,data = sdmDataSemTemperatureRange, methods=c('glm','brt','maxent','svm','rbf'),
                              replication='boot',n = 1, parallelSettings = list(ncore=4))

write.sdm(niche.temperature.only,filename = 'nicho_TemperaturaRange.sdm')
write.sdm(niche.temperature.semEla, filename = 'nicho_SemTemperaturaRange.sdm')

imp<-list()
i=1
for(i in 1:5){
  imp[[i]]<-getVarImp(x = niche.temperature.only,id = i,wtest = 'test.indep')
}

impcomtemp<-list()
i = 1
for(i in 1:5){
  impcomtemp[[i]]<-getVarImp(x = niche.temperature.semEla , id = i,wtest = 'test.indep')
}

