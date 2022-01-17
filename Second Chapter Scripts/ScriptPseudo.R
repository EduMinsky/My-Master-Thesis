library(raster)
library(rgdal)
library(dismo)
library(usdm)
library(sdm)
installAll()
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Preds/Preds_Usadas_Mero')
a<-list.files(pattern = '.img$')
preds<-stack(a)
###Botar na memoria os shapefiles de presença da especie focal###
#Setting working director para os shapefiles#
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Shapefiles')
meroTrain<-shapefile('Mero_Miller_Rarefeito.shp')
meroTrain$Species<-1###Criar uma coluna chamada Species e com valor 1
meroTrain@data<-meroTrain@data[,'Species',drop=F]###Fazer com que somente a coluna Species seja "ativada"

##Criar um objeto com razao 1/1, 2/1,10/1,100/1

data1x<-sdmData(Species~.,train=meroTrain,predictors=preds,bg=list(n=105,method='gRandom',remove=T))
dat2x<- sdmData(Species~.,train=meroTrain,predictors=preds,bg=list(n=210,method='gRandom',remove=T))
data10x<- sdmData(Species~.,train=meroTrain,predictors=preds,bg=list(n=1050,method='gRandom',remove=T))

setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Teste_Sensibilidade')
write.sdm(x = data1x,filename = 'data_1x.sdd')
write.sdm(x = dat2x, filename = 'data_2x.sdd')
write.sdm(x = data10x, filename = 'data_10x.sdd')

###
data1x<-read.sdm('data_1x.sdd')
dat2x<-read.sdm('data_2x.sdd')
data10x<-read.sdm('data_10x.sdd')

###
##Criar os modelos de nicho com cvFolds de 10

modelo1x<-sdm(Species~.,data=data1x,methods=c('brt','glm','maxent','svm','rbf'),
    replication='cv',cv.folds = 10,n = 10, parallelSettings = list(ncore=4))



modelo2x<-sdm(Species~.,data=dat2x,methods=c('brt','glm','maxent','svm','rbf'),
              replication='cv',cv.folds= 10,n = 10, parallelSettings = list(ncore=4))

modelo10x<-sdm(Species~.,data=data10x,methods=c('brt','glm','maxent','svm','rbf'),
               replication='cv',cv.folds = 10,n = 10, parallelSettings = list(ncore=4))


setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Teste_Sensibilidade/CV_10Folds')
write.sdm(modelo1x,filename = 'Modelo1x_CV10Folds.sdm')
write.sdm(modelo2x,filename = 'Modelo2x_CV10Folds.sdm')
write.sdm(modelo10x,filename = 'Modelo10x_CV10Folds.sdm')
####criar modelos de nicho para CV fold 5
modelo1x_CV5<-sdm(Species~.,data=data1x,methods=c('brt','glm','maxent','svm','rbf'),
              replication='cv',cv.folds = 5,n = 10, parallelSettings = list(ncore=4))

modelo2x_CV5<-sdm(Species~.,data=dat2x,methods=c('brt','glm','maxent','svm','rbf'),
              replication='cv',cv.folds =5, n =10 , parallelSettings = list(ncore=4))

modelo10x<-sdm(Species~.,data=data10x,methods=c('brt','glm','maxent','svm','rbf'),
               replication='cv',cv.folds =5,n = 10 , parallelSettings = list(ncore=4))


#####
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Teste_Sensibilidade/CV_5Folds')
write.sdm(modelo1x_CV5,filename = 'modelo1x_CV5.sdm')
write.sdm(modelo2x_CV5,filename = 'modelo2x_CV5.sdm')
write.sdm(modelo10x,filename = 'modelo10x_CV5.sdm')

#####
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Teste_Sensibilidade/SubSample90_10")
#Para 90%Treino e 10% Test
modelo1x_Sub<-sdm(Species~.,data=data1x,methods=c('brt','glm','maxent','svm','rbf'),
                  replication='sub',test.percent= 10,n = 100, parallelSettings = list(ncore=4))

modelo2x_sub<-sdm(Species~.,data=dat2x,methods=c('brt','glm','maxent','svm','rbf'),
                  replication='sub',test.percent= 10,n = 100, parallelSettings = list(ncore=4))

modelo10x_sub<-sdm(Species~.,data=data10x,methods=c('brt','glm','maxent','svm','rbf'),
                 replication='sub',test.percent= 10,n = 100, parallelSettings = list(ncore=4))

write.sdm(modelo1x_Sub,file = 'modelo1x_sub.sdm')
write.sdm(modelo2x_sub,file = 'modelo2x_sub.sdm')
write.sdm(modelo10x_sub,file = 'modelo10x_sub.sdm')
#######
##
##
##
######################################################################################################################
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Teste_Sensibilidade/CV_10Folds")
modelo1x_CV10<-read.sdm("Modelo1x_CV10Folds.sdm")
modelo2x_CV10<-read.sdm("Modelo2x_CV10Folds.sdm")
modelo10x_CV10<-read.sdm("Modelo10x_CV10Folds.sdm")
##########################################################################################################################
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Teste_Sensibilidade/CV_5Folds")
modelo1xCV5<-read.sdm('modelo1x_CV5.sdm')
modelo2xCV5<- read.sdm('modelo2x_CV5.sdm')
modelo10xCV5<-read.sdm("modelo10x_CV5.sdm")

########################################################################################################################
setwd('D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/Teste_Sensibilidade/SubSample90_10')
modelo1x9010<-read.sdm("modelo1x_sub.sdm")
modelo2x9010<-read.sdm('modelo2x_sub.sdm')
modelo10x9010<-read.sdm('modelo10x_sub.sdm')

###
#Estatisticas

sts_modelo1xCV10<-getEvaluation(modelo1x_CV10,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
mean(sts_modelo1xCV10$sensitivity)
mean(sts_modelo1xCV10$specificity)
mean(sts_modelo1xCV10$TSS)

sts_modelo2x_CV10<-getEvaluation(modelo2x_CV10,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
mean(sts_modelo2x_CV10$sensitivity)
mean(sts_modelo2x_CV10$specificity)
mean(sts_modelo2x_CV10$TSS)

sts_modelo10xCV10<-getEvaluation(modelo10x_CV10,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
mean(sts_modelo10xCV10$sensitivity)
mean(sts_modelo10xCV10$specificity)
mean(sts_modelo10xCV10$TSS)

####

sts_modelo1xCV5<-getEvaluation(modelo1xCV5,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
mean(sts_modelo1xCV5$sensitivity)
mean(sts_modelo1xCV5$specificity)
mean(sts_modelo1xCV5$TSS)

sts_modelo2xCV5<-getEvaluation(modelo2xCV5,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
mean(sts_modelo2xCV5$sensitivity)
mean(sts_modelo2xCV5$specificity)
mean(sts_modelo2xCV5$TSS)

sts_modelo10xCV5<-getEvaluation(modelo10xCV5,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
mean(sts_modelo10xCV5$sensitivity)
mean(sts_modelo10xCV5$specificity)
mean(sts_modelo10xCV5$TSS)

########

sts_modelo1x9010<-getEvaluation(modelo1x9010,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
mean(sts_modelo1x9010$sensitivity)
mean(sts_modelo1x9010$specificity)
mean(sts_modelo1x9010$TSS)

sts_modelo2x9010<-getEvaluation(modelo2x9010,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
mean(sts_modelo2x9010$sensitivity)
mean(sts_modelo2x9010$specificity)
mean(sts_modelo2x9010$TSS)

sts_modelo10x9010<-getEvaluation(modelo10x9010,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
mean(sts_modelo10x9010$sensitivity)
mean(sts_modelo10x9010$specificity)
mean(sts_modelo10x9010$TSS)
####
a<-sts_modelo1xCV10$modelID[which(sts_modelo1xCV10$TSS>= 0.700)]
length(a)
b<-sts_modelo1xCV5$modelID[which(sts_modelo1xCV5$TSS>=0.700)]
length(b)
c<-sts_modelo1x9010$modelID[which(sts_modelo1x9010$TSS>= 0.700)]
length(c)
