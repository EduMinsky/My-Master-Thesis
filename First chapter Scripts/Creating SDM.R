#####SCRIPT PARA CRIAR MODELOS DE NICHO E COMPARAR AS INCERTEZAS DA VIDA#######


#####Ler as variaveis preditoras que serão usadas na modelagem######

setwd("D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/VariaveisAmbientais/Var_Atlantico/Miller")
library(raster)
library(rgdal)
library(dismo)
library(usdm)
library(sdm)
installAll()
a<-list.files(pattern = '.img')
b<-a[c(1,2,6,9,10)]
preds<-stack(b)
plot(preds)
##################
#####Reading shapefiles for the modeling:
setwd("D:/Pesquisa/Mestrado/First_Chapter/ShapeFiles")
sp<-shapefile('Mero_America_Miller_50km.shp')
sp$species <- 1###########Create column where all values are number 1(1 means presence of species)
sp@data <- sp@data[,'species',drop=F]####Drop all columns and keep only the species column
sp_africa<-shapefile('Mero_Africa_Miller_50km.shp')
sp_africa$species <- 1
sp_africa@data <- sp_africa@data[,'species',drop=F]
sp_total<-shapefile('Mero_FullSet_Miller_50km.shp')
sp_total$species<-1
sp_total@data<-sp_total@data[,'species',drop=F]
###################
###Create SDM Data
setwd("D:/Pesquisa/Mestrado/First_Chapter/Modelos_de_Nicho")
d_africa<-sdmData(species~.,train=sp_africa,test = sp,predictors=preds,bg=list(n=71,method='vRandom',remove=T))
write.sdm(x = d_africa,filename = 'data_africa.sdd')

d_Total<-sdmData(species~.,train=sp_total,predictors = preds,bg=list(n=141,method='vRandom',remove=T))
write.sdm(d_Total, filename = 'data_total.sdd')

m3<-sdm(species~.,data = d_Total, methods=c('glm','brt','maxent','svm','rbf'),
        replication='boot',n = 50, parallelSettings = list(ncore=4))
###################
###Saving
setwd("D:/Pesquisa/Mestrado/First_Chapter/Modelos_de_Nicho")
write.sdm(m3,filename = 'modeloTotal.sdm')

########################################################
####reading models
setwd("D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/Modelos_de_Nicho")
m.america<-read.sdm(filename ='modeloTreinoAmerica.sdm')
m.africa<-read.sdm(filename = 'modeloTreinoAfrica.sdm')
m.total<-read.sdm(filename = 'modeloTotal.sdm')
####################
####Analysing models
gui(m.america)
roc(m.america)
getModelInfo(m.america)
#############
####Predictions#######
setwd("D:/Pesquisa/Mestrado/First_Chapter/Modelos_de_Nicho")
m.america<-read.sdm('modeloTreinoAmerica.sdm')
####Creating statistics
ev.america<-getEvaluation(m.america,wtest= 'test.indep',stat=c('sensitivity','specificity','TSS','AUC'),opt=2)

m.africa<-read.sdm(filename = 'modeloTreinoAfrica.sdm')
ev.africa<-getEvaluation(m.africa,wtest = 'test.dep',stat = c('sensitivity','specificity','TSS','AUC'),opt = 2)

m.total<-read.sdm('modeloTotal.sdm')
ev.total<-getEvaluation(m.total,wtest = 'test.dep',stat = c('sensitivity','specificity','TSS','AUC'),opt = 2)
###########
#Escolher IDs com TSS >=0.7
ID.america<-ev.america$modelID[which(ev.america$TSS >= 0.7)]

ID.africa<-ev.africa$modelID[which(ev.africa$TSS >= 0.7)]

ID.total<-ev.total$modelID[which(ev.total$TSS >= 0.7)]
#########

###Starting predictions

setwd("D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/Modelos_de_Nicho/predictions")

p.america.50 <- predict(m.america, preds,w = ID.america[c(1:50)],filename='predict_america_50Model.img')
p.america.100<-predict(m.america, preds,w = ID.america[c(51:100)],filename='predict_america_100Model.img')
p.america.200<-predict(m.america,preds, w = ID.america[c(101:200)], filename = 'predict_america_200Model.img')
p.america.235<-predict(m.america,preds, w = ID.america[c(201:235)], filename = 'predict_america_235Model.img')
#########
###Para a Africa
p.africa.50<-predict(m.africa,preds, w = ID.africa[c(1:50)],filename = 'predict_africa_50Model.img')
p.africa.100<-predict(m.africa, preds, w = ID.africa[c(51:100)], filename = 'predict_africa_100Model.img')
p.africa.150<-predict(m.africa, preds, w = ID.africa[c(101:150)], filename = 'predict_africa_150Model.img')
p.africa.200<-predict(m.africa, preds, w = ID.africa[c(151:200)],filename = 'predict_africa_200Model.img')
p.africa.246<-predict(m.africa, preds, w = ID.africa[c(201:246)], filename = ' predict_africa_246Model.img')

#######
###Para o Total
p.total.50<-predict(m.total,preds, w = ID.total[c(1:50)], filename = 'predict_total_50Model.img')
p.total.100<-predict(m.total, preds, w = ID.total[c(51:100)], filename = 'predict_total_100Model.img')
p.total.150<-predict(m.total, preds, w = ID.total[c(101:150)],filename = 'predict_total_150Model.img')
p.total.200<-predict(m.total, preds, w = ID.total[c(151:200)],filename = 'predict_total_200Model.img')
p.total.236<-predict(m.total, preds, w = ID.total[c(201:236)],filename = 'predict_total_236Model.img')
#########
###Salvar estatisticas de IDs com tss >=0.7
setwd("D:/Pesquisa/Mestrado/First_Chapter/Modelos_de_Nicho/Estatisticas")
##Para America
estat.america.ID<-ev.america$modelID[which(ev.america$TSS>=0.7)]
estat.america.AUC<-ev.america$AUC[which(ev.america$TSS>=0.7)]
estat.america.Sensitivity<-ev.america$sensitivity[which(ev.america$TSS>=0.7)]
estat.america.Specificity<-ev.america$specificity[which(ev.america$TSS>=0.7)]
estat.america.TSS<-ev.america$TSS[which(ev.america$TSS>=0.7)]
##Salvar
write.csv2(estat.america.ID,file = 'ID_America07.csv')
write.csv2(estat.america.AUC,file = 'AUC.America07.csv')
write.csv2(estat.america.Sensitivity,file = 'Sensitivity.America07.csv')
write.csv2(estat.america.Specificity,file = 'Specificity.America07.csv')
write.csv2(estat.america.TSS,file = 'TSS.America.csv')
######
##Para Africa
estat.africa.ID<-ev.africa$modelID[which(ev.africa$TSS>=0.7)]
estat.africa.AUC<-ev.africa$AUC[which(ev.africa$TSS>=0.7)]
estat.africa.Sensitivity<-ev.africa$sensitivity[which(ev.africa$TSS>=0.7)]
estat.africa.Specificity<-ev.africa$specificity[which(ev.africa$TSS>=0.7)]
estat.africa.TSS<-ev.africa$TSS[which(ev.africa$TSS>=0.7)]
##Salvar
write.csv2(estat.africa.ID, file = 'ID.Africa07.csv')
write.csv2(estat.africa.AUC, file = 'AUC.Africa07.csv')
write.csv2(estat.africa.Sensitivity, file = 'Sensitivity.Africa07.csv')
write.csv2(estat.africa.Specificity, file = 'Specificity.Africa07.csv')
write.csv2(estat.africa.TSS, file = 'TSS.Africa07.csv')
#####
###Para o Total
estat.total.ID<-ev.total$modelID[which(ev.total$TSS>=0.7)]
estat.total.AUC<-ev.total$AUC[which(ev.total$TSS>=0.7)]
estat.total.Sensitivity<-ev.total$sensitivity[which(ev.total$TSS>=0.7)]
estat.total.Specificity<-ev.total$specificity[which(ev.total$TSS>=0.7)]
estat.total.TSS<-ev.total$TSS[which(ev.total$TSS>=0.7)]
##Salvar
write.csv2(estat.total.ID, file ='ID.total07.csv')
write.csv2(estat.total.AUC, file ='AUC.total07.csv')
write.csv2(estat.total.Sensitivity, file ='Sensitivity.total07.csv')
write.csv2(estat.total.Specificity, file ='Specificity.total07.csv')
write.csv2(estat.total.TSS, file ='TSS.total07.csv')
#######
##Fazer os predicts pela media dos Algoritmos para America
setwd('D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/Modelos_de_Nicho/predictions/Pela_Media_Algoritmos/America')
m.america
ID.america

predict.america.GLM<-predict(m.america,preds,w=ID.america[1:44],mean = T, filename = 'Prediction.America.GLM.img')
plot(predict.america.GLM)
predict.america.BRT<-predict(m.america,preds,w = ID.america[45:94], mean = T, filename = 'Prediction.America.BRT.img')
plot(predict.america.BRT)
predict.america.maxent<- predict(m.america,preds, w =ID.america[95:143],mean = T, filename = 'Prediction.America.Maxent.img' )
plot(predict.america.maxent)
predict.america.SVM<-predict(m.america,preds, w = ID.america[144:192], mean = T, filename = 'Prediction.America.SVM.img')
plot(predict.america.SVM)
predict.america.RBF<-predict(m.america,preds,w = ID.america[193:235],mean = T,filename = 'Prediction.America.RBF.img')
plot(predict.america.RBF)
##########
###Calculando erros de comissão e omissao###################
setwd("D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/Modelos_de_Nicho/predictions/Pela_Media_Algoritmos/America")
#Rodar o brick com as predições
a<-list.files(pattern = '.img$')
predicted.america<-list()
i=1
for(i in 1:length(a)){
  predicted.america[[i]]<-raster(a[[i]])
}
###Botar na memoria shapefile de pseusoausencia e presença
setwd('D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/ShapeFiles')
background.america<-shapefile('Background_America.shp')
test.America<-shapefile('Mero_Africa_Miller_50km.shp')
train.America<-shapefile('Mero_America_Miller_50km.shp')
##Criar objeto com os valores de threshold que maximiza TSS i.e opt=2 dos modelos com TSS >=0,7 para BackGround, Treino e Test da America
#Escolher os valores de limiar somente dos modelos com TSS >=0.700 PARA TEST
threshold.America.test<-getEvaluation(m.america,wtest = 'test.dep',stat = c('threshold','TSS'),opt = 2)
threshold.America.GLM.test<-threshold.America.test[c(1:50),]
threshold.America.GLM.Mean.test<-mean(threshold.America.GLM$threshold[which(threshold.America.GLM$TSS>=0.700)])
#Escolher os valores de limiar somente dos modelos com TSS >=0.700 PARA TREINO
threshold.America.Train<-getEvaluation(m.america,wtest = 'train',stat = c('threshold','TSS'),opt = 2)
threshold.America.GLM.Train<-threshold.America.Train[c(1:50),]
threshold.America.GLM.Mean.Train<-mean((threshold.America.GLM.Train$threshold[which(threshold.America.GLM.Train$TSS>=0.700)]))
####Calcular erro de comissao e erro de omissao
Background.values.GLM<-extract(predicted.america[[2]],background.america)
erro.comissao.GLM.Test<-Background.values.GLM[which(Background.values.GLM[] >= threshold.America.GLM.Mean.test)]
erro.comissao.GLM.Treino<-Background.values.GLM[which(Background.values.GLM[] >=threshold.America.GLM.Mean.Train)]

Test.values.GLM<-extract(predicted.america[[2]],test.America)
erro.omissao.test.GLM<-Test.values.GLM[which(Test.values.GLM[] <=threshold.America.GLM.Mean.test)]

Train.values.GLM<-extract(predicted.america[[2]],train.America)
erro.omissao.treino.GLM<-Train.values.GLM[which(Train.values.GLM[]<=threshold.America.GLM.Mean.Train)]

####Salvar essas informações
setwd("D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/Modelos_de_Nicho/Erros")
write.table(erro.comissao.GLM.Test,file = 'erro_comissao_America_Glm_Test.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.comissao.GLM.Treino,file = 'erro_comissao_America_Glm_Treino.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.omissao.test.GLM,file = 'erro_omissaoTEST_America_Glm.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.omissao.treino.GLM,file = 'erro_omissaoTRAIN_America_Glm.txt',sep = ";", row.names = F,col.names = F)
###############

#Agora para o BRT
###BackGround Comissao BRT
threshold.America.test<-getEvaluation(m.america,wtest = 'test.dep',stat = c('threshold','TSS'),opt = 2)
threshold.America.BRT.test<-threshold.America[c(51:100),]
threshold.America.BRT.Mean.test<-mean(threshold.America.BRT$threshold[which(threshold.America.BRT$TSS>=0.700)])
#Escolher os valores de limiar somente dos modelos com TSS >=0.700 PARA TREINO
threshold.America.Train<-getEvaluation(m.america,wtest = 'train',stat = c('threshold','TSS'),opt = 2)
threshold.America.BRT.Train<-threshold.America.Train[c(51:100),]
threshold.America.BRT.Mean.Train<-mean((threshold.America.BRT.Train$threshold[which(threshold.America.BRT.Train$TSS>=0.700)]))
####Calcular erro de comissao e erro de omissao
Background.values.BRT<-extract(predicted.america[[1]],background.america)
erro.comissao.BRT.Test<-Background.values.BRT[which(Background.values.BRT[] >= threshold.America.BRT.Mean.test)]
erro.comissao.BRT.Treino<-Background.values.BRT[which(Background.values.BRT[] >=threshold.America.BRT.Mean.Train)]

Test.values.BRT<-extract(predicted.america[[1]],test.America)
erro.omissao.test.BRT<-Test.values.BRT[which(Test.values.BRT[] <=threshold.America..Mean.test)]

Train.values.BRT<-extract(predicted.america[[1]],train.America)
erro.omissao.treino.BRT<-Train.values.BRT[which(Train.values.BRT[]<=threshold.America.BRT.Mean.Train)]

####Salvar essas informações
setwd("D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/Modelos_de_Nicho/Erros")
write.table(erro.comissao.BRT.Test,file = 'erro_comissao_America_Brt_Test.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.comissao.BRT.Treino,file = 'erro_comissao_America_Brt_Treino.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.omissao.test.BRT,file = 'erro_omissaoTEST_America_Brt.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.omissao.treino.BRT,file = 'erro_omissaoTRAIN_America_Brt.txt',sep = ";", row.names = F,col.names = F)
######
#Agora para Maxent
###BackGround Comissao Maxent
threshold.America.test<-getEvaluation(m.america,wtest = 'test.dep',stat = c('threshold','TSS'),opt = 2)
threshold.America.MAXENT.test<-threshold.America.test[c(101:150),]
threshold.America.MAXENT.Mean.test<-mean(threshold.America.MAXENT.test$threshold[which(threshold.America.MAXENT.test$TSS>=0.700)])
#Escolher os valores de limiar somente dos modelos com TSS >=0.700 PARA TREINO
threshold.America.Train<-getEvaluation(m.america,wtest = 'train',stat = c('threshold','TSS'),opt = 2)
threshold.America.MAXENT.Train<-threshold.America.Train[c(101:150),]
threshold.America.MAXENT.Mean.Train<-mean((threshold.America.MAXENT.Train$threshold[which(threshold.America.MAXENT.Train$TSS>=0.700)]))
####Calcular erro de comissao e erro de omissao
Background.values.MAXENT<-extract(predicted.america[[3]],background.america)
erro.comissao.MAXENT.Test<-Background.values.MAXENT[which(Background.values.MAXENT[] >= threshold.America.MAXENT.Mean.test)]
erro.comissao.MAXENTT.Treino<-Background.values.MAXENT[which(Background.values.MAXENT[] >=threshold.America.MAXENT.Mean.Train)]

Test.values.MAXENT<-extract(predicted.america[[3]],test.America)
erro.omissao.test.MAXENT<-Test.values.MAXENT[which(Test.values.MAXENT[] <=threshold.America.MAXENT.Mean.test)]

Train.values.MAXENT<-extract(predicted.america[[3]],train.America)
erro.omissao.treino.MAXENT<-Train.values.MAXENT[which(Train.values.MAXENT[]<=threshold.America.MAXENT.Mean.Train)]

####Salvar essas informações
setwd("D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/Modelos_de_Nicho/Erros")
write.table(erro.comissao.MAXENT.Test,file = 'erro_comissao_America_Maxent_Test.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.comissao.MAXENTT.Treino,file = 'erro_comissao_America_Maxent_Treino.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.omissao.test.MAXENT,file = 'erro_omissaoTEST_America_Maxent.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.omissao.treino.MAXENT,file = 'erro_omissaoTRAIN_America_Maxent.txt',sep = ";", row.names = F,col.names = F)
#####
##Para o SVM
###BackGround Comissao SVM
threshold.America.test<-getEvaluation(m.america,wtest = 'test.dep',stat = c('threshold','TSS'),opt = 2)
threshold.America.SVM.test<-threshold.America.test[c(151:200),]
threshold.America.SVM.Mean.test<-mean(threshold.America.SVM.test$threshold[which(threshold.America.SVM.test$TSS>=0.700)])
#Escolher os valores de limiar somente dos modelos com TSS >=0.700 PARA TREINO
threshold.America.Train<-getEvaluation(m.america,wtest = 'train',stat = c('threshold','TSS'),opt = 2)
threshold.America.SVM.Train<-threshold.America.Train[c(151:200),]
threshold.America.SVM.Mean.Train<-mean((threshold.America.SVM.Train$threshold[which(threshold.America.SVM.Train$TSS>=0.700)]))
####Calcular erro de comissao e erro de omissao
Background.values.SVM<-extract(predicted.america[[5]],background.america)
erro.comissao.SVM.Test<-Background.values.SVM[which(Background.values.SVM[] >= threshold.America.SVM.Mean.test)]
erro.comissao.SVM.Treino<-Background.values.SVM[which(Background.values.SVM[] >=threshold.America.SVM.Mean.Train)]

Test.values.SVM<-extract(predicted.america[[5]],test.America)
erro.omissao.test.SVM<-Test.values.SVM[which(Test.values.SVM[] <=threshold.America.SVM.Mean.test)]

Train.values.SVM<-extract(predicted.america[[5]],train.America)
erro.omissao.treino.SVM<-Train.values.SVM[which(Train.values.SVM[]<=threshold.America.SVM.Mean.Train)]

####Salvar essas informações
setwd("D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/Modelos_de_Nicho/Erros")
write.table(erro.comissao.MAXENT.Test,file = 'erro_comissao_America_Maxent_Test.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.comissao.MAXENTT.Treino,file = 'erro_comissao_America_Maxent_Treino.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.omissao.test.MAXENT,file = 'erro_omissaoTEST_America_Maxent.txt',sep = ";", row.names = F,col.names = F)
write.table(erro.omissao.treino.MAXENT,file = 'erro_omissaoTRAIN_America_Maxent.txt',sep = ";", row.names = F,col.names = F)

m.america@models$species$svm$

################################################################


rcurve(m.america,id=1:10,n = 'bathy_atlantico',mean = T, confidence = T, size = 1000, includeTest = T,gg=T)

a<-stack('predict_america_200Model.img')

plot(p.africa.246[[1:3]])

