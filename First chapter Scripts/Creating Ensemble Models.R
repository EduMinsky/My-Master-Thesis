#########Script para concertar modelos e binarizar por algoritmo e Ensemble

setwd('D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/Modelos_de_Nicho/predictions/Pela_Media_Algoritmos/America')
svm<-raster('Prediction.America.SVM.img')
rbf[]> 1.0
values(rbf)[values(rbf)>1]=1
rbf
values(rbf)[values(rbf)<0] = 0
rbf
plot(rbf)
writeRaster(rbf, file = 'rbf2.img')
m.america
######
###Fazer o evaluates para pegar o valor que maximiza o TSS
##
setwd('D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/Modelos_de_Nicho/predictions/Pela_Media_Algoritmos/America')
svm.model<-stack('svm2.img')####Botar na memoria o modelo
plot(svm.model)
sp_africa
background.america$species<-0
background.america@data<-background.america@data[,'species',drop = F]
observado<-rbind(sp_africa@data,background.america@data)####Transformar numa matrix os valores de presença(1) e ausencia(0)
presenca_pseudo<-rbind(sp_africa,background.america)####Criar um shapefile unico com presenca e ausencia
evaluates(observado$species,extract(svm.model,presenca_pseudo))#####Visualizar qual valor que maximiza TSS
maxSESP<-0.4682320
PA.svm<-svm.model
PA.svm[]<-ifelse(svm.model[] >= maxSESP,1,0)
plot(PA.svm)
writeRaster(PA.svm,filename = 'PA_SVM.img')


####################
###Para o GLM
glm.model<- stack('Prediction.America.GLM.img')
plot(glm.model)
sp_africa
background.america$species<-0
background.america@data<-background.america@data[,'species',drop = F]
observado.glm<-rbind(sp_africa@data,background.america@data)####Transformar numa matrix os valores de presença(1) e ausencia(0)
presenca_pseudo.glm<-rbind(sp_africa,background.america)####Criar um shapefile unico com presenca e ausencia
evaluates(observado.glm$species,extract(glm.model,presenca_pseudo.glm))#####Visualizar qual valor que maximiza TSS
maxSESP<-0.481050074
PA.glm<-glm.model
PA.glm[]<-ifelse(glm.model[] >= maxSESP,1,0)
plot(PA.glm)
writeRaster(PA.glm,filename = 'PA_GLM.img')
######
##Para o BRT
brt.model<-stack('Prediction.America.BRT.img')
observado.brt<-rbind(sp_africa@data,background.america@data)####Transformar numa matrix os valores de presença(1) e ausencia(0)
presenca_pseudo.brt<-rbind(sp_africa,background.america)####Criar um shapefile unico com presenca e ausencia
evaluates(observado.brt$species,extract(brt.model,presenca_pseudo.brt))#####Visualizar qual valor que maximiza TSS
maxSESP<-0.4257268
PA.brt<-brt.model
PA.brt[]<-ifelse(brt.model[]>= maxSESP,1,0)
plot(PA.brt)
writeRaster(PA.brt, filename = 'PA_BRT.img')
########
##Para o Maxent
maxent.model<-stack('Prediction.America.MAXENT.img')
observado.maxent<-rbind(sp_africa@data,background.america@data)####Transformar numa matrix os valores de presença(1) e ausencia(0)
presenca_pseudo.maxent<-rbind(sp_africa,background.america)####Criar um shapefile unico com presenca e ausencia
evaluates(observado.maxent$species,extract(maxent.model,presenca_pseudo.maxent))#####Visualizar qual valor que maximiza TSS
maxSESP<-0.3817344
PA.maxent<-maxent.model
PA.maxent[]<-ifelse(maxent.model[]>= maxSESP,1,0)
plot(PA.maxent)
writeRaster(PA.maxent, filename = 'PA_MAXENT.img')
##########
##Para o RBF
rbf.model<-stack('RBF2.img')
observado.rbf<-rbind(sp_africa@data,background.america@data)####Transformar numa matrix os valores de presença(1) e ausencia(0)
presenca_pseudo.rbf<-rbind(sp_africa,background.america)####Criar um shapefile unico com presenca e ausencia
evaluates(observado.rbf$species,extract(rbf.model,presenca_pseudo.rbf))#####Visualizar qual valor que maximiza TSS
maxSESP<-1.00000000
PA.rbf<-rbf.model
PA.rbf[]<-ifelse(rbf.model[]>= maxSESP,1,0)
plot(PA.rbf)
writeRaster(PA.rbf, filename = 'PA_RBF.img')
#####Fazeer o ensemble

en <- ensemble(m.america, preds, filename='ensemble_sdm.img',
               setting=list(id =models,method='weighted',stat='TSS',opt=2))
ensemble<-stack('ensemble_sdm.img')
plot(en)
evaluates(observado.rbf$species,extract(en,presenca_pseudo.rbf))
binario<-ensemble

binario[]<-ifelse(en[]>=0.62130040,ensemble[],0)
plot(binario)
writeRaster(binario,filename = 'Ensemble_Binario_Continuo.img')
