library(raster)
library(rgdal)
library(dismo)
library(usdm)
library(sdm)
installAll()

##Projetar modelo Env Hab Separado
setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/Env_Hab_Separado/Modelo_SemVar_Correl")
m<-read.sdm("modelo_Env_Hab_Separados_semCorrel.sdm")
setwd("D:/Pesquisa/Mestrado/TERCEIRO_CAPITULO/Modelo_Env_HabSeparado/Preds_ParaOModeloEnvHabSeparado")
a<-list.files(pattern = '.img$')
preds<-stack(a)

sts<-getEvaluation(m,wtest= 'test.dep',stat=c('sensitivity','specificity','TSS','AUC','threshold'),opt=2)
ids<-sts$modelID[which(sts$TSS>=0.700)]

setwd("D:/Pesquisa/Mestrado/TERCEIRO_CAPITULO/Modelo_Env_HabSeparado")

en<-ensemble(m,preds, filename='ensemble_Env_Hab_Separado_SemCorrel_RCP85.img',
             setting=list(id =ids,method='weighted',stat='TSS',opt=2))

plot(en)
en<-stack("ensemble_Env_Hab_Separado_SemCorrel_RCP85.img")
thrs<-mean(sts$threshold)
PA<-en
PA[]<-ifelse(en[]>=thrs,1,0)
plot(PA)
writeRaster(PA,file = 'PA_Env_HabSeparado_RCP85.img')
############################3
#Todos os modelos
setwd("D:/Pesquisa/Mestrado/TERCEIRO_CAPITULO/Modelo_Env_HabSeparado/AllModels")
models<- predict(m,'models.img',newdata = preds, w = ids )
