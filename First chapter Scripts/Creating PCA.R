setwd('D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/ShapeFiles')
sp.america<-shapefile('Mero_America_Miller_50km.shp')
sp.america$species <- 'AM'###########Criar uma coluna em que todas as linhas terão numero um(indicando presença da especie
sp.america@data <- sp.america@data[,'species',drop=F]####Somente ter a coluna Species no dado
sp_africa<-shapefile('Mero_Africa_Miller_50km.shp')
sp_africa$species <- 'AF'######Criar uma coluna em que todas as linhas terão numero um(indicando presença da especie)
sp_africa@data <- sp_africa@data[,'species',drop=F]####Somente ter a coluna Species no dado
bg<-shapefile('backgroundPCA.shp')
bg$species<-'BG'
bg@data<-bg@data[,'species',drop=F]
set<-bind(sp.america,sp_africa,bg)

setwd("D:/Pesquisa/Mestrado/PRIMEIRO_CAPITULO_DIFERENCIADO/VariaveisAmbientais/Var_Atlantico/Miller")
a<-list.files(pattern = '.img')
b<-a[c(1,2,6,8,9)]
preds<-stack(b)


values<-data.frame(extract(preds,set))
Classes<-as.factor(c(sp.america$species,sp_africa$species,bg$species))

data<-cbind(values,Classes)
write.csv(data,file = "dadosparaPCA.csv",sep = ',',row.names = F)
meusdados<-read.csv2('dadosparaPCA.csv',header = T)
meusdados = meusdados[1:6]
na.omit(meusdados)

str(meusdados)
summary(meusdados)

################################################################################################
install.packages('psych')
library(psych)
pairs.panels(meusdados[,-6],gap = 0, bg = c('red','yellow','blue')[meusdados$Classes],pch = 21)
#PCA
pc<-prcomp(meusdados[,-6], center = T, scale.=T)
attributes(pc)
pc$scale
print(pc)
summary(pc)
library(ggbiplot)
library(ggplot2)
g<-ggbiplot(pc,obs.scale = 10,var.scale = 25,groups = meusdados$Classes,ellipse = T,circle = F,ellipse.prob = 0.95,alpha = 0.25,varname.size = 5,
            varname.adjust = 1.5,var.axes = FALSE)
g = g+ theme_bw()
g = g+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g<-g + scale_color_discrete(name = '')
g<-g + theme(legend.direction = 'horizontal',legend.position = 'top')

plot(g)


