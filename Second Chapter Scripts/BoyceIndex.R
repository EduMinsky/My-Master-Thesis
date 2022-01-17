setwd("D:/Pesquisa/Mestrado/SEGUNDO_CAPITULO/ModelosNicho/ClassificacaoBoyce")
a<-list.files(pattern = '.csv$')
hs<-read.csv(a[[7]])
ratio<-read.csv(a[[8]])
xticks<-seq(0,1,.2)
yticks<- seq(0,5,0.5)
par(1:1)
plot(ratio$x~hs$x,axes = F, ylab = 'P/E ratio',xlab = 'Habitat Suitability')
axis(2,at = yticks)
axis(1, at = xticks)
abline(h = 1,lty = 1)
abline (v = c(0.579460723109543,0.717601931847632),lty = 2)




xticks <- seq(0, 3, .5)
plot(x,y, axes = FALSE) # Sixth plot
axis(2, at = yticks, labels = yticks, col.axis="red", las=2)
axis(1, at = xticks, labels = xticks, col.axis="blue", las=2, tck=-.01)
title("Manipulated Y and X axes")

?abline
