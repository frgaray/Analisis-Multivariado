#Lectura de datos Y
datosY<-c(500,43,98,17,800,20,92,32,570,28,98,26,
          550,28,98,26,550,28,98,26,380,15,99,28,
          930,21,99,28,650,10,101,27,600,10,101,27,
          1500,19,99,23,1750,22,101,27,2000,58,100,
          18,2500,34,102,16,2000,21,105,20,7850,42,
          84,5,10500,50,81,-12)
datosY<-matrix(datosY,ncol=4,byrow=TRUE)
# nombres de las variables Y
colnames(datosY)<-paste("Y",1:4,sep="")
datosX<-c(0,3,22,57,17,1,0,16,20,38,13,13,0,6,28,46,17,3,
          0,4,19,47,27,3,0,1,8,50,35,6,0,2,19,44,32,3,0,
          0,15,50,27,8,10,21,40,25,4,0,14,26,32,28,0,0,0,
          1,6,80,12,1,1,4,34,33,22,6,0,7,14,66,13,0,0,9,
          15,47,21,8,3,7,17,32,27,14,0,5,7,84,4,0,0,3,1,
          94,4,0)
datosX<-matrix(datosX,ncol=6,byrow=TRUE)
# nombres de las variables X
colnames(datosX)<-paste("X",1:6,sep="")
#(solo se usan las primeas 5 variables X)
datosX<-datosX[,-6]
#Se organizan los datos en un ’data.frame’ (tabla 11.3)
ejemp9_2<-data.frame(cbind(datosX,datosY))
# matriz de correlaciones de los datos (tabla 11.4)
round(cor(ejemp9_2), 4)
# se carga la librería yacca (Butts 2009)
library(yacca)
acc<-cca(datosX,datosY,standardize.scores=TRUE)
summary(acc)
# correlaciones canónicas
acc["corr"]
# coeficientes canónicos planos para X
acc["xcoef"]
# coeficientes canónicos planos para Y
acc["ycoef"]
#Puntajes de las variables canónicas (estandarizadas)
#asociadas a las variables X’s
acc["canvarx"]
#Puntajes de las variables canónicas (estandarizadas)
#asociadas a las variables Y’s
acc["canvary"]
# correlación de la variables X’s con sus
# variables canónicas
acc["xcrosscorr"]
# correlación de la variables X’s con sus
# variables canónicas
acc["ycrosscorr"]
#Correlaciones estructurales (cargas) para
#las variables X’s en cada variable canónica.
acc["xstructcorr"]
#Correlaciones estructurales (cargas) para
#las variables Y’s en cada variable canónica.
acc["ystructcorr"]
#Cuadrado de las correlaciones estructurales
#(fracción de la varianza de X asociada con
#cada variable canónica
acc["xstructcorrsq"]
#Cuadrado de las correlaciones estructurales
#(fracción de la varianza de Y asociada con
#cada variable canónica
acc["ystructcorrsq"]
# Correlaciones entre las variables X’s y las
# variables canónicas de las variables Y’s
acc["xcrosscorr"]
# Correlaciones entre las variables Y’s y las
# variables canónicas de las variables X’s
acc["ycrosscorr"]
# grafico similar al que se muestra en la figura 11.1
plot(acc)

library(vegan)
salida<-CCorA(datosX,datosY)
plot(salida$Cx[,1], salida$Cy[,1])
# proyección de los individuos sobre el plano canónico
biplot(salida, "ob")
# proyección de las variables sobre el correspondiente
# el plano canónico con circulo de correlaciones
biplot(salida, "v", cex=c(0.7,0.6))
# los dos gráficos anteriores juntos
biplot(salida, "ov", cex=c(0.7,0.6))
# proyección de individuos y variables sobre
# el plano formado por las variables canónicas
biplot(salida, "b", cex=c(0.7,0.6))
biplot(salida, xlabs = NA, plot.axes = c(2,3))
biplot(salida, plot.type="biplots", xlabs = NULL)
