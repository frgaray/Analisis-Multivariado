#Ejercicio 2 simulacion
#Simulamos 20 valores de una variable aleatoria U uniforme(0,1)
set.seed(10)
u<-runif(20,0,1)

#Definimos el vector a=(a1,a2,a3)^T
a1<- 1
a2<- 4
a3<- 3
a<-c(a1, a2,a3)

#Generamos 20 observaciones del vector aleatorio X=Ua
u1<-a1*u
u2<-a2*u
u3<-a3*u

#Organizamos las observaciones en una matriz de datos
mdatos<-cbind(u1,u2,u3) 

#Hacemos el analisis de componentes principales
cp0<- princomp(mdatos)

#Obtenemos la matriz de eigenvectores de la matriz de covarianza
m<-cp0$loadings[]

#Obtenemos el vector (v1^T * a,v2^T * a,v3^T * a)
print(m%*%a)

#Graficamos la primer c.p. vs la segunda c.p.
plot(cp0$scores[,1:2],main=paste("Grafica del primer c. p. contra el segundo c. p. para los valores a1=",a1, " a2=", a2, " a3=", a3), xlab="Primer componente principal", ylab="Segundo componente principal")

#Graficamos la tercer c.p. vs la primer c.p.
plot(cp0$scores[,3:1],main=paste("Grafica del tercer c. p. contra el primer c. p. para los valores a1=",a1," a2=", a2, " a3=", a3),xlab="tercer componente principal", ylab="primer componente principal")

for (i in 1:3) {
  barplot(m[,i], names.arg=rownames(m),col="orange", main=paste("eigenvector de la c.p.",i))
  
}

screeplot(cp0, type="l", main="Diagrama de codo Ua")



#varianza<-cp0$sdev*cp0$sdev
#screeplot(cp0, type="l", main=paste("diagrama codo a1=",a1," a2=", a2, " a3=", a3))
#plot(cp0$scores[,2:3],main=paste("Grafica del segundo c. p. contra el tercer c. p. para los valores a1=",a1," a2=", a2, " a3=", a3),xlab="Segundo componente principal", ylab="Tercer componente principal")


#Ejercicio 4
#Leemos el archivo atheticrecord.txt
datos <-  read.table("athleticrecord.txt", row.names=1)

#Realizamos el analisis de componentes principales utilizando la matriz 
#de covarianza de los datos
cp<- princomp(datos)
summary(cp)

#Guardamos los eigenvalores de la matriz de covarianzas(varianzas de las 
#componentes principales)
evalores<- cp$sdev * cp$sdev
print(evalores)

#Guardamos los eigenvectores unitarios de la matriz de covarianzas
evect<-cp$loadings

#Realizamos el diagrama de codo de las componentes
screeplot(cp, type="l", main="Diagrama de codo para athleticrecord")
abline(1,0,col="red", lty=2)

#Se puede observar que la mayoría de la varianza total se concentra en 
#el componente 1. Solo la varianza de los componentes 1, 2 y 3 es mayor
# o igual a 1 (marcado por la línea roja). De acuerdo a el criterio de **** 
#estos son los componentes que debemos utilizar. 

#Guardamos los valores de las componentes principales (datos transformados)
datost <- cp$scores

#Graficamos los componentes principales 1 y 2 
plot(datost[,1:2], type="n", main="Grafica de los componentes 1 y 2 de athleticrecord")
points(datost[,1:2], cex=0.5)
text(datost[,1:2], label=rownames(datos),cex=0.5)

#Graficamos los componentes principales 2 y 3
plot(datost[,2:3], type="n", main="Grafica de los componentes 2 y 3 de athleticrecord")
points(datost[,2:3], cex=0.5)
text(datost[,2:3], label=rownames(datos),cex=0.5)

#Graficamos los componentes principales 1 y 3
plot(datost[,c(1,3)], type="n", main="Grafica de los componentes 1 y 3 de athleticrecord")
points(datost[,c(1,3)], cex=0.5)
text(datost[,c(1,3)], label=rownames(datos),cex=0.5)

#En las graficas se puede corroborar el orden de las varianzas de los
#componentes principales (var c.p. 1 > var c.p. 2 > var c.p. 3). En 
#la grafica de los c.p. 1 y 2 se puede observar que la mayoria de los
#datos estan en el rectangulo (-10,30)x(-2,2), solo 2, los correspondientes a 
#Paises Bajos y Mauritania, estan fuera de esta region. Dentro 
#de ella los datos se concentran en el rectangulo (-10,0)x(-2,2).


#Realizamos una grafica biplot de los componentes principales 1 y 2 
#biplot(cp,col=c("red","blue"),cex=c(0.4,0.35),scale=0.35)

barplot(datos$V2, names.arg=rownames(datos), las=2, col="red", main="datos$V2")
barplot(datost[,1],names.arg=rownames(datos), las=2, col="red", main="datost$Comp1")
barplot(datos$V3, names.arg=rownames(datos), las=2, col="blue", main="datos$V3")
barplot(datost[,2],names.arg=rownames(datos), las=2, col="blue", main="datost$Comp2")
barplot(datos$V4, names.arg=rownames(datos), las=2, col="yellow", main="datos$V4")
barplot(datost[,3],names.arg=rownames(datos), las=2, col="yellow", main="datost$Comp3")

#Notemos que la grafica de datos$Vi es similar a la de datos$Comp(i-1), pareciera
#que son los mismos datos pero estandarizados

for (i in 1:8) {
  barplot(evect[,i], names.arg=rownames(evect),col="orange", main=paste("eigenvector de la c.p.",i))
  
}

#Valores de los eigenvectores son grandes para una sola columna en comparacion
#a las demas. Esto influye en el valor de la componente que genera. Por ejemplo, 
#el 2° eigenvector, que genera la componente principal 2, tiene un valor
#muy alto para la columna V3 de los datos; si luego comparamos el histograma de
#V3 con el histograma de la componente principal 2 podemos ver que son muy similares. 
#Además los histogramas de las c.p's lucen como una estandarizacion 
#de los datos. 

#Inciso c, ¿Tiene alguna repercusión escalar la matriz de covarianzas?

#Repetimos exactamente el mismo proceso, la diferencia estará en pedirle a la función princomp que estandarice a la matriz de cov
cp_escalada<- princomp(datos, scale=TRUE)
summary(cp_escalada)

#valores propios de esta matriz escalada o estandarizada
e_escalada_valores<- cp_escalada$sdev * cp_escalada$sdev
print(e_escalada_valores)

#vectores propios
evect_escalada<-cp_escalada$loadings

#Realizamos el diagrama de codo de las componentes
screeplot(cp_escalada, type="l", main="Diagrama de codo para athleticrecord con matriz escalada")
abline(1,0,col="red", lty=2)

#Se puede observar que la mayoría de la varianza total se concentra en 
#el componente 1. Solo la varianza de los componentes 1, 2 y 3 es mayor
# o igual a 1. Exactamente lo mismo que se pudo observar en el caso de
#la matriz no escalada. La diferencia se nota en justamente la escala
#el brazo está mas estirado

#Guardamos las observaciones de las componentes principales (datos transformados)
datost_escalada <- cp_escalada$scores

#Graficamos los componentes principales 1 y 2, de igual forma
plot(datost_escalada[,1:2], type="n", main="Grafica de los componentes 1 y 2 de athleticrecord con matriz escalada")
points(datost_escalada[,1:2], cex=0.5)
text(datost_escalada[,1:2], label=rownames(datos),cex=0.5)

#Realizamos una grafica biplot de los componentes principales 1 y 2 
biplot(cp_escalada,col=c("red","blue"),cex=c(0.4,0.35),scale=0.35)

#En conclusión, lo que podemos observar es en esencia exactamente lo mismo
#la diferencia está justamente en las dimensiones por así decirlo
#tenemos todo mas estirado en este caso, por lo que es magnifico ver
#como escalar la matriz solo hace que todo se estire o se achique


#Ejercicio 6

#Guardamos los datos
info<-read.csv("corcan.csv", header=TRUE)

#Distribuimos las variables en 2 vectores: las 3 primeras forman el vector X=(X1,X2,X3)
#y las 3 ultimas el vector Y=(Y1,Y2,Y3). 

x<-as.matrix(info[,1:3])
y<-as.matrix(info[,4:6])


#Determinamos la matriz de covarianza muestral S=Cov(X,Y) y guardamos 
#los bloques S11=Cov(X,X), S22=Cov(Y,Y), S12=Cov(X,Y) y S21=Cov(Y,X)

S<-cov(info)
S11<-S[1:3,1:3]
S22<-S[4:6,4:6]
S12<-S[1:3,4:6]
S21<-S[4:6,1:3]

#Debemos calcular las matrices S11^(1/2) y S22^(1/2)

#Calculamos la descomposicion espectral de S22, es decir,
#las matrices gamma y lambda tales que S11=gamma(lambda)gamma^t,
#donde lambda es una matriz diagonal que tiene a los eigenvalores de S11 
#en la diagonal y gamma es una matriz ortogonal formada por los eigenvectores 
#estandarizados de S11:

#Determinamos la descomposición espectral de S11 para calcular S11^(-1/2)
spectS11<-eigen(S11,symmetric=TRUE)
gamma<- spectS11$vectors #matriz de eigenvectores estandarizados de S11 (gamma)

#Por definicion S11^(-1/2)=gamma(lambda)^(-1/2)gamma^t
lambda <- diag((spectS11$values)^(-1/2), nrow=3, ncol=3) #lambda^(-1/2)
rciS11 <- gamma %*% lambda %*% t(gamma) #S11^(-1/2) (raiz cuadrada inversa de S11)


#Calculamos la descomposicion espectral de S22, es decir,
#las matrices eta y omega tales que S22=eta(omega)eta^t,
#donde omega es una matriz diagonal que tiene a los eigenvalores de S22 
#en la diagonal y gamma es una matriz ortogonal formada por los eigenvectores 
#estandarizados de S22:

#Determinamos la descomposición espectral de S11 para calcular S11^(-1/2)
spectS22<-eigen(S22,symmetric=TRUE)
eta<- spectS22$vectors

#Por definicion S22^(-1/2)=eta(omega)^(-1/2)eta^t
omega <- diag((spectS22$values)^(-1/2), nrow=3, ncol=3)
rciS22 <- eta %*% omega %*% t(eta)

#Calculamos la matriz K=S11^(-1/2) S12 S22^(-1/2)
k<- rciS11 %*% S12 %*% rciS22

#Determinamos las matrices N1=kK^t y N2= (K^t)K
n1<-k%*%t(k)
n2<-t(k)%*%k

#Determinamos los eigenvalores y eigenvectores de N1 y N2
#La raiz cuadrada de dichos eigenvalores son los coeficientes de correlacion
#y a partir de los eigenvectores se obtienen los pares canonicos

spectn1<-eigen(n1)
print(spectn1$values)
#Como S11 tiene 3 eigenvalores distintos tendremos 3 pares de componentes
#canonicos. Ademas alfa y beta estan formados por 3 eigenvectores estandarizados
#distintos
alfa<-spectn1$vectors #(alfa1,alfa2,alfa3)

spectn2<-eigen(n2)
beta<-spectn2$vectors

d<-diag((spectn1$values)^(1/2),nrow=length(spectn2$values), ncol=length(spectn2$values))

#Los i-esimos pares canonicos de x y ye, ai y bi, estan dados por ai=S11^(-1/2)alfai
#y bi=S22^(-1/2)betai respectivamente; a continuacion los calculamos 
pcx<- rciS11%*%alfa #(a1,a2,a3)=S11^(-1/2)(alfa1,alfa2,alfa3)
pcy<- rciS22%*%beta #(b1,b2,b3)=S22^(-1/2)(beta1,beta2,beta3)

#Primer, segundo y tercer vector de correlacion canonica de x
print(pcx)

#Primer, segundo y tercer vector de correlacion canonica de y
print(pcy)

#A continuacion se muestran los coeficientes de correlacion 
#canonica, es decir, los coeficientes de correlacion 
#entre ai^(t)x y bi^(t)y: ro(a1^(t)x,b1^(t)y),
#ro(a2^(t)x,b2^(t)y), y ro(a3^(t)x,b3^(t)y) 
coef_correl<-(spectn1$values)^(1/2)

#Finalmente obtenemos 
vcocx<- x%*%pcx
vcocy<- y%*%pcy

library(yacca)

acc<-cca(x,y,standardize.scores=TRUE)
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

#Con estos plots comprobamos que en efecto, se requerian de 3 pares canónicos 
#para explicar la correlación de ambos sets de variables

