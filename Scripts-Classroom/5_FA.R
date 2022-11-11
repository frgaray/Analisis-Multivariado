###########################################
###       ANÁLISIS   MULTIVARIADO       ###
###        Jueves 24, junio 2021        ###
###########################################

# Recordemos, que estamos suponiendo:
# (X-u) = L*F + E
# donde:
# X tiene media u en R^p
# L son las cargas (matriz de pxm)
# F son los factores (vector en R^m)
# E es un error aleatorio en R^p

# Con este modelo, llegamos a que Sigma = L*L^t + Phi
# donde:
# Sigma es la matriz de varianzas de X
# Phi es la matriz de varianzas de E

# Por otro lado, sabemos que S = P*D*P^t, la 'descomposición espectral'.
# Es decir, S = P*(D^(1/2) * D^(1/2))*P^t
# De donde, omitiendo a Phi, L = P*D^(1/2)
# Note que P y D, deben tener valores propios ordenados.




#########################
##      Ejemplo 1      ##
#########################
# Para un estudio de referencia, se obtuvo una muestra aleatoria
# de consumidores a quienes se les indagó acerca de los siguientes atributos
# sobre un producto nuevo: gusto X1, costo X2, sabor X3, tamaño por porción
# X4 y calorías suministradas X5. La matriz de correlación es la siguiente:

R<-matrix(c(1,.02,.96,.42,.01,
            .02,1,.13,.71,.85,
            .96,.13,1,.5,.11,
            .42,.71,.50,1,.79,
            .01,.85,.11,.79,1),
          nrow=5); R

# Las correlaciones enmarcadas indican que las respectivas variables se pueden
# agrupar para formar nuevas variables. Así, los grupos de variables son {X1,X3}
# y {X2,X5}, mientras que la variable X4 está más cercana al segundo grupo que
# al primero.
# Las relaciones lineales que se pueden derivar de estas correlaciones sugieren que
# la información representada por estas variables se puede sintetizar a través de
# dos o tres factores.

# Los valores y vectores propios de la matriz de correlaciones R son:
eig<-eigen(R); eig
# Matriz P (vectores propios)
P<-eig$vectors; round(P,2)
# Valores propios
D<-eig$values; D
D12<-sqrt(diag(D))

# Proporción de variabilidad acumulada hasta el factor k
cumsum(D)/sum(D)
# Se nota que con dos factores (k = 2) se reúne una buena proporción de la
# variabilidad total presente en los datos iniciales (93.18%).

# Ponderaciones/Cargas factoriales
L<-P[,1:2]%*%D12[1:2,1:2]; round(L,3)
# Comunalidad
comun<-matrix(rowSums(L^2)); comun
# Varianza específica
1-comun
# Rotación varimax
varimax(L)
Lrot <- varimax(L)$loadings

# Resultados:
Variables <- c("Gusto X1","Costo X2","Sabor X3","Tamaño X4","Calorías X5")
f1 <- round(L[,1], 2)
f2 <- round(L[,2], 2)
f1_rot <- round(Lrot[,1], 2)
f2_rot <- round(Lrot[,2], 2)
data.frame(Variables,f1,f2,f1_rot,f2_rot)

# Podemos observar como las variables X2, X4 y X5 están altamente ligadas con 
# el primer factor, mientras que el segundo factor lo está con las variables 
# X1 y X3. 
# Se puede calificar al primer factor f1* con el nombre de factor nutricional 
# y al segundo f2* con el nombre de factor gustativo. En resumen, estas personas 
# prefieren el producto de acuerdo con sus características nutricionales y 
# gustativas (en este orden).

# Otras rotaciones:
library(GPArotation)
tv<-GPFoblq(L, method="quartimax",normalize=TRUE)
print(tv)
summary(tv)

# Otra forma, más directa:
fac<-factanal(factors=2, covmat=R,rotation="none")
# Pesos:
fac$loadings
#Varianza específica
fac$uniquenesses
# Comunalidades
1-fac$uniquenesses
# Matriz de correlaciones usada
fac$correlation
# Análisis de factores con rotación varimax
fac<-factanal(factors=2, covmat=R,rotation="varimax")
# Los pesos:
fac["loadings"]
# "varimax" es la opción por defecto para rotation
fac<-factanal(factors=2, covmat=R)


#########################
##      Ejemplo 2      ##
#########################
#Tenemos una base de datos de cereales y su contenido de nutrientes por porción. 
#Tenemos 77 cereales y 9 nutrientes medidos, que serán nuestras variables.
setwd("/Users/Guadalupe/Documents/Ayudantías/2021-II")
base<-read.csv("tarea4b.csv", header = T)
base$marca <- as.factor(base$marca)
base1<-base[1:77,3:11]
head(base[,1:11])
#Empleamos la función factanal de la librería stats, esta función nos ofrece
#el análisis de factores por máxima verosimilitud.
analisis=factanal(base1,factors=5)
analisis$loadings

library(nFactors)
ev <- eigen(cor(base1)) # valores propios
ap <- parallel(subject=nrow(base1),var=ncol(base1),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

#3 Factores:
fac3<-factanal(base1, factors = 3, rotation = "varimax")
fac3$loadings
cargas3=fac3$loadings
library(scatterplot3d)
s3d<-scatterplot3d(cargas3,color = "red", type = "h", pch = 16)
text(s3d$xyz.convert(cargas3), labels = names(base1), cex= 0.7, col = "darkblue", pos = 4)

scores3<-factanal(base1, factors = 3, scores = "regression")$scores
head(scores3)
scatterplot3d(scores3, col.grid="lightblue", main="Grafica de las
              puntuaciones", pch=16, angle=30)

colors <- c(2:9)
colors <- colors[as.numeric(base$marca[1:77])]
scatterplot3d(scores3, col.grid="lightblue", main="Grafica de las
              puntuaciones", pch=16, angle=30, color = colors)
legend("bottom", legend = levels(base$marca[1:77]),
       pch = c(16, 16, 16, 16, 16, 16), col = c(2:9),
       inset = -0.40, xpd = TRUE, horiz = TRUE)

pairs(scores3)

fac4<-factanal(base1, factors = 4, rotation = "varimax")
fac4$loadings
scores4<-factanal(base1, factors = 4, scores = "regression")$scores
head(scores4)
pairs(scores4,col = base$marca, pch=16)
