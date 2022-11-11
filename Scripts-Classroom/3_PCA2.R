##################################################
###          Seminario de Estadística          ###
###         Martes 28, septiembre 2021         ###
##################################################

#  _____________________________________  #
# |             Ejemplo 1:              | #
# | Análisis de Componentes Principales | #
# |_____________________________________| #

options(digits = 3, width=130); library(MASS)
data("USArrests")
par(lwd = 2, cex.axis = 1.5, cex.lab = 1.5, pch = 16)

# Utilizaremos la base de datos USArrests: Tasas de delitos violentos por
# estado de USA. Esta base de datos que contiene información estadística
# de arrestos por asalto, asesinato o violación por cada 100,000 habitantes
# en 50 estados de USA. Además, se incluye el porcentaje de población que
# habita en áreas urbanas dentro del estado. 

# La base está ordenada de manera alfabética, por nombre del estado:
states <- row.names(USArrests)
head(states)

# Tenemos 4 variables:
names(USArrests)

# Las medias, son muy diferentes:
apply(USArrests, 2, mean)
# Vemos que hay en promedio tres veces más violaciones que asesinatos
# y más de ocho veces más agresiones que violaciones.

# En cuanto a las varianzas:
apply(USArrests, 2, var)
# Esperábamos que las variables tuvieran varianzas diferentes, ya que la 
# variable UrbanPop mide el porcentaje de la población en cada estado que
# vive en un área urbana, que no es un número comparable al número de 
# violaciones en cada estado por cada 100,000 personas. Si no escalamos las
# variables antes de realizar PCA, entonces la mayoría de los componentes 
# principales que observamos serían impulsados por la variable Assault, ya 
# que tiene, por mucho, la media y la varianza más grandes. Por lo tanto, 
# es importante estandarizar las variables para que tengan una media de cero 
# y una desviación estándar de uno antes de realizar la PCA.

# Hacemos el Análisis de Componentes Principales:
pr.out <- prcomp(USArrests, scale = TRUE)
# Por defecto, la función prcomp () centra las variables para que tengan 
# media cero. Al usar la opción scale = TRUE, escalamos las variables para 
# tener desvianción estándar uno. Las salidas de prcomp( ) contiene lo
# siguiente:
names(pr.out)
# "center" y "scale" corresponden a las medias y desviaciones estándar de las
# variables que se utilizaron para escalar antes de implementar PCA.
pr.out$center
pr.out$scale
# "rotacion" proporciona a la matriz de cargas de los componentes principales; 
# cada columna de pr.out$rotación contiene el correspondiente vector de carga 
# del respectivo componente principal.
pr.out$rotation
# Vemos que hay cuatro componentes principales distintos. Esto es de esperar 
# porque, en general, hay min{n−1, p} componentes principales informativos 
# en un conjunto de datos con n observaciones y p variables.
# La matriz "pr.out$x" contiene a las nuevas variables o componentes principales,
# es decir, la n-ésima columna es el k-ésimo vector de puntuación del componente 
# principal.
pr.out$x
# Inicialmente teníamos 4 variables medidas en 50 estados
dim(pr.out$x)

# Podemos graficar a los primeros dos componentes principales:
biplot(pr.out, scale = 0)
## Biplot
## Las flechas tienen la punta en los loadings de cada variable en cada cp
## - Flechas a 90º indican correlación cercana a cero.
## - Flechas a 180º indican correlación cercana a -1
## - Flechas colineales indican correlación cercana a +1

# Desviación estándar de lo Componentes Principales:
pr.out$sdev
# Varianza explicada por cada CP:
pr.var <- pr.out$sdev^2
pr.var
# Proporción de varianza explicada por cada CP
pve <- pr.var/sum(pr.var)*100
pve
# Las 3 anteriores:
summary(pr.out)$importance

# Podemos graficar la proporción de varianza explicada por cada componente, 
# así como la proporción de varianza acumulada, de la siguiente manera:
par(mfrow = c(1, 2))
plot (pve , xlab = " Componente Principal ", 
      ylab = " Proporción de Varianza Explicada ", 
      ylim = c(0, 100), type = "b")
plot(cumsum(pve), xlab = " Componente Principal ", 
     ylab = " Proporción de Varianza Acumulada ", 
     ylim = c(0, 100), type = "b")
par(mfrow = c(1, 1))


#   -------------------------------------------
# || ¿Qué pasa si no se escalan las variables? || 
#   -------------------------------------------

# Hacemos el ACP, centrando las variables pero sin escalar la varianza:
pca2<-prcomp(USArrests, scale=FALSE)

# "rotation" nos da los vectores propios para cada componente principal. 
# El número de componentes principales en este caso, tenemos mín(50-1,4)=4 
# componentes principales:
pca2$rotation
# Comparamos
cbind(pr.out$rotation, pca2$rotation)
# Graficamos los primeros dos componentes principales:
par(mfrow=c(1,2))
biplot(pr.out, scale=0, col=c("darkblue", "darkred"), main="PCA con escalado")
biplot(pca2, scale=0, col=c("darkblue", "darkred"), main="PCA sin escalado")
par(mfrow=c(1,1))
# Del gráfico anterior se pueden hacer las siguientes observaciones;
#   a. No se distingue la correlación entre Murder, Rape y Assaults es alta como
#      en el gráfico con variables escaladas
#   b. El primer componente principal tendrá una mayor carga en Assault ya que 
#      su varianza es la más alta al no escalar las variables. 
#   c. La variable UrbanPop utiliza unidades de medida distinta, por lo que no 
#      podría ser comparables. 

# Analizamos el comportamiento de la varianza para cada uno de los componentes
# principales:
pca2$sdev
pca2.var<- pca2$sdev^2
pca2.pv<- pca2$sdev^2/sum(pca2$sdev^2)*100; pca2.pv
summary(pca2)$importance
# Comparamos con el análisis anterior:
cbind(summary(pr.out)$importance, summary(pca2)$importance)
# Graficamos:
par(mfrow = c(1, 2))
plot (pca2.pv , xlab = " Componente Principal ", 
      ylab = " Proporción de Varianza Explicada ", 
      ylim = c(0, 100), type = "b")
plot(cumsum(pca2.pv), xlab = " Componente Principal ", 
     ylab = " Proporción de Varianza Acumulada ", 
     ylim = c(0, 100), type = "b")
par(mfrow = c(1, 1))
# Comparamos:
par(mfrow = c(1, 2))
plot (cumsum(pve) , xlab = " Componente Principal ", 
      ylab = " Proporción de Varianza Explicada ", 
      ylim = c(0, 100), type = "b")
plot(cumsum(pca2.pv), xlab = " Componente Principal ", 
     ylab = " Proporción de Varianza Acumulada ", 
     ylim = c(0, 100), type = "b")
par(mfrow = c(1, 1))



#  _____________________________________  #
# |             Ejemplo 2:              | #
# | Análisis de Componentes Principales | #
# |_____________________________________| #
library("FactoMineR")
library("factoextra")

#Cargamos los datos:
data(decathlon2)
head(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6], 4)

#library("FactoMineR")
res.pca <- PCA(decathlon2.active, graph = FALSE)
res.pca


#Valores Propios / Varianza
#library("factoextra")
eig.val <- get_eigenvalue(res.pca)
eig.val
#Los valores propios suman 10. 
###Pueden usarse para determinar el núm. de comp princ a elegirse:
###Si,están estand, podemos elegir las comp con val prop > 1 (la CP expl mas que una variab)
###Usando el porcentaje
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


####################################
####        Variables           ####
####################################
var <- get_pca_var(res.pca)
var
head(var$coord, 4)
#Relación entre variables:
fviz_pca_var(res.pca, col.var = "darkblue")
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#Contribuciones a las CP
#Las variables mas correlacionadas con CP1 y CP2, son las mas importantes
#Las que no se correl con alguna CP o si con las ultim CP podrían eliminarse
head(var$contrib, 4)

library("corrplot")
corrplot(var$contrib, is.corr=FALSE)    
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


####################################
####       Individuos           ####
####################################
ind <- get_pca_ind(res.pca)
ind
ind$coord

fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
fviz_contrib(res.pca, choice = "ind", axes = 1:2)
fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = decathlon2$Competition[1:23], # color by groups
             palette = "jco",
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups")





iris
iris.pca <- PCA(iris[,-5], graph = FALSE)
iris.pca <- PCA(iris[,-5])
val.prop <- get_eigenvalue(iris.pca)
val.prop
fviz_eig(iris.pca, addlabels = TRUE, ylim = c(0, 80))
fviz_pca_var(iris.pca, col.var = "black")
fviz_contrib(iris.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(iris.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(iris.pca, choice = "var", axes = 1:2, top = 10)
fviz_pca_var(iris.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_pca_ind(iris.pca)
fviz_pca_ind(iris.pca,
             geom.ind = "point", 
             col.ind = iris$Species, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, 
             legend.title = "Grupos")

fviz_pca_biplot(iris.pca, 
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = iris$Species,
                col.ind = "black",
                col.var = factor(c("sepal", "sepal", "petal", "petal")),
                legend.title = list(fill = "Species", color = "Grupos"),
                repel = TRUE) +
  ggpubr::fill_palette("jco") +     
  ggpubr::color_palette("npg")

