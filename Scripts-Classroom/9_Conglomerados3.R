##################################################
###          Seminario de Estadística          ###
###         Miércoles 13, octubre 2021         ###
##################################################

#  ---------------------------  #
# |  Análisis No Supervisado  | #
# |         Ejemplo 1         | #
#  ---------------------------  #

# Usaremos la base NCI60, que contiene niveles de expresión en 6830 genes de 64 líneas 
# celulares de cáncer:
data("NCI60", package = "ISLR2")
nci.labs <- NCI60$labs
nci.data <- NCI60$data
# Cada línea celular está etiquetada con un tipo de cáncer. 
# No utilizamos los tipos de cáncer para realizar el análisis no supervisado. Pero 
# después de realizarlo, comprobaremos hasta qué punto estos tipos de cáncer coinciden
# con los resultados de estas técnicas no supervisadas.
# Los datos tienen 64 filas y 6'830 columnas:
dim(nci.data)
head(nci.labs, 7)
table(nci.labs)

# ¿Qué pasa si queremos hacer un scatter plot?
# Haremos un Análisis de Componentes Principales:
pr.out <- prcomp(nci.data, scale = TRUE)
# Graficamos los primeros componentes y coloreamos de acuerdo al tipo de cáncer:
Cols <- function(vec) {
   cols <- rainbow(length(unique(vec)))
   return(cols[as.numeric(as.factor(vec))])
}
par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19,
        xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1, 3)], col = Cols(nci.labs), pch = 19,
        xlab = "Z1", ylab = "Z3")
par(mfrow = c(1, 1))
# En general, los mismos tipos de cáncer tienden a estar cerca (en las 1ras CP).
# Podemos esperar que las líneas celulares del mismo tipo de cáncer tiendan a tener
# niveles de expresión génica bastante similares.

# Veamos las proporciones de varianzas explicadas:
pr.var <- pr.out$sdev^2
pve <- pr.var/sum(pr.var)*100
round(head(pve, 7), 2)
# Y el % de varianza acumulada:
cumsum(pve)[1:7]
par(mfrow = c(1, 2))
plot(pve, type = "o", ylab = "PVE ",
        xlab = "Componente Principal", col = "darkblue")
plot(cumsum(pve), type = "o", ylab = "PVE acumulada",
         xlab = "Componente Principal", col = "darkred")
# Vemos que, en conjunto, los siete primeros componentes principales explican 
# alrededor del 40% de la varianza de los datos. No se trata de una gran cantidad
# de varianza. Sin embargo, mientras cada uno de los siete primeros componentes 
# principales explica una cantidad sustancial de varianza, hay una marcada 
# disminución de la varianza explicada por los componentes principales adicionales. 
# Es decir, hay un codo en el gráfico después de aproximadamente el séptimo 
# componente principal. Esto sugiere que puede ser poco beneficioso examinar más 
# de siete componentes principales.

# Procedemos con el Análisis de Conglomerados:
# Clustering jerárquico con métrica euclideana y ligas Complete, Average y Single:
sd.data <- scale(nci.data)
par(mfrow = c(3, 1))
data.dist <- dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",
         labels = nci.labs, main = "Vinculación Complete")
plot(hclust(data.dist, method = "average"),
         labels = nci.labs, main = "Vinculación Average",
         xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"),
         labels = nci.labs, main = "Vinculación Single",
         xlab = "", sub = "", ylab = "")
# Normalmente, la vinculación single tiende a producir clusters muy grandes a los 
# que se unen las observaciones individuales una a una. Por otro lado, la vinculación
# complete y la vinculación average tienden a producir conglomerados más equilibrados
# y atractivos. Las líneas celulares de células de un mismo tipo de cáncer tienden a 
# agruparse, aunque la agrupación no es perfecta.
# Cortaremos para formar 4 grupos:
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)
# Observamos algunos patrones: Todas las líneas Leucemia se encuentran en el grupo 3, 
# mientras que las líneas de cáncer de mama están repartidas en tres grupos diferentes. 
# Podemos trazar el corte en el dendrograma que produce estos cuatro clusters:
par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "darkred")

# ¿Qué me diría K-Means? ¿Algo diferente?
set.seed (2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)

# Realizaremos el clustering jerárquico en los primeros componentes principales, en 
# lugar de usar toda la matriz de datos:
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs,
        main = "Clustering Jerarq en los primers 5 CP")
table(cutree(hc.out, 4), nci.labs)
# En este ejmplo, podríamos considerar el paso de los componentes principales como
# una eliminación de ruido de los datos. 





#  ---------------------------  #
# |  Análisis No Supervisado  | #
# |         Ejemplo 2         | #
# |     Datos con Grupos      | #
#  ---------------------------  #
library(cluster)
library(mclust)
library("fpc")
library("factoextra")

# Cargamos los datos:
data("faithful")
# Faithful: tiempo de espera entre erupciones y su duración en un geiser de 
# Yellowstone.
head(faithful)
plot(faithful,pch=20)
# Podemos notar 2 grupos, vamos a comparar los métodos:

# ---------------------------- #
# 1. Con la métrica euclidiana:
# ---------------------------- #
# Simple
simple <- hclust(dist(faithful), method='single')
# Completa
completa <- hclust(dist(faithful))
# Promedio
promedio <- hclust(dist(faithful), method='average')
# Varianzas
ward <- hclust(dist(faithful), method='ward.D2')
# Divisivo
div <- diana(faithful)
# K-Medias
km <- kmeans(scale(faithful), 2, nstart = 25)
# Fuzzy
fuz <- fanny(scale(faithful), k=2)
# Mezclas
mc <- Mclust(scale(faithful))
# D-B Clust
dbscan::kNNdistplot(scale(faithful), k =  5)
abline(h = 0.21, lty = 2)
db <- fpc::dbscan(scale(faithful), eps = 0.21, MinPts = 5)

library(ggsci)
par(mfrow=c(3,3))
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(simple,2)],
     main='Simple')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(completa,2)],
     main='Completa')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(promedio,2)],
     main='Promedio')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(ward,2)],
     main='Ward')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(div,2)],
     main='Divisivo')
plot(faithful, pch=20, col=pal_jco("default")(2)[km$cluster],
     main='K-Medias')
plot(faithful, pch=20, col=pal_jco("default")(2)[fuz$clustering],
     main='Difuso')
plot(faithful, pch=20, col=pal_jco("default")(3)[mc$classification],
     main='MClust')
plot(faithful, pch=20, col=pal_jco("default")(3)[(db$cluster)+1],
     main='DClust')

# --------------------------------- #
# 2. Simple, con distintas métricas
# --------------------------------- #
simple_euc <- hclust(dist(faithful), method='single')
simple_max <- hclust(dist(faithful, method = "maximum"), method='single')
simple_man <- hclust(dist(faithful, method = "manhattan"), method='single')
simple_can <- hclust(dist(faithful, method = "canberra"), method='single')
simple_bin <- hclust(dist(faithful, method = "binary"), method='single')
simple_min <- hclust(dist(faithful, method = "minkowski"), method='single')
simple_pea <- hclust(as.dist(get_dist(scale(faithful), method = "pearson")), method='single')
par(mfrow=c(3,3))
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(simple_euc,2)],
     main='Simple-Euclidiana')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(simple_max,2)],
     main='Simple-Máximo')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(simple_man,2)],
     main='Simple_Manhattan')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(simple_can,2)],
     main='Simple-Camberra')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(simple_bin,2)],
     main='Simple-Binaria')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(simple_min,2)],
     main='Simple-Minkowski')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(simple_pea,2)],
     main='Simple-Pearson')

# ----------------------------------- #
# 3. Completa, con distintas métricas
# ----------------------------------- #
completa_euc <- hclust(dist(faithful), method='complete')
completa_max <- hclust(dist(faithful, method = "maximum"), method='complete')
completa_man <- hclust(dist(faithful, method = "manhattan"), method='complete')
completa_can <- hclust(dist(faithful, method = "canberra"), method='complete')
completa_bin <- hclust(dist(faithful, method = "binary"), method='complete')
completa_min <- hclust(dist(faithful, method = "minkowski"), method='complete')
completa_pea <- hclust(as.dist(get_dist(scale(faithful), method = "pearson")), method='complete')
par(mfrow=c(3,3))
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(completa_euc,2)],
     main='Completa-Euclidiana')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(completa_max,2)],
     main='Completa-Máximo')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(completa_man,2)],
     main='Completa_Manhattan')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(completa_can,2)],
     main='Completa-Camberra')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(completa_bin,2)],
     main='Completa-Binaria')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(completa_min,2)],
     main='Completa-Minkowski')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(completa_pea,2)],
     main='Completa-Pearson')

# ------------------------------------------- #
# 4. Divisivo & Fuzzy, con distintas métricas
# ------------------------------------------- #
par(mfrow=c(3,3))
div_euc <- diana(faithful, metric="euclidean")
div_man <- diana(faithful, metric="manhattan")
fuz_euc <- fanny(scale(faithful), k=2, metric = "euclidean")
fuz_man <- fanny(scale(faithful), k=2, metric = "manhattan")
fuz_sqe <- fanny(scale(faithful), k=2, metric = "SqEuclidean")
plot(faithful, pch=20, col=pal_jco("default")(2)[fuz_euc$clustering],
     main='Difuso-Euclidiana')
plot(faithful, pch=20, col=pal_jco("default")(2)[fuz_man$clustering],
     main='Difuso-Manhattan')
plot(faithful, pch=20, col=pal_jco("default")(2)[fuz_sqe$clustering],
     main='Difuso-Euclidiana.Cuadrada')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(div_euc,2)],
     main='Divisivo-Euclidiana')
plot(faithful, pch=20, col=pal_jco("default")(2)[cutree(div_man,2)],
     main='Divisivo-Manhattan')
par(mfrow=c(1,1))





#  ---------------------------  #
# |  Análisis No Supervisado  | #
# |         Ejemplo 3         | #
# |     Datos no Sencillos    | #
#  ---------------------------  #

library(datasauRus)
library(dplyr)

# Escojamos un conjunto de datos:
ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
  geom_point()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=3)

# dataset = 'away','bullseye','circle','dino','dots','h_lines','high_lines',
#           'slant_down','slant_up','star','v_lines','wide_lines','x_shape'
Datos <- datasaurus_dozen[which(datasaurus_dozen$dataset=='away'), -1]
# ¿Cuántos grupos vemos?
grupos <- 4
# Epsilon para DBClust:
par(mfrow=c(1,1))
dbscan::kNNdistplot(scale(Datos), k =  5)
abline(h = 0.52, lty = 2)
e<-0.52
plot(Datos)

# ---------------------------- #
#  Con la métrica euclidiana:
# ---------------------------- #
# Simple
simple <- hclust(dist(Datos), method='single')
# Completa
completa <- hclust(dist(Datos))
# Promedio
promedio <- hclust(dist(Datos), method='average')
# Varianzas
ward <- hclust(dist(Datos), method='ward.D2')
# Divisivo
div <- diana(Datos)
# K-Medias
km <- kmeans(scale(Datos), grupos, nstart = 25)
# Fuzzy
fuz <- fanny(scale(Datos), k=grupos)
# Mezclas
mc <- Mclust(scale(Datos), G = grupos)
# D-B Clust
db <- fpc::dbscan(scale(Datos), eps = e, MinPts = 5)
par(mfrow=c(3,3))
plot(Datos, pch=20, col=pal_jco("default")(5)[cutree(simple,grupos)],
     main='Simple-Euclidiana')
plot(Datos, pch=20, col=pal_jco("default")(5)[cutree(completa,grupos)],
     main='Completa')
plot(Datos, pch=20, col=pal_jco("default")(5)[cutree(promedio,grupos)],
     main='Promedio')
plot(Datos, pch=20, col=pal_jco("default")(5)[cutree(ward,grupos)],
     main='Ward')
plot(Datos, pch=20, col=pal_jco("default")(5)[cutree(div,grupos)],
     main='Divisivo')
plot(Datos, pch=20, col=pal_jco("default")(5)[km$cluster],
     main='K-Medias')
plot(Datos, pch=20, col=pal_jco("default")(5)[fuz$clustering],
     main='Difuso')
plot(Datos, pch=20, col=pal_jco("default")(10)[mc$classification],
     main='MClust')
plot(Datos, pch=20, col=pal_jco("default")(10)[(db$cluster)+1],
     main='DClust')
par(mfrow=c(1,1))

# ---------------------------- #
#  Con la métrica camberra:
# ---------------------------- #
# Simple
simple <- hclust(dist(Datos, method = "canberra"), method='single')
# Completa
completa <- hclust(dist(Datos, method = "canberra"))
# Promedio
promedio <- hclust(dist(Datos, method = "canberra"), method='average')
# Varianzas
ward <- hclust(dist(Datos, method = "canberra"), method='ward.D2')
# Divisivo
div <- diana(Datos)
# K-Medias
km <- kmeans(scale(Datos), grupos, nstart = 25)
# Fuzzy
fuz <- fanny(scale(Datos), k=grupos)
# Mezclas
mc <- Mclust(scale(Datos), G = grupos)
# D-B Clust
db <- fpc::dbscan(scale(Datos), eps = e, MinPts = 5)
par(mfrow=c(3,3))
plot(Datos, pch=20, col=pal_jco("default")(5)[cutree(simple,grupos)],
     main='Simple-Camberra')
plot(Datos, pch=20, col=pal_jco("default")(5)[cutree(completa,grupos)],
     main='Completa')
plot(Datos, pch=20, col=pal_jco("default")(5)[cutree(promedio,grupos)],
     main='Promedio')
plot(Datos, pch=20, col=pal_jco("default")(5)[cutree(ward,grupos)],
     main='Ward')
plot(Datos, pch=20, col=pal_jco("default")(5)[cutree(div,grupos)],
     main='Divisivo')
plot(Datos, pch=20, col=pal_jco("default")(5)[km$cluster],
     main='K-Medias')
plot(Datos, pch=20, col=pal_jco("default")(5)[fuz$clustering],
     main='Difuso')
plot(Datos, pch=20, col=pal_jco("default")(10)[mc$classification],
     main='MClust')
plot(Datos, pch=20, col=pal_jco("default")(10)[(db$cluster)+1],
     main='DClust')
par(mfrow=c(1,1))
