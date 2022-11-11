##################################################
###          Seminario de Estadística          ###
###         Miércoles 13, octubre 2021         ###
##################################################


#  ---------------------------  #
# |       Conglomerados       | #
# |    K-Medias Jerárquico    | #
#  ---------------------------  #

# K-medias tiene algunas limitaciones: requiere que el usuario especifique 
# el número de clusters por adelantado y selecciona los centros iniciales 
# de forma aleatoria. La solución final de clustering de k-means es muy 
# sensible a esta selección aleatoria inicial de centros de cluster. El 
# resultado puede ser (ligeramente) diferente cada vez que se calcula k-medias.

# Este método, se apoye en clustering jerárquico para elegir los centros de
# k-medias.
df <- scale(USArrests)
library(factoextra)
res.hk <-hkmeans(df, 4)
# Resultados:
res.hk
# Dibujamos el árbol:
fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)
# Visualizamos la agrupación hkmedias
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())



#  ---------------------------  #
# |       Conglomerados       | #
# |          Difusos          | #
#  ---------------------------  #

# El clustering difuso se considera un clustering suave, en el que cada
# elemento tiene una probabilidad de pertenecer a cada cluster. En otras 
# palabras, cada elemento tiene un conjunto de coeficientes de pertenencia 
# que corresponden al grado de estar en un clúster determinado.

# La función es:
# fanny(x, k, metric = "euclidean", stand = FALSE)
#   x: matriz, data frame o matriz de disimilitudes
#   k: número de cluster a generar
#   metric: métrica para las disimilaridades

library(cluster)
df <- scale(USArrests)     # Estandarizamos
res.fanny <- fanny(df, 4)  # Conglomeramos con 4 grupos
# Probabilidades de pertenencia:
head(res.fanny$membership, 7)
# Agrupamiento por Ciudad:
head(res.fanny$clustering)
# Podemos ver de manera gráfica:
library(factoextra)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")
# Para evaluar la bondad de los resultados de la agrupación, calculamos
# el coeficiente de silueta:
fviz_silhouette(res.fanny, palette = "jco",
                ggtheme = theme_minimal())



#  ---------------------------  #
# |       Conglomerados       | #
# |     Basados en Modelos    | #
#  ---------------------------  #

# Los métodos tradicionales de clustering, como el clustering jerárquico y 
# el clustering de k-means, son heurísticos y no se basan en modelos formales. 
# Además dependen de la elección del número de clusters.
# Una alternativa es el clustering basado en modelos, que considera que los 
# datos provienen de una distribución que es una mezcla de dos o más clusters.
# El clustering basado en modelos utiliza una asignación suave, donde cada 
# punto de datos tiene una probabilidad de pertenecer a cada cluster.

# Cargamos los datos:
library("MASS")
data("geyser")
# Datos del géiser Old Faithful:
#   duration: Tiempo de erupción en minutos
#   waiting: Tiempo de espera para esta erupción
head(geyser)

# Scatter plot
library("ggpubr")
ggscatter(geyser, x = "duration", y = "waiting")+
  geom_density2d() # Densidad en 2D
# El gráfico sugiere que hay al menos 3 grupos en la mezcla. La forma 
# de cada uno de los 3 cúmulos parece ser aproximadamente elíptica, lo 
# que sugiere tres distribuciones normales bivariadas. Como las 3 elipses 
# parecen ser similares en términos de volumen, forma y orientación, 
# podríamos anticipar que los tres componentes de esta mezcla podrían tener
# matrices de covarianza homogéneas.

# Opciones de Modelos: 
# Parámetros de volumen, forma y orientación:
# E: Equals, V: Variable, I: Ejes coordenadas
# Por ejemplo: EVI denota un modelo en el que los volúmenes de todos los 
# conglomerados son iguales (E), las formas de los conglomerados pueden 
# variar (V), y la orientación es la identidad (I) o "ejes de coordenadas".

# Ajustamos el modelo:
library(mclust)
df <- scale(geyser) # Standardize the data
mc <- Mclust(df) # Model-based-clustering
summary(mc)
# Outputs:
mc$modelName                # Modelo óoptimo seleccionado ==> "VVI"
mc$G                        # Número óptimo de cluster => 4
head(mc$z, 10)              # Probas de pertenencia
head(mc$classification, 20) # Asignación de clusters
# Gráficos: library(factoextra)
# Número de cluster de acuerdo al BIC
fviz_mclust(mc, "BIC", palette = "jco")
# Clasificación o agrupación:
fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Incertidumbre
fviz_mclust(mc, "uncertainty", palette = "jco")

# También podemos usar datos multivariados:
library("mclust")
data("diabetes")
head(diabetes, 7)
# Estandarizamos y ajustamos el modelo:
df <- scale(diabetes[, -1])
mc <- Mclust(df)
summary(mc)
# Outputs:
mc$modelName                # Modelo óoptimo seleccionado ==> "VVV"
mc$G                        # Número óptimo de cluster => 3
head(mc$z, 10)              # Probas de pertenencia
head(mc$classification, 20) # Asignación de clusters
# Gráficos: library(factoextra)
# Número de cluster de acuerdo al BIC
fviz_mclust(mc, "BIC", palette = "jco")
# Clasificación o agrupación:
fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Incertidumbre
fviz_mclust(mc, "uncertainty", palette = "jco")
# Los símbolos más grandes indican las observaciones más inciertas.



#  ---------------------------  #
# |       Conglomerados       | #
# |  Basados en la Densidad   | #
#  ---------------------------  #

# Algoritmo de clusterización basado en la densidad, que puede utilizarse
# para identificar clusters de cualquier forma en un conjunto de datos que
# contenga ruido y valores atípicos.
# Los clusters son regiones densas en el espacio de datos, separadas por 
# regiones de menor densidad de puntos. La idea clave es que para cada punto
# de un clúster, la vecindad de un radio determinado tiene que contener al 
# menos un número mínimo de puntos.


library(factoextra)
# Usaremos un conjunto de datos que contiene clusters de múltiples formas:
data("multishapes", package = "factoextra")
head(multishapes, 7)
df <- multishapes[, 1:2]
# De manera gráfica, vemos 5 grupos:
plot(df)
# Número de grupos
table(multishapes$shape)
multishapes$shape <- as.factor(multishapes$shape)
p<-ggplot(multishapes, aes(x=x, y=y, color=shape)) + geom_point()
set_palette(p, "jco")

# ¿Qué me diría K-Medias?
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df,  geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

# Dos parámetros importantes son necesarios para la función DBSCAN: 
# epsilon ("eps") y puntos mínimos ("MinPts"). El parámetro eps define
# el radio de vecindad alrededor de un punto x. Se llama la ϵ-vecindad 
# de x. El parámetro MinPts es el número mínimo de vecinos dentro del 
# radio "eps".
# Cualquier punto x en el conjunto de datos, con un número de vecinos 
# mayor o igual a MinPts, se marca como punto central. Decimos que x es 
# un punto fronterizo, si el número de sus vecinos es menor que MinPts, 
# pero pertenece a la ϵ-vecindad de algún punto central z. Finalmente, 
# si un punto no es ni un punto central ni un punto fronterizo, entonces 
# se llama un punto de ruido o un valor atípico.

library("fpc")
# Ajustamos el modelo:
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
# Graficamos:
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
# Los puntos negros corresponden a los valores atípicos.
# Habría que calibrar/ver las distintas elecciones de eps y MinPts

# Se puede observar que DBSCAN funciona mejor para estos conjuntos de 
# datos y puede identificar el conjunto correcto de clusters en comparación
# con los algoritmos k-means.

# El modelo:
print(db)
# Los nombres de las columnas son números de clúster. El clúster 0 
# corresponde a los valores atípicos (puntos negros en el gráfico DBSCAN).

# Calibramos el parámetro eps:
library(dbscan)
# La idea es calcular, la media de las distancias de cada punto a sus k 
# vecinos más cercanos. El valor de k será especificado por el usuario y 
# corresponde a MinPts.
# El objetivo es determinar la "rodilla", que corresponde al parámetro eps óptimo.
dbscan::kNNdistplot(df, k =  5)
# Se puede observar que el valor óptimo de eps está alrededor de 0.15.
abline(h = 0.15, lty = 2)

