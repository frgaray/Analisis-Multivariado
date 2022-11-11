##################################################
###          Seminario de Estadística          ###
###           Martes 12, octubre 2021          ###
##################################################

###############################
##                           ##
##  ANÁLISIS NO SUPERVISADO  ##
##                           ##
###############################

# Trabajaremos con el conjunto de datos "USArrests", que contiene estadísticas
# de arrestos por cada 100.000 residentes por agresión, asesinato y violación 
# en cada uno de los 50 estados de EE.UU. en 1973. También incluye el porcentaje
# de la población que vive en zonas urbanas.
data("USArrests")  # Load the data set
df <- USArrests
names(df)
dim(df)

# Eliminamos los valores faltantes en caso de existir:
df <- na.omit(df)
dim(df)

# Estandarizamos para que las variables estén en la misma unidad:
df <- scale(df)
head(df, n = 7)

# Librerías necesarias
#install.packages(c("cluster", "factoextra"))


#  ----------------------  #
# | Medidas de Distancia | #
#  ----------------------  #

# Tomaremos un subconjunto de estados, para ilustrar:
# Subset of the data
set.seed(123)
ss <- sample(1:50, 15)
df1 <- USArrests[ss, ]
df.scaled <- scale(df1)

# method = “euclidean”, “maximum”, “manhattan”, “canberra”, “binary”, “minkowski”
dist.eucl <- dist(df.scaled, method = "euclidean")
# Matriz de distancias:
round(as.matrix(dist.eucl)[1:3, 1:3], 1)

# Distancia basada en la correlación:
# Considera que dos objetos son similares si sus características están muy 
# correlacionadas, aunque los valores observados puedan estar muy alejados en 
# términos de distancia euclidiana.
library("factoextra")
dist.cor <- get_dist(df.scaled, method = "pearson")
round(as.matrix(dist.cor)[1:3, 1:3], 1)

# Distancias para datos no numéricos:
# La función daisy(), del paquete cluster, proporciona una solución (la métrica 
# de Gower) para calcular la matriz de distancia, en la situación en que los 
# datos contienen columnas no numéricas.
library(cluster)
data(flower)
head(flower, 7)
str(flower)
dd <- daisy(flower)
round(as.matrix(dd)[1:3, 1:3], 2)

# Viusualización:
library(factoextra)
fviz_dist(dist.eucl)
# Rojo indica alta similitud, azul baja similitud



#  -----------------------  #
# |      K - Medias       | #
# |  Método de Partición  | #
#  -----------------------  #

# La función kmeans(), está en la paquetería stats
# kmeans(x, centers, iter.max = 10, nstart = 1)
# 1. x: vector, matriz o data frame numéricos
# 2. centers: Número de clusters (k) ó un conjunto de centros iniciales (distintos) de 
#             clusters. Si es un número, se elige un conjunto aleatorio de filas 
#             (distintas) en x como centros iniciales.
# 3. iter.max: El número máximo de iteraciones permitido. El valor default es 10.
# 4. nstart: El número de particiones iniciales aleatorias cuando los 'centers' es un 
#            número. Se recomienda probar con nstart > 1.

# Una estimación del número óptimo de clusters:
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
# La gráfica representa la varianza dentro de los clusters. Disminuye a medida que k
# aumenta, pero puede verse una curva (o "codo") en k = 4. Esta curva indica que los 
# conglomerados adicionales más allá del cuarto tienen poco valor. 

set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
# nstart = 25 significa que R probará 25 asignaciones iniciales aleatorias diferentes 
# y luego seleccionará el mejor resultado correspondiente a la que tenga la menor 
# variación dentro del cluster. El valor por defecto de nstart en R es uno.
print(km.res)

# Medias por Cluster:
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)
# Clasificación en los datos originales:
dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd, 7)

# Outputs:
# km.res$cluster: Número de cluster en cada observación:
head(km.res$cluster, 7)
# Tamaño de los grupos:
km.res$size
# Media de los centroides
km.res$centers

# ¿Cómo podemos graficar a los grupos formados?
# K-means de forma gráfica:
fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Elipses
             star.plot = TRUE, # Segmentos hacia los centroides
             repel = TRUE, # Sobretrazado de etiquetas
             ggtheme = theme_minimal()
)



#  -----------------------  #
# |      Jerárquicos      | #
# |  Método Aglomerativo  | #
#  -----------------------  #

# Matriz de disimilitud:
res.dist <- dist(df, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]
# Función hclust():
res.hc <- hclust(d = res.dist, method = "ward.D2")
# 1. d: una estructura de disimilitud producida por la función dist().
# 2. method: El método de aglomeración que se utilizará para calcular la 
#    distancia entre clusters. Posibles valores: "ward.D", "ward.D2", "single", 
#    "complete", "average", "mcquitty", "median" o "centroid".
#    a) complete: La distancia entre dos clusters se define como el valor máximo 
#                 de todas las distancias por pares entre los elementos del cluster 
#                 1 y los elementos del cluster 2. Tiende a producir clusters más 
#                 compactos.
#    b) single: La distancia entre dos clusters se define como el valor mínimo de 
#               todas las distancias por pares entre los elementos del cluster 1 y 
#               los elementos del cluster 2. Tiende a producir clusters largos.
#    c) average: La distancia entre dos clusters se define como la distancia media 
#                entre los elementos del cluster 1 y los elementos del cluster 2.
#    d) centroid: La distancia entre dos clusters se define como la distancia entre 
#                 el centroide del cluster 1 y el centroide del cluster 2.
#    e) ward.D: Minimiza la varianza total dentro del clúster. En cada paso se 
#               fusionan los pares de clusters con la mínima distancia entre clusters.

# Dendograma:
fviz_dend(res.hc, cex = 0.5)
# Las alturas, proporcionada en el eje vertical, indican las similitudes/distancias
# entre dos objetos/grupos. Cuanto mayor sea la altura, menos similares serán los 
# objetos. Esta altura se conoce como la distancia cofenética entre los dos objetos.

# Calcularemos la correlación entre las distancias cofenéticas y las distancias de los
# datos originales generados por la función dist(), para evaluar si las distancias 
# (es decir, las alturas) en el árbol reflejan las distancias originales con precisión.
res.coph <- cophenetic(res.hc)
cor(res.dist, res.coph)

# Usamos otro método:
res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, cophenetic(res.hc2))
# El coeficiente de correlación muestra que el uso de un método de enlace diferente 
# crea un árbol que representa las distancias originales ligeramente mejor.

# Cortamos el árbol en 4 grupos:
grp <- cutree(res.hc, k = 4)
head(grp, n = 4)
# Número de elementos en cada grupo:
table(grp)
# ¿Quiénes están en el grupo 1?
rownames(df)[grp == 1]
# Coloreamos el árbol de acuerdo a la agrupación
fviz_dend(res.hc, k = 4, # Corta en 4 grupos
          cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # colorea por grupos
          rect = TRUE # Rectangula los grupos
)
# Vemos de manera gráfica:
fviz_cluster(list(data = df, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Elipses
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Otra función dentro de la paquetería 'cluster':
library("cluster")
# Agglomerative Nesting
res.agnes <- agnes(x = USArrests, # matriz de datos
                   stand = TRUE, # estandarizar
                   metric = "euclidean", # métrica
                   method = "ward" # método
)
fviz_dend(res.agnes, cex = 0.6, k = 4)
# Dibujarlo de otras formas:
fviz_dend(res.agnes, cex = 0.5, k = 4,
          k_colors = "jco", type = "circular")
require("igraph")
fviz_dend(res.agnes, k = 4, k_colors = "jco",
          type = "phylogenic", repel = TRUE)



#  -----------------------  #
# |      Jerárquicos      | #
# |    Método Divisivo    | #
#  -----------------------  #
# El clustering divisivo es bueno para identificar clusters grandes, 
# mientras que el clustering aglomerativo es bueno para identificar 
# clusters pequeños.
library(cluster)
# DIvisive ANAlysis Clustering
res.diana <- diana(USArrests, stand = TRUE, metric = "euclidean")
fviz_dend(res.diana, cex = 0.5,
          k = 4, # Corte en 4 grupos
          palette = "jco")



#  ----------------------  #
# |    Comparación de    | #
# |     Dendogramas      | #
#  ----------------------  #

# Tomamos una parte de la base:
df <- scale(USArrests)
set.seed(123)
ss <- sample(1:50, 10)
df <- df[ss,]

library(dendextend)
# Matriz de distancias
res.dist <- dist(df, method = "euclidean")
# Hacemos dos agrupaciones:
hc1 <- hclust(res.dist, method = "average")
hc2 <- hclust(res.dist, method = "ward.D2")
# Y los dos dendogramas:
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
# Creamos una lista:
dend_list <- dendlist(dend1, dend2)
# Dibujamos ambos:
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% # Mejor alineación
  tanglegram()                       # Dibuja los 2 dendogramas
# Enredo/entrelazamiento. La medida es cercana a 0 (no hay enredo)
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% 
  entanglement()                     # Calidad de la alineación
# Algunos parámetros:
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% 
  tanglegram(
    highlight_distinct_edges = FALSE, # Líneas punteadas
    common_subtrees_color_lines = FALSE, # Líneas de colores
    common_subtrees_color_branches = TRUE # Color ramas comúnes 
  )
# Matriz de correlación entre una lista de dendrogramas:
# Valores cercanos a 0 significan que los dos árboles no son 
# estadísticamente similares
cor.dendlist(dend_list, method = "cophenetic")
cor_cophenetic(dend1, dend2)


#  ----------------------  #
# |    Evaluación de     | #
# | Tendencia Agrupativa | #
#  ----------------------  #

# Antes de aplicar cualquier método de clustering, es importante evaluar si 
# los datos contienen clusters significativos, es decir, estructuras no 
# aleatorias, o no. En caso afirmativo, entonces nos podemos preguntar el
# número de clusters existente.

# Usaremos los siguientes datos:
head(iris, 7)
# Quitamos la variable categórica
df <- iris[, -5]
# Datos aleatorios generados a partir del conjunto de datos iris
random_df <- apply(df, 2, 
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)
# Estandarizamos ambos conjuntos:
df <- iris.scaled <- scale(df)
random_df <- scale(random_df)

# Graficamos ambos conjuntos de datos:
# Datos originales
fviz_pca_ind(prcomp(df), title = "PCA - Iris data", 
             habillage = iris$Species,  palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

# Datos generados
fviz_pca_ind(prcomp(random_df), title = "PCA - Random data", 
             geom = "point", ggtheme = theme_classic())
# Se puede observar que el conjunto de datos iris contiene 3 clusters reales. 
# Sin embargo, el conjunto de datos generado aleatoriamente no contiene ningún 
# clúster significativo.

# Evaluaremos la tendencia de los clusters:
set.seed(123)
# K-means en los datos iris
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())
# K-means en los datos generados
km.res2 <- kmeans(random_df, 3)
fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

# Clustering jerárquico en los datos generados
fviz_dend(hclust(dist(random_df)), k = 3, k_colors = "jco",  
          as.ggplot = TRUE, show_labels = FALSE)

# Estadística de Hopkins:
# Hip Nula -> el conjunto de datos D está distribuido de manera uniforme
# (es decir, no hay conglomerados significativos)
# Hip Altern -> el conjunto de datos D no está distribuido de manera uniforme
# (es decir, contiene clusters significativos)
# Podemos realizar la prueba estadística de Hopkins, utilizando 0.5 como 
# referencia para rechazar la hipótesis alternativa. Es decir, si H < 0.5, 
# entonces es poco probable que D tenga agrupaciones estadísticamente 
# significativas.
# Dicho de otro modo, si el valor del estadístico Hopkins es cercano a 1, 
# entonces podemos rechazar la hipótesis nula y concluir que el conjunto de 
# datos D es agrupable de manera significativa.

# Datos iris:
res <- get_clust_tendency(df, n = nrow(df)-1, graph = FALSE)
res$hopkins_stat
# Datos generados:
res <- get_clust_tendency(random_df, n = nrow(random_df)-1,
                          graph = FALSE)
res$hopkins_stat
# Se puede observar que el conjunto de datos del iris es altamente 
# agrupable (el valor H = 0.82, que está muy por encima de la referencia 0.5). 
# Sin embargo, el conjunto de datos random_df no es agrupable (H = 0.46)



#  ----------------------  #
# |      Número de       | #
# |        grupos        | #
#  ----------------------  #
#library(factoextra)
library(NbClust)
df <- scale(USArrests)
head(df)

# Métodos de Codo, Siluetas y GAP
# Codo: Considera la variación total intra-cluster en función del número de
#       conglomerados. Se debe elegir un número de clusters tal que añadir 
#       otro cluster no mejore mucho la variación intra-cluster total.
#       La variación totañ intra-cluster mide la compacidad de la agrupación 
#       y queremos que sea lo más pequeña posible
# Siluetas: Mide la calidad de una agrupación. Es decir, determina lo bien que
#           se encuentra cada objeto dentro de su cluster. Una anchura media 
#           de la silueta elevada indica una buena agrupación.
#           El número óptimo de clusters k es el que maximiza la silueta media 
#           en un rango de valores posibles para k 
# GAP: Compara la variación total dentro del cluster para diferentes valores de 
#      k con sus valores esperados bajo una distribución de referencia nula de 
#      los datos. La estimación de los conglomerados óptimos será el valor que 
#      maximice el estadístico de GAP. Esto significa que la estructura de 
#      agrupación se aleja de la distribución aleatoria uniforme de puntos.

fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
set.seed(123)
# nboot=50 para hacerlo rápido, se recomienda nboot=500
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

# Podemos pedir más opiniones:
nb <- NbClust(df, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)



#  ----------------------  #
# |      Validación      | #
#  ----------------------  #
# Los datos iris sin las categorías:
df <- iris[, -5]
# Estandarizamos:
df <- scale(df)
# Analisis de Cluster con otra función:
# eclust(x, FUNcluster = "kmeans", hc_metric = "euclidean", ...)
#   x: vector numérico, matriz o data frame
#   FUNcluster: “kmeans”, “pam”, “clara”, “fanny”, “hclust”, 
#               “agnes” o “diana”
#   hc_metric: Cuando seleccionamos “hclust”, “agnes” o “diana”.
#              Podemos elegir: “euclidean”, “manhattan”, “maximum”, 
#              “canberra”, “binary”, “minkowski”, “pearson”, “spearman” 
#              o “kendall”

# K-medias clustering
km.res <- eclust(df, "kmeans", k = 3, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())
# Clustering jerárquico
hc.res <- eclust(df, "hclust", k = 3, hc_metric = "euclidean", 
                 hc_method = "ward.D2", graph = FALSE)
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

# Gráfica de SILUETAS:
# El análisis de siluetas mide el grado de agrupación de una observación
# y estima la distancia media entre clusters. El gráfico de siluetas 
# muestra una medida de la proximidad de cada punto de un clúster a los 
# puntos de los clústeres vecinos.
fviz_silhouette(km.res, palette = "jco",
                ggtheme = theme_classic())
# Valores Si cercanos a 1 indica que el objeto está bien agrupado. En otras 
# palabras, el objeto i es similar a los demás objetos de su grupo.
# Valores Si cercano a -1 indica que el objeto está mal agrupado, y que la 
# asignación a algún otro grupo probablemente mejoraría los resultados globales.

# Información de Silhouette
silinfo <- km.res$silinfo
names(silinfo)
# Anchura de la silueta de cada observación
head(silinfo$widths[, 1:3], 10)
# Anchura media de la silueta de cada cluster
silinfo$clus.avg.widths
# La media total (media de todas las anchuras de silueta individuales)
silinfo$avg.width
# Tamaño de cada cluster
km.res$size

# Se puede observar que varias muestras, en el cluster 2, tienen un 
# coeficiente de silueta negativo. Esto significa que no están en el 
# cluster correcto. Podemos encontrar el nombre de estas muestras y 
# determinar los clusters a los que están más cerca (cluster vecino):

# Valores de cada observación:
sil <- km.res$silinfo$widths[, 1:3]
# Observaciones con valores negativos:
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

# Índice de Rand
# ¿Coincide la agrupación de K-means con la verdadera estructura de los datos?
table(iris$Species, km.res$cluster)
# Es posible cuantificar la concordancia entre los clusters de Species y 
# k-medias utilizando el índice de Rand:
library("fpc")
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(df), 
                             species, km.res$cluster)
clust_stats$corrected.rand
# El índice de Rand corregido proporciona una medida para evaluar la similitud 
# entre dos particiones, ajustada al azar. Su rango es de -1 (sin acuerdo) a 1 
# (acuerdo perfecto).

# Lo mismo, para un método jerárquico:
# Agreement between species and HC clusters
res.hc <- eclust(df, "hclust", k = 3, graph = FALSE)
table(iris$Species, res.hc$cluster)
cluster.stats(d = dist(iris.scaled), 
              species, res.hc$cluster)$corrected.rand



#  --------------------------  #
# |      Comparación de      | #
# |     distintos métodos    | #
#  --------------------------  #
# Podemos usar la sig función:
# clValid(obj, nClust, clMethods = "hierarchical", 
#         validation = "stability", maxitems = 600,
#         metric = "euclidean", method = "average")
#
# obj: Matriz numérica o data frame
# nClust: Vector numérico que especifica el número de conglomerados a evaluar
# clMethods: “hierarchical”, “kmeans”, “diana”, “fanny”, “som”, “model”, 
#            “sota”, “pam”, “clara”, “agnes”
# validation: “internal”, “stability”
# maxitems: El número máximo de elementos (filas en la matriz) que se pueden agrupar.
# metric: Metrica para la matriz de distancias, “euclidean”, “correlation”,
#         “manhattan”
# method: “ward”, “single”, “complete”, “average”

# Ejemplificaremos con los datos Iris:
library(clValid)
df <- scale(iris[, -5])
clmethods <- c("hierarchical","kmeans","diana", "agnes")
intern <- clValid(df, nClust = 2:6, 
                  clMethods = clmethods, validation = "internal")
summary(intern)
# Se observa que la agrupación jerárquica con dos clusters es la que mejor 
# funciona en cada caso (es decir, para las medidas de conectividad, Dunn y 
# Silueta). Independientemente del algoritmo de agrupación, el número óptimo 
# de conglomerados parece ser dos utilizando las tres medidas.

# Medidas de Estabilidad
clmethods <- c("hierarchical","kmeans","diana", "agnes")
stab <- clValid(df, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
# Valores óptimos:
optimalScores(stab)
# Para las medidas APN y ADM, la agrupación jerárquica con dos clusters vuelve
# a da la mejor puntuación. Para las demás medidas, diana con seis clusters 
# tiene la mejor puntuación.

# La proporción promedio de no solapamiento (APN):
# mide la proporción media de observaciones no colocadas en el mismo cluster
# mediante la agrupación basada en los datos completos y la agrupación basada 
# en los datos con una sola columna eliminada.
# La distancia promedio (AD):
# mide la distancia media entre las observaciones colocadas en el mismo cluster
# en ambos casos (conjunto de datos completo y eliminación de una columna).
# La distancia promedio entre medias (ADM):
# mide la distancia media entre los centros de los clusters para las observaciones
# situadas en el mismo cluster en ambos casos.
# La cifra de mérito (FOM):
# mide la varianza media intracluster de la columna eliminada, cuando la agrupación
# se basa en las columnas restantes (no eliminadas).

