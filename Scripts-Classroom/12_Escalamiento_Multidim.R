##################################################
###           Análisis Multivariado            ###
###        Escalamiento Multidimensional       ###
##################################################

# El escalamiento multidimensional (EMD) es un enfoque de análisis de datos multivariados que se 
# utiliza para visualizar la similitud/desigualdad entre las muestras mediante el trazado de puntos
# en gráficos bidimensionales.

# EMD devuelve una solución óptima para representar los datos en un espacio de menor dimensión, 
# donde se debe especificar previamente el número de dimensiones k. Por ejemplo, elegir k = 2 
# optimiza las ubicaciones de los objetos para un gráfico de dispersión bidimensional.

# Un algoritmo EMD toma como dato de entrada la matriz de disimilitud, que representa las distancias 
# entre pares de objetos.

# Los datos de entrada para EMD son una matriz de disimilitud que representa las distancias entre 
# pares de objetos.


   # 1. Escalamiento multidimensional clásico
   #    Conserva la métrica de distancia original, entre puntos, lo mejor posible. Esas son las 
   #    distancias ajustadas en el mapa EMD y las distancias originales están en la misma métrica. 
   #    EMD clásico pertenece a la llamada categoría de escalado multidimensional métrico.
   #
   #    También se le conoce como análisis de coordenadas principales. Es adecuado para datos 
   #    cuantitativos.

   # 2. Escalamiento multidimensional no métrico
   #    También se conoce como EMD ordinal. Aquí, no es la métrica de un valor de distancia lo 
   #    que es importante o significativo, sino su valor en relación con las distancias entre 
   #    otros pares de objetos.
   #
   #    EMD ordinal construye distancias ajustadas que están en el mismo orden de rango que la 
   #    distancia original. Por ejemplo, si la distancia de los objetos separados 1 y 5 ocupa el 
   #    quinto lugar en los datos de distancia originales, entonces también debería ocupar el 
   #    quinto lugar en la configuración de EMD.
   #
   #    Es adecuado para datos cualitativos.



#  --------------------
# |     Ejemplo 1.     |
#  --------------------

# Usaremos la base 'swiss': datos socioeconómicos y de fertilidad de 47 provincias de habla 
# francesa en Suiza.
data("swiss")
head(swiss)

# ESCALAMIENTO MULTIDIMENSIONAL CLÁSICO:
# Paquetería requerida:
library(magrittr)
library(dplyr)
library(ggpubr)
library(MASS)
# Ajuste del EMC
mds <- swiss %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Graficamos el EMC
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          size = 1,
          repel = TRUE)
# Podemos crear 3 grupos con ayuda de K-means:
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Graficamos y coloreamos por grupo:
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)


# ESCALAMIENTO MULTIDIMENSIONAL NO MÉTRICO:

# Método Kruskal
mds <- swiss %>%
  dist() %>%          
  isoMDS() %>%
  .$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Graficamos:
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          size = 1,
          repel = TRUE)

# Método Sammon
mds <- swiss %>%
  dist() %>%          
  sammon() %>%
  .$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          size = 1,
          repel = TRUE)



#  --------------------
# |     Ejemplo 2.     |
#  --------------------

# Escalamiento Multidimensional para visualizar una matriz de correlación:

# La correlación en realidad mide la similitud, pero es fácil transformarla en una 
# medida de disimilitud. La distancia entre los objetos se puede calcular como
# 1-correlación.

res.cor <- cor(mtcars, method = "spearman")
mds.cor <- (1 - res.cor) %>%
  cmdscale() %>%
  as_tibble()
colnames(mds.cor) <- c("Dim.1", "Dim.2")
ggscatter(mds.cor, x = "Dim.1", y = "Dim.2", 
          size = 1,
          label = colnames(res.cor),
          repel = TRUE)
# Los objetos correlacionados positivos están muy juntos en el mismo lado de la gráfica.