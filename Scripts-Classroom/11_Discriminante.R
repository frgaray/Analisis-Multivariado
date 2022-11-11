##################################################
###          Seminario de Estadística          ###
###           Martes 26, octubre 2021          ###
##################################################

##########################################
#               Ejemplo 1:               #
#          Discriminante Lineal          #
##########################################
library(tidyverse)
library(caret)
theme_set(theme_classic())
# Utiliza combinaciones lineales de predictores para pronosticar la
# clase de una determinada observación. Supone que las p variables 
# predictoras se distribuyen normalmente y que las clases tienen 
# varianzas o matrices de covarianzas idénticas.

# Usaremos los siguientes datos
library('mlbench')
# PimaIndiansDiabetes2:
# pregnant -> Número de embarazos
# glucose  -> Concentración de glucosa en plasma
# pressure -> Presión arterial diastólica (mm Hg)
# triceps  -> Espesor del pliegue cutáneo del tríceps (mm)
# insulin  -> Insulina sérica de 2 horas (mu U/ml)
# mass     -> Índice de Masa Corporal
# pedigree -> Función de pedigrí de la diabetes
# age      -> Edad en años
# diabetes -> Variable de Clase (prueba de diabetes)
data("PimaIndiansDiabetes2", package = "mlbench")
str(PimaIndiansDiabetes2)
dim(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
dim(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2, 7)

# Dividimos los datos en entrenamiento y prueba con proporción 80-20:
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]

# Normalizamos los datos. Las variables categóricas se ignoran en 
# automático:
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
# Transformamos los datos:
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

# LDA
library(MASS)
# Ajustamos el modelo:
model <- lda(diabetes~., data = train.transformed)
model
# LDA determina las medias de los grupos y calcula, para cada individuo, la 
# probabilidad de pertenecer a los distintos grupos. A continuación, el individuo
# es clasificado al grupo con la mayor puntuación de probabilidad.
# Outputs:
#    - Prior probabilities of groups: la proporción de observaciones de 
#      entrenamiento en cada grupo. Por ejemplo, hay un 67% de las observaciones de 
#      entrenamiento en el grupo negativo.
#    - Group means: centro de gravedad del grupo, muestra la media de cada variable 
#      en cada grupo.
#    - Coefficients of linear discriminants: Muestra la combinación lineal de las 
#      variables predictoras que se utilizan para formar la regla de decisión del LDA.

# Distribuciones de las clases: Deben verse normal.
plot(model)

# Pronósticos:
predictions <- model %>% predict(test.transformed)
names(predictions)
# Outputs:
#    - class: clase pronosticada para cada observación
#    - posterior: matriz cuyas columnas son los grupos, las filas son los individuos y 
#      los valores son la probabilidad posterior de que la observación correspondiente 
#      pertenezca a los grupos.
#    - x: contiene los discriminantes lineales
# Clases pronosticadas:
head(predictions$class, 7)
# Probabilidades de pertenencia:
head(predictions$posterior, 7) 
# Discriminantes lineales
head(predictions$x, 7) 

# Graficamos la frontera de decisión:
library(FactoMineR)
Componentes=PCA(PimaIndiansDiabetes2[,-9], ncp = 2)
Componentes$eig
Y1<-Componentes$ind$coord[,1]
Y2<-Componentes$ind$coord[,2]

Base_Componentes<-data.frame(Y1,Y2, PimaIndiansDiabetes2$diabetes)
train.data <- Base_Componentes[training.samples, ]
test.data <- Base_Componentes[-training.samples, ]

lda_train <- lda(PimaIndiansDiabetes2.diabetes~., data = train.data)
plot(train.data[,1:2])

contour_data <- expand.grid(Y1 = seq(-4, 4, length = 100),
                            Y2 = seq(-4, 4, length = 100))
lda_predict <- data.frame(contour_data,
                          PimaIndiansDiabetes2.diabetes = predict(lda_train, contour_data)$class)

ggplot(lda_predict, aes(x = Y1, y = Y2, color = PimaIndiansDiabetes2.diabetes)) + 
  geom_point(size=1) +
  geom_point(data=train.data, 
             color=c("darkred","darkblue")[as.numeric(train.data$PimaIndiansDiabetes2.diabetes)], 
             size=4, shape=18) 

# Precisión del modelo:
mean(predictions$class==test.transformed$diabetes)




##########################################
#               Ejemplo 2:               #
#       Discriminante Cuadrático         #
##########################################

# Más flexible que el LDA. Aquí no se asume que la matriz de covarianza de las 
# clases sea la misma.
library(MASS)
# Ajustamos el modelo:
model <- qda(diabetes~., data = train.transformed)
model
# Predicciones:
predictions <- model %>% predict(test.transformed)
# Precisión:
mean(predictions$class == test.transformed$diabetes)

# Graficamos la frontera de decisión:
qda_train <- qda(PimaIndiansDiabetes2.diabetes~., data = train.data)
plot(train.data[,1:2])

contour_data <- expand.grid(Y1 = seq(-4, 4, length = 100),
                            Y2 = seq(-4, 4, length = 100))
qda_predict <- data.frame(contour_data,
                          PimaIndiansDiabetes2.diabetes = predict(qda_train, contour_data)$class)

ggplot(qda_predict, aes(x = Y1, y = Y2, color = PimaIndiansDiabetes2.diabetes)) + 
  geom_point(size=1) +
  geom_point(data=train.data, 
             color=c("darkred","darkblue")[as.numeric(train.data$PimaIndiansDiabetes2.diabetes)], 
             size=4, shape=18) 


library(pROC)
# Compute roc
res.roc <- roc(test.data$PimaIndiansDiabetes2.diabetes, predictions$posterior[,1])
plot.roc(res.roc, print.auc = TRUE)

#########################################
#           ¿Y la normalidad?           #
#         ¡Hay que verificarla!         #
#########################################