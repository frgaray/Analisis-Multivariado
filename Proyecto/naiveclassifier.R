Datos <- read.csv("datos-covid.csv", na.strings = c("", "NA", "SE IGNORA"), stringsAsFactors = T)

colMeans(is.na(Datos) * 100) # Porcentaje de NA's

# Quitar variables con más del 20% de NA's. TOMMUE constante "SÍ"
nombres <- c("FECDEF", "INTUBADO", "ANOSMIA", "DISGEUSIA", "TXCROBIA", "ANTIVIRA",
             "FECINITXANTIVI", "CONOCASO", "CONANIMA", "FECVAEST", "TOMMUE",
             "PUERPERIO", "DIASPUERP", "UCI")
Datos <- Datos[, !names(Datos) %in% nombres]

# Cambiando a factor CVEENTUNI (clave entidad federativa)
Datos[, "CVEENTUNI"] <- factor(Datos[, "CVEENTUNI"])
# Listo para la imputación


#install.packages("mice")
library(mice)

init <- mice(Datos, maxit=0)
meth <- init$method
predM <- init$predictorMatrix

# Método de imputación para datos binarios
meth[c("ORIGEN", "SEXO", "TIPACIEN", "DIAGPROB")] <- "logreg"
# Método de imputación para datos categóricos nominales
meth[c("SECTOR", "CVEENTUNI", "ENTIDAD", "OCUPACIO", "SERINGRE", "RESDEFIN")] <- "polyreg"
# Método de imputación para datos numéricos
meth[c("EDAD")] <- "norm"

# Visualizamos los métodos de imputación por variable
meth
set.seed(1)
imputed <- mice(Datos, method = meth, predictorMatrix = predM, maxit=0)
imputed <- complete(imputed) 
summary(imputed) # Estos son los datos imputados
colMeans(is.na(imputed) * 100) # Porcentaje de NA's después de la imputación (0%)
# Datos imputados

Datos <- within(imputed, {
  FECHREG  <- as.Date(FECHREG)
  FECINGRE <- as.Date(FECINGRE)
  FECINISI <- as.Date(FECINISI)
})



#' Reducimos a dos variables. Si se complica el paciente o si no se complica
levels(Datos$EVOLUCI) <- list(nocomplica="ALTA - CURACION",
                              nocomplica="ALTA - MEJORIA",
                              nocomplica="ALTA - TRASLADO",
                              nocomplica="ALTA - VOLUNTARIA",
                              complica="CASO GRAVE -",
                              complica="CASO GRAVE - TRASLADO",
                              nocomplica="CASO NO GRAVE",
                              complica="DEFUNCION",
                              nocomplica="EN TRATAMIENTO",
                              nocomplica="REFERENCIA",
                              nocomplica="SEGUIMIENTO DOMICILIARIO",
                              nocomplica="SEGUIMIENTO TERMINADO")

levels(Datos$EVOLUCI) # Referencia nocomplica
#' Vamos a calificar usando repeated holdout a diferentes modelos para poder predecir
#' nuevas observaciones.

str(Datos)


library(caret)
library(metrica)

set.seed(1)
B <- 50
partition <- createDataPartition(Datos$EVOLUCI, p = .80, list = FALSE, times = B)

#' Naive Classifier
library(e1071) 

nc.rep.holdout <- function(partition, B=50) {
  ncTrain <- data.frame(1, 2, 3)[-1,]
  colnames(ncTrain) <- c("global", "sensibilidad", "especificidad")
  for (i in 1:B) {
    train <- partition[,i]
    test <- (-train)
    trainModel <- naiveBayes(EVOLUCI ~ ., Datos[train,])
    predb <- predict(trainModel, Datos[test,])
    t <- table(Datos[test,"EVOLUCI"], predb)
    global <- (t[1,1]+t[2,2])/sum(t)
    spec <- t[1,1]/sum(t[1,])
    sens <- t[2,2]/sum(t[2,])
    ncTrain[i,] <- c(global, spec, sens)
  }
  return(colMeans(ncTrain))
}

nc.rep.holdout(partition)
# 0.9029029 0.9045977 0.8914729

ncFit <- naiveBayes(EVOLUCI ~ ., Datos) # Modelo Final

