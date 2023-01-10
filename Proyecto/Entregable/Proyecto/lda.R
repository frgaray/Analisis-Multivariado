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

str(Datos$EVOLUCI)


library(caret)
library(metrica)

set.seed(1)
B <- 50
partition <- createDataPartition(Datos$EVOLUCI, p = .80, list = FALSE, times = B)

#' LDA y QDA
library(MASS)

lda.rep.holdout <- function(partition, B=50, qda = F) {
  ldaTrain <- data.frame(1, 2, 3)[-1,]
  colnames(ldaTrain) <- c("global", "sensibilidad", "especificidad")
  for (i in 1:B) {
    if (!(i %in% c(2,6,7,8,14,15,16,17,18,20,21,22,24,28,31,43,44,45,46,47,48))) {
      train= partition[,i]
      test = (-train)
      if (qda) {
        trainModel <- qda(EVOLUCI ~ ., Datos[train,])
      } else {
        trainModel <- lda(EVOLUCI ~ ., Datos[train,])
      }
      predictModel <- predict(trainModel, Datos[test,])
      predb <- predictModel$class
      t <- table(Datos[test,"EVOLUCI"], predb)
      global <- (t[1,1]+t[2,2])/sum(t)
      spec <- t[1,1]/sum(t[1,])
      sens <- t[2,2]/sum(t[2,])
      ldaTrain[nrow(ldaTrain)+1,] <- c(global, spec, sens)
    }
  }
  return(colMeans(ldaTrain))
}

lda.rep.holdout(partition) # B=29
# 0.9194022 0.9233452 0.8928094
ldaFit <- lda(EVOLUCI ~ ., Datos) # Modelo Final
coef(ldaFit)
sort(coef(ldaFit), decreasing = T)
# SERINGREUCI                                             2.314279e+00
# TIPACIENHOSPITALIZADO                                   1.229430e+00
# DIGCLINESI                                              9.434082e-01
# SERINGREURGENCIAS ADULTOS                               8.804336e-01
# SECTORIMSS-OPORTUNIDADES                                7.875900e-01
# SERINGREUTIP                                            6.864615e-01
# DIAGPROBINFECCION RESPIRATORIA AGUDA GRAVE (IRAG)       6.605724e-01
# RESDEFINNO ADECUADO                                     6.200018e-01

#' influyentes para res negativo:
#' SERINGREUCIN
#' RESDEFINCORONA 229E
#' RESDEFININF AH1N1 PMD
#' URGENCIAS CIRUGÍA?

lda.rep.holdout(partition, qda=T)
