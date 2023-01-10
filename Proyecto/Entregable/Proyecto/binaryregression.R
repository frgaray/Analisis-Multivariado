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



#' Modelos lineales generalizados binarios, múltiples ligas
binary.rep.holdout <- function(partition, B = 50, link = "logit", interactions = F) {
  binaryTrain <- data.frame(1, 2, 3)[-1,]
  colnames(binaryTrain) <- c("global", "sensibilidad", "especificidad")

  for (i in 1:B) {
    if (!(i %in% c(2,6,7,8,9,14,15,16,17,18,20,21,22,24,26,28,31,33,34,35,36,43,44,45,46,47,48))) {
      train <- partition[,i]
      test <- (-train)
      if (interactions)
        trainModel <- glm(EVOLUCI ~ .^2, Datos[train,], family = binomial(link = link))
      else
        trainModel <- glm(EVOLUCI ~ ., Datos[train,], family = binomial(link = link))
      testPredict <- predict(trainModel, newdata = Datos[test,], type = "response")
      predb <- ifelse(testPredict >= 0.5,
                      levels(Datos$EVOLUCI)[2],
                      levels(Datos$EVOLUCI)[1])
      t <- table(Datos[test,"EVOLUCI"], predb)
      global <- (t[1,2]+t[2,1])/sum(t)
      spec <- t[1,2]/sum(t[1,])
      sens <- t[2,1]/sum(t[2,])
      binaryTrain[nrow(binaryTrain)+1,] <- c(global, spec, sens)
    }
  }
  return(colMeans(binaryTrain))
}

for (link in c("logit", "probit", "cauchit", "cloglog")) {
  print(paste("Liga:", link))
  print(binary.rep.holdout(partition = partition, link = link, interactions = T))
}

# [1] "Liga: logit"
# global  sensibilidad especificidad 
# 0.9171780     0.9480760     0.7087968 
# [1] "Liga: probit"
# global  sensibilidad especificidad 
# 0.9178744     0.9482259     0.7131783 
# [1] "Liga: cauchit"
# global  sensibilidad especificidad 
# 0.9126083     0.9442779     0.6990226 
# [1] "Liga: cloglog"
# global  sensibilidad especificidad 
# 0.9144797     0.9514743     0.6649815 

levels(Datos$EVOLUCI)
logitFit <- glm(EVOLUCI ~ ., Datos, family = binomial()) # Modelo final
sort(coef(logitFit), decreasing = T)
# RESDEFINNO ADECUADO                                     RESDEFINSARS-CoV-2 
# 1.510742e+01                                           1.500130e+01 
# RESDEFINRECHAZADA                                       RESDEFINNEGATIVO 
# 1.453848e+01                                           1.414816e+01 
# RESDEFINENTEROV//RHINOVIRUS                               SECTORIMSS-OPORTUNIDADES 
# 1.404648e+01                                           1.399820e+01 
# RESDEFINNO RECIBIDA                                     SECTORPEMEX 
# 1.332085e+01                                           1.174761e+01 
# SECTORSEDENA                                              SECTORSSA 
# 1.170541e+01                                           1.112118e+01 
# SECTORIMSS                                    SECTORUNIVERSITARIO 
# 1.098488e+01                                           1.074709e+01 
# SECTORISSSTE                                          SECTORESTATAL 
# 1.073513e+01                                           1.026751e+01 
# SECTORPRIVADA                                            SECTORSEMAR 
# 9.882712e+00                                           9.641127e+00 
# TIPACIENHOSPITALIZADO                                   RESDEFININF A 
# 4.362150e+00                                           3.038075e+00 
# CVEENTUNI7                                    RESDEFINCORONA NL63 
# 2.527839e+00                                           2.448796e+00 
# RESDEFINNO SUBTIPIFICADO                                CVEENTUNI11 
# 1.868494e+00                                           1.816068e+00 
# CVEENTUNI5                                            CVEENTUNI27 
# 1.585702e+00                                           1.558444e+00 
# CVEENTUNI15                      OCUPACIOGERENTES O PROPIETARIOS DE EMPRESAS O NEGOCIOS 
# 1.517810e+00                                           1.443330e+00 
# CVEENTUNI26                                            CVEENTUNI32 
# 1.324634e+00                                           1.323600e+00 
# CVEENTUNI21                                            CVEENTUNI10 
# 1.297160e+00                                           1.273322e+00 
# CVEENTUNI28                DIAGPROBINFECCION RESPIRATORIA AGUDA GRAVE (IRAG) 
# 1.209605e+00                                           1.110628e+00 
# SERINGREOBSERVACION DE URGENCIAS                        CVEENTUNI19 
# 1.108904e+00                                           1.042253e+00 
# CVEENTUNI9                          
# 1.027236e+00

#' influyentes para res negativo:
#' SERINGREUCIN
#' DENTISTAS
#' LABORATORISTAS
#' RESDEFIN CORONA 229
#' RESDEFININF AH1N1 PMD
#' SECTOR MUNICIPAL
#' SERINGRE URGENCIAS CIRUGÍA
#' ENFERMERAS