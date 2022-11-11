###########################################
###       ANÁLISIS   MULTIVARIADO       ###
###  Martes 23 - Jueves 25, marzo 2021  ###
###########################################



###########################################
# Inferencia para \mu con \Sigma conocida #
###########################################
X1 <- c(69,74,68,70,72,67,66,70,76,68,72,79,74,67,66,71,74,75,75,76)
X2 <- c(153,175,155,135,172,150,115,137,200,130,140,265,185,112,140,
        150,165,185,210,220)
Datos <- data.frame(X1, X2); Datos

# Se registran la estatura X1 (en pulgadas) y el peso X2 (en libras) 
# para 20 estudiantes.
# Se asume que esta m.a. es generada en una población normal bivariada
# con la siguiente matriz de covarianzas:
Sigma <- matrix(data = c(20, 100, 
                         100, 1000), 
                nrow =2, ncol = 2, byrow = T)
Sigma

# Se quiere verificar la hipótesis que la estatura media es 70 y el peso
# medio es 170.
# Queremos verificar Ho: mu = (70, 170)^T
mu0 <- c(70,170)
x_bar <- as.numeric(colMeans(Datos))
# Calcularemos el estadístico ji-cuadrado:
J <- length(X1)*t(x_bar-mu0)%*%solve(Sigma)%*%(x_bar -mu0); J

# El cuantil correspondiente es:
q <- qchisq(p = 0.95, df = 2, lower.tail = T); q

# La región de rechazo univariada es:
{curve(dchisq(x = x, df = 2), from = 0, to = 10, 
      col = "darkblue", lwd = 4, ylab = "f(x)", 
      main = "Región de Rechazo", xlab = "Estadístico",
      ylim = c(0,0.6))
abline(v = 0, h = 0, lwd = 3, lty = 1, col = 1)
x <- runif(1000,q,10)
y <- runif(1000,0,dchisq(q, df =2))
points(x[y < dchisq(x, df = 2)], y[y < dchisq(x, df = 2)],
       type = "p", pch = 16, col = "darkred", cex = 0.3)
text(1, 0.1, expression(paste("1-",alpha)), cex = 2, lwd = 4,
     col = "darkgreen")
points(J, 0, pch=16, col = "darkgreen", cex = 2)
text(J, 0.05, expression(chi[0]^2), cex = 1.5, lwd = 3,
     col = "darkgreen")}
# Rechazamos Ho.

# Analicemos la región de rechazo bivariada para la muestra:
{# library(conics)
v <- c(2,-0.4,0.04,-212,14.4,6190.01)
conicPlot(v, main="Región de no rechazo bivariada", 
          xlab="X1", ylab="X2",xlim = c(67,73), ylim = c(150,190),
          col = "darkblue", lwd = 5)
grid(12, 8, lwd = 2)
for (k in 1:10) {
  vv<-c(2,-0.4,0.04,-212,14.4,(6190+k))
  conicPlot(vv, add=T, lty=2, col = "darkgreen")
}
for (k in 1:8) {
  vv<-c(2,-0.4,0.04,-212,14.4,(6190-5*k))
  conicPlot(vv, add=T, lty=2, col = "darkred")
}
points(mu0[1], mu0[2], pch = 16, cex = 2, col = "dimgray")
text(70, 167, expression(mu[0]), cex = 1.5, lwd = 3,
     col = "dimgray")
text(67.6, 183, expression(chi[0]^2>5.991), cex = 1.75, lwd = 8,
     col = "darkred")
text(71, 178, expression(chi[0]^2<5.991), cex = 1.5, lwd = 5,
     col = "darkgreen")
arrows(70, 157.762, 71, 155, col = "darkblue", code = 2, cex = 2)
text(71.8, 155, expression(paste(chi[0]^2,"=",5.991)), cex = 1.75, lwd = 5,
     col = "darkblue")
points(x_bar[1], x_bar[2], pch = 8, cex = 1.5, col = "dimgray", lwd = 3)
text(71.7, 165, expression(bar(x)), cex = 2, lwd = 3,
     col = "dimgray")}
# La covarianza entre X1 y X2 fuera negativa, el eje principal de la elipse tendría
# una pendiente negativa; es decir, la elipse tendría una orientación diferente y la
# media podría estar dentro de la región de aceptación.

# Podemos hacer las pruebas univariadas, para comparar:
# Calculamos los estadísticos:
z1 <- (x_bar[1] -mu0[1])/(sqrt(Sigma[1,1])/sqrt(20)) ;z1
z2 <- (x_bar[2] -mu0[2])/(sqrt(Sigma[2,2])/sqrt(20)) ;z2
# Recordemos que la región de aceptación de estos estadísticos a un nivel de 
# significancia de alpha = 0.05 es :
c(qnorm(p = 0.05/2, mean = 0, sd = 1), qnorm(p = 1-(0.05/2), mean = 0, sd = 1))
# De manera univariada, no rechazamos que mu0[1] = 70 y mu0[2] = 170

# De forma gráfica, tenemos lo siguiente:
{
  v <- c(2,-0.4,0.04,-212,14.4,6190.01)
  conicPlot(v, main="Región de rechazo y no rechazo para pruebas 
            univariadas y multivariadas", 
            xlab="X1", ylab="X2",xlim = c(67,73), ylim = c(150,190),
            col = "darkblue", lwd = 5)
  grid(12, 8, lwd = 2)
  points(mu0[1], mu0[2], pch = 16, cex = 2, col = "dimgray")
  text(70.3, 170, expression(mu[0]), cex = 1.5, lwd = 3,
       col = "dimgray")
  points(x_bar[1], x_bar[2], pch = 8, cex = 1.5, col = "dimgray", lwd = 3)
  text(71.7, 165, expression(bar(x)), cex = 2, lwd = 3,
       col = "dimgray")
  lines(x=c(mu0[1]-1.96*(sqrt(Sigma[1,1])/sqrt(20)), 
            mu0[1]+1.96*(sqrt(Sigma[1,1])/sqrt(20))),
        y=c(mu0[2]-1.96*(sqrt(Sigma[2,2])/sqrt(20)), 
            mu0[2]-1.96*(sqrt(Sigma[2,2])/sqrt(20))), 
        lty = 2, col = "darkred", lwd = 3)
  lines(x=c(mu0[1]-1.96*(sqrt(Sigma[1,1])/sqrt(20)), 
            mu0[1]+1.96*(sqrt(Sigma[1,1])/sqrt(20))),
        y=c(mu0[2]+1.96*(sqrt(Sigma[2,2])/sqrt(20)), 
            mu0[2]+1.96*(sqrt(Sigma[2,2])/sqrt(20))), 
        lty = 2, col = "darkred", lwd = 3)
  lines(x=c(mu0[1]-1.96*(sqrt(Sigma[1,1])/sqrt(20)), 
            mu0[1]-1.96*(sqrt(Sigma[1,1])/sqrt(20))),
        y=c(mu0[2]-1.96*(sqrt(Sigma[2,2])/sqrt(20)), 
            mu0[2]+1.96*(sqrt(Sigma[2,2])/sqrt(20))), 
        lty = 2, col = "darkred", lwd = 3)
  lines(x=c(mu0[1]+1.96*(sqrt(Sigma[1,1])/sqrt(20)), 
            mu0[1]+1.96*(sqrt(Sigma[1,1])/sqrt(20))),
        y=c(mu0[2]-1.96*(sqrt(Sigma[2,2])/sqrt(20)), 
            mu0[2]+1.96*(sqrt(Sigma[2,2])/sqrt(20))), 
        lty = 2, col = "darkred", lwd = 3)
  text(67.5, 190, "Mult. Rachaza", cex = 1, lwd = 2,
       col = "dimgray")
  text(67.5, 188, "Univ. Acepta", cex = 1, lwd = 2,
       col = "dimgray")
  arrows(68.3, 178, 67.5, 186, col = "dimgray", code = 2, 
         cex = 3, lwd = 3)
  arrows(71, 159, 68, 186, col = "dimgray", code = 2, 
         cex = 3, lwd = 3)
  text(72.5, 153, "Mult. Acepta", cex = 1, lwd = 2,
       col = "dimgray")
  text(72.5, 151, "Univ. Rechaza", cex = 1, lwd = 2,
       col = "dimgray")
  arrows(72.2, 183, 72.6, 156, col = "dimgray", code = 2, 
         cex = 3, lwd = 3)
  arrows(68.2, 154, 71.7, 152, col = "dimgray", code = 2, 
         cex = 3, lwd = 3)
}





#####################################################
#    Inferencia para \mu con \Sigma desconocida     #
# Comparación de dos poblaciones con misma varianza #
#####################################################
# Vectores de medias:
X1_bar <- c(15.97,15.91,27.19,22.75); X1_bar
X2_bar <- c(12.34,13.91,16.59,21.94); X2_bar
# Matrices insesgadas
S1_in <- matrix(data = c(5.192, 4.545, 6.522, 5.250,
                      4.545, 13.184, 6.760, 6.266,
                      6.522, 6.760, 28.67, 14.468,
                      5.250, 6.266, 14.468, 16.645),
            nrow = 4, ncol = 4, byrow = T)
S2_in <- matrix(data = c(9.136, 7.549, 5.531, 4.151,
                      7.549, 18.60, 5.446, 5.446,
                      5.531, 5.446, 13.55, 13.55,
                      4.151, 5.446, 13.55, 28.00),
             nrow = 4, ncol = 4, byrow = T)
# En cada grupo hay 32 personas:
n1 <- 32
n2 <- 32
# La dimensión es 4
p <- 4
# Calculamos S1, S2 y Sp
S1 <- ((n1-1)/n1)*S1_in; S1
S2 <- ((n2-1)/n2)*S2_in; S2
Sp <- n1*S1 +n2*S2

# Calculamos la estadística T^2:
T2 <- ((n1*n2)/(n1+n2))*(n1+n2-2)*t(X1_bar-X2_bar)%*%solve(Sp)%*%(X1_bar-X2_bar)

# Y debemos comparar con:
q <- p*(n1+n2-2)/(n1+n2-p-1)*qf(p = 0.95, df1 = 4, df2 = n1+n2-4-1, lower.tail = T)

# ¿Rechazamos H0?
T2 > q





############################################
#  Contraste sobre observaciones pareadas  #
############################################
Datos <- matrix(data = c(1, 73, 31, 51, 35,
                         2, 43, 19, 41, 14,
                         3, 47, 22, 43, 19,
                         4, 53, 26, 41, 29,
                         5, 58, 36, 47, 34,
                         6, 47, 30, 32, 26,
                         7, 52, 29, 24, 19,
                         8, 38, 36, 43, 37,
                         9, 61, 34, 53, 24,
                         10, 56, 33, 52, 27,
                         11, 56, 19, 57, 14,
                         12, 34, 19, 44, 19,
                         13, 55, 26, 57, 30,
                         14, 65, 15, 40, 7,
                         15, 75, 18, 68, 13),
                ncol = 5, byrow = T)
colnames(Datos) <- c("Localidad", "X1", "X2", "Y1", "Y2")
Datos

# Calculamos la media y varianza de la muestra D1,...,Dn
Dif1 <- Datos[,2]-Datos[,4]
Dif2 <- Datos[,3]-Datos[,5]
D_bar <- c(mean(Dif1), mean(Dif2))
Sd <- ((15-1)/14)*var(matrix(c(Dif1,Dif2), ncol = 2, byrow = F))

# Calculamos el estadístico T^2:
T2 <- t(D_bar)%*%solve(Sd)%*%D_bar
# Despejamos al estadístico F(p,n-p) = (n-p)/p T^2
Est_F <- (15-2)*T2/2; Est_F

# Cuantil F(p,n-p) con alpha = 0.05
qf(p = 0.05, df1 = 2, df2 = 13, lower.tail = F)

# ¿Rechazo Ho?



##############################################################
#  Ejercicio: Simulaciones para comparar inferencias en \mu  #
##############################################################
# Los parámetros para simular son :
p <- 2
n <- 100
u <- c(1,2)
covMatrix <- matrix(c(1,0.5,0.5,2), ncol = 2) 

# Podemos simular con la librería mvtnorm
# rmvnorm(n=100, mean=u, sigma=covMatrix)
# También podemos simular normales estándar y luego transformar:
set.seed(1)
standardNormal <- matrix(rnorm(n*p),nrow=p)

# vectores y valores propios (para encontrar la matriz raíz)
evec <- eigen(covMatrix,symmetric=TRUE)$vectors
eval <- eigen(covMatrix,symmetric=TRUE)$values

# Matriz Sigma^(1/2)
covMatrix12 <- evec%*%diag(sqrt(eval))%*%t(evec)
covMatrix12

# Esta sería la muestra X1,...Xn;
X <- covMatrix12%*%standardNormal+u
X[,1:8]

# Valores para la prueba de hipótesis:
A <- t(c(2,-1))
a <- 0.2

xBar <- apply(X,1,mean)
xBar

#######################
# Caso Sigma conocida #
#######################
# Estadístico de prueba
Ji <- n*(A%*%xBar-a)*solve(A%*%covMatrix%*%t(A))*(A%*%xBar-a)
Ji
# Cuantil
cuant = qchisq(0.95,1)
cuant

##########################
# Caso Sigma desconocida #
##########################
S <- var(t(X))*((n-1)/n)
# Estadístico de prueba
T2 <- (n-1)*(A%*%xBar-a)*solve(A%*%S%*%t(A))*(A%*%xBar-a)
T2
# Cuantil
cuantil = qf(0.95,1,n-1)
cuantil*(1*(n-1))/(n-1-1+1)

# ¿Qué diferencias hay en las pruebas?





###########################################
#                                         #
#   Pruebas Multivariadas de Normalidad   #
#                                         #
###########################################

# Cargamos las librerias
library(MVN)
library(mvnormtest)

# Usaremos unos datos que ya conocemos:
data(iris)
setosa <- iris[1:50, 1:4]

# La segunda función
mshapiro.test(t(setosa))

# La primer función en general:
mvn(data, mvnTest = c("mardia", "hz", "royston", "dh", "energy"), 
    covariance = TRUE, scale = FALSE, desc = TRUE,
    univariateTest = c("SW", "CVM", "Lillie", "SF", "AD"),
    univariatePlot = c("qq", "histogram", "box", "scatter"),
    multivariatePlot = c("qq", "persp", "contour"), 
    bc = FALSE, bcType = "rounded",
    showOutliers = FALSE, showNewData = FALSE)

# Algunas pruebas:
result <- mvn(data = setosa, mvnTest = "mardia")
result$multivariateNormality

result <- mvn(data = setosa, mvnTest = "royston")
result$multivariateNormality

# Gráfia de cuantiles:
mvn(data = setosa, multivariatePlot = "qq")

# Otros gráficos bivariados:
setosa2 <- iris[1:50, 1:2]
result <- mvn(setosa2, multivariatePlot = "persp")
result <- mvn(setosa2, multivariatePlot = "contour")
