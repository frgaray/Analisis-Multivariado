##################################################
###          Seminario de Estadística          ###
###         Jueves 23, septiembre 2021         ###
##################################################

#  _____________________________________
# |             Ejercicio:              |
# | Análisis de Componentes Principales |
# |_____________________________________|

# Consideremos los siguientes datos:
# ______________________________
# | Observación |  X1  |   X2   |
#  -----------------------------
# |     1       |  16  |    8   |
# |     2       |  12  |   10   |
# |     3       |  13  |    6   |
# |     4       |  11  |    2   |
# |     5       |  10  |    8   | 
# |     6       |   9  |   -1   |
# |     7       |   8  |    4   |
# |     8       |   7  |    6   |
# |     9       |   5  |   -3   |
# |    10       |   3  |   -1   |  
# |    11       |   2  |   -3   |
# |    12       |   0  |    0   |
#  -----------------------------

X1 <- c(16, 12, 13, 11, 10, 9, 8, 7, 5, 3, 2, 0)
X2 <- c(8, 10, 6, 2, 8, -1, 4, 6, -3, -1, -3, 0)
ggplot(data.frame(X1,X2), aes(x=X1, y=X2, label=1:12)) + 
      xlim(-10, 18) + ylim(-10, 12) +
      geom_point(size=4, shape=16, color="darkblue") +
      geom_text(size=4, hjust=2, vjust=0, color = "blue") +
      geom_hline(yintercept=0, color="gray", size=1.5) +
      geom_vline(xintercept=0, color="gray", size=1.5) +
      ggtitle("Datos originales") + 
      xlab("X1") + ylab("X2") +
      theme(
        plot.title = element_text(color="darkred", size=14, face="bold"),
        axis.title.x = element_text(color="darkblue", size=14, face="bold"),
        axis.title.y = element_text(color="darkblue", size=14, face="bold") )

options(digits = 6)
# La media de los datos es (8,3): 
mean(X1); mean(X2)
# Y la varianza es (23.091, 21.091)
var(X1); var(X2)
# Centramos a los datos:
X1_ <- X1-mean(X1); X2_ <- X2-mean(X2)
# __________________________________________________
# | Observación |   X1   |   X1_  |   X2   |   X2_  |
#  -------------------------------------------------
# |     1       |   16   |    8   |    8   |    5   |
# |     2       |   12   |    4   |   10   |    7   |    
# |     3       |   13   |    5   |    6   |    3   |
# |     4       |   11   |    3   |    2   |   -1   |
# |     5       |   10   |    2   |    8   |    5   | 
# |     6       |    9   |    1   |   -1   |   -4   |
# |     7       |    8   |    9   |    4   |    0   |
# |     8       |    7   |   -1   |    6   |    3   |
# |     9       |    5   |   -3   |   -3   |   -6   |
# |    10       |    3   |   -5   |   -1   |   -4   |  
# |    11       |    2   |   -6   |   -3   |   -6   |
# |    12       |    0   |   -8   |    0   |   -3   |
#  -------------------------------------------------
# |    Media    |    8   |    0   |    3   |    0   |
# |-------------------------------------------------|
# |   Varianza  | 23.091 | 23.092 | 21.091 | 21.091 |
# |-------------------------------------------------|

X <- data.frame(X1_, X2_)
# Las matrices de covarianzas S y de correlaciones muestral R son:
var(X); cor(X)

# Las varianzas de X1 y X2 son 23.091 y 21.091, respectivamente, 
# y la varianza total de las dos variables es 23.091+21.091 = 44.182. 
# Además, que las variables X1 y X2 están correlacionadas, con un 
# coeficiente de correlación de 0.746.
# Los porcentajes de la variabilidad total retenida por X1 y X2 son, 
# respectivamente, 52.26% y 47.74%.

# Sea Y1 un nuevo eje que forma un ángulo a con el eje X1_. La proyección 
# de las observaciones sobre el eje Y1 da las coordenadas de las 
# observaciones con respecto a Y1. Estas coordenadas son una combinación 
# lineal de las coordenadas originales.
# Sabemos:      y1 = cos(a)*x1_ +sen(a)*x2_

# Por ejemplo, para un valor a=10º, la ecuación para la combinación lineal es:
# y1 = 0.985x1_ + .174x2_
#    = 0.985(x1 −8) + 0.174(x2 −3)
#    = −8.402 +0.985x1 +0.174x2

# Las coordenadas o proyecciones de las observaciones sobre Y1 pueden 
# considerarse como los valores y1 de esta nueva variable.
Y1 <- cos(10*pi/180)*X1_ +sin(10*pi/180)*X2_
newdata <- data.frame(X1_ = Y1*cos(10*pi/180),
                      X2_ = Y1*sin(10*pi/180))

ggplot(X, aes(x=X1_, y=X2_, label=1:12)) + 
  xlim(-10, 10) + ylim(-10, 10) +
  geom_point(size=4, shape=16, color="darkblue") +
  geom_text(size=4, hjust=2, vjust=0, color = "blue") +
  geom_hline(yintercept=0, color="gray", size=1.5) +
  geom_vline(xintercept=0, color="gray", size=1.5) +
  geom_segment(aes(x = -10*cos(10*pi/180), y = -10*sin(10*pi/180), 
                   xend = 10*cos(10*pi/180), yend = 10*sin(10*pi/180)),
               linetype="dashed", size = 1) +
  geom_point(data=newdata, color="darkred", size=4, shape=18) +
  geom_text(size=4, hjust=2, vjust=0, color = "blue") +
  geom_segment(aes(x = X1_[1], y = X2_[1], 
                   xend = Y1[1]*cos(10*pi/180), yend = Y1[1]*sin(10*pi/180)),
               linetype="dashed", color="darkred") +
  ggtitle("Datos centrados y Proyectados sobre Y1") + 
  xlab("X1 centrada") + ylab("X2 centrada") +
  annotate(geom="text", x=10, y=1, label="bold(Y1)", color="darkred", 
           size=5, parse = TRUE) +
  theme(
    axis.title.x = element_text(color="darkblue", size=14, face="bold"),
    axis.title.y = element_text(color="darkblue", size=14, face="bold") )

# Tenemos lo siguientes puntajes para la primera componente:
# ______________________________________
# | Variable |   X1_  |   X2_  |   Y1   |
# --------------------------------------
# --------------------------------------
# |   Media  |    0   |    0   |    0   |
# --------------------------------------
# | Varianza | 23.091 | 21.091 | 28.658 |
# --------------------------------------

# De esta tabla se observa que: 
# (1) La nueva variable permanece corregida (con media igual a cero), 
# (2) la varianza de Y1 es 28.659 y retiene el 64.87% (28.659/44.182) 
#     del total de la varianza de los datos. Nótese que la varianza 
#     retenida por Y1 es mayor que la retenida por cualquiera de las 
#     variables originales.

# Podemos hacer variar al valor del ángulo a:

porcent_var_Y1 <- function(x){
  n<-length(x)
  aux <- rep(0,n)
  X1 <- c(16, 12, 13, 11, 10, 9, 8, 7, 5, 3, 2, 0)
  X2 <- c(8, 10, 6, 2, 8, -1, 4, 6, -3, -1, -3, 0)
  X1_ <- X1-mean(X1)
  X2_ <- X2-mean(X2)
  for (k in 1:n) {
    aux[k]<-round(var(cos(x[k]*pi/180)*X1_ +sin(x[k]*pi/180)*X2_),3)
  }
  return(aux*100/44.182)
}
curve(porcent_var_Y1(x), 0, 90, 
      main = "Porcentaje de la varianza total retenida por Y1",
      ylab = "Porcentaje de varianza (%)",
      xlab = "Ángulo a", col = "darkblue", lwd=4, 
      frame.plot=F, ylim=c(40,90), xlim=c(0,100))
grid(10,8)
max<-optimize(porcent_var_Y1, interval=c(40, 50), maximum=TRUE)
a_max <- round(max$maximum,2)
var_max <- round(max$objective,2)
lines(x=c(a_max, a_max), y=c(30,var_max), type="l", 
      lwd=3, col="darkred", lty=2)
lines(x=c(-10,a_max), y=c(var_max,var_max), type="l", 
      lwd=3, col="darkred", lty=2)
text(52.5, 40, paste("a = ", a_max), col = "darkblue", cex=1.2)
text(12, 85, paste("% máximo = ", var_max), col = "darkblue", cex=1)

# La figura permite apreciar que el porcentaje de varianza 
# explicado por Y1 crece en tanto el ángulo a crece, y que 
# después de cierto valor maximo, la varianza reunida por 
# Y1 decrece. De tal forma, que hay un único eje nuevo y 
# es una variable que retiene la máxima cantidad de la 
# variabilidad contenida en los datos. Con la ayuda del gráfico,
# se observa que el valor del ángulo óptimo del eje respecto 
# a X1 es cercano a 43.26º y retiene el 87.31% de la variabilidad
# total de los datos. Por tanto, es posible identificar un segundo
# eje, que corresponda a una segunda nueva variable, tal que 
# reúna el máximo de varianza no retenida por el primer eje Y1. 

# Sea Y2 el nuevo segundo eje, el cual se considera ortogonal a Y1. 
# Así, como el ángulo entre Y1 y X1 es a entonces el ángulo
# entre Y2 y X2 también es a. De manera análoga, la combinación 
# lineal para conformar y2 es:
#                              y2 = −sen(a)*x1_ +cos(a)*x2_
# Para a = 43.261 la ecuación anterior es
# y2 = −sen(43.261)*x1_ +cos(43.261)*x2_
#    = −0.685*x1_ +0.728*x2_
#    = −3.296 −0.685x1 + 0.728x2

g1 <- ggplot(X, aes(x=X1_, y=X2_, label=1:12)) + 
            xlim(-10, 10) + ylim(-10, 10) +
            geom_point(size=4, shape=16, color="darkblue") +
            geom_text(size=4, hjust=2, vjust=0, color = "blue") +
            geom_hline(yintercept=0, color="gray", size=1.5) +
            geom_vline(xintercept=0, color="gray", size=1.5) +
            ggtitle("Datos centrados") +  xlab("X1") + ylab("X2") +
            theme(
              plot.title = element_text(color="darkred", size=14, face="bold"),
              axis.title.x = element_text(color="darkblue", size=14, face="bold"),
              axis.title.y = element_text(color="darkblue", size=14, face="bold") )

Y1 <- cos(a_max*pi/180)*X1_ +sin(a_max*pi/180)*X2_
Y2 <- -sin(a_max*pi/180)*X1_ +cos(a_max*pi/180)*X2_ 
Y <- data.frame(Y1, Y2)
g2 <- ggplot(Y, aes(x=Y1, y=Y2, label=1:12)) + 
            xlim(-10, 10) + ylim(-10, 10) +
            geom_point(size=4, shape=16, color="darkblue") +
            geom_text(size=4, hjust=2, vjust=0, color = "blue") +
            geom_hline(yintercept=0, color="gray", size=1.5) +
            geom_vline(xintercept=0, color="gray", size=1.5) +
            ggtitle("Componentes Principales") +  xlab("Y1") + ylab("Y2") +
            theme(
              plot.title = element_text(color="darkred", size=14, face="bold"),
              axis.title.x = element_text(color="darkblue", size=14, face="bold"),
              axis.title.y = element_text(color="darkblue", size=14, face="bold") )

library(gridExtra)
library(ggpubr)
grid.arrange(g1, g2, ncol=2,
             top = text_grob("Transformación de Datos", 
                             color = "darkred", size = 16, face = "bold"))



# Usando Valores y Vectores Propios:
S <- var(X)
componentes <- eigen(S)
# Los valores propios deben coincidir con las varianzas: (87.31%)
componentes$values
(38.5758/sum(38.5758+5.6060))*100
# Los vectores propios deben coincidir con la matriz de rotación:
vect_prop <- componentes$vectors; vect_prop
# Los componentes principales serán:
Comp_Princ <- as.matrix(X)%*%vect_prop; Comp_Princ
# Deben coincidir con Y:
Y

