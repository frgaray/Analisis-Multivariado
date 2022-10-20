
data<-read.csv("T912.csv",header=TRUE)
data2<-scale(data)
mod0<-princomp(data2)
mod1<-principal(data2,nfactors = 2)
mod2<-factanal(data2, 2)

mod3<-factanal(data2, 3, rotation="varimax", scores="regression")
mod4<-principal(data2, nfactors=3, rotate="varimax", scores = TRUE)

