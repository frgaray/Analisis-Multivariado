library(cluster)
library(mclust)

data("faithful")
plot(faithful)

clas1<-hclust(dist(faithful),method="single")
plot(clas1)
plot(faithful,type="n")
sel1<-cutree(clas1,k=4)
points(faithful[sel1==1,1],faithful[sel1==1,2],col="red")
points(faithful[sel1==2,1],faithful[sel1==2,2],col="green")
points(faithful[sel1==3,1],faithful[sel1==3,2],col="blue")
points(faithful[sel1==4,1],faithful[sel1==4,2],col="black")
si1<-silhouette(sel1,dist(faithful))
plot(si1)

clas2<-hclust(dist(faithful),method="complete")
plot(clas2)
plot(faithful,type="n")
sel2<-cutree(clas2,k=2)
points(faithful[sel2==1,1],faithful[sel2==1,2],col="red")
points(faithful[sel2==2,1],faithful[sel2==2,2],col="green")
si2<-silhouette(sel2,dist(faithful))
plot(si2)


faithS<-var(faithful)
MD<-matrix(0,272,272)
for(i in 1:271){
  for(j in 1:272)
  {MD[i,j]<-mahalanobis(as.matrix(faithful[i,]),as.matrix(faithful[j,]),faithS)}
}


MD1<-as.dist(MD)


clas3<-hclust(MD1,method="average")
plot(clas3)
plot(faithful,type="n")
sel3<-cutree(clas3,k=2)
points(faithful[sel3==1,1],faithful[sel3==1,2],col="red")
points(faithful[sel3==2,1],faithful[sel3==2,2],col="green")
si3<-silhouette(sel3,MD1)
plot(si3)



clas4<-diana(faithful,metric="euclidean")
plot(clas4)
plot(faithful,type="n")
sel4<-cutree(clas4,k=2)
points(faithful[sel4==1,1],faithful[sel4==1,2],col="red")
points(faithful[sel4==2,1],faithful[sel4==2,2],col="green")
si4<-silhouette(sel4,dist(faithful))
plot(si4)

adjustedRandIndex(sel3,sel4)



clas5<-kmeans(faithful,2)
plot(faithful,type="n")
points(faithful[clas5$cluster==1,1],faithful[clas5$cluster==1,2],col="red")
points(faithful[clas5$cluster==2,1],faithful[clas5$cluster==2,2],col="green")
si5<-silhouette(clas5$cluster,dist(faithful))
plot(si5)




clas6<-Mclust(faithful)
plot(clas6)
points(faithful[clas6$classification==1,1],faithful[clas6$classification==1,2],col="red")
points(faithful[clas6$classification==2,1],faithful[clas6$classification==2,2],col="green")
sel6<-clas6$classification
for(i in 1:272) {if(sel6[i]==3) sel6[i]=1}
si6<-silhouette(sel6,dist(faithful))
plot(si6)

adjustedRandIndex(sel3,sel6)
adjustedRandIndex(clas5$cluster,sel6)

