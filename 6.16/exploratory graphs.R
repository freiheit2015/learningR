fileUrl <- "https://raw.githubusercontent.com/jtleek/modules/master/04_ExploratoryAnalysis/exploratoryGraphs/data/avgpm25.csv"
download.file(fileUrl, destfile = "./data/avgpm25.csv")

pollution<-read.csv("data/avgpm25.csv",colClasses=c("numeric","character","factor","numeric","numeric"))
head(pollution)

summary(pollution$pm25)

boxplot(pollution$pm25,col="blue")

hist(pollution$pm25,col="green")
rug(pollution$pm25)

hist(pollution$pm25,col="green",breaks=100)
rug(pollution$pm25)

boxplot(pollution$pm25,col="blue")
abline(h=12)

hist(pollution$pm25,col="green")
abline(v=12,lwd=2)
abline(v=median(pollution$pm25),col="magenta",lwd=4)

barplot(table(pollution$region),col="wheat",main="Number of Countries in Each Region")

boxplot(pm25~region, data=pollution,col="red")

par(mfrow=c(2,1),mar=c(4,4,2,1))
hist(subset(pollution,region=="east")$pm25,col="green")
hist(subset(pollution,region=="west")$pm25,col="green")

with(pollution,plot(latitude,pm25))
abline(h=12,lwd=2,lty=2)

with(pollution,plot(latitude,pm25,col=region))

par(mfrow=c(1,2),mar=c(5,4,2,1))
with(subset(pollution,region=="west"),plot(latitude,pm25,main="West"))
with(subset(pollution,region=="east"),plot(latitude,pm25,main="East"))

library(datasets)
data(cars)
with(cars,plot(speed,dist))

#the lattice system
library(lattice)
state<-data.frame(state.x77,region=state.region)
xyplot(Life.Exp~Income|region,data=state,layout=c(4,1))
#ggplot2 system
library(ggplot2)
data(mpg)
qplot(displ,hwy,data=mpg)


library(datasets)
hist(airquality$Ozone) ##Draw a new plot

library(datasets)
with(airquality,plot(Wind,Ozone))

library(datasets)
airqualty<-transform(airquality,Month=factor(Month))
boxplot(Ozone~Month, airquality,xlab="Month",ylab="Ozone(ppb)")

library(datasets)
with(airquality,plot(Wind,Ozone))
title(main="Ozone and Wind in New York City")  #Add a title

with(airquality,plot(Wind, Ozone,main="Ozone and Wind in New York City"))
with(subset(airquality,Month==5),points(Wind,Ozone,col="blue"))

with(airquality,plot(Wind, Ozone, main="Ozone and Wind in New York City",type="n"))
with(subset(airquality,Month==5),points(Wind,Ozone,col="blue"))
with(subset(airquality,Month!=5),points(Wind,Ozone,col="red"))
legend("topright",pch=1,col=c("blue","red"),legend=c("May","Other Months"))

#base plot with regression line
with(airquality, plot(Wind, Ozone,main="Ozone and Wind in New York City",pch=20))
model<- lm(Ozone~Wind, airquality)
abline(model,lwd=2)

#multiple base plots
par(mfrow=c(1,2))
with(airquality,{
  plot(Wind,Ozone,main ="Ozone and Wind")
  plot(Solar.R,Ozone,main="Ozone and Solar Radiation")
})

par(mfrow=c(1,3),mar=c(4,4,2,1),oma=c(0,0,2,0))
with(airquality,{
  plot(Wind,Ozone,main="Ozone and Wind")
  plot(Solar.R,Ozone,main="Ozone and Solar Radiation")
  plot(Temp,Ozone,main="Ozone and Temperature")
  mtext("Ozone and Weather in New York City",outer=TRUE)
})

pdf(file="myplot.pdf")
with(faithful,plot(eruptions,waiting))
title(main="Old Faithful Geyser data")
dev.off()

library(datasets)
with(faithful,plot(eruptions,waiting))
title(main="Old Faithful Geyser data")
dev.copy(png,file="geyserplot.png")
dev.off()
#lattice plotting system
library(lattice)
library(datasets)
xyplot(Ozone~Wind,data=airquality)

library(datasets)
library(lattice)
airquality<-transform(airquality,Month=factor(Month))
xyplot(Ozone~Wind|Month,data=airquality,layout=c(5,1))

set.seed(10)
x<-rnorm(100)
f<-rep(0:1,each=50)
y<-x+f-f*x+rnorm(100,sd=0.5)
f<- factor(f,labels=c("Group 1","Group 2"))
xyplot(y~x|f,layout=c(2,1))

xyplot(y~x|f,panel=function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(h=median(y),lty=2)
})

xyplot(y~x|f,panel=function(x,y,...){
  panel.xyplot(x,y,...)
  panel.lmline(x,y,col=2)
})

#ggolot2
library(ggplot2)
str(mpg)
qplot(displ,hwy,data=mpg)
qplot(displ,hwy,data=mpg,color=drv)

qplot(displ,hwy,data=mpg,geom=c("point","smooth"))

qplot(hwy,data=mpg,fill=drv)
qplot(displ,hwy,data=mpg,facets=.~drv)
qplot(hwy,data=mpg,facets=drv~.,binwidth=2)

testdat<-data.frame(x=1:100,y=rnorm(100))
testdat[50,2]<-100
plot(testdat$x,testdat$y,type="l",ylim=c(-3,3))

g<-ggplot(testdat,aes(x=x,y=y))
g+geom_line()
g+geom_line()+ylim(-3,3)
g+geom_line()+coord_cartesian(ylim=c(-3,3))

cutpoints<-quantile(maacs$logno2_new,seq(0,1,length=4),na.rm=TRUE)
maacs$dec<-cut(maacs$logno2_new,cutpoints)
levels(maacs$no2dec)

#hierarchical clustering
set.seed(1234)
par(mar=c(0,0,0,0))
x<-rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y<-rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))
#distance metrix
dataFrame<-data.frame(x=x,y=y)
dist(dataFrame)
#cluster dentrogram
dataFrame<-data.frame(x=x,y=y)
distxy<-dist(dataFrame)
hClustering<-hclust(distxy)
plot(hClustering)

myplclust<-function(hclust,lab=hclust$labels,lab.col=rep(1,length(hclust$labels)),
    hang=0.1,...){
    y<-rep(hclust$height,2)
    x<-as.numeric(hclust$merge)
    y<-y[which(x<0)]
    x<-x[which(x<0)]
    x<-abs(x)
    y<-y[order(x)]
    x<-x[order(x)]
    plot(hclust,labels=FALSE,hang=hang,...)
    text(x=x,y=y[hclust$order]-(max(hclust$height)*hang),labels=lab[hclust$order],col=lab.col[hclust$order],srt=90,adj=c(1,0.5),xpd=NA,...)
  }

dataFrame<- data.frame(x=x,y=y)
distxy<-dist(dataFrame)
hClustering<-hclust(distxy)
myplclust(hClustering,lab=rep(1:3,each=4),lab.col=rep(1:3,each=4))

dataFrame<-data.frame(x=x,y=y)
set.seed(143)
dataMatrix<-as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix)

#K-means Clustering
set.seed(1234)
par(mar=c(0,0,0,0))
x<-rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y<-rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
dataFrame<-data.frame(x,y)
kmeansObj<- kmeans(dataFrame,centers=3)
names(kmeansObj)

par(mar=rep(0.2,4))
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)

set.seed(1234)
dataMatrix<-as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2<-kmeans(dataMatrix,centers=3)
par(mfrow=c(1,2),mar=c(2,4,0.1,0.1))
image(t(dataMatrix)[,nrow(dataMatrix):1],yaxt="n")
image(t(dataMatrix)[,order(kmeansObj$cluster)],yaxt="n")

set.seed(12345)
par(mar=rep(0.2,4))
dataMatrix<-matrix(rnorm(400),nrow=40)
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
par(mar=rep(0.2,4))
heatmap((dataMatrix))


set.seed(678910)
for(i in 1:40){
  coinFlip<-rbinom(1,size=1,prob=0.5)
  if(coinFlip){
    dataMatrix[i,]<-dataMatrix[i, ]+rep(c(0,3),each=5)
  }
}
par(mar=rep(0.2,4))
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
#patterns in rows and columns
hh<-hclust(dist(dataMatrix))
dataMatrixOrdered<-dataMatrix[hh$order,]
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1,,xlab="Row Mean",ylab="Row",pch=19)
plot(colMeans(dataMatrixOrdered),xlab="Column",ylab="Column Mean",pch=19)

svd1<-svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1,,xlab="Row",ylab="First left singular vector",pch=19)
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19)

par(mfrow=c(2,1))
plot(svd1$d,xlab="Column",ylab="Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)

set.seed(678910)
for(i in 1:40){
  coinFlip1<- rbinom(1,size=1,prob=0.5)
  coinFlip2<- rbinom(1,size=1,prob=0.5)
  if(coinFlip1){
    dataMatrix[i,]<-dataMatrix[i,]+rep(c(0,5),each=5)
  }
  if(coinFlip2){
    dataMatrix[i,]<-dataMatrix[i,]+rep(c(0,5),5)
  }
}
hh<-hclust(dist(dataMatrix))
dataMatrixOrdered<-dataMatrix[hh$order,]

svd2<-svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rep(c(0,1),each=5),pch=19,xlab="Column",ylab="Pattern 1")
plot(rep(c(0,1),5),pch=19,xlab="Column",ylab="Pattern 2")

library(impute)
dataMatrix2<-dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=FALSE)]<-NA
dataMatrix2<-impute.knn(dataMatrix2)$data
svd1<-svd(scale(dataMatrixOrdered));svd2<-svd(scale(dataMatrix2))
par(mfrow=c(1,2));plot(svd1$v[,1],pch=19);plot(svd2$v[,1],pch=19)

load("data/face.rda")
image(t(faceData)[,nrow(faceData):1])
svd1<-svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2),pch=19,xlab="Singular vector",ylab="Variance explained")

svd1<-svd(scale(faceData))
approx1<-svd1$u[,1]%*%t(svd1$v[,1])*svd1$d[1]
approx5<-svd1$u[,1:5]%*%diag(svd1$d[1:5])%*%t(svd1$v[,1:5])
approx10<-svd1$u[,1:10]%*%diag(svd1$d[1:10])%*%t(svd1$v[,1:10])

par(mfrow=c(1,4))
image(t(approx1)[,nrow(approx1):1],main="(a)")
image(t(approx5)[,nrow(approx5):1],main="(b)")
image(t(approx10)[,nrow(approx10):1],main="(c)")
image(t(faceData)[,nrow(faceData):1],main="(d)")

library(RColorBrewer)
cols<-brewer.pal(3,"BuGn")
cols
pal<-colorRampPalette(cols)
image(volcano,col=pal(20))

x<-rnorm(10000)
y<-rnorm(10000)
smoothScatter(x,y)
plot(x,y,col=rgb(0,0,0,0.2),pch=19)