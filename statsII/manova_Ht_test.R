# This script will be a tutorial on using MANOVA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

### load libraries and datasets
library(vegan)
library(stats)
library(ICS)
library(Hotelling)

### generate random
clay.sp1 <- rnorm(40,mean=1,sd=3)
clay.sp2 <- rnorm(40,mean=4,sd=3)
clay.sp3 <- rnorm(40,mean=4,sd=3)

water.sp1 <- rnorm(40,mean=5,sd=3)
water.sp2 <- rnorm(40,mean=1,sd=3)
water.sp3 <- rnorm(40,mean=1,sd=3)

sand.sp1 <- rnorm(40,mean=5,sd=3)
sand.sp2 <- rnorm(40,mean=1,sd=3)
sand.sp3 <- rnorm(40,mean=1,sd=3)

clay<-cbind(clay.sp1,clay.sp2,clay.sp3)
water<-cbind(water.sp1,water.sp2,water.sp3)
sand<-cbind(sand.sp1,sand.sp2,sand.sp3)



D<-rbind(clay,water,sand)


materials<-c(rep("clay",40),rep("water",40),rep("sand",40))

DS <- data.frame(materials,D)

write.table(DS,file="~/workspace/marmicStats/statsII/manova.table")

### check variable distribution
shapiro.test(D[materials=="clay",1])
shapiro.test(D[materials=="clay",2])
shapiro.test(D[materials=="clay",3])

mvnorm.kur.test(D[materials=="clay",])
mvnorm.kur.test(D[materials=="water",])
mvnorm.kur.test(D[materials=="sand",])


#### check homogeneity of variances
bartlett.test(D[,1],g=materials)
bartlett.test(D[,2],g=materials)
bartlett.test(D[,3],g=materials)

#### perform MANOVA
fit <- manova(D~ materials)
summary(fit,test = "Wilks")

### Hotelling t squared test
htest <- hotelling.test(D[materials=="water",],D[materials=="sand",])
htest <- hotelling.test(D[materials=="water",],D[materials=="clay",])

### ANOSIM


