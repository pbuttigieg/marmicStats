# This script will be a tutorial on using RDA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

### load libraries and datasets
library(vegan)
library(corrgram)
data("varespec")
data("varechem")

dim(varechem)
dim(varespec)

### examine data
boxplot(varespec,las=2,cex.names=0.8)

dpar <- par() 
par(mfrow=c(11,4))
par(mfrow=c(11,4),mar=c(1.5,1,1,1),cex.lab=0.2)
for (i in 1:dim(varespec)[2] ) {
  hist(varespec[,i],main=colnames(varespec)[i])
}

par(mfrow=c(7,2))
for (i in 1:dim(varechem)[2] ) {
  hist(varechem[,i],main=colnames(varespec)[i])
}

par(dpar) 
corrgram(varespec,lower.panel = panel.shade, upper.panel = NULL, text.panel=panel.txt)
corrgram(cbind(varespec,varechem),lower.panel = panel.shade, upper.panel = NULL, text.panel=panel.txt)

### data transformations
varespec.he <- decostand(varespec,"hell")
varespec.log <- decostand(varespec,"log")
varechem.sc <- as.data.frame(scale(varechem))

### perform and RDA
varespec.rda1 <- rda(varespec.he ~ ., 
                    data = varechem.sc)
varespec.rda2 <- rda(varespec.he ~ N + Humdepth + S + Al, 
                    data = varechem.sc)

ordiplot(varespec.rda2,type="t")

hist(residuals(varespec.rda1))
hist(residuals(varespec.rda1)[,1])

### PCA scores
varespec.rda2$CCA



