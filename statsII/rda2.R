# This script will be a tutorial on using RDA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

### load libraries and datasets
library(vegan)
library(psych)
library(ggplot2)
data("varespec")
data("varechem")


dim(varechem)
dim(varespec)

### examine data
boxplot(varespec,las=2,cex.names=0.8)

dpar <- par() 
par(mfrow=c(11,4))
par(mfrow=c(11,4),mar=c(2,1,1,1))
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

#### apply the Hellinger transformation
varespec.he <- decostand(varespec, 'hell')
varechem.sc <- as.data.frame(scale(varechem))

### perform RDA
varespec.rda1 <- rda(varespec.he ~ ., 
                    data = varechem.sc)
varespec.rda2 <- rda(varespec.he ~ N + Humdepth + S + Al, 
                    data = varechem.sc)

varespec.rda1
varespec.rda2


