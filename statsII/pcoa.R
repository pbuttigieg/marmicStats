# This script will be a tutorial on using PCoA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

### load libraries and datasets
library(vegan)
library(corrgram)
data(dune)

### compute distance matrix based on Bray Curtis distances
dune.bc.dist <- vegdist(dune,method = "bray")

### perform the PCoA
myPcoa <- cmdscale(dune.bc.dist,k=2,eig = TRUE)

x<-myPcoa$points[,1]
y<-myPcoa$points[,2]

plot(x, y, pch = 19, xlim=range(x) + c(-0.1,0.1), ylim=range(y)+c(-0.1,0.1))
text(x, y, pos = 4, labels = rownames(dune), cex = 0.8)

### see variation captured in the PCoA axes
barplot(myPcoa$eig)

### transform the distances
myPcoa2 <- cmdscale(sqrt(dune.bc.dist),k=2,eig = TRUE)
x<-myPcoa2$points[,1]
y<-myPcoa2$points[,2]

plot(x, y, pch = 19, xlim=range(x) + c(-0.1,0.1), ylim=range(y)+c(-0.1,0.1))
text(x, y, pos = 4, labels = rownames(dune), cex = 0.8)
barplot(myPcoa2$eig)



