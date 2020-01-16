# This script will be a tutorial on using clustering. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

# see http://cc.oulu.fi/~jarioksa/opetus/metodi/sessio3.pdf for a
# tutorial by Jari Oksanen

### load libraries and datasets
library(vegan)
library(cluster)
require(tidyverse)
data(dune)

### non hierarchical clustering: kmeans
dune.kc <- kmeans(dune,centers=5)
class(dune.kc)
dune.kc$cluster

#test significance of clustering
#use as.factor for vector of clusters otherwise it may be interpreted as ordered
anosim(dune, #data fram of response variables
       as.factor(dune.kc$cluster)) #factor for grouping
adonis(dune ~ as.factor(dune.kc$cluster)) #adonis is PERMANOVA

rda(dune ~ as.factor(dune.kc$cluster)) %>% plot

dune.eu.dist <- dist(dune,method = "euclidean")
clusplot(x=dune.eu.dist,diss=TRUE,clus = dune.kc$cluste,lines=0,labels = 2)



### hierarchical clustering 
dune.eu.dist <- dist(dune,method = "euclidean")
dune.wc <- hclust(dune.eu.dist,method = "ward.D2")
dune.cc <- hclust(dune.eu.dist,method = "complete")

plot(dune.wc)
plot(dune.cc)

rbind(cutree(dune.wc,k=5),cutree(dune.cc,k=5))

### hierarchical clustering based on PCoA first 2 axes
myPca <- rda(dune,scale=TRUE)
dune.eu.dist2 <- dist(myPca$CA$u[,1:2],method = "euclidean")
dune.wc2 <- hclust(dune.eu.dist2,method = "ward.D2")

plot(dune.wc)
plot(dune.wc2)

table(cutree(dune.wc,k=5),cutree(dune.wc2,k=5))


