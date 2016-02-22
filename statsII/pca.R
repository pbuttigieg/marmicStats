# This script will be a tutorial on using PCA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.


### load libraries and datasets
library(vegan)
library(corrgram)
data(varespec)

### inspect the data
summary(varespec)
head(varespec)
dim(varespec)

# Let's build a correlelogram 
corrgram(
  varespec,
  lower.panel = panel.shade,
  upper.panel = NULL,
  text.panel = panel.txt
  )

### perform PCA 
myPca <- rda(
  varespec, 
  scale=TRUE
  )

plot(myPca)
biplot(myPca, choices = 1:2, scaling=-1)
biplot(myPca, choices = 1-3, scaling=-1)

### total variance
sum(myPca$CA$eig)
barplot(myPca$CA$eig,cex.names = 0.8,las=2)
screeplot(myPca)

### variance captured in the first 3 PCs
(sum(myPca$CA$eig[1:3])/sum(myPca$CA$eig))*100

### variable contributions to the first PC
barplot(myPca$CA$v[,"PC1"],cex.names=0.8,las=2)

