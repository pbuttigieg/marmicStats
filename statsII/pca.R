# This script will be a tutorial on using PCA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.


### load libraries and datasets
# you may not be able to load the latest version due to having an older
# version of R installed. Either update R or download a version of vegan
# that is compatible with an older version of R from the pacakge archives.
# you can use install.packages using a path to tell R where to find your 
# tar.gz file and setting repos to NULL
#   install.packages("~/myPackage.tar.gz", repos = NULL)
library(vegan)
library(corrgram)
data(varespec)

### inspect the data
?varespec
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
# you'll be using vegan's rda() function, check out ?rda
# for more details and to find out about the various arguments you can use
# It's also worth following the vegan tutorial written by Jari Oksanen. 
myPca <- rda(
  varespec, 
  scale = TRUE
  )

plot(myPca)
# check out ?biplot to understand the arguments
# not all packages treat scaling in the same way!
biplot(myPca, choices = 1:2, scaling=-1)
biplot(myPca, choices = c(1,3), scaling=-1)

### total variance
# We're going into the PCA object created by vegan using the "$" operator
# use str() to find out what's inside a PCA object.
# we're now looking at the sum of the eigenvalues
sum(myPca$CA$eig)

# we can plot them to find out which PCs capture the most variation
barplot(myPca$CA$eig,cex.names = 0.8,las=2)

# we can do the same with a screeplot...
screeplot(myPca)

### variance captured in the first 3 PCs
(sum(myPca$CA$eig[1:3])/sum(myPca$CA$eig))*100

### variable contributions to the first PC
barplot(myPca$CA$v[,"PC1"],cex.names=0.8,las=2)

