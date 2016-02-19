# This script will be a tutorial on using PCA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

require(vegan)
data(dune)


myPca <- rda(dune)
ordiplot(myPca)
