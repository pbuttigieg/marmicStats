require(vegan)
require(pipeR)

palClust <- pipeline(
  dist(scotchData_pal),
  hclust(method = "average")
)

noseClust <- pipeline(
  dist(scotchData_nose),
  hclust(method = "average")
)

plot(noseClust)

cutree(noseClust, k = 3)
cutree(noseClust, h = 1.3)


# The cophenetic function in stats package generates the cophenetic dissimilarity matrix of an hclust object.
# The correlation fuction cor can then be used to find out if two dendrograms correlate
cor(
  cophenetic(noseClust),
  cophenetic(palClust)
)

# or used a package for dendrograms...
# install.packages("dendextend")
require(dendextend)
cor_cophenetic(noseClust, palClust)
