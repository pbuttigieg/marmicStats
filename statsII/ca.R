# This script will be a tutorial on using PCA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

require(vegan)
data("varespec")
data("varechem")

?varespec
?varecehm

#TODO: plot all of varespec's species along gradiens in
# varechem to see if there are unimodal distributions

varespec.hel <- decostand(varespec, "hellinger")

matplot(varespec.hel, type = "l",pch=1, col = 1:dim(varespec)[2])

myCa <- cca(varespec)

# find out more about the object
?cca.object


#We can find out the total chi squared value of the data
myCa$CA$tot.chi
#[1] 1.07848


ordiplot(myCa)


myCa <- cca(varespec.hel)
ordiplot(myCa)

# What kind of distance is conserved betwen points?
