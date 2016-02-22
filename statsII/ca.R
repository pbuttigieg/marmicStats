# This script will be a tutorial on using CA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

require(vegan)
data("varespec")
data("varechem")

?varespec
?varechem


# Here, we transform the data using a Hellinger transformation
# does this improve our data set? Is is appropriate?
varespec.hel <- decostand(varespec, "hellinger")

matplot(varespec, type = "l",pch=1, col = 1:dim(varespec)[2])
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
