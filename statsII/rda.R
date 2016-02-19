# This script will be a tutorial on using RDA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

require(vegan)
data(dune)
data("dune.env")

?dune.env
# Important to understand your data!
class(dune.env$Moisture)
class(dune.env$Management)

myRda <- rda(
  dune ~ 
    A1 + Moisture + Management + Use + Manure,
   data = dune.env
  )

str(myRda)

myRda$terminfo

# Some variables are so well correlated that the vegan RDA
# routine removes all but one and uses that as the "alias"
# for the others. Let's find out which ones were aliased...
myRda$CCA$alias



hist(residuals(myRda))

dune.hel <- decostand(dune, method = "hellinger")

myRda <- rda(
  dune.hel ~ 
    A1 + Moisture + Management + Use + Manure,
  data = dune.env
)

hist(residuals(myRda))

ordiplot(myRda, type = "text" )
#identify(scores(myRda, choices = 1:2, display = "species"), labels = )

set.seed(123)
myRda.anova <- anova.cca(myRda)
str(myRda.anova)



