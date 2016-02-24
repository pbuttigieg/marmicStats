# This script will be a tutorial on using db-RDA This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

### load libraries and datasets
library(vegan)
trees <- read.csv("https://raw.githubusercontent.com/pbuttigieg/marmicStats/master/statsII/trees.csv",header=T)

#### prepare the data
row.names(trees)<-trees$ECOSYS
spec<-trees[,11:23]
env<- trees[,3:10]

### rows with only zeros cannot be handled by capscale
spec = spec + 0.001

### select a dissimilarity index
rankindex(env,spec,indices = c("euc","bra"), method = "spearman")

### perform dbRDA
mydbRDA <- capscale(spec ~ var1 + var2 + var3 + var4, env, dist="bray") 

ordiplot(mydbRDA,type="t",scaling=2)
ordiplot(mydbRDA,type="t",scaling=1)

### check constrained and unconstrained variance
mydbRDA$CCA$tot.chi/mydbRDA$tot.chi

### permutation tests to access significance of constraints
anova(mydbRDA) ## overall test of the significance 
anova(mydbRDA, by="terms", perm.max=999)

### correlation of the variables to the first two dbRDA axes
dbRDA.species.scores=scores(mydbRDA,choices =1:2,display = "sites")
cor(cbind(dbRDA.species.scores,env[,1:4]))


