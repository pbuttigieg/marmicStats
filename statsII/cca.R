# This script will be a tutorial on using CCA. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.


### load libraries and datasets
require(vegan)
library(corrgram)
data("varespec")
data("varechem")

### perform and plot CCA varespec ~ varechem
myCca <- cca(varespec ~ ., varechem)
ordiplot(myCca,type="t",scaling=1)
ordiplot(myCca,type="t",scaling=2)

### total inertia
myCca$tot.chi

### proportion of constrained inertia
myCca$CCA$tot.chi/myCca$tot.chi

### unconstrained inertia
myCca$CA$tot.chi

### inertia captured in the CCA axes
screeplot(myCca)

### explore correlation between CCA axis and explanatory variables. 
CCA1 <- myCca$CCA$u[,"CCA1"]
corrgram(cbind(CCA1, varechem), lower.panel = panel.shade, upper.panel = NULL, text.panel = panel.txt)
CCA2 <-myCca$CCA$u[,"CCA2"]
corrgram(cbind(CCA2, varechem), lower.panel = panel.shade, upper.panel = NULL, text.panel = panel.txt)

### proportion of constrainded inertia captured captured 
### in the first two CCA axes
sum(myCca$CCA$eig[1:2])/sum(myCca$CCA$eig)

### test significance of the overall solution 
anova.cca(myCca)

### test significance of the association between each 
### canonical axes and explanatory variables
myCca.anova.axis <- anova.cca(myCca,by="axis")
myCca.anova.axis

### analyze multi linear correlation coefficients
myCca.mlm <- as.mlm(myCca)
myCca.mlm
