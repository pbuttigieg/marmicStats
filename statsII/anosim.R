# This script will be a tutorial on using ANOSIM. This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

### load libraries and datasets
library(vegan)
samples <- read.csv("https://raw.githubusercontent.com/pbuttigieg/marmicStats/master/statsII/groups.table.csv",header = TRUE)
samples

### performe ANOSIM
samples.anosim <- anosim(dat = samples[,2:4],grouping = samples[,1],permutations = 999,distance = "bray")
summary(samples.anosim)
plot(samples.anosim)
