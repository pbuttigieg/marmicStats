# This script will be a tutorial on using NMDS This tutorial should 
# be run with an instructor present, but independent development is 
# encouraged.

### load libraries and datasets
library(vegan)
library(scatterplot3d)
data("varespec")

### perform NMDS
myNmds <- metaMDS(comm = varespec,distance ="bray",k = 2,trymax = 20,trace = FALSE )
myNmds$converged
myNmds$stress 
plot(myNmds,type="t")
plot(myNmds,type="t",display = "sites")

myNmds <- metaMDS(comm = varespec,distance ="bray",k = 3,trymax = 20,trace = FALSE )
scatterplot3d(myNmds$points[,1],myNmds$points[,2],myNmds$points[,3],highlight.3d=TRUE)

### perform Shepard plot
stressplot(object = myNmds)

### stress values
stress<-vector()
for (i in 1:4) {
  myNmds <- metaMDS(comm = varespec,distance ="bray",k = i,trymax = 20,trace = FALSE )  
  stress <- append(stress,myNmds$stress)
}

plot(y=stress,c(1,2,3,4),pch=19,xlab="dimensions allowed",ylab="stress")
