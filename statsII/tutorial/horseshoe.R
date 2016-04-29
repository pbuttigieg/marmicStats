library(vegan)
library(rgl)




sp1=-seq(-10,10,0.20)^2 + 150 + rnorm(101,mean=0,sd=10)
sp2=-seq(-10,10,0.20)^2 + 150 + rnorm(101,mean=0,sd=10)  + 50
sp3=seq(0,300,3) + rnorm(101,mean=0,sd=10) 

plot(sp1,col=rgb(0,0,0.5,0.5),main = "",xlab = "environmental gradient",ylab="abundance",pch=19,ylim=c(0,200))
points(sp2,add=T,col=rgb(0.5,0,0,0.5),pch=19)
points(sp3,add=T,col=rgb(0.5,0.8,0,0.5),pch=19)


plot3d(sp1,sp2,sp3,col="red", size=3)


plot(sp1,sp2)
plot(sp2,sp3)


X <- abs(cbind(sp1,sp2,sp3))

Xh <- decostand(X,method="hellinger")
Xt <- decostand(X,method="total")


plot(X[,1],X[,3])
plot(Xh[,1],Xh[,3])
plot(Xt[,1],Xt[,3])


