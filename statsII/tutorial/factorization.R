# load the table mfeat-pix.txt (be aware that you might have to change the path).
X <- read.table("mfeat-pix.txt") 

#center the data 
X.c <- apply(X,2,function(x){x-mean(x)})

A <- (t(X.c) %*% X.c) / 24

U <- eigen(A)$vectors
V <- eigen(A)$values

# A*u1 = lambda*u1 (equation from slide 3)
round(A%*%U[,1],4) == round(V[1]*U[,1],4)

# manually compute PC1
PC1m <- (X.c)%*%U[,1]

# function to compute PC1
PCA <- princomp(X,center = T,scale. = F)
PC1f <- PCA$scores[,1]
 
# compare both approaches 
plot(PCA1[,1],PCA2$scores[,1])
round(PCA1[,1],4) == round(PCA2$scores[,1],4)



