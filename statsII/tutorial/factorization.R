X <- read.table("~/Google Drive/Doctorado//CoursesAndWorkshops//Machine_Learning/exercises//ex9/Homework9handmadeFeatures4Digits//Homework9handmadeFeatures4Digits/mfeat-pix.txt")

#center the data 
X.c <- apply(X,2,function(x){x-mean(x)})

A <- (t(X.c) %*% X.c) / 24

U <- eigen(A)$vectors
V <- eigen(A)$values

# Au1 = ??u1
round(A%*%U[,1],4) == round(V[1]*U[,1],4)

# manually get PC1
PC1m <- (X.c)%*%U[,1]

# function to get PC1
PCA <- princomp(X,center = T,scale. = F)
PC1f <- PCA$scores[,1]
 
# compare both approaches 
plot(PCA1[,1],PCA2$scores[,1])
round(PCA1[,1],4) == round(PCA2$scores[,1],4)



