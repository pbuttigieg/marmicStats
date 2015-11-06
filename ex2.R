#Exercise 2

#1)
x=seq(-5,5,0.01)
plot(x,dnorm(x,0,1),type="l")
points(x,dt(x,df=100),col="blue",type="l")


#2) 
x=seq(0,20,1)
plot(x,dbinom(x,size = 20,prob = 0.5))
y<-seq(0,50,1)
plot(y,dbinom(y,size = 50,prob = 0.5))


#Normal is continuous and binomial is discrete (n*p > 5 and n*(1-p)), sd = n*p*(1-p)

y<-seq(0,100,1)
plot(y,dbinom(y,size = 100,prob = 0.5))
points(y,dnorm(y,50,sqrt(100*0.5*(1-0.5))),col="red")

#3)

#Normal is continuous and poission is discrete 
#lamnda is both the expected value and the variance
#lamnda has to be bigger than 10

hist(rpois(1000,12))
hist(rnorm(1000,12,12))

#4)
pnorm(1.96,0,1)

pt(1.96,2)
pt(1.96,5)
pt(1.96,10)
pt(1.96,30)
pt(1.96,100)
pt(1.96,1000000)

#5)
MyTableBio<-read.csv("~/Dropbox/Doctorado/CoursesAndWorkshops/MarMic_Statistics/bioenv-2.csv",header=T,sep=",")


# case a vs temperature
#x<-MyTableBio$Temperature
#y <-  4 * x^3 + rnorm(150, 300, 300)
#y<-abs(y)
#x.df<-as.data.frame(round(x,1))
#y.df<-as.data.frame(round(abs(y),0))
#plot(x,y)
#plot(lm(y~x))

plot(MyTableBio$a,MyTableBio$Temperature)

#case b vs temperature


#MyTableBio$Temperature
#x<-MyTableBio$Temperature
#y<- 4*x + 3 +rnorm(150,0,5)
#y.df<-as.data.frame(round(y,0))
#plot(x,y)
#plot(lm(y~x))

plot(MyTableBio$b,MyTableBio$Temperature)

#case c vs temperature

#y<-x
#x<-MyTableBio$Temperature
#quantile(x)
#sum(x<=8.9)
#y[x<=9.3]=rnorm(sum(x<=9.3),10,3)
#y[x>9.3]=rnorm(sum(!x<=9.3),100,10)
#y.df<-as.data.frame(round(y,0))

plot(MyTableBio$Temperature,MyTableBio$c)

#case d vs temperature

#x<-MyTableBio$Temperature
#y <- 5*x + rnorm(150,3,2)
#y.df<-as.data.frame(round(y,0))
#y[x>9.9] <- rnorm(sum(x>9.9),120,4)
#plot(x,y)

plot(MyTableBio$Temperature,MyTableBio$d)

smlc<-lm(MyTableBio$d~MyTableBio$Temperature)
sum(var(cbind(MyTableBio$c,MyTableBio$Temperature)))
var(residuals(smlc))
var(fitted(smlc))

####################

#6)

#### a
plot(MyTableBio$Temperature,MyTableBio$a)
slm.a_vs_t <- lm(MyTableBio$a~MyTableBio$Temperature)
abline(slm.a_vs_t,col="red",lwd=2)

hist(residuals(slm.a_vs_t))
plot(fitted(slm.a_vs_t),residuals(slm.a_vs_t))

nq <- quantile(dnorm(seq(-3.5,3.5,by=0.1),0,1),probs=seq(0,1,by=0.01))
eq <- quantile(residuals(slm.a_vs_t),probs=seq(0,1,by=0.01))
plot(nq,eq)

#### b
plot(MyTableBio$Temperature,MyTableBio$b)
slm.b_vs_t <- lm(MyTableBio$b~MyTableBio$Temperature)
abline(slm.b_vs_t,col="red",lwd=2)

hist(residuals(slm.b_vs_t))
plot(fitted(slm.b_vs_t),residuals(slm.b_vs_t))
text()

nq <- quantile(dnorm(seq(-3.5,3.5,by=0.1),0,1),probs=seq(0,1,by=0.01))
eq <- quantile(residuals(slm.b_vs_t),probs=seq(0,1,by=0.01))
plot(nq,eq)

##### c
plot(MyTableBio$Temperature,MyTableBio$c)
slm.c_vs_t <- lm(MyTableBio$c~MyTableBio$Temperature)
abline(slm.c_vs_t,col="red",lwd=2)

hist(residuals(slm.c_vs_t))
plot(fitted(slm.c_vs_t),residuals(slm.c_vs_t))

nq <- quantile(dnorm(seq(-3.5,3.5,by=0.1),0,1),probs=seq(0,1,by=0.01))
eq <- quantile(residuals(slm.c_vs_t),probs=seq(0,1,by=0.01))
plot(nq,eq)


##### d
plot(MyTableBio$Temperature,MyTableBio$d)
slm.d_vs_t <- lm(MyTableBio$d~MyTableBio$Temperature)
abline(slm.d_vs_t,col="red",lwd=2)

hist(residuals(slm.d_vs_t))
plot(fitted(slm.d_vs_t),residuals(slm.d_vs_t))

nq <- quantile(dnorm(seq(-3.5,3.5,by=0.1),0,1),probs=seq(0,1,by=0.01))
eq <- quantile(residuals(slm.d_vs_t),probs=seq(0,1,by=0.01))
plot(nq,eq)


#7)
slm<-lm(MyTableBio$a~MyTableBio$Temperature)
summary(slm)

r <- var(residuals(slm))
f <- var(fitted(slm))
t <- var(MyTableBio$d)

(f/(r +f))

var(residuals(slm))*(150-1) + var(fitted(slm))*(150-1)

