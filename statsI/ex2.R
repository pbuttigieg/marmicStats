###############################################################
#Practical class 2
###############################################################

############
#1)
############

# plot the density function of N(0,1)
xseq <- seq(-5,5,0.01)
xdnorm.0.1 <- dnorm(xseq,0,1)
plot(xseq,xdnorm.0.1,type="l",lwd=2.5)

# add the density function of T(2) in the plot
xdt.2 <- dt(xseq,df=2)
points(xseq,xdt.2,col="blue",type="l")

# add the density function of T(5) in the plot
xdt.5 <- dt(xseq,df=5)
points(xseq,xdt.5,col="blue",type="l")

# add the density function of T(5) in the plot
xdt.10 <- dt(xseq,df=10)
points(xseq,xdt.10,col="blue",type="l")

# add the density function of T(5) in the plot
xdt.30 <- dt(xseq,df=30)
points(xseq,xdt.30,col="blue",type="l")

# add the density function of T(5) in the plot
xdt.100 <- dt(xseq,df=100)
points(xseq,xdt.100,col="blue",type="l")

############
#2) 
############

set.seed(3)
binom.1K.20.5 <- rbinom(n=1000,size = 20,prob = 0.5)
hist(binom.1K.20.5)

set.seed(3)
binom.1K.50.5 <- rbinom(n=1000,size = 50,prob = 0.5)
hist(binom.1K.50.5,freq = F)

# binom.1K.50.5 will be used for the approximation
binmo.1K.50.5.mean <- 50*0.5
binmo.1K.50.5.sd <- sqrt(50*0.5*0.5)

xseq<-seq(0,50,0.1)
norm.approx <-  dnorm(xseq,binmo.1K.50.5.mean,binmo.1K.50.5.sd)

lines(xseq,norm.approx,col="red")

############
#3)
############

# Overlay histograms
hist(rpois(1000,12),xlim=c(0,30),ylim=c(0,250),col=rgb(0,0,0.5,0.5))
hist(rnorm(1000,12,sqrt(12)),add=T,col=rgb(0.5,0,0,0.5))

############
#4)
############

pnorm(1.96,0,1,lower.tail=T)

pt(1.96,2)
pt(1.96,5)
pt(1.96,10)
pt(1.96,30)
pt(1.96,100)
pt(1.96,1000000)

############
#5)
############

# Load bioenv-2

bioenv2 <- read.csv(
  "https://raw.githubusercontent.com/pbuttigieg/marmicStats/master/statsI/bioenv-2.csv",
  header=T,
  sep=",",
  row.names=1)

# inspect that the table is correct
head(bioenv2)

# species a vs temperature
cor(bioenv2$a,bioenv2$Temperature, method="pearson")
cor(bioenv2$a,bioenv2$Temperature, method="spearman")

cor.test(bioenv2$a,bioenv2$Temperature, method="pearson")
cor.test(bioenv2$a,bioenv2$Temperature, method="spearman")

plot(bioenv2$Temperature,bioenv2$a)

# species b vs temperature
cor(bioenv2$b,bioenv2$Temperature, method="pearson")
cor(bioenv2$b,bioenv2$Temperature, method="spearman")

cor.test(bioenv2$b,bioenv2$Temperature, method="pearson")
cor.test(bioenv2$b,bioenv2$Temperature, method="spearman")

plot(bioenv2$Temperature,bioenv2$b)

# species c vs temperature
cor(bioenv2$c,bioenv2$Temperature, method="pearson")
cor(bioenv2$c,bioenv2$Temperature, method="spearman")

cor.test(bioenv2$c,bioenv2$Temperature, method="pearson")
cor.test(bioenv2$c,bioenv2$Temperature, method="spearman")

plot(bioenv2$Temperature,bioenv2$c)

# species d vs temperature
cor(bioenv2$d,bioenv2$Temperature, method="pearson")
cor(bioenv2$d,bioenv2$Temperature, method="spearman")

cor.test(bioenv2$d,bioenv2$Temperature, method="pearson")
cor.test(bioenv2$d,bioenv2$Temperature, method="spearman")

plot(bioenv2$Temperature,bioenv2$d)


############
#6)
############

# divide the figure in a two by two array
par(mfrow=c(2,2))

# plot and create the linear models of species a, b, c and d vs temperature.

# species a vs temperature
plot(bioenv2$Temperature,bioenv2$a)
a_vs_temp.lm <- lm(bioenv2$a~bioenv2$Temperature)
abline(a_vs_temp.lm,lwd=2,col="blue")

# species b vs temperature
plot(bioenv2$Temperature,bioenv2$b)
b_vs_temp.lm <- lm(bioenv2$b~bioenv2$Temperature)
abline(b_vs_temp.lm,lwd=2,col="blue")

# species c vs temperature
plot(bioenv2$Temperature,bioenv2$c)
c_vs_temp.lm <- lm(bioenv2$c~bioenv2$Temperature)
abline(c_vs_temp.lm,lwd=2,col="blue")

# species d vs temperature
plot(bioenv2$Temperature,bioenv2$d)
d_vs_temp.lm <- lm(bioenv2$d~bioenv2$Temperature)
abline(d_vs_temp.lm,lwd=2,col="blue")

# you can see all the information of the linear models using the summary function.
summary(a_vs_temp.lm)
summary(b_vs_temp.lm)
summary(c_vs_temp.lm)
summary(d_vs_temp.lm)

# To analyze the linear regression assumptions you can directly plot the linear models. 
plot(a_vs_temp.lm)
plot(b_vs_temp.lm)
plot(c_vs_temp.lm)
plot(d_vs_temp.lm)

############
#7)
############

# The total variability can be computed as the variability of the fitted values
# plus the variability of the residual values.

# species a vs temperature linear model.
r.variability <- var(residuals(a_vs_temp.lm))*(150-1)
f.variability <- var(fitted(a_vs_temp.lm))*(150-1)
t.variability <- var(bioenv2$a)*(150-1)

t.variability
r.variability + f.variability

# compute the coefficient of determination (r^2)
# you can find this value in the linear model using the function summary.
f.variability/(r.variability + f.variability)


