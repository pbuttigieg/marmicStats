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

# As the degrees of freedom (or the sample size) of the t-student distribution 
# increases (and the variance decreases), it approximates the standard normal 
# distribution.

############
#2) 
############

set.seed(3)
binom.1K.20.5 <- rbinom(n=1000,size = 20,prob = 0.5)
hist(binom.1K.20.5)

set.seed(3)
binom.1K.50.5 <- rbinom(n=1000,size = 50,prob = 0.5)
hist(binom.1K.50.5,freq = F)

# The normal distribution can be used to approximate the binomial distribution,
# by defining the mean as n*p and the variance as n*p*(1-p), where n is the 
# number of trials and p the probability of success. 

# Do not confuse the number of trials with the sample size, which can also be 
# represented by the letter n.
# Note: the normal distribution is continuous and the binomial is discrete. 
# Note: the normal distribution can be used as an approximation when n*p > 5 and n*p* (1-p) > 5.

# binom.1K.50.5 distribution will be used for the approximation

binmo.1K.50.5.mean <- 50*0.5
binmo.1K.50.5.sd <- sqrt(50*0.5*0.5)

xseq<-seq(0,50,0.1)
norm.approx <-  dnorm(xseq,binmo.1K.50.5.mean,binmo.1K.50.5.sd)

lines(xseq,norm.approx,col="red")

############
#3)
############

# The lambda parameter of the Poisson distribution is both the expected value
# and the variance.

# Note: the normal distribution is continuous and the Poisson is discrete. 
# Note: the normal distribution can be used as an approximation when lambda is 
# bigger than 10.

# Overlay histograms
hist(rpois(1000,12),xlim=c(0,30),ylim=c(0,250),col=rgb(0,0,0.5,0.5))
hist(rnorm(1000,12,sqrt(12)),add=T,col=rgb(0.5,0,0,0.5))

############
#4)
############

# The following command with the pnorme function, will give us the probability 
# of obtaining a number equal of greater than 1.96: P(x >= 1.96).

# If the parameter lower.tail equals TRUE, then it will give us the probability
# of obtaining a number equal or less than 1.96: P(x <= 1.96), or 1 - P(x >= 1.96).
# The value 1.96 is often used in two tail tests based on the normal distribution,
# when the critical value alpha is 0.5. See https://en.wikipedia.org/wiki/1.96 

# Notice that the probability value of the different t-students distributions 
# approaches the probability value of the normal distribution N(0,1), when the 
# degrees of freedom increase.
# As exercise 1, this comparison illustrates why the t-distribution is commonly 
# substituted by the normal distribution when the sample size is greater than 30.

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

# Make sure to write the path to the folder where bioenv-2 is. 
# You can use tab to auto complete and check if the path is correct.

bioenv2 <- read.csv(
           "https://raw.githubusercontent.com/pbuttigieg/marmicStats/master/statsI/bioenv-2.csv",
           header=T,
           sep=",",
           row.names=1)

# inspect that the table is correct

head(bioenv2)

# Calculate the Pearson (parametric) and Spearman (nonparametric) correlations,
# and test if these are significant.
# Remember, it is always important to visualize the variables in a plot, to 
# understand better their relationship.

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

# Notice that the linear regression models the relationship between a dependent
# variable (species abundances) and an explanatory variable (temperature).
# The fitted values represent the explained variability of the dependent 
# variable, by the linear model.
# The residual values represent the unexplained variability of the dependent 
# variable, by the linear model.

# divide the figure in a 2 by 2 array
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

# you can see all the information of the linear model with the summary function.
summary(a_vs_temp.lm)
summary(b_vs_temp.lm)
summary(c_vs_temp.lm)
summary(d_vs_temp.lm)

# To analyze the linear regression assumptions you can directly plot the linear models. 
plot(a_vs_temp.lm)
plot(b_vs_temp.lm)
plot(c_vs_temp.lm)
plot(d_vs_temp.lm)

# Species b would be the ideal case, where the residuals are normally and 
# equally distributed along the fitted values.
# You can find an explanation of these plots in 
# http://www.r-bloggers.com/checking-glm-model-assumptions-in-r/

############
#7)
############

# The total variability can be computed as the variability in the fitted values
# plus the variability in the residual values.

# species a vs temperature linear model.
r.variability <- var(residuals(a_vs_temp.lm))*(150-1)
f.variability <- var(fitted(a_vs_temp.lm))*(150-1)
t.variability <- var(bioenv2$a)*(150-1)

t.variability
r.variability + f.variability

# compute the coefficient of determination (r^2)
# you can find this value in the linear model using the function summary.
f.variability/(r.variability + f.variability)


