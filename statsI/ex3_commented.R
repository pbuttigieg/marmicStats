###############################################################
#Practical class 3
###############################################################

############
#1.1)
############
# Make sure to write the path to the folder where bioenv-2 is. 
# You can use tab to auto complete and check if the path is correct.
# You can also use the setwd() function to move to the directory where
# bioenv3 is.

bioenv3 <- read.csv(
  "https://raw.githubusercontent.com/pbuttigieg/marmicStats/master/statsI/bioenv-3.csv",
  header = T,
  row.names = 1
  )

# inspect the table 
head(bioenv3)

# compute the mean and the standard deviation
a.mean <- mean(bioenv3$a)
a.sd <- sd(bioenv3$a)

# manually compute the t statistic and the p-value 
t.stat <- (a.mean - 10 ) / (a.sd / sqrt(length(bioenv3$a)))

# use the probability function to calculate the area under the curve
# from our t statistic value to the end of the lower tail of a
# t distribution with 24 degrees of freedom
pt(
  t.stat,         # our t statistic
  df = 24,        # the degrees of freedom in the t distribution pt() should calculate
  lower.tail = F  # tells pt() to find the area under the curve from our t stat to the
                  # lower tail of the distribution
  )

# visualize the test
plot(
  seq(-4, 4 , by = 0.1),              # use a sequence of numbers from -4 to 4 in steps of 0.1 as the x-axis
  dt(seq(-4, 4, by = 0.1), df = 24),  # use a density plot of the t distribution with 24 degrees of freedom as the y-axis 
  type = "l"                          # make this a line plot
  )
  
abline(v = t.stat) # draw a vertical (v) line on the plot at the value of our calculated t statistic

# do the test using a existing function from R 
t.test(
  bioenv3$a,                # here's our data variable
  mu = 10,                  # the mean to test against is defined as 10
  alternative = "greater"   # the alternative hypothesis is that the empirical mean is greater than the stated mean
  )

############
#1.2)
############

# compute the mean and the standard deviation
b.mean <- mean(bioenv3$b)
b.sd <- sd(bioenv3$b)

# compute the t statistic and the p-value   
t.stat <- (b.mean - 10 )/(b.sd/sqrt(length(bioenv3$b)))
t.stat
pt(t.stat, df=24,lower.tail=F )*2

# do the test using a function from R 
t.test(bioenv3$b,mu=10,alternative="two.sided")

############
#1.3)
############

# compute the t statistic and the p-value     
t.stat <- (a.mean - b.mean)/sqrt( (a.sd^2)/length(bioenv3$a) + (b.sd^2)/length(bioenv3$b)  )
t.stat
pt(t.stat,df=48,lower.tail=T)

# do the test using a function from R 
# how are the degrees of freedom computed?
t.test(bioenv3$a,bioenv3$b,alternative="less")
t.test(bioenv3$a,bioenv3$b,alternative="less", var.equal = TRUE)

############
#2)
############

# statistical power: the probability of accepting the alternative hypothesis (H1) when it is true.

# first install and load the package pwr
install.packages("pwr")
library("pwr")

# test 1.1)
pwr.t.test(n = 25, d=0.4, sig.level = 0.05 ,type="one.sample")

############
#3)
############

# confidence interval: range of values that act as good estimates of the unknown population parameter

set.seed(1)
mynorm.35.3.5 <- rnorm(35,3,5)

m <- mean(mynorm.35.3.5)
s <- sd(mynorm.35.3.5)

s.error = qnorm(0.975)*(s/sqrt(35))

up.lim <- m + s.error
low.lim <- m - s.error

up.lim
low.lim

# double check
x<-seq(0,6,by=0.01)
plot(x,dnorm(x,m,s/sqrt(35)),type="l")
abline(v=up.lim)
abline(v=low.lim)

pnorm(up.lim,m,s/sqrt(35))
pnorm(low.lim,m,s/sqrt(35))

###############
#4)
###############

# ANOVA: analysis of variance 
# If there are more than 2 samples, the ANOVA should be performed instead of 
# pairwise t tests. ANOVA will consider all samples at the same time in order 
# to determine if there is a significant difference. If the ANOVA shows a 
# significant p-value, then pairwise t tests or tukey tests can be performed 
# in order to identify the means that are significantly different.

set.seed(1)
norm.a <- rnorm(50,4,5)
norm.b <- rnorm(50,6,5)
norm.c <- rnorm(50,12,4.5)

groups = factor(rep(c("a","b","c"), each = 50))
mydata <- c(norm.a,norm.b,norm.c)

mydata.df <- data.frame(mydata,groups)

# Boxplots are a convenient way of graphically depicting groups of numerical 
# data through their quartiles. The spacings between the different parts of 
# the box indicate the degree of dispersion (spread) and skewness in the data, 
# and show outliers. 

plot(mydata ~ groups)

# boxplot example
par(mfrow=c(2,1),mar=c(1,1,1,1))
boxplot(norm.a,horizontal = T)
hist(norm.a)

# test homogeneity of variance (assumption of the ANOVA)
bartlett.test(mydata,groups) # the null hypothesis is that the variances are homogeneous

# perform the ANOVA
a.results <- aov(mydata ~ groups,data=mydata.df)
summary(a.results)

# Df = degree of freedom
# Sum Sq = deviance (within groups, and residual)
# Mean Sq = variance (within groups, and residual)
# F value = the value of the Fisher statistic test, computed as (variance within groups) / (variance residual)
# Pr(>F) = p-value

# We clearly reject the null hypothesis of equal means for all three groups. 
# We have to find out which ones are different. 
# One way to do this is applying pairwise t-test 
pairwise.t.test(mydata, groups, p.adjust="bonferroni")

# The Bonferroni correction is based on the idea that if an experimenter is 
# testing m hypotheses, then one way of maintaining the family - wise error 
# rate (FWER) is to test each individual hypothesis at a statistical 
# significance level of 1/m times what it would be if only one hypothesis were
# tested. So, if the desired significance level for the whole family of tests 
# should be (at most) alpha then the Bonferroni correction would test each 
# individual hypothesis at a significance level of alpha/m.

# Other option is applying the function TukeyHSD().
TukeyHSD(a.results, conf.level=0.95)

###############
#5) chi squared and fisher's exact tests
###############

# these are used to test the correspondence between categorical variables
# first the variables have to be converted into a contingency table

bioenv3$Biofilm[bioenv3$Biofilm=="Present"]="present"
bioenv3 <- droplevels(bioenv3)

chisq.test(table(bioenv3$Salinity,bioenv3$Biofilm))
fisher.test(table(bioenv3$Salinity,bioenv3$Biofilm))

###############
#6)
###############


# Test by randomization if the means of two samples differ. 

# First, we generate two samples of size n = 50 from two different Poisson 
# distributions (Pois(3) and Pois(5)) and store their elements in one vector.

p.comb <- c(rpois(40,3),rpois(40,4))

# Our null hypothesis is that there is no difference between the means.
# Let's compute the null hypothesis distribution: 

rand <- vector (mode="numeric",length=10000)

for (i in 1:length(rand)) {                          # Here we are reordering the vector.
  tmp <- sample(p.comb)                              # Now the elements of both samples will have random positions in the vector.
  rand[i] <- mean(tmp[1:40]) - mean(tmp[41:80])     # After the samples are mixed we compute the difference of the means.
}                                                    # By repeating this several times, we can get an estimation of the null hypothesis distribution.

                                              
# Plot the null hypothesis distribution
par(mfrow=c(1,1))
hist(rand)

r.stat <- mean(p.comb[1:40]) - mean(p.comb[41:80])
r.stat
abline(v=r.stat,col="red")



# Let's see how the distribution of H0 and H1 look like 
mean_diff <- vector (mode="numeric",length=10000)

for ( i in 1:length(mean_diff)) {
  set.seed(i)
  p.comb <- c(rpois(40,3),rpois(40,4))
  mean_diff[i] <- mean(p.comb[1:40]) - mean(p.comb[41:80])
}






hist(mean_diff)


hist(rand,
     col=rgb(0,0,0.5,0.5), 
     main="H0 and H1 distributions",
     xlab = "mean difference",
     xlim=c(min(mean_diff),max(rand))
     )

hist(mean_diff, 
     add=T,
     col=rgb(0.5,0,0,0.5)
     )


# See where the real difference falls under the null hypothesis
r.stat <- mean(p.comb[1:40]) - mean(p.comb[41:80])
r.stat
abline(v=r.stat,col="red")
