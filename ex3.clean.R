############
#1.1)
############

bioenv3=read.csv("bioenv-3.csv",header=T,row.names=1)


a.mean <- mean(bioenv3$a)
a.sd <- sd(bioenv3$a)

t.stat <- (a.mean - 10 )/(a.sd/sqrt(length(bioenv3$a)))
t.stat
pt(t.stat, df=24,lower.tail=F )

# visualize the test
plot(seq(-4,4,by=0.1),dt(seq(-4,4,by=0.1),df=24),type="l")
abline(v=t.stat)


# do the test with the R function 
t.test(bioenv3$a,mu=10,alternative="greater")

############
#1.2)
############

b.mean <- mean(bioenv3$b)
b.sd <- sd(bioenv3$b)

t.stat <- (b.mean - 10 )/(b.sd/sqrt(length(bioenv3$b)))
t.stat

pt(t.stat, df=24,lower.tail=F )*2

# do the test with the R function 
t.test(bioenv3$b,mu=10,alternative="two.sided")

############
#1.3)
############

t.stat <- (a.mean - b.mean)/sqrt( (a.sd^2)/length(bioenv3$a) + (b.sd^2)/length(bioenv3$b)  )
t.stat

pt(t.stat,df=48,lower.tail=T)

# do the test with the R function 
# how are the degrees of freedom computed?
t.test(bioenv3$a,bioenv3$b,alternative="less")
t.test(bioenv3$a,bioenv3$b,alternative="less", var.equal = TRUE)

############
#2)
############

# statistical power: the probability of accepting the alternative hypothesis (H1) when it is true
# test 1.1)

pwr.t.test(n = 25, d=0.4, sig.level = 0.05 ,type="one.sample")

############
#3)
############

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

set.seed(1)
mynorm.1 <- rnorm(50,4,5)
mynorm.2 <- rnorm(50,6,5)
mynorm.3 <- rnorm(50,12,4.5)

groups = factor(rep(c("A","B","C"), each = 50))
mydata <- c(mynorm.1,mynorm.2,mynorm.3)

mydata.df <- data.frame(mydata,groups)

# boxplot is a convenient way of graphically depicting 
# groups of numerical data through their quartiles.
# The spacings between the different parts of the box 
# indicate the degree of dispersion (spread) 
# and skewness in the data, and show outliers. 

plot(mydata ~ groups)


# box plot example
boxplot(cbind(mynorm.1,mynorm.2,mynorm.3))
par(mfrow=c(2,1),mar=c(1,1,1,1))
boxplot(mynorm.1,horizontal = T)
hist(mynorm.1)

# test homogeneity of variance

bartlett.test(mydata,groups) # the null hypothesis is that the variances are homogeneous

# perform anova

a.results <- aov(mydata ~ groups,data=mydata.df)
summary(a.results)

# Df = degree of freedom
# Sum Sq = deviance (within groups, and residual)
# Mean Sq = variance (within groups, and residual)
# F value = the value of the Fisher statistic test, so computed (variance within groups) / (variance residual)
# Pr(>F) = p-value

# We clearly reject the null hypothesis of equal means for all three drug groups. 
# We have to find out which ones are different. 
# One way to do this is applying pairwise t-test 
pairwise.t.test(mydata, groups, p.adjust="bonferroni")

# The Bonferroni correction is based on the idea that if an experimenter is testing m hypotheses,
# then one way of maintaining the familywise error rate (FWER) is to test each individual hypothesis 
# at a statistical significance level of 1/m times what it would be if only one hypothesis were tested.
# So, if the desired significance level for the whole family of tests should be (at most) a 
# then the Bonferroni correction would test each individual hypothesis at a significance level of a/m.

# Other option is applyin the function  TukeyHSD().
# This creates a set of confidence intervals on the differences between means 
# with the specified family - wise  probability of coverage. 
# The g eneral  form is TukeyHSD(x, conf.level = 0.95)

TukeyHSD(a.results, conf.level=0.95)

###############
#5) chitest
###############

bioenv3$Biofilm[bioenv3$Biofilm=="Present"]="present"
bioenv3 <- droplevels(bioenv3)

chisq.test(table(bioenv3$Salinity,bioenv3$Biofilm))
fisher.test(table(bioenv3$Salinity,bioenv3$Biofilm))

###############
#6)
###############

set.seed(1)
p.comb <- c(rpois(50,3),rpois(50,4))

rand <- vector (mode="numeric",length=10000)

for (i in 1:length(rand)) {
  tmp <- sample(p.comb) 
  rand[i] <- mean(tmp[1:50]) - mean(tmp[51:100])
}

hist(rand)

r.stat <- mean(p.comb[1:50]) - mean(p.comb[51:100])
r.stat
sum(rand<=r.stat)/length(rand)
abline(v=r.stat)

