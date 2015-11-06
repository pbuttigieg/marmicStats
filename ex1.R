# Exercise 1

#1.1
MyNorm.01.100 <- rnorm(n=100,mean=0,sd=1)

class(MyNorm.01.100)
MyNorm.01.100

#2)
mean(MyNorm.01.100)
median(MyNorm.01.100)
MyNorm.01.100.df<-as.data.frame(table(MyNorm.01.100,1))
MyNorm.01.100.df[which.max(MyNorm.01.100.df[,2]),]

sd(MyNorm.01.100)
range(MyNorm.01.100)

#3)
MyNorm.53.100 <- rnorm(100, 5, 3)
MyNorm.510.100 <- rnorm(100, 5, 10)

hist(MyNorm.510.100)
hist(MyNorm.53.100)

#4) 

scale(MyNorm.510.100)
scale(MyNorm.53.100)

(MyNorm.53.100 - mean(MyNorm.53.100))/sd(MyNorm.53.100)

#5) 

MyTableBio<-read.csv("~/Dropbox/Doctorado/CoursesAndWorkshops/MarMic_Statistics/bioenv-1.csv",header=T,sep="\t")
MyTableBio
#note: as numeric over the factor data, gives the codes of the factor leves
MyTableBio$a <- as.numeric(as.character(MyTableBio$a))
hist((MyTableBio$a))
hist((MyTableBio$b))

hist(MyTableBio$a^(1/3))
hist(sqrt(MyTableBio$a))
hist(MyTableBio$a)

hist((MyTableBio$c))

x<-seq(5,15,length=100)

x<-runif(30,0,2)
y<-sqrt(x)


plot(x,y,pch=19,xlim=c(0,2),ylim=c(0,1.5))
points(seq(0,2,by=0.1),seq(0,2,by=0.1),x,type="l")
points(x,log(x))

#############
# when a number lower than 1 is squared, it gives a bigger number. 
# Since to generate a number lower than 1, by multiplying to equal numbers, 
# this will be reducing them selfes  


#############
# Ex1
#############

# The geometric mean answers the question, "if all the quantities had the same value, 
# what would that value have to be in order to achieve the same product?" 
# For example, suppose you have an investment which earns 10% the first year, 
# 50% the second year, and 30% the third year. 
# What is its average rate of return? It is not the arithmetic mean, 
# because what these numbers mean is that on the first year your investment was multiplied (not added to) by 1.10, 
# on the second year it was multiplied by 1.60, and the third year it was multiplied by 1.20. 
# The relevant quantity is the geometric mean of these three numbers. 

# The question about finding the average rate of return can be rephrased as: 
# "by what constant factor would your investment need to be multiplied by each year
# in order to achieve the same effect as multiplying by 1.10 one year, 1.60 the next, and 1.20 the third?"

##################
# Ex2
##################
# For example, if a strain of bacteria increases its population by 20% in the first hour, 
# 30% in the next hour and 50% in the next hour, 
# we can find out an estimate of the mean percentage growth in population.
# Geometric Mean = (a1 ?? a2 . . . an)^1/n
# Geometric Mean = (1.2 ?? 1.3 x 1.5)^1/3
