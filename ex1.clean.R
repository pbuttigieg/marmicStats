###############################################################
#Practical class 1
###############################################################

############
#1)
############

# It is recommended to use a systematic naming of the objects.
set.seed(10)
myNorm.01.100 <- rnorm(n=100,mean=0,sd=1)

############
#2)
############

# Mean and median.
mean(myNorm.01.100)
median(myNorm.01.100)

# Computing the mode requires a little more work.
# We'll create a function to do this. 
# Notice that there could be more than one mode.

MyMode <- function (x) {

  x.round <- round(x,1)  
  x.table <- table(x.round)
  mode <- x.table[x.table==max(x.table)]

  return(mode)
  
} 

MyMode(myNorm.01.100)

# Range and standard deviation.
sd(myNorm.01.100)
range(myNorm.01.100)

############
#3)
############

myNorm.53.100 <- rnorm(100, 5, 3)
myNorm.310.100 <- rnorm(100, 5, 10)

# visualize the differences between the two.
hist(myNorm.510.100)
hist(myNorm.53.100)

############
#4) 
############ 

# "manual" calculation of z-scores
myNorm.53.100.zscore <- (myNorm.53.100 - mean(myNorm.53.100))/sd(myNorm.53.100)
myNorm.510.100.zscore <- (myNorm.510.100 - mean(myNorm.510.100))/sd(myNorm.510.100)

# calculate z-scores using the scale function.
scale(myNorm.510.100)
scale(myNorm.53.100)

############
#5.1) 
############ 

# Make sure to write the path to the folder where bioenv-1 is. 
# You can use tab to auto complete and check if the path is correct.
# You can also use the setwd() function to move to the directory where
# bioenv3 is.

bioenv1 <- read.csv("~/Dropbox/Doctorado/CoursesAndWorkshops/MarMic_Statistics/tables/bioenv-1.csv",header=T,sep="\t")

bioenv1 <- read.csv(
  "bioenv-1.csv",
  header=T,
  sep="\t"
  )

# Inspect the table
head(bioenv1,20)

# The row (sample site) where there is no data for species "a" will be removed. 
# Note: as.numeric over the factor data, gives the codes of the factor levels, 
# so it should first be converted to character, and then to numeric.

bioenv1 <- bioenv1[bioenv1$a!="no data",]
class(bioenv1$a)
bioenv1$a <- as.numeric(as.character(bioenv1$a))

############
#5.2) 
############ 

# Compute the measures of location and spread for the species counts
# species "a"
mean(bioenv1$a)
median(bioenv1$a)
MyMode(bioenv1$a)

sd(bioenv1$a)
range(bioenv1$a)

# species "b"
mean(bioenv1$b)
median(bioenv1$b)
MyMode(bioenv1$b)

sd(bioenv1$b)
range(bioenv1$b)

# species "c"
mean(bioenv1$c)
median(bioenv1$c)
MyMode(bioenv1$c)

sd(bioenv1$c)
range(bioenv1$c)


############
#5.3) 
############ 

# visualize the distributions before and after the transformations
# species "a"
hist(bioenv1$a)
hist(sqrt(bioenv1$a))
hist(log(bioenv1$a))

# species "b"
hist(bioenv1$b)
hist(sqrt(bioenv1$b))
hist(log(bioenv1$b))

# species "c"
hist(bioenv1$c)
hist(sqrt(bioenv1$c))
hist(log(bioenv1$c))

############
#6) 
############

# Coefficient of Variation
sa.cv <- mean(sd(bioenv1$a/bioenv1$a)) 
sb.cv <- mean(sd(bioenv1$b/bioenv1$b)) 
sc.cv <- mean(sd(bioenv1$c/bioenv1$c)) 

############
#7) 
############

# Compute the geometric mean.
# Notice that if there is a 0, the geometric mean will be 0. 
# A workaround is to add 1 to all the values and then subtract 1 from the geometric mean.
prod(bioenv1$a + 1 )^(1/length(bioenv1$a)) -1 
prod(bioenv1$b + 1)^(1/length(bioenv1$b)) -1 
prod(bioenv1$c )^(1/length(bioenv1$c))
