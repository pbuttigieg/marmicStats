#----------------------------------
#Practical class 1
#----------------------------------


#----------#
# 1)
#----------#

set.seed(10)

myNorm.01.100 <- rnorm(
  n = 100,  
  mean = 0, 
  sd = 1     
)

#----------#
#2)
#----------#


mean(myNorm.01.100)
median(myNorm.01.100)

MyMode <- function (x) {  
  
  x.round <- round(x,1)  
  x.table <- table(x.round)  

  mode <- x.table[x.table == max(x.table)] 
  
  return(mode)
  
} 

MyMode(myNorm.01.100)

# Thankfully, calculating the spread is more straightforward
var(myNorm.01.100) # variance
sd(myNorm.01.100) # standard deviation
range(myNorm.01.100) # range


#----------#
#3)
#----------#

set.seed(10)
myNorm.53.100 <- rnorm(100, 5, 3)

set.seed(10)
myNorm.510.100 <- rnorm(100, 5, 10)


hist(myNorm.510.100)
hist(myNorm.53.100)


par(mfrow = c(1,2)) 
# plotting

hist(myNorm.510.100)
hist(myNorm.53.100)

#----------#
#4) 
#----------# 

# We can calculate the z-scores of each vector ourselves....
myNorm.53.100.zscore <- (myNorm.53.100 - mean(myNorm.53.100)) / sd(myNorm.53.100)
myNorm.510.100.zscore <- (myNorm.510.100 - mean(myNorm.510.100)) / sd(myNorm.510.100)


scale(myNorm.510.100)
scale(myNorm.53.100)

#----------#
#5.1) 
#----------# 

# First we use read.csv() to import the table in bioenv-1.csv


bioenv1 <- read.csv(
  "https://raw.githubusercontent.com/pbuttigieg/marmicStats/master/statsI/bioenv-1.csv",
  header = T,
  sep = "\t", 
  row.names = 1
)


# Inspect the first 20 rows of the table using head()
head(bioenv1, 20)

# you can also do this with slicing:
bioenv1[1:20,] 

class(bioenv1$a) # this reports that R understands the data in this
# column as categorical or 'factor' data.

class(bioenv1$b) # the other columns are understood as having 
# integer values.

bioenv1 <- bioenv1[bioenv1$a != "no data", ]

class(bioenv1$a)

# we use as.numeric() to 
bioenv1$a <- as.numeric(as.character(bioenv1$a))

#----------#
#5.2) 
#----------# 

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


#----------#
#5.3) 
#----------# 

# visualize the distributions before and after the transformations
# species "a"

par(mfrow = c(1,3))

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

#----------#
#6) 
#----------#

# Coefficient of Variation
sa.cv <- sd(bioenv1$a)/mean(bioenv1$a) 
sb.cv <- sd(bioenv1$b)/mean(bioenv1$b) 
sc.cv <- sd(bioenv1$c)/mean(bioenv1$c) 

#----------#
#7) 
#----------#

# Compute the geometric mean.
prod(bioenv1$a + 1 )^(1/length(bioenv1$a)) -1 
prod(bioenv1$b + 1)^(1/length(bioenv1$b)) -1 
prod(bioenv1$c )^(1/length(bioenv1$c))
