#----------------------------------
#Practical class 1
#----------------------------------

# remember to use "?" frequently to read about each function
# you use!

#----------#
# 1)
#----------#

# In order to be able to reproduce all "random number" operations,
# we use set.seed() to 'seed' the random number generator in R.
# If the same seed is used, 
set.seed(10)

# For this exercise, we need to create a vector of 100 numbers
# sampled from a normal distribution with a mean of 0 and a 
# standard deviation of 1 (i.e. the standard normal distribution)
# We name the object sensibly, and use the function rnorm() to 
# generate random deviates (i.e. numbers that randomly deviate
# from the mean, with a spread controlled by the sd)

# we put the results into an object with an intelligible name...

myNorm.01.100 <- rnorm(
  n = 100,   # create 100 numbers
  mean = 0,  # set the mean
  sd = 1     # set the sd
  )

#----------#
#2)
#----------#

# Using R's built-in functions, we can calculate 
# different statistics describing the location of 
# the data's distribution.

mean(myNorm.01.100)
median(myNorm.01.100)

# Computing the mode requires a little more work,
# as R doesn't have a standard function for this.
# We'll create our own function to do this. 
# Let's call it, MyMode

MyMode <- function (x) {  # here we specify that the function
                          # takes one argument, which we call
                          # "x". x represents any object the user
                          # of this function wants to use as input

  x.round <- round(x,1)  # the first thing we do is to round the
                         # values in x to the tenths position. Note that
                         # you *wouldn't* do this normally, we do it
                         # here to ensure we get a mode as the numbers
                         # in myNorm.01.100 have an overly high precision
                         # so getting two identical numbers is very 
                         # unlikely.
                         # we put the results into an object called
                         # x.round. Note that any object created
                         # inside a function is *only* available to 
                         # that function and not your general environment.
  x.table <- table(x.round)  # we use the table function to organise the data
                             # There will be a column for each unique value
                             # found in the data and then the number of 
                             # occurrences of each number will be listed
                             # below it.
  
  
  # next we extract the mode or modes by 'slicing' x.table with a 
  # logical vector (a list of TRUEs and FALSEs) which contain the
  # answer to the question: x.table == max(x.table)
  # in other words, is each frequency value in x.table equal to the
  # maximum frequency value of the whole table? The max frequency 
  # value is the mode.
  mode <- x.table[x.table == max(x.table)] 

  return(mode) # return() will report the contents of an object
               # in a function to the console (so you can see it
               # or save it to an object in your main environment)
  
} 

# we now run our mode function on our vector of 100 numbers
# we should get 2 modes, -0.2 and 0.7, which both occur
# 6 times
MyMode(myNorm.01.100)

# Thankfully, calculating the spread is more straightforward
var(myNorm.01.100) # variance
sd(myNorm.01.100) # standard deviation
range(myNorm.01.100) # range


#----------#
#3)
#----------#



# Create some differnt normal distributions for comparison.
# Set the seed again to be sure of reproducibility
set.seed(10)

myNorm.53.100 <- rnorm(100, 5, 3)
myNorm.510.100 <- rnorm(100, 5, 10)

# visualize the differences between the two using
# R's histogram function...
hist(myNorm.510.100)
hist(myNorm.53.100)

# we can change the plotting parameters with par() to
# create multiple panels with the mfrow argument.
par(mfrow = c(1,2)) # have one row and two columns for
                    # plotting

hist(myNorm.510.100)
hist(myNorm.53.100)

# pay attention to the bins used by the histograms.

#----------#
#4) 
#----------# 

# We can calculate the z-scores of each vector ourselves....
myNorm.53.100.zscore <- (myNorm.53.100 - mean(myNorm.53.100))/sd(myNorm.53.100)
myNorm.510.100.zscore <- (myNorm.510.100 - mean(myNorm.510.100))/sd(myNorm.510.100)

# ... or by using a built in function calculate z-scores 
# using the scale() function.
scale(myNorm.510.100)
scale(myNorm.53.100)

#----------#
#5.1) 
#----------# 

# First we use read.csv() to import the table in bioenv-1.csv


bioenv1 <- read.csv(
  "https://raw.githubusercontent.com/pbuttigieg/marmicStats/master/statsI/bioenv-1.csv",
  header = T, # we let R know that the first row should be 
              # understood as a header (i.e. a row of column names)
  sep = "\t", # we let R know that the column separator is not a
              # comma, but a tab, represented by "\t"
  row.names = 1 # we let R know to consider the first column as a
                # column of row names, rather than a variable
  )


# Ordinarily, you wouldn't be using a URL, but would give a file path
# like "C:/Users/myName/My Documents/myFile.csv"
# but R understands URLs too and can fetch data accordingly.
# If you want to use a file path instead (e.g. if you have the file
# on your computer and don't want to / can't download it again)
# just replace the URL with the file path. The file can be anywhere
# as long as you give the full path. If you are working in the same
# directory the file is in (check this with getwd()), then you can
# just give the file name. To change your working directory,
# use setwd() - this can be a bit more convenient, but isn't needed
# if you give the full path.

# Inspect the first 20 rows of the table using head()
head(bioenv1, 20)

# you can also do this with slicing:
bioenv1[1:20, ] # for a table-like structure such as a data frame or
                # a matrix, you select rows first, add a comma, and
                # then select columns. If you leave one slot blank
                # then all rows/columns are selected, so a blank is
                # like "no constraint". In our command, we say select
                # rows from 1 to 20 and all columns.


# The row (sample site) where there is no data for species "a" will be removed. 
# Note: as.numeric over the factor data, gives the codes of the factor levels, 
# so it should first be converted to character, and then to numeric.

# We set a little trap in this data import - while most of the data is 
# numeric, there is one entry that is a string of characters:
# "no data". This messes R up: it now assumes that you do not have
# numeric data in that column:

class(bioenv1$a) # this reports that R understands the data in this
                 # column as categorical or 'factor' data.

# notice that we used the "$" rather than slicing with [ ,"a"]
# both are equivalent when dealing with data.frames, and select
# a named column and all rows.

class(bioenv1$b) # the other columns are understood as having 
                 # integer values.

# let's delete the row with missing data...
# we do this by replacing bioenv1 with a subset of itself
# containing all rows where column a does not match
# ("!=") the string "no data"

bioenv1 <- bioenv1[bioenv1$a != "no data", ]

class(bioenv1$a)
# the row is gone, but class() reveals that R doesn't automatically
# switch the class of the "a" vector (i.e. the column, remember that
# dataframes are just vectors stuck together as rows or columns)
# to "integer" or "numeric", so we do it ourselves.
# This may seem troublesome, but keep in mind you'd take a similar 
# set of actions in Excel and other software to be sure your 
# data is being processed as you'd expect it to be processed.

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
# Notice that if there is a 0, the geometric mean will be 0. 
# A workaround is to add 1 to all the values and then subtract 1 from the geometric mean.
prod(bioenv1$a + 1 )^(1/length(bioenv1$a)) -1 
prod(bioenv1$b + 1)^(1/length(bioenv1$b)) -1 
prod(bioenv1$c )^(1/length(bioenv1$c))
