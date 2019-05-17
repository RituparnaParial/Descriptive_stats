# Descriptive statistics ----------------------------------------------
# we’ll look at measures of central tendency, variability
# and distribution shape for continuous variables

my_variables <- c("mpg", "hp", "wt")
head(mtcars[my_variables])

mtcars_variables <- mtcars[my_variables]

#summary gives min, max, quartiles
#mean for numerical values and frequencies
#in mtcars dataset
summary(mtcars[my_variables])

mtcars <- mtcars #dataframe so can view full
#datset in r 

#library(help = "datasets") shows all available dataset in r to work with 
library(propagate)
my_stats <- function(x, na.omit = FALSE){
  if(na.omit)
    x <- x[!is.na(x)] #omits missing values
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m) ^ 3 / s ^ 3)/n
  kurt <- sum((x-m) ^ 4 / s ^ 4)/n - 3
  #skew <- skewness(x)
  #kurt <- kurtosis(x)
  
  return(c(n = n, mean = m, stdev = s, skew = skew,
           kurtosis = kurt))
}

head(mtcars_variables)
sapply(mtcars_variables, my_stats)

#Results show mean mpg is 20.1, with a SD of 6.0. 
# The distribution is skewed to the right(+0.61) 
# and is somewhat flatter than a normal distribution (–0.37).

d <- density(mtcars_variables$mpg) 
plot(d, main = "chart showing MPG")
abline(v = 20,#mean
       lty = 2, col = "blue")
install.packages("Hmisc")
library(Hmisc)

#describe() fn gives number of variables, observations.
#missing, unique values,
#mean, quantiles, five highes and lowest values
describe(mtcars_variables)

#stat.desc gives loads of stats on the dataset
#basic = TRUE gives basic stats
#desc = TRUE gives mean std dev, etc.
#norm = TRUE gives normal distirbution stats
#i.e. how the data varies from a normal dist.
install.packages("pastecs")
library(pastecs)
stat.desc(mtcars_variables,norm = TRUE)

# the psych package also has a function called describe() that
# provides the number of nonmissing observations, mean, sd, median,
# trimmed mean, median absolute deviation (mad), minimum, maximum, 
# range, skew, kurtosis, and standard error of the mean

install.packages("psych")
library(psych)
describe(mtcars_variables)

# Descriptive statistics by group using aggregate() ---------------
# When comparing groups of individuals or observations
# the focus is usually on the descriptive statistics of each group
# rather than the total sample

my_variables <- c("mpg", "hp", "wt")
# Note the use of list(am=mtcars$am)
# If you used list(mtcars$am), the am column
# would be labeled Group.1 rather than am.
aggregate(mtcars_variables, list(am = mtcars$am), FUN = mean)

# Aggregate won’t return several statistics at once
# single-value functions only eg mean, sd
# We use the by() function for several outputs
# using a function that operates on all columns of
# a data frame
dstats <- function(x) sapply(x,my_stats)
# Here we're applying the mystats function
# to each column of the data frame.
# Placing it in the by() function gives 
# you summary statistics for each level of am.
# Remember - am  = 0 for automatic
# or 1 for manual transmission
by(mtcars_variables,mtcars$am, dstats)

# doBy - -------------------------------------------

# doBy provide functions for descriptive statistics by group
install.packages("doBy")
library(doBy)
# Variables on the left of the ~ are the numeric variables 
# to be analysed, and variables on the right are categorical 
# grouping variables
# Uses the mystats function defined earlier
summaryBy(mpg + hp +wt  ~ am, data = mtcars, FUN = my_stats)