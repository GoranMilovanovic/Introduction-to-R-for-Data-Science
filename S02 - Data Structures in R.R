
## Introduction to R for Data Science
## ═════════════════════════════════════════

## Lecturers
## ══════════════════════════════════════════════════════════════════════════════════
# Goran S. Milovanović, Phd
# Data Science Mentor @Springboard, Data Science Serbia
# ing Branko Kovač, Data Scientist @Tradecores
# Data Science Mentor @ Springboard, Data Science Serbia
## ══════════════════════════════════════════════════════════════════════════════════

## SESSION 01: DATA STRUCTURES IN R
## ═════════════════════════════════════════

### A. Data Types in R, Subsetting, and Coercion
## ═══════════════════════════════════════════════════════

# clear all
rm(list=ls())

# libraries
library(datasets)

# Let's start with some vectors:
char_vect <- character(length = 0) # empty character vect
char_vect
num_vect <- numeric(length = 10) # length can be != 0, but 0 is default value
num_vect
log_vect <- logical(length = 3) # default value is FALSE
log_vect

# c() is commonly used for the same purpose
num_vect_2 <- c(1, 14, 2, NA, 999, 101) # numerics
num_vect_2
log_vect_2 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE) # some Ts and Fs
log_vect_2
char_vect_2 <- c("this", "is", "a", "data", "science", "workshop") # characters
char_vect_2

# Some useful functions
length(char_vect)
class(num_vect)
typeof(num_vect)
duplicated(log_vect_2)

is.numeric(num_vect)
is.character(num_vect)
as.logical(num_vect)
is.na(num_vect_2)

# "is.x" functions check if something is x or not. 
# Replace x with numeric, character, NA, etc.

# Vector subsetting in R
char_vect_2[4] # select single element
log_vect_2[1:3] # or some interval
num_vect_2[3:length(num_vect_2)] # or even length() function

# New objects can be created when subsetting
test <- num_vect_2[-c(3,4)] # gives all except 3rd and 4th element
test
test_2 <- num_vect_2 %in% test # operator %in% can be very useful
test_2
not_na <- num_vect_2[!is.na(num_vect_2)] # removing NAs
not_na

# Vector ordering
sort(test, decreasing = T) # using sort() function
test[order(test, decreasing = T)] # or with order() function


# The difference between sort() and order():
someVector <- c(7,8,1,2)
sort(someVector, decreasing = T) # returns a vector sorted
order(someVector, decreasing = T) # returns a vector of indices to sort the input vector with! 

# Vector sequences
seq(1,22,by = 2) # seq is for "sequence"
rep(1, 4) # rep means: "replicate"
num_vect_2
rep(num_vect_2, 2) # replicate num_vect_2, 2 times

# Concatenation
new_num_vect <- c(num_vect, num_vect_2) # using 2 vectors to create new one
new_num_vect

new_combo_vect <- c(num_vect_2, log_vect) # combination of num and log vector
new_combo_vect # all numbers? false to zero? coercion in action

new_combo_vect_2 <- c(char_vect_2, num_vect_2) #works as well
new_combo_vect_2 # where are the numbers?
class(new_combo_vect_2) # all characters: coercion in R
# Coercion order in R: logical < integer < numeric < complex < character < list.

### B. Matrices
## ═══════════════════════════════════════════════════════

matr <- matrix(data = c(1,3,5,7,NA,11), nrow = 2, ncol = 3) # 2x3 matrix
matr
class(matr) # yes, it's matrix
typeof(matr) # double as expected 

matr[,2] # 2nd column
matr[3,] # oops, out of bounds, there's no 3rd row
matr[2,3] # element in 2nd row and 3rd column

matr_2 <- matrix(data = c(1,3,5,"7",NA,11), nrow = 2, ncol = 3) # another 2x3 matrix

class(matr_2) # matrix again
typeof(matr_2) # but not double anymore, type conversion in action!
t(matr_2) # transponed matr_2

### C. Scalar vs. Vector Operators in R
## ═══════════════════════════════════════════════════════

arr1 <- seq(2,20,2)
arr2 <- seq(1,19,2)

# get vectorized in R: element-wise multiplication:
arr1 * arr2

# similar:
arr1 + arr2
arr1 - arr2
arr1 / arr2
arr1 ^ arr2

# Never forget about  recylcing in R:
n1 <- c(1,2,3)
n2 <- c(4,5,6,7)
n1 + n2

# scalar ("inner", "dot") product in R:
arr1 %*% arr2
# again:
t(arr1) %*% arr2
# NOTE: see https://stat.ethz.ch/R-manual/R-devel/library/base/html/matmult.html
crossprod(arr1,arr2) # faster
# as scalar:
drop(crossprod(arr1, arr2))
# do: ?drop

# tcrossproduct() in R:
tcrossprod(arr1, arr2) # faster
# or:
arr1 %*% t(arr2) # slower

# And we now learned that R defaults to...
arr1

# ... column vectors - because only this is a row vector:
t(arr1)

# Multplying vectors and matrices
arr1 <- 1:4
arr1

arr2 <- matrix(rep(1:4,4),ncol = 4,byrow = T)
arr2

arr1 * arr2 # element-wise

# crossprod()
arr1 %*% arr2
t(arr1) %*% arr2
crossprod(arr1, arr2)

# tcrossprod()
arr1 %*% t(arr2)
tcrossprod(arr1, arr2)

# Multiplying matrices
arr1 <- matrix(rep(2,16),ncol = 4,byrow = T)

# element-wise:
arr1 * arr2

# t(arr1) %*% arr2
crossprod(arr1,arr2)
# and this does the same:
arr1 %*% arr2

tcrossprod(arr1,arr2)
# which is, again, the same as:
arr1 %*% t(arr2)

# An important operation on arrays is the outer product. 
# If a and b are two numeric arrays, their outer product is an array 
# whose dimension vector is obtained by concatenating their two dimension 
# vectors (order is important), and whose data vector is got by forming
# all possible products of elements of the data vector of a with those of b.
# [source: https://cran.r-project.org/doc/manuals/r-patched/R-intro.html#The-outer-product-of-two-arrays]

# outter product of two vectors:
a <- 1:4
b <- 1:3
a %o% b
# or:
outer(a,b,"*")

### D. Lists in R
## ═══════════════════════════════════════════════════════

# We use lists a lot in R:
list_1 <- list(num_vect_2, char_vect_2, log_vect_2) # this is a list
list_1 # this is our list

str(list_1) # about a list
length(list_1)

as.list(char_vect_2) # another way to create a list

# Lists manipulation
names(list_1) <- c("numeric", "words", "logical")

list_1[3] # 3rd element?
list_1[[3]] # 3rd element?

is.list(list_1[3]) # is this a list?
is.list(list_1[[3]]) # and this?

class(list_1[[3]]) # also a list? hm?

list_1$words # we can also extract an element this way
list_1[["words"]] # or even like this

length(list_1$words) # 2 as expected

list_1[["words"]][1] # digging even deeper

list_1$new_elem <- c(TRUE, FALSE, FALSE, TRUE) # add new element

length(list_1) # now list has 4 elements
list_1$new_elem <- NULL # but we can remove it easily

new_vect <- unlist(list_1) # creating a vector from list

### E. Data Frames  
## ═══════════════════════════════════════════════════════

# Introducing data frames in R

# data
data("mtcars")
# data set description:
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html

# Some useful information about data frames
str(mtcars) # lets see object structure
summary(mtcars) # summary data frame information
colnames(mtcars) # column names
head(mtcars) # see first 6 (by default) rows
tail(mtcars, 10) # see last 10 rows

# Think of data frame columns as vectors! Because they are!
mean(mtcars$mpg) # mean of mtcars mpg (miles per galon) column
median(mtcars$cyl) # median of mtcars cyl (cylinders) column

# Lets do some data frame subsetting
mtcars[-1, ] # first row out
mtcars[ ,-1] # first column out

mtcars[c(1,3)] # keeping 1st and 3rd column only
mtcars[-c(1,3)] # removing 1st and 3rd column

subset(mtcars, mpg < 19) # this is one way (and it can be slow!)
mtcars[mtcars$mpg < 19, ] # this is another one (faster)
mtcars[which(mtcars$mpg < 19), ] # and another one (usually even more faster)

mtcars[mtcars$mpg > 20 & mtcars$am == 1, ] # multiple conditions

# Data frame transformations
mtcars$trans <- ifelse(mtcars$am == 0, "automatic", "manual") # we can add new colums

mtcars$trans <- NULL # or we can remove them

mtcars[c(1:3,11,4,7,5:6,8:10)] # this way we change column order

# Separation and joining of data frames
low_mpg <- mtcars[mtcars$mpg < median(mtcars$mpg), ] # new data frame with mpg < med(mpg)

high_mpg <- mtcars[mtcars$mpg >= mtcars$mpg, ] # new data frame with mpg >= med(mpg)

mpg_bind <- rbind(low_mpg, high_mpg) # combine 2 data frames with same columns

car_condition <- data.frame(sample(c("old","new"), replace = T, size = 32))
car_condition # creating random data frame with "old" and "new" values

colnames(car_condition) <- "condition" # for all kinds of objects
head(car_condition)

rownames(car_condition) <- rownames(mtcars) # use row names of one data frame
head(car_condition)
# as row names of other

mpg_join <- cbind(mpg_bind, car_condition) # or combine data frames like this
