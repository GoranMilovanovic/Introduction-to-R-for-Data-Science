
## Introduction to R for Data Science
## ══════════════════════════════════════════════════════════════════════════════════
## SESSION 03: CONTROL FLOW AND FUNCTIONS IN R
## ══════════════════════════════════════════════════════════════════════════════════
## Autumn 2016.
## Data Science Serbia @Startit Centre, Savska 5, Belgrade, Serbia

## Lecturers
## ══════════════════════════════════════════════════════════════════════════════════
# Goran S. Milovanović, Phd
# Data Science Mentor @Springboard, Data Science Serbia
# ing Branko Kovač, Data Scientist @Tradecores
# Data Science Mentor @ Springboard, Data Science Serbia
## ══════════════════════════════════════════════════════════════════════════════════

## SESSION 03: CONTROL FLOW AND FUNCTIONS IN R
## ══════════════════════════════════════════════════════════════════════════════════

# clear all
rm(list=ls())

# Starting with simple 'if'
num <- 2
if (num > 0) print("num is positive") # if condition num > 0 stands than print()
                                      # is executed

# Sometimes 'if' has its 'else'
if (num > 0) {
  print("num is positive")
} else {
  print("num is negative")
}

# NOTE: "{" and "}" are generally used in R to mark the beginning and the end of block!

# Multiple 'else's are also possible
if (!is.numeric(num)) {
  print("this is not a number")
} else if (num > 0) {
  print("num is positive")
} else if (num < 0) {
  print("num is negative")
} else print("ZERO!")

# R is vectorized so there's vectorized if-else
simple_vect <- c(1, 3, 12, NA, 2, NA, 4)
ifelse(is.na(simple_vect), "nothing here", "some number") # nothing here if it's
                                                          # NA or it's a number

# For loops in R:
for (i in 1:10) print(i) # iterate in set of values

# Be aware that loops can be slow if
vec  <-  numeric()
system.time(
  for(i in seq_len(50000-1)) {
    some_calc <- sqrt(i/10)
    vec <- c(vec, some_calc) # this is what makes it slow
  }  
)

# This solution is slightly faster
iter <- 50000
vec <- numeric(length=iter) # this makes it faster...
system.time(
  for(i in seq_len(iter-1)) {
    some_calc <- sqrt(i/10)
    vec[i] <- some_calc # ...not this!
  }
)

# This solution is even more faster
iter <- 50000
vec <- numeric(length=iter) # not because of this...
system.time(
  for(i in seq_len(iter-1)) {
    vec[i] <- sqrt(i/10) # ...but this!
  }
)

# Another example how loops can be slow (loop vs vectorized function)
iter <- 50000

system.time(
  for (i in 1:iter) {
    vec[i] <- rnorm(n=1, mean=0, sd=1) # approach from previous example
  }
)

system.time(y <- rnorm(iter, 0, 1)) # but this is much much faster

# R also knows about while loop
r <- 1 # initializing some variable
while (r < 5) {
  print(r)
  r <- r + 1
}

# Loops can be nested
for(i in 1:5) { # outer loop
  for(j in 1:5) { # inner loop
    print(paste0(i,j)) # sample code
  }
}

# Loops can be altered using break and next
for(i in 1:5) {
  if (i == 4) break # jump out of loop if condition is true
  print(i)
}

for(i in 1:5) {
  if (i == 4) next # just skip current iteration if condition is true
  print(i)
}

# Nope, we didn't forget 'repeat' loop
i <- 1
repeat { # there is no condition...
  print(i)
  i <- i + 1
  if (i == 10) break # ...so we have to break it if we don't want infinite loop
}

# And there's something called 'switch' :)
switch(2, "Data", "Science", "Dubai") # choose one option based on value

# More on switch:
switchIndicator <- "Data"
switchIndicator <- "Science"
switchIndicator <- "Dubai"
# rare situations where you do not need to enclose strings: ' ', or " "
switch(switchIndicator,
       Data = {print(switchIndicator)},
       Science = {print(switchIndicator)
                    print(switchIndicator)},
       Dubai = {print(nchar(switchIndicator))}
)

# now:
type = 4
cc <- c("First", "Second", "Third")
switch(type,
       c1 = {print(cc[1])},
       c2 = {print(cc[2])},
       c3 = {print(cc[3])},
       {print("Beyond C...")} # default choice
)

# Switch and if-else are similar, but switch is faster
# Switch and if-else are similar, but switch is faster
ix <- 2
system.time(
  if(ix == 1) {
    print("Data")
  } else if(ix == 2) {
    print("Science")
  } else print("Serbia"))

system.time(switch(ix,print("Data"), print("Science"), print("Serbia")))

# elementary function: a definition of a single argument function in R
fun <- function(x) x+10
fun(5)

# Functions which takes two arguments
fun2 <- function(x,y) x+y
fun2(3,4)

# Function using "{" and "}" to enclose multiple R expresions in the function body
fun <- function(x,y) {
  a <- sum(x)
  b <- sum(y)
  a-b
}

r <- c(5,4,3)
q <- c(1,1,1)
fun(r,q)
fun(c(5,4,3),c(1,1,1)) 

# a function is a function:
is.function(fun)
is.function(log) # log is built-in

# printing function to acess their source code
fun
log # try: is.primitive(log) this one is written in C, 
    # belongs to the base package - it's "under the hood"

# built in functions:
x <- 16
sqrt(x)
x <- c(1,2,3,4,5,6,7,8,9)
mean(x)
# watch for NAs in statistics (!)
x <- c(1,2,3,4,5,6,7,8,NA)
mean(x)
mean(x, na.rm = T) # right!
median(x)
sd(x)
sum(x)
sum(x, na.rm = T) # a-ha!

# Lexical scoping in R + nested functions
x <- 1
h <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
h()

# Messing up argument names (never do this in nested functions unless you have to)
rm(x, h)
x <- 1
h <- function(x) {
  y <- x+1
  i <- function(x) {
    z <- x+2
    z
  }
  z <- i(x)
  c(x,y,z)
}
h(x)

# Two things that come handy: lapply and apply

# Step 1: here's a list:
list_1 <- list(c(1,2),
               c(4,5,6),
               c(7,8,9),
               c(10,11,12))

# Step 2: I want to apply the following function:
my_fun <- function(x) {
  x[1]+x[2]-x[3]
}

# to all elements of the list_1 list, and get the result as a list again. Here it is:
res <- lapply(list_1, function(x) {
  x[1]+x[2]-x[3]
})
unlist(res) # to get a vector
rm(my_fun)

# Now say we have got a matrix
my_matr <- matrix(c(1,2,3,4,5,6,7,8,9),
                  nrow=3,
                  ncol=3)

# let's check if matrix() is a function
is.function(matrix)

# reminder
class(my_matr)
typeof(my_matr)

# now, I want the sums of all rows:
rs_my_matr <- apply(my_matr, 1, function(x) {
  sum(x)
})
rs_my_matr
is.list(rs_my_matr)

# sum of all columns:
cs_my_matr <- apply(my_matr, 2, function(x) {
  sum(x)
})

# with existings functions such as sum(), this will do:
rs_my_matr_2 <- apply(my_matr, 1, sum)
rs_my_matr_2
cs_my_matr_2 <- apply(my_matr, 2, sum)
cs_my_matr_2

# But R makes your life really simple with these 2 built in functions
rowSums(my_matr)
colSums(my_matr)

