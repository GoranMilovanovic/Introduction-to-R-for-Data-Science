
## Introduction to R for Data Science
## ══════════════════════════════════════════════════════════════════════════════════
## EXERCUSE 02: THE CENTRAL LIMIT THEOREM
## ══════════════════════════════════════════════════════════════════════════════════
## Autumn/Winter 2016.
## Data Science Serbia @Startit Centre, Savska 5, Belgrade, Serbia

## Lecturers
## ══════════════════════════════════════════════════════════════════════════════════
# Goran S. Milovanović, Phd
# Data Science Mentor @Springboard, Data Science Serbia
# ing Branko Kovač, Data Scientist @Tradecore
# Data Science Mentor @ Springboard, Data Science Serbia
## ══════════════════════════════════════════════════════════════════════════════════

## EXERCUSE 02: THE CENTRAL LIMIT THEOREM
## ══════════════════════════════════════════════════════════════════════════════════

# clear all
rm(list=ls())

# set plot  parameters
par(mfrow=c(2,2))

# sum 100 IID random variables, Chi-Square, m = 10
chiSqSums100 <- unlist(lapply(seq(1:100), function(x) {
  sum(rchisq(100, 10))
}))
# sum 1000 IID random variables, Chi-Square, m = 10
hist(chiSqSums100, 50, col="deepskyblue")
chiSqSums1000 <- unlist(lapply(seq(1:1000), function(x) {
  sum(rchisq(100, 10))
}))
# sum 10000 IID random variables, Chi-Square, m = 10
hist(chiSqSums1000, 50, col="deepskyblue")
chiSqSums10000 <- unlist(lapply(seq(1:10000), function(x) {
  sum(rchisq(1000, 10))
}))
hist(chiSqSums10000, 50, col="deepskyblue")
# sum 1000000 IID random variables, Chi-Square, m = 10
chiSqSums100000 <- unlist(lapply(seq(1:100000), function(x) {
  sum(rchisq(1000, 10))
}))
hist(chiSqSums100000, 50, col="deepskyblue")

# repeat for Poisson with lambda = 2
# set plot  parameters
par(mfrow=c(2,2))
for (sumSize in c(100, 1000, 10000, 100000)) {
  poisSums <- unlist(lapply(seq(1:sumSize), function(x) {
    sum(rpois(1000, 2))
  }))
  hist(poisSums, 50, col="red")
}

# repeat for Normal with mean = 100, sd = 25
# set plot  parameters
par(mfrow=c(2,2))
for (sumSize in c(100, 1000, 10000, 100000)) {
  normSums <- unlist(lapply(seq(1:sumSize), function(x) {
    sum(rnorm(1000, mean = 100, sd = 25))
  }))
  hist(normSums, 50, col="green")
}

# repeat for Cauchy with location = 0, scale = 1
# set plot  parameters
par(mfrow=c(2,2))
for (sumSize in c(100, 1000, 10000, 100000)) {
  cauchySums <- unlist(lapply(seq(1:sumSize), function(x) {
    sum(rcauchy(1000, location = 0, scale = 1))
  }))
  hist(cauchySums, 50, col="orange")
}