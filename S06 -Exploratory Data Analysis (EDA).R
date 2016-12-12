
## Introduction to R for Data Science
## ══════════════════════════════════════════════════════════════════════════════════
## SESSION 06: EXPLORATORY DATA ANALYSIS (EDA)
## ══════════════════════════════════════════════════════════════════════════════════
## Autumn 2016.
## Data Science Serbia @Startit Centre, Savska 5, Belgrade, Serbia

## Lecturers
## ══════════════════════════════════════════════════════════════════════════════════
# Goran S. Milovanović, Phd
# Data Science Mentor @Springboard, Data Science Serbia
# ing Branko Kovač, Data Scientist @Tradecore
# Data Science Mentor @ Springboard, Data Science Serbia
## ══════════════════════════════════════════════════════════════════════════════════

## SESSION 06: EXPLORATORY DATA ANALYSIS (EDA)
## ══════════════════════════════════════════════════════════════════════════════════

# clear all
rm(list=ls())

# libraries
library(datasets)
library(dplyr)
library(ggplot2)

# data
data(mtcars)

# mtcars data set description:
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html

## Summarize Data Set
## ═════════════════════════════════════════
summary(mtcars) # {base}
glimpse(mtcars) # {dplyr}
head(mtcars, 5) # {base}
tail(mtcars,10) # {base}
dim(mtcars)
# {dplyr} group_by() and summarise()
cylinders <- group_by(mtcars, cyl)
class(cylinders)
summarise(cylinders, 
          mean = mean(mpg), 
          stdDev = sd(mpg))

### Descriptive Statistics
## ═════════════════════════════════════════
## Mean
mean(mtcars$mpg)
mtcars$mpg[10]<- NA
mean(mtcars$mpg) # carefully!
mean(mtcars$mpg, na.rm=T) # right...
data(mtcars)
## Variance
var(mtcars$mpg)
## Standard Deviation
sd(mtcars$mpg)
## sample quantiles
quantile(mtcars$mpg, probs = seq(0,1,.01))
## max, min, range
max(mtcars$mpg)
min(mtcars$mpg)
range(mtcars$mpg)
# really...
r <- range(mtcars$mpg)[2]-range(mtcars$mpg)[1]
r
# median
median(mtcars$mpg)

### EDA plots
## ═════════════════════════════════════════
# boxplot mtcars$mpg
boxplot(mtcars$mpg,
        horizontal = TRUE, 
        xlab="MPG",
        main = "Boxplot: MPG")

## NOTE: Boxplot "fences" and outlier detection
## ═════════════════════════════════════════
# Boxplot in R recognizes as outliers those data points that are found beyond OUTTER fences
# Source: http://www.itl.nist.gov/div898/handbook/prc/section1/prc16.htm
# Q3 = 75 percentile, Q1 = 25 percentile
Q3 <- quantile(mtcars$mpg,.75)
Q3
Q1 <- quantile(mtcars$mpg,.25)
Q1
# IQ = Q3 - Q1; Interquartile range
IQ <- unname(Q3 - Q1)
IQ
# lower inner fence: Q1 - 1.5*IQ
# upper inner fence: Q3 + 1.5*IQ
# lower outer fence: Q1 - 3*IQ
# upper outer fence: Q3 + 3*IQ 
# A point beyond an inner fence on either side is considered a mild outlier
# A point beyond an outer fence is considered an extreme outlier

## histogram: mtcars$mpg
## ═════════════════════════════════════════
## freq = T for frequencies
hist(mtcars$mpg, 
     main="Histogram: MPG", 
     xlab="MPG", 
     freq=T)
## prob=T for probability density
hist(mtcars$mpg, 
     main="Histogram: MPG",
     xlab="MPG",
     col = "blue",
     prob=T)
# overlay mtcars$mpg density function over the empirical distribution
## NOTE: this is kernel density estimation in R
## You are not testing any distribution yet.
lines(density(mtcars$mpg),
      lty="dashed", 
      lwd=2.5, 
      col="red")
# check this out:
d_Mpg <- density(mtcars$mpg)
d_Mpg
class(d_Mpg)
# more on probability functions to follow
## experiment with hist()
# plot layout: 2 x 2
par(mfcol = c(2,2))
myHist <- lapply(seq(4,10,2), function(x) {
  binSize <- (range(mtcars$mpg)[2]-range(mtcars$mpg)[1])/x
  binEnds <- seq(0,x,1)*binSize+min(mtcars$mpg)
  hist(mtcars$mpg,
       main="",
       xlab="MPG",
       freq=T,
       col = "deepskyblue",
       breaks = binEnds)
})

class(myHist)
class(myHist[[1]])

## Q-Q Plot
## ═════════════════════════════════════════
# plot layout: 1 x 2
par(mfcol = c(1,2))
qqnorm(mtcars$mpg)
qqline(mtcars$mpg, 
       col="red",
       lwd = 2)
# c.f. histogram
hist(mtcars$mpg, 
     main="", 
     xlab="MPG", 
     prob=T)
# overplot Gaussian (Normal) Distribution
curve(dnorm(x, mean=mean(mtcars$mpg),sd=sd(mtcars$mpg)), 
      add=TRUE, 
      col="red",
      lwd=2) 

# tables in R
tCyl <- table(mtcars$cyl)
tCyl
class(tCyl)
# plot layout: 1 x 1
par(mfcol = c(1,1))
plot(tCyl) # ha!
# cross-tabulation with table()
t1 <- table(mtcars$cyl,mtcars$mpg)
t1
t2 <- table(mtcars$cyl,mtcars$gear)
t2
plot(t2,
     xlab="Cyl",
     ylab="Gear")

# Introducing Chi-Square, a basic non-parametric test...
# Testing the independence of rows and columns
chisq.test(t2)
# ... error, because there are cells with < 5 observations 
# workaround:
chisq.test(t2, simulate.p.value = TRUE)
# Better: Fisher Exact Test for Count Data
fisher.test(t2)
