
## Introduction to R for Data Science
## -----------------------------------------------------------------------------------
## SESSION 09: SIMPLE LINEAR REGRESSION
## -----------------------------------------------------------------------------------
## Autumn/Winter 2016.
## Data Science Serbia @Startit Centre, Savska 5, Belgrade, Serbia

## Lecturers
## -----------------------------------------------------------------------------------
# Goran S. Milovanović, Phd
# Data Science Mentor @Springboard, Data Science Serbia
# ing Branko Kovač, Data Scientist @Tradecore
# Data Science Mentor @ Springboard, Data Science Serbia
## -----------------------------------------------------------------------------------

## SESSION 09: SIMPLE LINEAR REGRESSION
## --------------------------------------------

## Linear Regression: Assumptions
## --------------------------------------------
# 1. Linearity
# 2. Constant variance == homoscedasticity
# 3. Independence of errors == no autocorrelation
# 4. No significant outliers or influential cases
# 5. Level of measurement: interval or ratio scale

### --- Clear, Libraries, Data Sets
# clear all
rm(list=ls())

# libraries
library(datasets)
library(dplyr)
library(Hmisc)
library(QuantPsyc)
library(ggplot2)
library(broom)
library(car)

### --- data
data(iris)
# iris data set description:
# https://stat.ethz.ch/R-manual/R-devel/library/iriss/html/iris.html

### --- Exploratory Data Analysis (EDA)
## ═════════════════════════════════════════
glimpse(iris)
summary(iris)

## EDA plots
# plot layout: 2 x 2
par(mfcol = c(2,2))
# boxplot iris$Sepal.Length
boxplot(iris$Sepal.Length,
        horizontal = TRUE, 
        xlab="Sepal Length")
# histogram: iris$Sepal.Length
hist(iris$Sepal.Length, 
     main="", 
     xlab="Sepal.Length", 
     prob=T)
# overlay iris$Sepal.Length density function over the empirical distribution
lines(density(iris$Sepal.Length),
      lty="dashed", 
      lwd=2.5, 
      col="red")
# boxplot iris$Petal.Length
boxplot(iris$Petal.Length,
        horizontal = TRUE, 
        xlab="Petal Length")
# histogram: iris$Petal.Length,
hist(iris$Petal.Length,
     main="", 
     xlab="Petal Length", 
     prob=T)
# overlay iris$Petal.Length density function over the empirical distribution
lines(density(iris$Petal.Length),
      lty="dashed", 
      lwd=2.5, 
      col="red")
# reset plot paramateres
par(mfcol = c(1,1))

## Pearson correlation in R {base}
## ═════════════════════════════════════════
cor1 <- cor(iris$Sepal.Length, iris$Petal.Length, 
            method="pearson")
cor1
# Let's test the assumption of linearity:
par(mfcol = c(1,1))
plot(iris$Sepal.Length, iris$Petal.Length,
     main = "Sepal Length vs Petal Length",
     xlab = "Sepal Length", ylab = "Petal Length")

# Is Pearson's product-moment correlation coefficient significant?
cor2 <- rcorr(iris$Sepal.Length, # {hmisc}
              iris$Petal.Length, 
              type="pearson")
cor2$r # correlations
cor2$r[1,2] # Ok, the one we're looking for
cor2$P[1,2] # significant at
cor2$n[1,2] # num. observations

### --- Linear Regression with lm()
## ═════════════════════════════════════════

# Predicting: Petal Length from Sepal Length
reg <- lm(Petal.Length ~ Sepal.Length, 
          data=iris) 
class(reg)
summary(reg)
coefsReg <- coefficients(reg)
coefsReg
slopeReg <- coefsReg[2]
interceptReg <- coefsReg[1]

# Prediction from this model
# watch the variable names in the new data.frame:
newSLength <- data.frame(Sepal.Length = rnorm(100,
                                              mean(iris$Sepal.Length), 
                                              sd(iris$Sepal.Length)))

predictPLength <- predict(reg, newSLength)
predictPLength

### --- Results, more thoroughly
regSum <- summary(reg)
regSum

# Coefficient of determination
regSum$r.squared

# Residuals
regSum$residuals

# Coefficients
regSum$coefficients

# F-statistic
regSum$fstatistic

# Confidence Intervals
confint(reg, level=.95) # 95% CI
confint(reg, level=.99) # 99% CI
# 95% CI for slope only
confint(reg, "Sepal.Length", level=.95)
# 95% CI for intercept only
confint(reg, "(Intercept)", level=.95)

### --- Standardized regression coefficients {QuantPsych}
lm.beta(reg)

# Reminder: standardized regression coefficients are...
# What you would obtain upon performing linear regression over standardized variables
# z-score in R
zSLength <- scale(iris$Sepal.Length, center = T, scale = T) # computes z-score
zPLength <- scale(iris$Petal.Length, center = T, scale = T) # again; ?scale
# new dSet w. standardized variables
dSet <- data.frame(Sepal.Length <- zSLength,
                   Petal.Length <- zPLength)
# Linear Regression w. lm() over standardized variables
reg1 <- lm(Petal.Length ~ Sepal.Length, data=dSet) 
summary(reg1)
# compare
coefficients(reg1)[2] # beta from reg1
lm.beta(reg) # standardized beta w. QuantPsyc lm.beta from reg

## Linear Regression: Assumptions
## ═════════════════════════════════════════
# 1. Linearity
# 2. Constant variance == homoscedasticity
# 3. Independence of errors == no autocorrelation
# 4. No significant outliers or influential cases
# 5. Level of measurement: interval or ratio scale

#### Test 1: Linearity assumption
# Predictor vs Criterion {base}
reg <- lm(iris$Petal.Length ~ iris$Sepal.Length, 
          data = iris)
plot(iris$Sepal.Length, iris$Petal.Length,
     main = "Petal Length vs Sepal Length",
     xlab = "Sepal Length",
     ylab = "Petal Length")
abline(reg,col="red")

#### Test 2: Normality of Residuals
resStReg <- rstandard(reg) # get *standardized* residuals from reg
qqnorm(resStReg)
qqline(resStReg, col="red")

# Predicted vs. residuals {ggplot2}
predReg <- predict(reg) # get predictions from reg
resReg <- residuals(reg) # get residuals from reg
# resStReg <- rstandard(reg) # get residuals from reg
plotFrame <- data.frame(predicted = predReg,
                        residual = resReg)
# plot w. {ggplot2}
ggplot(data = plotFrame,
       aes(x = predicted, y = residual)) +
  geom_point(size = 1.5, colour = "black") +
  geom_point(size = 1, colour = "white") +
  geom_smooth(aes(colour = "blue"),
              method='lm',
              size = .25) + 
  ggtitle("Predicted vs Residual Lengths") + 
  xlab("Predicted Lengths") + ylab("Residual") + 
  theme_classic() +
  theme(legend.position = "none")

#### Test 3: Outliers and Influential Cases
# Detect influential cases w. influence.measures()
infMeas <- influence.measures(reg)
class(infMeas)
str(infMeas)
# as data.frame
infReg <- as.data.frame(influence.measures(reg)$infmat)

## Cook's Distance: Cook and Weisberg (1982):
# values greater than 1 are troublesome
wCook <- which(infReg$cook.d >1)
wCook # we're fine here

# Leverage: hat values
# Average Leverage = (k+1)/n, k - num. of predictors, n - num. observations
# Also termed: hat values, range: 0 - 1
# see: https://en.wikipedia.org/wiki/Leverage_%28statistics%29
# Various criteria (twice the leverage, three times the average...)
# Say, twice the leverage:
k <- 1 # number of predictors
n <- dim(iris)[1] # number of observations
wLev <- which(infReg$hat > 2*((k+1)/n))
wLev # hm...

## Influence plot
plotFrame <- data.frame(residual = resStReg,
                        leverage = infReg$hat,
                        cookD = infReg$cook.d)
ggplot(plotFrame,
       aes(y = residual,
           x = leverage)) +
  geom_point(size = plotFrame$cookD*100, shape = 1) +
  ggtitle("Influence Plot\nSize of the circle corresponds to Cook's distance") +
  theme(plot.title = element_text(size=8, face="bold")) +
  ylab("Standardized Residual") + xlab("Leverage")

#### Test 4: Durbin-Watson Test for auto-correlation of residuals
durbinWatsonTest(reg) # D-W Statistic < 1 --> problematic {car}
