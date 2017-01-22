
## Introduction to R for Data Science
## -----------------------------------------------------------------------------------
## SESSION 10: MULTIPLE LINEAR REGRESSION
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

## SESSION 10: MULTIPLE LINEAR REGRESSION
## --------------------------------------------

## Linear Regression: Assumptions
## --------------------------------------------
# 1. Linearity
# 2. Constant variance == homoscedasticity
# 3. Independence of errors == no autocorrelation
# 4. No significant outliers or influential cases
# 5. Level of measurement: interval or ratio scale

# clear
rm(list=ls())

#### read data
library(datasets)
library(broom)
library(ggplot2)
library(lattice)
library(QuantPsyc)
library(car)
library(ppcor)

#### load
data(iris)
str(iris)

#### simple linear regression: Sepal Length vs Petal Lenth
# Predictor vs Criterion {ggplot2}
ggplot(data = iris,
       aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(size = 2, colour = "black") +
  geom_point(size = 1, colour = "white") +
  geom_smooth(aes(colour = "black"),
              method='lm') +
  ggtitle("Sepal Length vs Petal Length") +
  xlab("Sepal Length") + ylab("Petal Length") +
  theme(legend.position = "none")

# What is wrong here?
# let's see...
reg <- lm(Petal.Length ~ Sepal.Length, data=iris) 
summary(reg)
# Hm, everything seems fine to me...

# And now for something completelly different (but in R)...

#### Problems with linear regression in iris
# Predictor vs Criterion {ggplot2} - group separation
ggplot(data = iris, 
       aes(x = Sepal.Length,
           y = Petal.Length,
           color = Species)) + 
  geom_point(size = 2) +
  ggtitle("Sepal Length vs Petal Length") +
  xlab("Sepal Length") + ylab("Petal Length")

# Predictor vs Criterion {ggplot2} - separate regression lines
ggplot(data = iris, 
       aes(x = Sepal.Length,
           y = Petal.Length,
           colour=Species)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  ggtitle("Sepal Length vs Petal Length") +
  xlab("Sepal Length") + ylab("Petal Length")

### Ooops...
### overview and considerations
plot(iris[,c(1,3,5)],
     main = "Inspect: Sepal vs. Petal Length \nfollowing the discovery of the Species...",
     cex.main = .75,
     cex = .6)
### better... {lattice}
xyplot(Petal.Length ~ Sepal.Length | Species, # {latice} xyplot
       data = iris,
       xlab = "Sepal Length", ylab = "Petal Length"
)

# Petal Length and Sepal Length: Conditional Densities
densityplot(~ Petal.Length | Species, # {latice} xyplot
            data = iris,
            plot.points=FALSE,
            xlab = "Petal Length", ylab = "Density",
            main = "P(Petal Length|Species)",
            col.line = 'red'
)
densityplot(~ Sepal.Length | Species, # {latice} xyplot
            data = iris,
            plot.points=FALSE,
            xlab = "Sepal Length", ylab = "Density",
            main = "P(Sepal Length|Species)",
            col.line = 'blue'
)

# Linear regression in subgroups
species <- unique(iris$Species)
w1 <- which(iris$Species == species[1]) # setosa
reg <- lm(Petal.Length ~ Sepal.Length, data=iris[w1,]) 
tidy(reg)
w2 <- which(iris$Species == species[2]) # versicolor
reg <- lm(Petal.Length ~ Sepal.Length, data=iris[w2,]) 
tidy(reg)
w3 <- which(iris$Species == species[3]) # virginica
reg <- lm(Petal.Length ~ Sepal.Length, data=iris[w3,]) 
tidy(reg)

#### Dummy Coding: Species in the iris dataset
is.factor(iris$Species)
levels(iris$Species)
reg <- lm(Petal.Length ~ Species, data=iris) 
tidy(reg)
glance(reg)
# Never forget what the regression coefficient for a dummy variable means:
# It tells us about the effect of moving from the baseline towards the respective reference level!
# Here: baseline = setosa (cmp. levels(iris$Species) vs. the output of tidy(reg))
# NOTE: watch for the order of levels!
levels(iris$Species) # Levels: setosa versicolor virginica
iris$Species <- factor(iris$Species, 
                       levels = c("versicolor", 
                                  "virginica",
                                  "setosa"))
levels(iris$Species)
# baseline is now: versicolor
reg <- lm(Petal.Length ~ Species, data=iris) 
tidy(reg) # The regression coefficents (!): figure out what has happened!

### another way to do dummy coding
rm(iris); data(iris) # ...just to fix the order of Species back to default
levels(iris$Species)
contrasts(iris$Species) = contr.treatment(3, base = 1)
contrasts(iris$Species) # this probably what you remember from your stats class...
iris$Species <- factor(iris$Species, 
                       levels = c ("virginica","versicolor","setosa"))
levels(iris$Species)
contrasts(iris$Species) = contr.treatment(3, base = 1)
# baseline is now: virginica
contrasts(iris$Species) # consider carefully what you need to do

### Petal.Length ~ Species (Dummy Coding) + Sepal.Length 
rm(iris); data(iris) # ...just to fix the order of Species back to default
reg <- lm(Petal.Length ~ Species + Sepal.Length, data=iris)
# BTW: since is.factor(iris$Species)==T, R does the dummy coding in lm() for you
regSum <- summary(reg)
regSum$r.squared
regSum$coefficients
# compare w. Simple Linear Regression
reg <- lm(Petal.Length ~ Sepal.Length, data=iris) 
regSum <- summary(reg)
regSum$r.squared
regSum$coefficients

### Comparing nested models
reg1 <- lm(Petal.Length ~ Sepal.Length, data=iris)
reg2 <- lm(Petal.Length ~ Species + Sepal.Length, data=iris) # reg1 is nested under reg2
# terminology: reg2 is a "full model"
# this terminology will be used quite often in Logistic Regression

# NOTE: Nested models
# There is a set of coefficients for the nested model (reg1) such that it
# can be expressed in terms of the full model (reg2); in our case it is simple 
# HOME: - figure it out.

anova(reg1, reg2) # partial F-test; Species certainly has an effect beyond Sepal.Length
# NOTE: for partial F-test, see:
# http://pages.stern.nyu.edu/~gsimon/B902301Page/CLASS02_24FEB10/PartialFtest.pdf

# Influence Plot
regFrame <- augment(reg2)
## Influence plot
# influnce data
infReg <- as.data.frame(influence.measures(reg)$infmat)
# data.frame for ggplot2
plotFrame <- data.frame(residual = regFrame$.std.resid,
                        leverage = regFrame$.hat,
                        cookD = regFrame$.cooksd)
# plot w. ggplot2
ggplot(plotFrame,
       aes(y = residual,
           x = leverage)) +
  geom_point(size = plotFrame$cookD*100, shape = 1) +
  ggtitle("Influence Plot\nSize of the circle corresponds to Cook's distance") +
  theme(plot.title = element_text(size=8, face="bold")) +
  ylab("Standardized Residual") + xlab("Leverage")

#### Multiple Regression - by the book
# Following: http://www.r-tutor.com/elementary-statistics/multiple-linear-regression
# (that's from your reading list, to remind you...)
data(stackloss)
str(stackloss)
# Data set description
# URL: https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/stackloss.html
# Air Flow represents the rate of operation of the plant. 
# Water Temp is the temperature of cooling water circulated through coils in the absorption tower. 
# Acid Conc. is the concentration of the acid circulating.
# stack.loss (the dependent variable) is 10 times the percentage of the ingoing ammonia to 
# the plant that escapes from the absorption column unabsorbed;
# that is, an (inverse) measure of the over-all efficiency of the plant.
stacklossModel = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., 
                    data=stackloss)

# let's see:
summary(stacklossModel)
glance(stacklossModel) # {broom}
tidy(stacklossModel) # {broom}

# predict new data
obs = data.frame(Air.Flow=72, Water.Temp=20, Acid.Conc.=85)
predict(stacklossModel, obs)

# confidence intervals
confint(stacklossModel, level=.95) # 95% CI
confint(stacklossModel, level=.99) # 99% CI
# 95% CI for Acid.Conc. only
confint(stacklossModel, "Acid.Conc.", level=.95)

# default regression plots in R
plot(stacklossModel)

# multicolinearity
# John Fox's {car} package:
VIF <- vif(stacklossModel)
VIF
sqrt(VIF)
# Variance Inflation Factor (VIF)
# The increase in the ***variance*** of an regression ceoff. due to colinearity
# NOTE: sqrt(VIF) = how much larger the ***SE*** of a reg.coeff. vs. what it would be
# if there were no correlations with the other predictors in the model
# NOTE: lower_bound(VIF) = 1; no upper bound; VIF > 2 --> (Concerned == TRUE)
Tolerance <- 1/VIF # obviously, tolerance and VIF are redundant
Tolerance
# NOTE: you can inspect multicolinearity in the multiple regression mode
# by conducting a Principal Component Analysis over the predictors;
# when the time is right.

#### R for partial and part (semi-partial) correlations
# NOTE: In multiple regression, this is the semi-partial (or part) correlation
# that you need to inspect:
# assume a model with X1, X2, X3 as predictors, and Y as a criterion
# You need a semi-partial of X1 and Y following the removal of X2 and X3 from Y
# It goes like this: in Step 1, you perform a multiple regression Y ~ X2 + X3;
# In Step 2, you take the residuals of Y, call them RY; in Step 3, you regress (correlate)
# RY ~ X1: the correlation coefficient that you get from Step 3 is the part correlation
# that you're looking for.

# Recall our model...
stacklossModel = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                    data=stackloss)
str(stackloss)
summary(stacklossModel)

# What is the semi-partial (part correlation) of stack.loss and Air.Flow? 
spartCor1 <- spcor.test(x = stackloss$Air.Flow, 
                        y = stackloss$stack.loss,
                        z = stackloss[, c("Water.Temp", "Acid.Conc.")],
                        method = "pearson")
spartCor1

# The unique contribution of Air.Flow:
spartCor1$estimate
spartCor1$p.value
spartCor1$statistic

# Or, you can do:
spartCor2 <- spcor(stackloss, method = "pearson")
spartCor2$estimate
spartCor2$p.value
# NOTE: using spcor() from {ppcor}
# Convention: variance is removed from the SECOND variable
# And the second variables is read from the output's COLUMNS!

