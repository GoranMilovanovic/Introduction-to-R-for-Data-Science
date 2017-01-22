
## Introduction to R for Data Science
## -----------------------------------------------------------------------------------
## SESSION 08: PROBABILITY FUNCTIONS IN R
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

## SESSION 08: CORRELATION
## --------------------------------------------

## Linear Correlation: Assumptions
## --------------------------------------------
# To be discussed in greater detail during S10: Simple Linear Regression
# 1. Linearity
# 2. Constant variance == homoscedasticity
# 3. Independence of errors == no autocorrelation
# 4. No significant outliers or influential cases
# 5. Level of measurement: interval or ratio scale

## Clear, Libraries, Data Sets
## --------------------------------------------

# clear all
rm(list=ls())

# libraries
library(datasets)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ppcor)
library(Hmisc)

# data
data(iris)
### Iris data set description:
# https://stat.ethz.ch/R-manual/R-devel/library/iriss/html/iris.html

## EDA for Sepal.Length and Petal.Length
## --------------------------------------------

### EDA plots
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
# reset plot layout
par(mfcol = c(1,1))

## Scatter plot
## --------------------------------------------

## scatter plot w. {base}
plot(iris$Sepal.Length, iris$Petal.Length,
     main = "Sepal Length vs Petal Length",
     xlab = "Sepal Length", ylab = "Petal Length",
     cex.main = .85,
     cex.lab = .75)

ggplot(data = iris, aes(x = Sepal.Length,
                        y = Petal.Length,
                        color = Species)
) + 
  geom_point() + 
  geom_smooth(method = lm, se = F) +
  theme_classic()

ggplot(data = iris, aes(x = Sepal.Length,
                        y = Petal.Length)) + 
  geom_point(color = "darkblue", size = 1.5) + 
  geom_point(color = "white", size = 1) +
  geom_smooth(method = lm, se = F) +
  theme_classic()

## Covariance
## --------------------------------------------

cov(iris$Sepal.Length, iris$Petal.Length)

## Correlation == Covariance of Standardized Variables
## ---------------------------------------------------

zSepalLength <- (iris$Sepal.Length-mean(iris$Sepal.Length))/sd(iris$Sepal.Length)
zPetalLength <- (iris$Petal.Length-mean(iris$Petal.Length))/sd(iris$Petal.Length)
cov(zSepalLength, zPetalLength)

## Z-Scores
## --------------------------------------------

zSepalLength1 <-  scale(iris$Sepal.Length, center = T, scale = T)
sum(zSepalLength1 == zSepalLength) == length(zSepalLength)

## Correlation
## --------------------------------------------

cor(iris[, c(1:4)])

## Correlation Matrix
## --------------------------------------------

dSet <- iris
# Remove one nominal variable - Species
dSet$Species <- NULL
# introduce NA in dSet$Sepal.Length[5]
dSet$Sepal.Length[5] <- NA
# Pairwise and Listwise Deletion:
cor1a <- cor(dSet,use="complete.obs") # listwise deletion
cor1a

## Pairwise Deletion
## --------------------------------------------

cor1b <- cor(dSet, use = "pairwise.complete.obs") # pairwise deletion
cor1b

## Deletion: use = "all.obs"
## --------------------------------------------

cor1c <- cor(dSet, use = "all.obs") # all observations - error
cor1c

## Deletion: default
## --------------------------------------------

cor1d <- cor(dSet, use = "everything") # default
cor1d

## Visualizing Correlation Matrices
## --------------------------------------------

# {base} approach
data("mtcars")
str(mtcars)

corMatrix <- cor(mtcars[, 1:8])
plot(as.data.frame(corMatrix))

# {corrplot} approach
corMatrix <- cor(mtcars)
# {corrplot} "circle" method: 
corrplot(corMatrix, 
         method="circle")

# {corrplot} "ellipse" method: 
corrplot(corMatrix, 
         method="ellipse")

# "mixed"
corrplot.mixed(corMatrix, 
               lower="ellipse", 
               upper="circle")

## Significance testing for correlation
## --------------------------------------------

cor.test(iris$Sepal.Length, iris$Petal.Length)

## Significance testing for correlation w. {Hmisc}
## --------------------------------------------

dSet <- as.matrix(iris[, c(1:4)])
cor2 <- rcorr(dSet, 
              type="pearson")
cor2$r # correlations

cor2$P # significant at

cor2$n # num. observations


## Spearman's rank-order correlation
## --------------------------------------------

cor2b <- rcorr(as.matrix(dSet),
               type="spearman") # NOTE: as.matrix
cor2b

## Partial Correlation
## --------------------------------------------

# Residuals
# Scatter: Sepal.Length vs. Petal.Length

linFit <- lm(data = iris,
             Petal.Length ~ Sepal.Length)
linFitPlot <- data.frame(
  x = iris$Sepal.Length,
  y = iris$Petal.Length,
  predicted = linFit$fitted.values,
  residuals = linFit$residuals
)
ggplot(data = linFitPlot,
       aes(x = x, y = y)) +
  geom_smooth(method = lm, se = F, color = "blue", size = .25) +
  geom_segment(aes(x = x, y = predicted, 
                   xend = x, yend = predicted+residuals),
               color = "blue", size = .2) +
  geom_point(aes(x = x, y = y), color = "blue", size = 1.25) +
  geom_point(aes(x = x, y = y), color = "white", size = 1) +
  geom_point(aes(x = x, y = predicted), color = "blue", size = 1) +
  xlab("Sepal.Length") + ylab("Petal.Length") +
  theme_classic()

# Residuals
# Scatter: Sepal.Width vs. Sepal.Length + Sepal.Width vs. Petal.Length

linFit <- lm(data = iris,
             Sepal.Length ~ Sepal.Width)
linFitPlot1 <- data.frame(
  x = iris$Sepal.Width,
  y = iris$Sepal.Length,
  predicted = linFit$fitted.values,
  residuals = linFit$residuals
)
linFit <- lm(data = iris,
             Petal.Length ~ Sepal.Width)
linFitPlot2 <- data.frame(
  x = iris$Sepal.Width,
  y = iris$Petal.Length,
  predicted = linFit$fitted.values,
  residuals = linFit$residuals
)
linFitPlot <- rbind(linFitPlot1, linFitPlot2)
linFitPlot$Plot <- factor(c(rep("Sepal.Length",150), rep("Petal.Length",150)),
                          levels = c("Sepal.Length", "Petal.Length"))

ggplot(data = linFitPlot,
       aes(x = x, y = y)) +
  geom_smooth(method = lm, se = F, color = "blue", size = .25) +
  geom_segment(aes(x = x, y = predicted, 
                   xend = x, yend = predicted+residuals),
               color = "blue", size = .2) +
  geom_point(aes(x = x, y = y), color = "blue", size = 1.25) +
  geom_point(aes(x = x, y = y), color = "white", size = 1) +
  geom_point(aes(x = x, y = predicted), color = "blue", size = 1) +
  xlab("Sepal.Width") + ylab("") +
  theme_classic() +
  facet_grid(. ~ Plot) +
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 6)) + 
  theme(axis.text.y = element_text(size = 6)) 


## What is Partial Correlation?
## --------------------------------------------

partialCor <- cor(linFitPlot$residuals[1:150],  # Sepal.Length residuals
                  linFitPlot$residuals[151:300] # Petal.Length residuals
)
partialCor

cor(iris$Sepal.Length, iris$Petal.Length)

## Partial Correlation w. {ppcor}
## --------------------------------------------

dataSet <- iris
dataSet$Species <- NULL
partialCor1 <- pcor.test(dataSet$Sepal.Length, dataSet$Petal.Length,
                         dataSet$Sepal.Width,
                         method = "pearson")
partialCor1$estimate

partialCor1$p.value

partialCor1$statistic

## Matrix of Partial Correlations w. {ppcor}
## --------------------------------------------
dataSet <- iris
dataSet$Species <- NULL
irisPCor <- pcor(dataSet, method="pearson")
irisPCor$estimate # partial correlations

irisPCor$p.value # results of significance tests

irisPCor$statistic
# NOTE: t-test on n-2-k degrees of freedom ; k = num. of variables conditioned

## Part (semi-partial) Correlation: what is it?
## --------------------------------------------

partCor <- cor(iris$Sepal.Length,  # Sepal.Length in itself
               linFitPlot$residuals[151:300] # Petal.Length residuals
)
partCor

## Part (semi-[partial]) Correlation w. {ppcor}
## --------------------------------------------

partCor <- spcor.test(dataSet$Sepal.Length, dataSet$Petal.Length,
                      dataSet$Sepal.Width,
                      method = "pearson")
# NOTE: this is a correlation of dataSet$Sepal.Length w. dataSet$Petal.Length
# when the variance of dataSet$Petal.Length (2nd variable) due to
# dataSet$Sepal.Width is removed!
partCor$estimate

partCor$p.value

partCor$statistic

## Matrix of Part (semi-[partial]) Correlations w. {ppcor}
## --------------------------------------------

irisSPCor <- spcor(dataSet, method = "pearson")

irisSPCor$estimate

irisSPCor$p.value

irisSPCor$statistic
