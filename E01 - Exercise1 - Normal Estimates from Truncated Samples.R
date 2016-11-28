
## Introduction to R for Data Science
## ══════════════════════════════════════════════════════════════════════════════════
## SESSION 01-03: EXERCISE 01
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

## SESSION 01-03: EXERCISE 1
## -- Truncating samples from the Normal Distribution
## ══════════════════════════════════════════════════════════════════════════════════

## -- clear all
rm(list=ls())

## -- ggplot2
library(ggplot2)

## -- Assume Normal Distribution in the Population.
## -- Assume we know the true population mean and variance:
mu <- 5
sigma <- 1.25
sdev <- sqrt(sigma)
sdev

## -- let's plot this
curve(dnorm(x,mu,sdev),
      from = 0,
      to = 10, n = 1000,
      col = "red",
      main = "In the Population: Normal",
      ylab = "Density",
      cex.main = .85)
# in R: curve() is for ploting functions: ?curve
# dnorm is for Normal (Gaussian) Density: ?dnorm
# , and then: ?pnorm, ?rnorm, ?qnorm

## -- Good. Let's draw some samples now:
s1 <- rnorm(100000, mu, sdev)
# Estimates?
mean(s1)
sd(s1)
var(s1) # not so bad...

## --  Ok. Now...
estimates <- data.frame(
  mean = sapply(seq(1:1000), function(x){
    mean(rnorm(100000,mu,sdev))
    }),
  var = 
    sapply(seq(1:1000), function(x){
      var(rnorm(100000,mu,sdev))
    })
)

## --  How biased are these estimates?
mean_Bias <- mean(estimates$mean) - mu
mean_Bias
var_Bias <- mean(estimates$var) - sigma
var_Bias

# Ok. Let's see what happens when we truncate the samples from
# this normal distribution before estimation:

## --  Ok. Now... truncating!
estimatesTrunc <- data.frame(
  mean = sapply(seq(1:1000), function(x){
    rSample <- rnorm(100000, mu, sdev)
    quants <- quantile(rSample,c(.1,.9))
    rSample <- rSample[which(rSample>quants[1] & rSample<quants[2])]
    mean(rSample)
  }),
  var = sapply(seq(1:1000), function(x){
    rSample <- rnorm(100000, mu, sdev)
    quants <- quantile(rSample,c(.1,.9))
    rSample <- rSample[which(rSample>quants[1] & rSample<quants[2])]
    var(rSample)
    })
  )

# And how biased are these estimates?
mean_BiasTrunc <- mean(estimatesTrunc$mean) - mu
mean_BiasTrunc
var_BiasTrunc <- mean(estimatesTrunc$var) - sigma
var_BiasTrunc

# Let's compare:
mean_Bias < mean_BiasTrunc
var_Bias < var_BiasTrunc
# The difference?
mean_Bias - mean_BiasTrunc
var_Bias - var_BiasTrunc # ooops

# How about the variance of the estimates?
var(estimates$mean)/var(estimatesTrunc$mean)
var(estimates$var)/var(estimatesTrunc$var) # oh oh oh...

## --  Let' see: histogram()
par(mfcol=c(1,2))
hist(estimates$var,50,
     main = "Variance Estimates\n(sigma = 1.25, 50 bins)",
     cex.main = .85,
     xlab = "Variance",
     xlim=c(1.2,1.3))
hist(estimatesTrunc$var,50,
     main = "Truncated Variance Estimates\n(sigma = 1.25, 50 bins)",
     cex.main = .85,
     xlab = "Variance",
     xlim=c(.5,.6))
par(mfcol=c(1,1))

## -- Another statistical experiment

# Step 1 : Keep population sd = 1.5, vary population mean:
sampleMeansTrunc <- sapply(seq(5,50,5), function(x) {
  rSample <- rnorm(100000,x,1.5)
  quants <- quantile(rSample,c(.1,.9))
  rSample <- rSample[which(rSample>quants[1] & rSample<quants[2])]
  mean(rSample)
  })
plotFrameMeans <- data.frame(theoretical = seq(5,50,5),
                        empirical = sampleMeansTrunc)
plotFrameMeans
ggplot(plotFrameMeans, aes(x = empirical, y = theoretical)) +
  geom_path(color="blue") +  geom_point(colour="black", size = 2.5) + 
  geom_point(colour="white", size = 2.25) +
  xlim(0,65) + ylim(0,65) +
  ggtitle("Empirical vs. Theoretical Std.Deviatons\nSample Truncated: q10 < x < q90") +
  theme(title = element_text(size = 10))

# Step 2: Now keep population mean = 15, vary population variance:
sampleVarsTrunc <- sapply(seq(1,20,1), function(x) {
  rSample <- rnorm(100000,15,x)
  quants <- quantile(rSample,c(.1,.9))
  rSample <- rSample[which(rSample>quants[1] & rSample<quants[2])]
  var(rSample)
})
plotFrameVars <- data.frame(population = seq(1,20,1)^2,
                        estimated = sampleVarsTrunc)
plotFrameVars
ggplot(plotFrameVars, aes(y = estimated, x = population)) +
  geom_line(aes(x = plotFrameVars$population, y = plotFrameVars$population),
            color = "red") +
  geom_path(color="blue") +  geom_point(colour="black", size = 2.5) + 
  geom_point(colour="white", size = 2.25) +
  xlim(0,405) + ylim(0,405) +
  ggtitle("Empirical vs. Theoretical Std.Deviatons\nSample Truncated: q10 < x < q90") +
  theme(title = element_text(size = 10))

## -- What if we truncate -3SD < x < +3SD in Step 2?
sampleVarsTrunc <- sapply(seq(3,20,1), function(x) {
  rSample <- rnorm(100000,15,x)
  quants <- c(mean(rSample)-3*sd(rSample), mean(rSample)+3*sd(rSample))
  rSample <- rSample[which(rSample>quants[1] & rSample<quants[2])]
  var(rSample)
})
plotFrameVars <- data.frame(population = seq(3,20,1)^2,
                            estimated = sampleVarsTrunc)
plotFrameVars
ggplot(plotFrameVars, aes(y = estimated, x = population)) +
  geom_line(aes(x = plotFrameVars$population, y = plotFrameVars$population),
            color = "red") +
  geom_path(color="blue") + 
  geom_point(colour="black", size = 2.5) + 
  geom_point(colour="white", size = 2.25) +
  xlim(0,410) + ylim(0,410) +
  ggtitle("Population vs. Sample Variance\nSample Truncated: -3SD < x < +3SD") +
  theme(title = element_text(size = 10))

## -- What if we truncate -2SD < x < +2SD in Step 2?
sampleVarsTrunc <- sapply(seq(3,20,1), function(x) {
  rSample <- rnorm(100000,15,x)
  quants <- c(mean(rSample)-2*sd(rSample), mean(rSample)+2*sd(rSample))
  rSample <- rSample[which(rSample>quants[1] & rSample<quants[2])]
  var(rSample)
})
plotFrameVars <- data.frame(population = seq(3,20,1)^2,
                            estimated = sampleVarsTrunc)
plotFrameVars
ggplot(plotFrameVars, aes(y = estimated, x = population)) +
  geom_line(aes(x = plotFrameVars$population, y = plotFrameVars$population),
            color = "red") +
  geom_path(color="blue") +  geom_point(colour="black", size = 2.5) + 
  geom_point(colour="white", size = 2.25) +
  xlim(0,410) + ylim(0,410) +
  ggtitle("Population vs. Sample Variance\nSample Truncated: -2SD < x < +2SD") +
  theme(title = element_text(size = 10))
