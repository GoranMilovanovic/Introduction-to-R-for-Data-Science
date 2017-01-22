
## Introduction to R for Data Science
## ----------------------------------------------------------------------------------
## SESSION 07: PROBABILITY FUNCTIONS IN R
## ----------------------------------------------------------------------------------
## Autumn/Winter 2016.
## Data Science Serbia @Startit Centre, Savska 5, Belgrade, Serbia

## Lecturers
## ----------------------------------------------------------------------------------
# Goran S. Milovanović, Phd
# Data Science Mentor @Springboard, Data Science Serbia
# ing Branko Kovač, Data Scientist @Tradecore
# Data Science Mentor @ Springboard, Data Science Serbia
## ----------------------------------------------------------------------------------

## SESSION 07: PROBABILITY FUNCTIONS IN R
## ----------------------------------------------------------------------------------

# clear all
rm(list=ls())

#### Binomial Distribution
# A basic statistical experiment: tossing a coing that has a probability p for Heads;
# repeat the experiment N times

# Example:
# A person throws a fair dice ten times.
# What is the probability of obtaining five or less sixes at random?
# Let's see: the probability of getting exactly five sixes at random is:
pFiveSixes <- dbinom(5, size = 10, p = 1/6)
pFiveSixes
# The probability of getting five or less than five sixes 
# from ten statistical experiments is:
pFiveAndLessSixes <- sum(
  dbinom(0, size = 10, p = 1/6),
  dbinom(1, size = 10, p = 1/6),
  dbinom(2, size = 10, p = 1/6),
  dbinom(3, size = 10, p = 1/6),
  dbinom(4, size = 10, p = 1/6),
  dbinom(5, size = 10, p = 1/6)
)
pFiveAndLessSixes # Wow... but that's five AND less than five... :(

# or...
pFiveAndLessSixes <- sum(sapply(seq(0,5), function(x) {
  dbinom(x, size = 10, p = 1/6)
}))
pFiveAndLessSixes

# or, in a vectorized programming language
pFiveAndLessSixes <-sum(dbinom(seq(0,5), size = 10, p =1/6))
pFiveAndLessSixes

# We could have used a cummulative distribution function to figure out this as well:
pFiveAndLessSixes <- pbinom(5, size = 10, p = 1/6)
pFiveAndLessSixes

# Do not forget: the binomial distribution models a statistical experiment with
# two outcomes only. In the present example, its parameter, p = 1/6, has a complement
# of 1-p == 5/6, and the following semantics: either 5 comes out, OR everything else.
# The binomial distribution does not model a dice. It models a coin (fair or unfair).
# To model a dice, you need the multinomial distribution, which is the multivariate
# generalization of the binomial. We will cover only some univariate distributions here.

# Generate a sample of random binomial variates:
randomBinomials <- rbinom(100, size = 1, p = .5)
randomBinomials
randomBinomials <- rbinom(100, size = 100, p = .5)
randomBinomials # see the difference?
randomBinomials <- rbinom(100, size = 100000, p = .5)
length(randomBinomials)
hist(randomBinomials, n = 50,
     main = "100000 Statistical Experiments",
     xlab = "Number of heads",
     ylab = "Frequency",
     col = "deepskyblue")
# interpretion: we were running 100 statistical experiments,
# each time drawing a sample of 100000 observations of a fair coin (p = .5)
randomBinomials <- rbinom(100000, size = 100000, p = .5)
# interpretion: we were running 10000 statistical experiments,
# each time drawing a sample of 100000 observations of a fair coin (p = .5)
hist(randomBinomials, n = 50,
     main = "100000 Statistical Experiments",
     xlab = "Number of heads",
     ylab = "Frequency",
     col = "deepskyblue")

# Quantile Function of the Binomial distribution:
# The quantile is defined as the smallest value x such that F(x) ≥ p, 
# where F is the distribution function.
qbinom(p = .01, size = 100, prob = .5)
qbinom(p = .99, size = 100, prob = .5)
qbinom(p = .01, size = 200, prob = .5)
qbinom(p = .99, size = 200, prob = .5)

# Now take a look at this:
randomUniformNumbers <- runif(100000,0,1)
# this generated 100 uniformly distributed random numbers
# on an interval from 0 to 1
randomBinomialNumbers <- qbinom(randomUniformNumbers,
                                size = 100000,
                                prob = .5)
hist(randomBinomialNumbers, n = 50,
     main = "100000 Statistical Experiments",
     xlab = "Number of heads",
     ylab = "Frequency",
     col = "red")

# In other words, and for all those of you who will be interested in
# running simulations of stochastic models anytime in the future:
# If you can generate random numbers from a uniform distribution on (0,1)
# you can also generate random numbers from the binomial distribution - or any other
# distribution as well.
# However, R offers random number generators 
# for a huge number of probability densities...

#### Poisson Distribution
# A discrete probability distribution with mean and variance correlated...
# This is the distribution of the number of occurrences
# of independent events in a given interval.

# Example (following and adapting from Ladislaus Bortkiewicz, 1898)
# On the average, 10 soldiers in the Prussian army 
# were killed accidentally by horse kick monthly.
# What is the probability that 17 or more soldies
# in the Prussian army will be accidently killed by horse kicks
# during the month? Let's see:
tragedies <- ppois(17, lambda=10, lower.tail=FALSE)   # upper tail
tragedies
# lambda is the mean of the Poisson distribution and its only parameter
# NOTE: the lower.tail=FALSE argument converts from the Cumulative to the
# Decumulative distribution (also known as a Survivor function)
# i.e. P(X > x), for decumulative, in contrast to P(X <= x), for cumulative probability
# Compare:
tragedies <- ppois(17, lambda=10, lower.tail=TRUE)   # lower tail
tragedies # this is the answer to the question of what would be the probability of
# 17 and *less than 17* deaths

# The same logic to generate random deviates as we have observed in the Binomial case:
poiss100000 <- rpois(100000,lambda = 5)
hist(poiss100000)

# An important property of the Poisson distribution:
lambda <- seq(1,100,1)
poissonMean <- unlist(lapply(lambda, function(x) {
  mean(rpois(100000,x))
}))
poissonVar <- unlist(lapply(lambda, function(x) {
  var(rpois(100000,x))
}))
# plot!
plot(poissonMean, poissonVar,
     main = "An important property of the Poisson",
     xlab = "Mean", ylab = "Variance")
# The moral of the story:
# use the Poisson distribution to model phenomena
# where you expect the mean to be correlated with the variance.

#### And now for something completelly different: a continuous distribution
#### The Gaussian (Normal) Distribution

# the same story:
# What is the probability that a person is 185 cm tall if we draw a person 
# at random from a population with mean height = 178 cm, standard deviation = 15 cm...
p185cm <- dnorm(185, mean = 178, sd = 15) #?
p185cm
# No, it is not 0.02385223... that is probability density
# The normal distribution is continuous: it doesn't really make sense
# to ask for a probability of a point from its domain.

# Try this:
# What is the probability that a person is between 180 cm and 185 cm tall
# if we draw a person at random from a population with 
# mean height = 178 cm, standard deviation = 15 cm?
p180_185cm <- dnorm(185, mean = 178, sd = 15) - dnorm(180, mean = 178, sd = 15)
p180_185cm # oops...

# Maybe:
p180_185cm <- pnorm(185, mean = 178, sd = 15, lower.tail = T) - 
  pnorm(180, mean = 178, sd = 15, lower.tail = T)
p180_185cm # yes, that is correct: 0.1265957

# Gaussian, the bell curve, the famous one:
x <- seq(0,150,.01)
# set plot parameters
par(mfrow=c(1,2))
curve(dnorm(x,mean=75,sd=15),from = 0, to = 150, main = "Gaussian Density")
curve(pnorm(x,mean=75,sd=15),from = 0, to = 150, main = "Gaussian Cumulative")

### Checking whether a variable has a normal distribution
## check whether the assumption of normality holds: Sepal Length

library(datasets)
data(iris)

# The Kolmogorov-Smirnov Test
ksSLength <- ks.test(iris$Sepal.Length,
                     "pnorm",
                     mean(iris$Sepal.Length),
                     sd(iris$Sepal.Length),
                     alternative = "two.sided",
                     exact = NULL)

ksSLength # KS test says this is a normal distribution
ksSLength

## AGAIN: check whether the assumption of normality holds: Sepal.Lenght
# Shapiro-Wilk Test :: better for small samples + for Normal distribution only!
# NOTE: The most powerful statistical test given for a given alpha and compared
# to other tests similar in purpose!
swPLength <- shapiro.test(iris$Sepal.Length)
swPLength # SW test says it is not normally distributed...

# Take-home message: good luck with normality tests...

# plots to check the normality assumption
theoNormSLength <- qnorm(ppoints(iris$Sepal.Length),
                         mean(iris$Sepal.Length),
                         sd(iris$Sepal.Length))
# ?ppoints - produces evenly spaced probability points
# set plot parameters
par(mfrow=c(1,1))
# qqplot
qqplot(theoNormSLength,iris$Sepal.Length,
       main = "Q-Q Plot: Sepal Length",
       xlab = "Theoretical Quantiles",
       ylab = "Empirical Quantiles",
       xlim = c(min(theoNormSLength),max(theoNormSLength)),
       ylim = c(min(theoNormSLength),max(theoNormSLength)),
       pch = 19,
       cex.lab = .65,
       cex.main = .7
)
abline(0,1,col="red")

#### The Chi-Square Distribution and the related statistical test
# Theory: say X follows a Standard Normal Distribution
# Standard Normal Distribution: Mean = 0, Variance = Standard Deviation = 1
# Take m = 3 such variables, square them, sum up the squares, 
# and repeat the experiment 100,000 times
stdNormals3 <- unlist(lapply(seq(1,100000), function(x) {
  sum((rnorm(3, mean = 1, sd = 1))^2)
}))
# how are these sums of standar normal distributions distributed?
# set plot parameters
par(mfrow=c(1,2))
hist(stdNormals3, 50, main = "m = 3")
# repeat with m = 30
stdNormals30 <- unlist(lapply(seq(1,100000), function(x) {
  sum((rnorm(30, mean = 1, sd = 1))^2)
}))
hist(stdNormals30, 50, main = "m = 30")
# The sum of squared IID random variables, each ~ standard norm(al distribution
# has a Chi-Square distribution
par(mfrow=c(1,2))
curve(dchisq(x,3), from = 0, to = 40, main = "m = 3")
curve(dchisq(x,30), from = 0, to = 120, main = "m = 30")
# This probability distribution plays 
# an important role in statistical hypothesis testing

### The Chi-Square Goodness-of-Fit (GoF) Test
# Assume the following: an urn contains white, blue, and red balls
# in proportion of 5:3:2.
# We draw a sample of size n = 100 balls from the urn.
n <- 100
# Our task is to determine whether the sample reflects the hypothesized distribution
# of balls in the urn.

# Step 1: Population parameters (probabilities)
populationP <- c(.5, .3, .2)
expectedCounts <- n*populationP
expectedCounts
# Step 2: Sampling
# random draw from a multinomial distribution of three events:
sample <- as.numeric(rmultinom(1, 100, prob = populationP))
sample
# Chi-Square Statistic:
chiSq <- sum(((sample - expectedCounts)^2)/expectedCounts)
chiSq
df <- 3 - 1 # k == 3 == number of events
df
sig <- pchisq(chiSq, df, lower.tail=F)
sig
sig < .05
# Chi-Square test NOT significant: the empirical distribution follows
# the theoretical distribution (i.e. suggests that the expected counts 
# are NOT different from the observed)

# again, with a sample from a different distribution:
# random draw from a multinomial distribution of three events:
sample <- as.numeric(rmultinom(1, 100, prob = c(.3, .3, .4)))
sample
# Chi-Square Statistic:
chiSq <- sum(((sample - expectedCounts)^2)/expectedCounts)
chiSq
df <- 3 - 1 # k == 3 == number of events
df
sig <- pchisq(chiSq, df, lower.tail=F)
sig
sig < .05
# Chi-Square test significant: the empirical distribution does not follow
# the theoretical distribution (i.e. suggests that the expected counts are 
# different from the observed)