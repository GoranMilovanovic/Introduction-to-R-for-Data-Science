
## Introduction to R for Data Science
## ══════════════════════════════════════════════════════════════════════════════════
## SESSION 01: INTRODUCTION TO R FROM RSTUDIO IDE
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

## SESSION 01: INTRODUCTION TO R FROM RSTUDIO IDE
## ══════════════════════════════════════════════════════════════════════════════════

# clear workspace
rm(list=ls())
# try this out:
ls()

# R as a calculator
2+2
2/3-1+5
a <- 1/3
b = 7
a + b

# what is my working directory?
getwd()

# save R object as .rds file
saveRDS(a, "a.rds")
a <- readRDS("a.rds")

# save several R objects as .RData file
save(list = c("a", "b"), file = "obj.RData")

# remove a, b
rm(a,b)
getwd()
# load .RDatafile to R
load(file = "obj.RData")

# list of loaded objects
ls()

# remove all objects
rm(list=ls())

# installing packages:
# install.packages("dplyr") # replace "dplyr" with other package name
installed.packages() # list of installed packages

# The shape of things to come:
numbers <- c(1,2,3,4,5)
words <- c("data", "science", "r")
boolean <- c(TRUE, FALSE)
data("mtcars")
head(mtcars,5)

# define a function in R:

factorial <- function(x) {
  if (x == 0 | x == 1) {
    return(1)
  }
  else {
    return(x*factorial(x-1))
  }
}

# calls to factorial():
factorial(5)
factorial(10)
factorial(100)

# a vector of type numeric obtained from a sequence:
x <- seq(0,30,1)

# applying the non-vectorized factorial() function to a vector x:
xFactorials <- unlist(lapply(x, function(x) {factorial(x)}))

# a {base} R plot
plot(x,xFactorials,
     xlab = "Integers",
     ylab = "Factorials",
     main = "Insane Plot",
     cex.main = .85)
lines(x,xFactorials, col="red")

# R is a vector programming language. All programming languages dream of becoming
# vector languages one day. For example, to do vector programming in Python, you first need
# a package called NumPy to turn it into MATLAB, then a package named Pandas to turn it into R.
# Just kidding :) - we love Python too and have the highest of respect only for the
# Python community in Data Science. But R is a "natural" vector programming language:

# create 100 random deviates from the Uniform on [0,1]
x <- runif(100,0,1)

# ask for *each element in x* whether it is >= .5 or not:
x>=.5

# Where's the function? Isn't >= an operator?
# R is a functional programming language too. Thus:
logical1 <- '>='(x,.5)
logical2 <- x>=.5
which(logical1 != logical2) # none: integer(0)

# Back to vectorized functions in R; similarly, we have:
x <- seq(2,100,2)
x2 <- sqrt(x)
x2

# PCA
library(psych)
library(FactoMineR)
library(ggplot2)
dataSet <- bfi[,1:25]
dim(dataSet)
# replace NA w. respective column means
for(i in 1:ncol(dataSet)){
  dataSet[is.na(dataSet[,i]), i] <- mean(dataSet[,i], na.rm = TRUE)
}
colnames(dataSet)

# plot correlation matrix
library(corrplot)
corrplot(cor(dataSet),
         tl.col='black',
         tl.cex=.75)

# perform PCA w. PCA() from {factoMineR}
pcaSolution <- PCA(dataSet,
                   ncp=5,
                   scale.unit=F,
                   graph=FALSE)
# Eigenvalues:
eigenvalues <- pcaSolution$eig$eigenvalue
names(eigenvalues) <- paste("Component: ", seq(1,length(eigenvalues)), sep = "")
eigenvalues

# Screeplot:
screePlot <- data.frame(Component = rownames(pcaSolution$eig),
                        Eigenvalue = pcaSolution$eig$eigenvalue,
                        stringsAsFactors = F)
ggplot(screePlot,
       aes(x = seq(1,length(pcaSolution$eig$eigenvalue),1),
           y = Eigenvalue),
       label = Component) +
  geom_path(color="blue") +
  geom_point(color="blue",size = 2) +
  geom_point(color="white", size = 1.5) + 
  xlab("Component") + 
  ggtitle("Screeplot") +
  theme_bw()

# Plot loadings on PC1 and PC2
# get item coordinates
coordinates <- pcaSolution$var$coord
# compute loadings (dividing coordinates by sqrt(coord_eigenvalue))
for (i in 1:5) {
  coordinates[,i] <- coordinates[,i]/sqrt(eigenvalues[i])
}
colnames(coordinates) <- paste("Component",seq(1,dim(coordinates)[2],1),sep="")
category <- paste("Component ",unname(apply(coordinates,1,function(x){which(abs(x)==max(abs(x)))})),sep="")
loading <- unname(apply(coordinates,1,function(x){x[which(abs(x)==max(abs(x)))]}))
# prepare data.frame for {ggplot2}
plotTable <- as.data.frame(coordinates)
plotTable$Category <- category
plotTable$Loading <- loading
plotTable$Label <- rownames(plotTable)
# plot factor loadings w. {ggplot2}:
# PCA Projection on Components 1 and 2
ggplot(plotTable,aes(x = Component1,
                     y = Component2,
                     size = Loading,
                     fill = Category,
                     label = Label)) +
  geom_point(color="black",shape=21,alpha=0.75) +
  geom_text(hjust = 1.5, vjust = -1, size = 3.5) +
  theme_bw() +
  scale_size(range = c(1,6)) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="right") +
  theme(legend.key = element_rect(colour = NA)) +
  theme(legend.key.size=unit(1,"cm")) +
  theme(legend.background = element_rect(colour = "white", fill="white")) +
  theme(panel.border = element_rect(linetype = "solid", colour = "white", fill="NA")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_blank()) +
  theme(axis.text.x = element_text(size=11,colour = "black")) +
  theme(axis.text.y = element_text(size=11,colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  xlab("Component 1") + ylab("Component 2") +
  ggtitle("PCA: Projection on Components 1 and 2\n")


## ══════════════════════════════════════════════════════════════════════════════════
## Data Science Serbia 2016. www.datascience.rs
## ══════════════════════════════════════════════════════════════════════════════════

