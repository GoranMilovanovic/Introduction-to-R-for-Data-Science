
### - --- Introduction to R for Data Science
### - --- ----------------------------------------------------------------------------------
### - --- SESSION 01: INTRODUCTION TO R FROM RSTUDIO IDE
### - --- ----------------------------------------------------------------------------------
### - --- Autumn 2018.
### - --- Goran S. Milovanovic, DataKolektiv, @Startit Centre, Savska 5, Belgrade, Serbia

### - --- Code
### - --- ----------------------------------------------------------------------------------
# - Goran S. Milovanović, Phd
# - Data Scientist, Wikimedia Deutschland
# - Data Science Mentor @Springboard, Data Science Serbia
# - ing Branko Kovač, Data Scientist @Tradecores
# - Data Science Mentor @ Springboard, Data Science Serbia
### - --- ----------------------------------------------------------------------------------

### - --- SESSION 01: INTRODUCTION TO R: RSTUDIO IDE + Examples
### - --- ----------------------------------------------------------------------------------

# - create a variable
a <- 5
print(a)
# - in Console:
a
# - try this out:
ls()
# - clear workspace
rm(list = ls())
# - remove only one object
a <- 17
a
rm(a)
print(a)

# - R as a calculator
2 + 2
2/3 - 1 + 5
a <- 1/3
b = 7
a + b

# - what is my working directory?
getwd()

# - use setwd() to set your working directory
# - setwd('path_to_your_working_directory')
setwd(paste0(getwd(), '/_tempData'))
getwd()

# - save R object as .rds file
saveRDS(a, "a.rds")
c <- readRDS("a.rds")
print(c)

# - save several R objects as .RData file
save(list = c("a", "b"), 
     file = "obj.RData")

# - remove a, b
rm(a, b)
# - load .RDatafile to R
load(file = "obj.RData")

# - list of loaded objects
ls()

# - remove all objects
rm(list = ls())

### --- installing packages:
# - install.packages("dplyr") # - replace "dplyr" with 
# - any other package name to install it from CRAN
installed.packages() # - list of installed packages

### --- The shape of things to come:
numbers <- c(1, 2, 3, 4, 5)
nums <- 1:5
words <- c("data", "science", "r")
boolean <- c(TRUE, FALSE)
data("mtcars")
head(mtcars, 5)

# - define a function in R:
factorial <- function(x) {
  if (x == 0 | x == 1) {
    return(1)
  }
  else {
    return(x * factorial(x - 1))
  }
}

# - calls to factorial():
factorial(5)
factorial(10)
factorial(100)

# - a vector of type numeric obtained from a sequence:
xVec <- seq(0, 30, 1)

# - applying the non-vectorized factorial() function to a vector x:
xFactorials <- sapply(xVec, factorial)
xFactorials

# - applying some other non-vectorized function to a vector:
xSomething <- sapply(xVec, function(x) {
  x <- x + 100
})
xSomething

# - a {base} R plot
plot(xVec, xFactorials,
     xlab = "Integers",
     ylab = "Factorials",
     main = "Insane Plot",
     cex.main = .85)
lines(xVec, xFactorials, col = "red")

# - R is a vector programming language. All programming languages dream of becoming
# - vector languages one day. For example, to do vector programming in Python, you first need
# - a package called NumPy to turn it into MATLAB, then a package named Pandas to turn it into R.
# - Just kidding :) - we love Python too and have the highest of respect only for the
# - Python community in Data Science. But R is a "natural" vector programming language:

# - create 100 random deviates from the Uniform on [0,1]
xUnif <- runif(100, 0, 1)
xNorm <- rnorm(100, 5, .75)
# - Hint: ?runif, ?rnorm == asking for help --> documentation

# - ask for *each element in x* whether it is >= .5 or not:
xUnif >= .5
# - Where's the function? Isn't >= an operator?

# - R is a functional programming language too. Thus:
logical1 <- '>='(xUnif, .5)
logical1
logical2 <- xUnif >= .5
# - Now: which corresponding elements in logical1
# - and logical 2 are different? 
which(logical1 != logical2) # - none: integer(0)


# - Back to vectorized functions in R; similarly, we have:
xSeq <- seq(2, 100, 2)
x2 <- sqrt(xSeq)
x2


### --- Stretching its legs
### --- PCA example
library(psych)
library(FactoMineR)
data(bfi)
head(bfi)
dim(bfi)
dataSet <- bfi[, 1:25]
dim(dataSet)
# - replace NA w. respective column means
for (i in 1:ncol(dataSet)) {
  dataSet[is.na(dataSet[,i]), i] <- mean(dataSet[,i], na.rm = TRUE)
}
colnames(dataSet)

# - plot correlation matrix
library(corrplot)
corrplot(cor(dataSet),
         tl.col = 'black',
         tl.cex = .75)

# - perform PCA w. PCA() from {factoMineR}
pcaSolution <- PCA(dataSet,
                   ncp = 5,
                   scale.unit = F,
                   graph = FALSE)
# - Eigenvalues:
eigenvalues <- pcaSolution$eig[, 1]
names(eigenvalues) <- paste("Component: ", seq(1,length(eigenvalues)), sep = "")
eigenvalues

# - Screeplot:
screePlot <- data.frame(Component = rownames(pcaSolution$eig),
                        Eigenvalue = eigenvalues,
                        stringsAsFactors = F)
library(ggplot2)
library(ggrepel)
ggplot(screePlot,
       aes(x = seq(1, length(eigenvalues), 1),
           y = Eigenvalue, 
           label = round(Eigenvalue, 2)
           )
       ) +
  geom_path(color = "blue", size = .25) +
  geom_point(color = "blue",size = 2) +
  geom_point(color = "white", size = 1.5) + 
  geom_text_repel(size = 3) + 
  xlab("Component") + 
  ggtitle("Screeplot") +
  theme_bw()

# - Plot loadings on PC1 and PC2
# - get item coordinates
coordinates <- pcaSolution$var$coord
# - compute loadings (dividing coordinates by sqrt(coord_eigenvalue))
for (i in 1:5) {
  coordinates[, i] <- coordinates[, i]/sqrt(eigenvalues[i])
}
colnames(coordinates) <- paste("Component",
                               seq(1, dim(coordinates)[2],1 ), sep = "")
category <- paste("Component ", 
                  unname(
                    apply(coordinates, 1, function(x) {
                      which(abs(x) == max(abs(x)))
                      })
                    ), 
                  sep = "")
loading <- unname(
  apply(
    coordinates, 1, function(x) {
      x[which(abs(x) == max(abs(x)))]
      })
  )
# - prepare data.frame for {ggplot2}
plotTable <- as.data.frame(coordinates)
plotTable$Category <- category
plotTable$Loading <- loading
plotTable$Label <- rownames(plotTable)
# - plot factor loadings w. {ggplot2}:
# - PCA Projection on Components 1 and 2
ggplot(plotTable,aes(x = Component1,
                     y = Component2,
                     size = Loading,
                     fill = Category,
                     label = Label)) +
  geom_point(color = "black", shape = 21, alpha = 0.75) +
  geom_text_repel(size = 3) +
  theme_bw() +
  scale_size(range = c(1,6)) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "right") +
  theme(legend.key = element_rect(colour = NA)) +
  theme(legend.key.size = unit(1, "cm")) +
  theme(legend.background = element_rect(colour = "white", fill = "white")) +
  theme(panel.border = element_rect(linetype = "solid", colour = "white", fill = "NA")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_blank()) +
  theme(axis.text.x = element_text(size = 11,colour = "black")) +
  theme(axis.text.y = element_text(size = 11,colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(plot.title = element_text(size = 12)) +
  xlab("Component 1") + ylab("Component 2") +
  ggtitle("PCA: Projection on Components 1 and 2\n")
