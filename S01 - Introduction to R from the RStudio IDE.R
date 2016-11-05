
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

# try out:
ls()

# R as a calculator
2+2
2/3-1+5
a <- 1/3
b = 7
a + b

# save R object as .rds file
saveRDS(a, "a.rds")
a <- readRDS("a.rds")

# save several R objects as .RData file
save(list = c("a", "b"), file = "obj.RData")

# load .RData file to R
load(file = "obj.RData")

# list of loaded objects
ls()

# remove all objects
rm(list=ls())

# what is my working directory?
getwd()

# installing packages:
# install.packages("dplyr") # replace "dplyr" with other package name
installed.packages() # list of installed packages

# What's coming next...
numbers <- c(1,2,3,4,5)
words <- c("data", "science", "r")
boolean <- c(TRUE, FALSE)

data("mtcars")
