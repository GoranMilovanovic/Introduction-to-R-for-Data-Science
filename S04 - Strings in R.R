
## Introduction to R for Data Science
## ----------------------------------------------------------------------------------
## SESSION 04: FUNCTIONS TO WORK WITH STRINGS IN R
## ----------------------------------------------------------------------------------
## Autumn 2016.
## Data Science Serbia @Startit Centre, Savska 5, Belgrade, Serbia

## Lecturers
## ----------------------------------------------------------------------------------
# Goran S. Milovanović, Phd
# Data Science Mentor @Springboard, Data Science Serbia
# ing Branko Kovač, Data Scientist @Tradecore
# Data Science Mentor @ Springboard, Data Science Serbia
## ----------------------------------------------------------------------------------

## SESSION 04: FUNCTIONS TO WORK WITH STRINGS IN R
## ----------------------------------------------------------------------------------

# clear all
rm(list=ls())

# libraries
library(stringr)
library(stringi)

# strings in R are character vectors
string_1 <- "Hello world"
string_2 <- "Sun shines!"
string_1
string_2
is.character(string_1) # TRUE
as.character(200*5)
as.numeric("1000")
as.double("3.14")

# Using " and '
# either:
string_1 <- "Hello 'World'"
string_1
# or
string_1 <- 'Hello "World"'
string_1 # prints: "Hello \"World\"" - what is this: \ ?
# try:
writeLines(string_1)
print(string_1)
# Escaping in R: use \, the R escape character
string_1 <- 'Hello \"World\"'
string_1
writeLines(string_1)
# Escaping escape character
writeLines("\\") # nice

# Length of strings
length(string_1) # of course
nchar(string_1) # base function

# String Concatenation in R
string_3 <- c(string_1, string_2) # a character vector of length == 2
string_3 <- paste(string_1, string_2, sep = ", ") # length == 1, base function
writeLines(string_3)

# 
strD <- c("First", "Second", "Third")
# both paste {base} and str_c {stringr} are vectorized
paste("Prefix-", strD, sep = "-")
str_c("Prefix-", strD, sep = "-") # {stringr}

# Splitting strings in R
# with strsplit {base}
string_1 <- "The quick brown fox jumps over the lazy dog";
splitA <- strsplit(string_1, " ") # is.list(splitA) == T
splitA <- unlist(strsplit(string_1, " "))

# "The quick brown" from "The quick brown fox jumps over the lazy dog"
splitA <- paste(unlist(strsplit(string_1," "))[1:3], collapse = " ")
string_1
splitA <- strsplit(string_1," ", fixed = T) # fixed=T says: match the split argument 
# exactly, otherwise, split is an regular
# expression; default is: fixed = FALSE

# very useful:
string_11 <- "Above all, don't lie to yourself. 
The man who lies to himself and listens to his own lie comes to a point that he cannot distinguish the truth within him, or around him, and so loses all respect for himself and for others. 
And having no respect           he ceases to love."
str_split(string_11, boundary("word"))
str_split(string_11, boundary("word", skip_word_none = F)) # including punctuation 
# and special

# Subsetting strings
string_1 <- c("Data", "Science", "Serbia")
# {base}
substr(string_1, 1, 3)

# {base}
string_2 <- string_1 # just a copy of string_1
substr(string_2,1,3) <- "WowWow" # check the result!
string_2
substr(string_2,1,4) <- "WowWow" # check the result!
string_2
substr(string_2,1,6) <- "WowWow" # check the result!
string_2

# UPPER CASE to lower case and vice versa in R
string_1 <- "Belgrade"
# {base}
tolower(string_1)
string_1 <- tolower(string_1)
toupper(string_1)
string_1 <- toupper(string_1)
# capitalize first letter
str_to_title(string_1) # {stringr}

# Remove whitespace
string_1 <- c("  Remove whitespace  ");
string_1
str_trim(string_1) # {stringr}

# remove leading whitespace
str_trim(string_1, side = "left")
# remove trailing whitespace
str_trim(string_1, side = "right")

# remove all whitespace?
string_1 <- c("  Remove    whitespace  ") # how about this one?
# there are different ways to do it. Try:
gsub(" ", "", string_1, fixed = T) # (!(fixed==T)), the first (pattern) argument is regex

# replacing, in general:
string_1 <- "The quick brown fox jumps over the lazy dog The quick brown"
gsub("The quick brown", "The slow red", string_1, fixed=T)

# Searching for something in a string {stringr}
str_detect(string_1, "The quick brown") # T or F
str_locate(string_1, "The quick brown")[[1]] # first match
str_locate_all(string_1, "The quick brown")[[1]] # all matches
# term frequency, as we know, is very important in text-mining:
term1 <- str_locate_all(string_1, "The quick brown")[[1]] # all matches for term1 
# ie. "The quick brown"
term1
dim(term1)[1] # how many matches = how many rows in the str_locate_all output matrix

# Sorting character vectors in R {base}
string_1 <- c("New York", "Paris", "London", "Moscow", "Tokyo")
string_1
sort(string_1)
sort(string_1, decreasing = T)

