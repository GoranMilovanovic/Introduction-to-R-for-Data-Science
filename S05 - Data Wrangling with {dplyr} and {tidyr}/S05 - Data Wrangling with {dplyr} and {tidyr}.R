
## Introduction to R for Data Science
## ----------------------------------------------------------------------------------
## SESSION 05: DATA WRANGLING WITH {dplyr} AND {tidyr}
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

## SESSION 05: DATA WRANGLING WITH {dplyr} AND {tidyr}
## ----------------------------------------------------------------------------------

# clear all
rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(hflights)

# load hflights
data("hflights")

# learn about hflights:
?hflights
dim(hflights)
str(hflights)
summary(hflights)
colnames(hflights)
head(hflights, 10)

# Some examples of "traditional" subsetting and wrangling
example_1 <- hflights[hflights$Month == "1" & hflights$DayofMonth == 1, ]
head(example_1, 10)

example_2 <- hflights[c("DepTime", "ArrTime", "FlightNum")]
head(example_2, 10)

example_3 <- aggregate(Diverted ~ DayOfWeek, 
                       data = hflights, 
                       FUN = sum)
example_3

### --- Introducing {dplyr}

# the way of {dplyr}:
example_1 <- filter(hflights, 
                    Month == 1, 
                    DayofMonth == 1)
head(example_1, 20)

example_2 <- select(hflights, 
                    DepTime, 
                    ArrTime, 
                    FlightNum)
head(example_2, 20)

example_3 <- summarise(group_by(hflights, 
                                DayOfWeek), 
                       divSum = sum(Diverted)
                       )
example_3

# dplyr::glimpse() is just a replacement for str()
glimpse(hflights)

# Select the following columns: from 'Year' to 'DayofMonth'
# + all which have 'Taxi' in their name and 
# + all which names end with 'Delay'.
head(select(hflights, 
            Year:DayofMonth, 
            contains("Taxi"), 
            ends_with("Delay")
            )
     )

# If you want to subset data by some criteria, you're going to use filter(). 
# It is easy to use - just give it your criteria. 
# NOTE: Using '&' is the same as using ','
# and it means AND; if you want to apply OR condition, use '|'.
head(
  filter(hflights, 
         (UniqueCarrier=="AA" | UniqueCarrier=="UA"), 
         Month == 5)
  )

# Function nesting to select only some columns from dataset 
# and use filter on it?
result <- filter(
  select(hflights, UniqueCarrier, DepDelay), 
                 DepDelay > 60
  )
head(result, 20)

# NOTE: The pipe operator - %>% - originates not from {dplyr}, 
# but from another important R package, namely: {magrittr}.

result <- select(hflights, 
                 UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)
head(result, 20)


# Often we want to sort data by some criteria. Let's
# see how to do it using base::order() and dplyr::arrange().

# {base}
head(
  hflights[order(hflights$DepDelay), c("UniqueCarrier", "DepDelay")]
  ) 

# {dplyr} with arrange()
hflights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay) %>%
  head()

# summarise() 
hflights %>% 
  filter(Month > 5, Month < 10) %>%
  select(DayOfWeek, Cancelled) %>%
  group_by(DayOfWeek) %>%
  summarise(tot_cancelled = sum(Cancelled)) %>%
  head()

# what does the group_by() function do?
groupedHflights <- 
  hflights %>% group_by(Origin)
groupedHflights

# summarise()
summarise(groupedHflights,
          meanTaxiOut = mean(TaxiOut, na.rm = T))

# This is how we can calculate percentage of cancelled 
# and diverted flights from  Houston by destination airport:
# N.B Not percent!
hflights %>%
  group_by(Dest) %>%
  summarise_each(funs(mean), Cancelled, Diverted)

# n() is nifty little function which can be called only 
# inside other dplyr functions; e.g.
# Counting the number of flights by airport and month:
hflights %>%
  group_by(Month, Dest) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

# {dplyr} tally():
hflights %>%
  group_by(Month, Dest) %>%
  tally(sort = T)

# n_distinct() returns the number of unique values:
hflights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), 
            plane_count = n_distinct(UniqueCarrier))

# One more 'n' function is top_n(). It's clear that it gives top n 
# number of values where we decide what that number is:
hflights %>%
  select(Month, DayofMonth, DepDelay) %>%
  group_by(Month) %>%
  top_n(2, DepDelay) %>%
  arrange(desc(Month))

# dplyr::mutate() is used if you want to make new variables using existing ones
flights_with_km <- hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(total_distance = sum(Distance)) %>%
  mutate(distance_km = total_distance * 1.609)
flights_with_km

# cleaning up with {dplyr}
x <- c(1, 2, NA, NA, 5)
coalesce(x, 0)
x <- c(1, 2, NA, NA, 5)
y <- c(1, 2, 3, 4, 5)
x <- coalesce(x, y)
x

typeof(hflights$TaxiIn)
# - N.B. 'double' not 'integer'
hflights$TaxiIn <- coalesce(hflights$TaxiIn, 0L)

# joining with {dplyr}
# Imagine that you want to have full carrier name instead of abbreviation 
# in your data.frame. However, full names are stored in another data frame. 
# It's easy to solve that problem by joining:

# Manually created dataset with carrier names of interest
carier_names <- data.frame(carrier_abb = as.character(flights_with_km$UniqueCarrier), 
                           carrier_name = c("American Airlines", "Alaska Airlines", 
                                            "JetBlue", "Continental Airlines", 
                                            "Delta", "Atlantic Southeast Airlines", 
                                            "Frontier Airlines", "Airtran Airways", 
                                            "American Eagle Airlines", 
                                            "Skywest Airlines", "United Airlines", 
                                            "US Airways", "Southwest Airlines", 
                                            "SouthCentral Air", "Air Nostrum"), 
                           stringsAsFactors = F)

# join w. {dplyr}:
flights_with_km <- left_join(flights_with_km, 
                             carier_names, 
                             by = c("UniqueCarrier" = "carrier_abb"))
flights_with_km

### --- Introducing {tidyr}

hflights %>% 
  select(Year, Month, DayofMonth) %>% head()

# the col argument of unite() is the unquoted name of the new column
# that we are about to produce; then we list the original columns (unquoted!)
# finally, we introduce the separator by the sep argument:
hflightsUnited <- hflights %>% unite(col = date, 
                                     Year, Month, DayofMonth, 
                                     sep = "/")
head(hflightsUnited)

# let's drop the original variables from the new data set:
hflightsUnited$Year <- NULL
hflightsUnited$Month <- NULL
hflightsUnited$DayofMonth <- NULL
hflightsUnited$DayOfWeek <- NULL
head(hflightsUnited)

# But if we ever need year, month and day data again separately...
# {tidyr} separate() - the inverse of unite()
hflightsSeparated <- 
  hflightsUnited %>% separate(date, 
                              c("Year", "Month", "DayofMonth"), 
                              sep = "/")
head(hflightsSeparated)

mon_fli_carr <- hflights %>%
  group_by(Month, Dest) %>%
  tally(sort = T)
head(mon_fli_carr, 20)

# in {tidyr} spread(): key is the name of the column whose values will be used
# as new columns' names; value is the name of the column whose values 
# will populate the cells:
mon_fli_carr <- hflights %>%
  group_by(Month, Dest) %>%
  tally(sort = T) %>% 
  spread(key = Month, 
         value =  n) # tidyr::spread()
colnames(mon_fli_carr) <- c("Dest", month.name)
head(mon_fli_carr)

# Again, note the usage of unquoted column names in
# {tidyr} gather(), which is the inverse of spread():
mon_fli_carr <- 
  mon_fli_carr %>% gather(key = Month, 
                          value = n, 
                          January:December)
head(mon_fli_carr)
