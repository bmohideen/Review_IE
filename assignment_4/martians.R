#Coding in R, Assignment 4
#Ibrahim Emam
#July 15, 2024

#load relevant libraries
library(tidyverse)
library(lubridate)

#load the data from a csv into a dataframe.
data <- read.csv("ufo_subset.csv")

#get a quick overview of the data
head(data)
summary(data)

#fill in missing country data as NA
data$country[data$country == ""] <- NA

#format the country as a factor since we will group them together later
data$country <- as.factor(data$country)
levels(data$country)

#format the dates as dates rather than character strings