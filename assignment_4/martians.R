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

#Remove hoax data first, so no time is spent cleaning data that will be deleted anyway
pattern <- "(?i)\\(\\([^(]*HOAX[^)]*\\)\\)"
data <- data %>% 
  filter(!str_detect(data$comments, pattern))
  
#fill in missing country data as NA
data$country[data$country == ""] <- NA

#format the country as a factor since we will group them together later
data$country <- as.factor(data$country)
levels(data$country)

#fill in missing shape data as NA
data$shape[data$shape == "" | data$shape == "unknown"] <- NA

#format the datetime column as dates rather than character strings
data$datetime <- ymd_hm(data$datetime)

#format the date_posted column as dates rather than character strings
data$date_posted <- dmy(data$date_posted)

#IMPORTANT: clean up duration.seconds column??