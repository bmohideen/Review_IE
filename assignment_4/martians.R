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

#check if there are duplicate rows in the data (there are none)
any(duplicated(data))

#Remove hoax data first, so no time is spent cleaning data that will be deleted anyway
pattern <- "(?i)\\(\\([^(]*HOAX[^)]*\\)\\)"
data <- data %>% 
  filter(!str_detect(data$comments, pattern))
  
#fill in missing country data as NA
data$country[data$country == ""] <- NA

#fill in missing shape data as NA
data$shape[data$shape == ""] <- NA

#format the datetime column as dates rather than character strings
#specify timezone so the function returns POSIX
data$datetime <- parse_date_time(data$datetime, order = "ymd HM")

#format the date_posted column as dates rather than character strings
#specify timezone so the function returns POSIX
data$date_posted <- dmy(data$date_posted, tz = "America/Toronto")

#create new column with the delay from sighting to report in days.
data$report_delay <- as.numeric((data$date_posted - data$datetime) / 86400)

#Sighting duration above 1 day will be considered outliers
data$duration.seconds[data$duration.seconds > 86400] <- NA

#remove rows where the incident was reported before it happened
data <- data[!data$report_delay < 0,]

#create a table that summarizes average report delay per country (rounded to the nearest day)
average_delay_by_country <- data %>%
  filter(!is.na(country)) %>% 
  group_by(country) %>%
  summarize(average_report_delay = round(mean(report_delay, na.rm = TRUE), 0))

print(average_delay_by_country)

#create a histogram showing frequency of UFO sighting duration
#logarithm is used due to large range of reported sighting durations
hist(log(data$duration.seconds),
     main = "UFO Sighting Duration", xlab = "log Duration (seconds)")
