#Coding in R, Assignment 4
#Ibrahim Emam
#July 15, 2024

#BM:Overall, well done on this assignment. Your commenting is clear; thank you
#for explaining what each line of code does. I've left notes in specific spots 
#where I felt that you could have expanded a bit more. Everything works smoothly
#with the code, no issues with running it. All of the code is logically sound 
#and well-written. I can tell that you have a solid understanding of course 
#content. 

#BM: Great work with loading these libraries
#to ensure that the user has the required functions
#for data cleaning

#load relevant libraries
library(tidyverse)
library(lubridate)

#BM: This works well, but the UFO data can be stored
#under a more descriptive dataframe name, instead of
#just "data" (for example, "ufo_data" would be better)

#load the data from a csv into a dataframe.
data <- read.csv("ufo_subset.csv")

#BM: Great use of head() to peek at the data

#get a quick overview of the data
head(data)
summary(data)

#BM: Good use of this function to check for duplicate rows
#I used a different approach (directly removing duplicates
#using the distinct() function), but this works just as well

#check if there are duplicate rows in the data (there are none)
any(duplicated(data))

#BM: This is a great use of regex to define patterns and filter out
#hoaxes! Very interesting, and I did not consider using regex for
#this purpose. I filtered using the phrase NUFORC Note (as they 
#are stated to comment on all hoax sightings)
#Ideally, the best approach seems to be filtering using both phrases
#(HOAX and NUFORC)

#Remove hoax data first, so no time is spent cleaning data that will be deleted anyway
pattern <- "(?i)\\(\\([^(]*HOAX[^)]*\\)\\)"
data <- data %>% 
  filter(!str_detect(data$comments, pattern))
  
#BM: Good work! Next time, you should show how you identified that all
#missing data for country and shape columns was filled as ""
#You could also add a note justifying filling in missing data as NA
#rather than removing the rows all together (for the purposes of our
#analyses, I believe the results would be the same)

#fill in missing country data as NA
data$country[data$country == ""] <- NA

#fill in missing shape data as NA
data$shape[data$shape == ""] <- NA

#BM: Interesting use of parse_date_time() function, great work
#Time zone not specified for the datetime column but it shouldn't 
#make a difference

#format the datetime column as dates rather than character strings
#specify timezone so the function returns POSIX
data$datetime <- parse_date_time(data$datetime, order = "ymd HM")

#format the date_posted column as dates rather than character strings
#specify timezone so the function returns POSIX
data$date_posted <- dmy(data$date_posted, tz = "America/Toronto")

#BM: I understand why you divided by 86400 (conversion from seconds to days)
#but a note specifying this should be added for others going through the code

#create new column with the delay from sighting to report in days.
data$report_delay <- as.numeric((data$date_posted - data$datetime) / 86400)

#BM: Yes, above 1 day as outliers makes sense

#Sighting duration above 1 day will be considered outliers
data$duration.seconds[data$duration.seconds > 86400] <- NA

#BM: Efficient way to filter out the negative report delay values

#remove rows where the incident was reported before it happened
data <- data[!data$report_delay < 0,]

#BM: Good job, this is how I created the table as well 
#rounding to the nearest day is a nice touch

#create a table that summarizes average report delay per country (rounded to the nearest day)
average_delay_by_country <- data %>%
  filter(!is.na(country)) %>% 
  group_by(country) %>%
  summarize(average_report_delay = round(mean(report_delay, na.rm = TRUE), 0))

print(average_delay_by_country)

#BM: Good use of log in the histogram
#main title and x-axis title are appropriate

#create a histogram showing frequency of UFO sighting duration
#logarithm is used due to large range of reported sighting durations
hist(log(data$duration.seconds),
     main = "UFO Sighting Duration", xlab = "log Duration (seconds)")

