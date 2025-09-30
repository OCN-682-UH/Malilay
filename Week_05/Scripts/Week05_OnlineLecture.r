### Today we are going to practice using the lubridate package ####
### Created by: Ally Malilay #############
### Updated on: 2025-09-29 ####################

#### Load Libraries ######
library(tidyverse)
library(here)
library(lubridate) #package to deal with dates and times
library(cowsay)

### Using different functions in lubridate ######
now(tzone = "GMT") #what time in GMT
today() #check the date today
am(now()) #find out if the lecture is in the morning or night
leap_year(now()) #check if it is a leap year

## convert data to ISO date
ymd("2025-09-28")
mdy("09/28/2025")
mdy("September 28 2025")
dmy("28/09/25")
ymd_hms("25-09-28 9:36:04 PM")
mdy_hms("09/28/25 21:36:04") #test for year 25 vs 2025
mdy_hm("September 28 25 9:36pm") #test all caps vs lowercase pm PM

#make a vector of dates
datetimes <- c("09/26/25 9:39am",
               "09/27/25 10:00pm",
               "09/28/25 9:39pm")

#convert vector of dates to datetimes
datetimes <- mdy_hm(datetimes)
month(datetimes, label = TRUE, abbr = FALSE) #helpful to make clean plots if need to know the month

#extract the day
day(datetimes)

#extract day of week
weekdays(datetimes)
wday(datetimes, label = TRUE, abbr = FALSE)

#add days and times 
datetimes + days(1) #add one day to the dataset

#round dates/times
round_date(datetimes, "10 mins")

#Read in the conductivity data (CondData.csv) and convert the date column to a datetime. 
#Use the %>% to keep everything clean.

CondData <- read_csv(here("Week_05","Data", "CondData.csv"))

CondData_datecolumn <- CondData %>%
  mutate(datetime = mdy_hms(date)) #convert date column to ISO datetime
