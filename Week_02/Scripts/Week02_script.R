## This is my first script. I am learning how to import data.
## Created by: Ally Malilay
## Created on: 2025-09-08
#############################################################

### Load libraries ###
library(tidyverse)
library(here)

##Read in my data ###
WeightData <- read.csv(here("Week_02","Data","weightdata.csv"))

### Data Analysis ###
head(WeightData)
tail(WeightData) ## look at the bottom 6 lines ##
view(WeightData) ## look at entire dataset ##

