### Today we are going to practice joins with data from Becker and Silbiger (2020) ####
### Created by: Ally Malilay #############
### Updated on: 2025-09-27 ####################
#############################################

## Load libraries ##
library(here)
library(tidyverse)
library(lubridate)
library(cowsay)

## Read in data ##
CondData <- read_csv(here("Week_05","Data","CondData.csv")) 
DepthData <- read_csv(here("Week_05","Data","DepthData.csv"))

#Join two dataframes together
CondDepthData <- CondData %>%
  mutate(date = mdy_hms(date)) %>% #change date to ISO format
  mutate(date = round_date(date, "10 seconds")) %>% #round up 10 seconds to match Depth Data
  inner_join(DepthData) %>% #join two dataframes together with no NAs
  mutate(Minute = floor_date(date, "minute")) %>% #round to minute
  group_by(Minute) %>% #group times by minute and means of variables
  summarise(Mean_Depth = mean(Depth), #calculate the mean depth
            Mean_Temp = mean(Temperature),  #calculate the mean temp
            Mean_Salinity = mean(Salinity)) %>%  #calculate the mean salinity
  write_csv(here("Week_05","Output","CondDepthData.csv")) #export as csv into Output folder

### Pivot to long data for plotting
CondDepthData_longer <- CondDepthData %>%
  pivot_longer(cols = Mean_Depth:Mean_Salinity, #choose columns to pivot 
                                    names_to = "Variables", #names of new columns
                                    values_to = "Values") %>%
  filter(Variables != "Mean_Depth") %>% #filter variables not equal to mean_depth
  group_by(Minute, Variables) #group time and variables together

## Create plot
CondDepthDataPlot <- CondDepthData_longer %>% #pipe long data for plotting
  ggplot(mapping = aes(x = Values, fill = Variables, color = Variables)) + #shade density plot and color by variables
  geom_density(alpha = 0.3) + #opacity of fill
  labs(title = "Density Estimates of Average Salinity and Temperature", #edit titles and axes of plot
       subtitle = "Data collected on January 15, 2021",
       x = "Average Values per Minute",
       y = "Density") +
  guides(fill="none") + #remove automatically generated legend
  scale_color_manual(labels = c("Average Salinity (PSU)", "Average Temperature (ºC)"), #generate new legend with proper title names
                     values = c("red","blue")) + #change color of legend
  xlim(28.4,35) + #set limits for x axis to make data clear

CondDepthDataPlot
# Export plot
ggsave(here("Week_05", "Output","Assignment 5: Density Estimates of Average Salinity and Temperature.png"),
       plot = CondDepthDataPlot)  

say("Yippee ANOTHER WEEK ANOTHER PLOT", by = "stegosaurus")
