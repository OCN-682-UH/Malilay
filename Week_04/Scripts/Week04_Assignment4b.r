### Homework 4b ####
### Created by: Ally Malilay #############
### Updated on: 2025-09-23 ####################
################################################
## Plug in libraries
library(tidyverse)
library(here)
library(beyonce)

## Load data ######
ChemData <- read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))

## What is the relationship between relative amount of submarine groundwater discharge in the water
## and silicate based on site?
ChemData_clean <- ChemData %>%
  drop_na() %>% #filter out everything that isn't complete row
  separate(col = Tide_time, #choose tide_time column
           into = c("Tide", "Time"), #question: how do I know when to use c() ?. initially just wrote Tide, Time, no quotations either
           sep = "_", #separate by _
           remove = FALSE) %>% #keep Tide_Time column
  filter(Time == "Night") #filter for Night because silicate levels are no longer being actively absorbed from the water column

#pivot to long data form and back to wide 
ChemData_pivot <- ChemData_clean %>%
  pivot_longer(cols = Temp_in:percent_sgd, #choose columns to pivot 
               names_to = "Variables", #names of new columns
               values_to = "Values")  %>% #name of new column holding all variable values
  group_by(Variables, Site, Season, Zone, Tide, Time) %>%
  summarise(mean_vals= mean(Values, na.rm=TRUE)) %>%
  pivot_wider(names_from=Variables,
            values_from= mean_vals) %>% ###want to widen the data to make it look better
  write_csv(here("Week_04", "Output", "Assignment 4b Summary.csv")) #export summary into output folder

### Create a plot
p <- ggplot(data = ChemData_pivot,
               mapping = aes(x = percent_sgd, y = Silicate)) +
         geom_bin_2d() + #show increasing correlation with higher silicate lvls, higher % groundwater discharge
  facet_wrap(~Tide, scales = "free") + #display plots based on tide
  labs(title = "Average submarine groundwater discharge vs average silicate", 
       subtitle = "High tide vs low tide",
       x = "Submarine Groundwater Discharge (%)", y = "Silicate (umol/L)",
       shape = "Tide",  #differentiate color by zone
       caption = "Source: Dr. Nyssa Silbiger") +
  theme_bw() + #change theme to dark to highlight plot points
  theme(axis.title = element_text(size = 12), #change size of axis titles
        title = element_text(size = 10, face = "bold"), #increase title size and make bold
        plot.subtitle = element_text(face = "plain"), #subtitle plain text instead of bold
        plot.caption = element_text(face = "plain")) #caption plain text instead of bold#add geometry

p
### Export plot
ggsave(filename = here("Week_04", "Output", #where to save it
                        "Assignment 4b: Average submarine groundwater discharge vs average silicate.png"), #name to save it
        plot = p) 
