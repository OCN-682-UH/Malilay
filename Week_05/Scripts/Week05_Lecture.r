### Today we are going to practice joins with data from Becker and Silbiger (2020) ####
### Created by: Ally Malilay #############
### Updated on: 2025-09-23 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(cowsay)
### Load data ######
# Environmental data from each site
EnviroData<-read_csv(here("Week_05","Data", "site.characteristics.data.csv"))
#Thermal performance data
TPCData<-read_csv(here("Week_05","Data","Topt_data.csv"))

#pivot the data
EnviroData_wide <- EnviroData %>% 
  pivot_wider(names_from = parameter.measured,
              values_from = values) %>%
  arrange(site.letter) # arrange the dataframe by site

#left_join() takes everything from the x and puts it in the y
FullData_left <- left_join(TPCData, EnviroData_wide) %>%
  relocate(where(is.numeric), .after = where(is.character)) %>%# relocate all the numeric data after the character data
  pivot_longer(cols = E:substrate.cover, #choose columns to pivot 
               names_to = "Variables", #names of new columns
               values_to = "Values") %>%
  group_by(site.letter, Variables) %>%
  summarise(mean_vals= mean(Values, na.rm=TRUE),
               var_vals = var(Values, na.rm = TRUE))
View(FullData_left)

#tibble is like a dataframe
#  Make 1 tibble
T1 <- tibble(Site.ID = c("A", "B", "C", "D"), 
             Temperature = c(14.1, 16.7, 15.3, 12.8))
T1

# make another tibble
T2 <-tibble(Site.ID = c("A", "B", "D", "E"), 
            pH = c(7.3, 7.8, 8.1, 7.9))
T2

left_join(T1, T2) #joins to T1, keeps T1 as base dataframe
right_join(T1, T2) #joins to T2, keeps T2 as base dataframe
inner_join(T1,T2) #no C or E because both are base dataframe and only keeps what they both have
full_join(T1,T2) #keeps everything!
semi_join(T1, T2) #semi_join keeps all rows from the first data set where there are matching values in the second data set, keeping just columns from the first data set.
anti_join(T1, T2) #Saves all rows in the first data set that do not match anything in the second data set. This can help you find possible missing data across datasets.


say("I like to eat food", by = "stegosaurus")




