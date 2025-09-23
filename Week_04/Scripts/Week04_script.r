### Today we are going to plot penguin data ####
### Created by: Ally Malilay #############
### Updated on: 2025-09-16 ####################

#### Load Libraries ######
library(palmerpenguins)
library(tidyverse)
library(here)

### Load data ######
# The data is part of the package and is called penguins
glimpse(penguins)

# Filter only the female penguins
filter(.data = penguins, sex =="female") #is sex only equal to female

#filter penguins measures in year 2008
filter(.data = penguins, year == 2008)

#filter penguins body mass greater than 5000
filter(.data = penguins, body_mass_g > (5000))

#filter with multiple conditions
## filter penguins collected in either 2008 or 2009 
filter(.data = penguins, year %in% c(2008,2009))

## penguins that are not from the island dream
filter(.data = penguins, island !="Dream")

## penguins in the species Adelia and Gentoo
filter(.data = penguins, species %in% c("Adelia", "Gentoo"))

## mutate to add new column converting body mass in g to kg and save in new dataframe called data2
data2 <- mutate(.data = penguins,
                body_mass_kg = body_mass_g/1000)
View(data2)

#change multiple columns at once
data2 <- mutate(.data = penguins,
                body_mass_kg = body_mass_g/1000,
                bill_length_depth = bill)
View(data2)

#create a new column to add flipper length and body mass together
data3 <- mutate(.data = penguins,
                body_mass_and_flipper_length = body_mass_g + flipper_length_mm)
View(data3)

#body mass greater than 4000 = big and everything else = small
data3 <- mutate(.data = penguins,
                big_small = ifelse(body_mass_g > 4000, "big", "small"))
View(data3)

#filter only female penguins and add new column that calculates the log body mass
penguins %>% # use penguin dataframe
  filter(sex == "female") %>% #select females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log biomass
  select(species, island, sex, log_mass)

# calculate the mean flipper length 
penguins%>%
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE)) #na.rm to ensure there are no NAd

#Let's calculate the mean and max bill length by island
penguins %>%
  group_by(island) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))
