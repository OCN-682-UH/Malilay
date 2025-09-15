### Data for Palmer's Penguins ###
## Measurements for penguin species, island in Palmer Archipelago, size (flipper length, body mass, bill dimensions), and sex.###
### Created by: Ally Malilay
## Created on: 2025-09-09
#############################################################

## Load libraries ##
library(palmerpenguins)
library(tidyverse)
library(praise)

## load data ##
glimpse(penguins)

## Make a plot ##
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) +
  geom_point() + 
  geom_smooth(method = "lm") + #aids the eye in seeing patterns in the presence of overplotting, fit linear model
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") +
  scale_color_viridis_d() +
  coord_flip()

praise() #need some external validation

