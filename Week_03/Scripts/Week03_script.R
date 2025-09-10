### Data for Palmer's Penguins ###
## Measurements for penguin species, island in Palmer Archipelago, size (flipper length, body mass, bill dimensions), and sex.###
### Created by: Ally Malilay
## Created on: 2025-09-09
#############################################################

## Load libraries ##
library(palmerpenguins)
library(tidyverse)
glimpse(penguins)


## start with penguin dataframe ##
ggplot(data = penguins, #comma used to separate arguments
       mapping = aes(x = bill_depth_mm, #add x axis
                     y = bill_length_mm, #add y axis,
                     color = species, #add color for species and legend
                     shape = island)) +  # + used to add layers and components to ggplot
  geom_point() + #add geometry 
  labs(title = "Bill depth and length", #use quotes when adding data that is not part of raw data
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package",
       shape = "Island") +
  scale_color_viridis_d()

