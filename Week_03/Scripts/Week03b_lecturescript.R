### Data for Palmer's Penguins ###
## Measurements for penguin species, island in Palmer Archipelago, size (flipper length, body mass, bill dimensions), and sex.###
### Created by: Ally Malilay
## Created on: 2025-09-09
#############################################################

## Load libraries ##
library(palmerpenguins)
library(tidyverse)
library(praise)
library(beyonce)
library(here)

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
  coord_flip() + #flip coordinates
  scale_color_manual(values = beyonce_palette(72)) + #specify color palette w beyonce library
  theme_minimal() +  #change theme
  theme(axis.title = element_text(size = 20), #change size of axis titles
        panel.background = element_rect(fill = "grey")) #change panel background to grey

ggsave(here("Week_03","Output","penguin.png"),
       width = 7, height = 5) # in inches


praise() #need some external validation

