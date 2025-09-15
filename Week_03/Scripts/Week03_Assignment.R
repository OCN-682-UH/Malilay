### Week 3 Assignment - Make a Plot ###
## Assignment Directions: Come up with the best possible plot with the penguin data that you can in 1 hour.###
### Created by: Ally Malilay
## Created on: 2025-09-09
#############################################################

## Load libraries ##
library(palmerpenguins)
library(tidyverse)
library(beyonce)
library(ggthemes)
library(praise)
library(here)
glimpse(penguins) #data in package, do not need to read in data

##download Beyonce color palette 
devtools::install_github("dill/beyonce")

### Data Analysis ###
head(penguins) ## look at first 6 lines ##
tail(penguins) ## look at the bottom 6 lines ##
view(penguins) ## look at entire dataset ##

## Question to answer with the plot:
## Are there trends in flipper and bill length for penguins that differ by species?
## start with penguin dataframe ##
ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y = flipper_length_mm, z = species, #add variables
                     color = species)) + #change color of graph based on species
  geom_density_2d() + #add geometry, a 2D kernel density estimation using MASS::kde2d() 
                      #display the results with contours. This can be useful for dealing with overplotting. 
  labs(title = "Density estimation of penguin bill and flipper length", 
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill length (mm)", y = "Flipper length (mm)",
       color = "Species",  #differentiate color by species
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  scale_y_continuous(limits = c(170,235), #change the y axis limits because the default range cut off data point at 231
                     breaks = c(175, 195, 215, 235)) + #add breaks to y-axis
  scale_color_manual(values = beyonce_palette(72)) + #add beyonce color palette
  theme_igray() + #change theme to igray 
  theme(axis.title = element_text(size = 13), #change size of axis titles
        legend.key.size = unit(1.4, "line"), #extend length of line in legend
        title = element_text(size = 14, face = "bold"), #increase title size and make bold
        plot.subtitle = element_text(face = "plain"), #subtitle plain text instead of bold
        plot.caption = element_text(face = "plain")) #caption plain text instead of bold

## need some external validation for my work after I finished the plot ##
praise()

## Save image to output folder ##
ggsave(here("Week_03","Output","Density estimation of penguin bill and flipper length.png"))
