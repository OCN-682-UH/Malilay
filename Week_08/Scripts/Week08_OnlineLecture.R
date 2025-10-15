### Today we are going to practice advanced plotting ####
### Created by: Ally Malilay #############
### Updated on: 2025-10-14 ####################
#######################################

## Load libraries
library(tidyverse)
library(here)
library(patchwork)
library(ggrepel)
library(gganimate)
library(magick)
library(palmerpenguins)

## add plots 
p1 <- ggplot(penguins,
            aes(x = body_mass_g,
            y = bill_length_mm,
            color = species)) +
  geom_point()
p1

# plot 2
p2<-penguins %>%
  ggplot(aes(x = sex, 
             y = body_mass_g, 
             color = species))+
  geom_jitter(width = 0.2)
p2
#visual to bring p1 and p2 together
p1 + p2 +
  plot_layout(guides = "collect") + #combine legends
  plot_annotation(tag_levels = "A") #add labels to each plot: A, B

#put one plot on top of the other
p1/p2 +
  plot_layout(guides = "collect") + #combine legends
  plot_annotation(tag_levels = "1")


+
  geom_text() + # creates a text label
  geom_point(color = 'red')

#for more information about using library(patchwork), go to https://patchwork.data-imaginist.com/articles/patchwork.html

##without gg repel
ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text() + # creates a text label
  geom_point(color = 'red')

### HOW TO USE GGREPEL #############
ggplot(mtcars, aes(x = wt, 
                   y = mpg,
       label = rownames(mtcars))) +
  geom_text_repel() + #create a text label and repel the words from each other
  geom_point(color = 'red') 

## add a label
ggplot(mtcars, aes(x = wt, 
                   y = mpg,
                   label = rownames(mtcars))) +
  geom_label_repel() + #create a text label and repel the words from the coords and other text
  geom_point(color = 'red') 

##### HOW TO USE GGANIMATE #########
    ### good way to highlight how things change over time, transition
penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year, # what are we animating by
    transition_length = 2, #The relative length of the transition.
    state_length = 1 # The length of the pause between transitions
  ) +
  ease_aes("sine-in-out") +
  labs(title = 'Year: {closest_state}') +
  anim_save(here("Week_08","Output","mypengiungif.gif"))

### HOW TO USE MAGICK ######
  ### convert photos to greyscale, binary, manipulate images
penguin<-image_read("https://pngimg.com/uploads/penguin/pinguin_PNG9.png")
penguin

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point()
# to put it on a plot, need to save as an image
ggsave(here("Week_08","Output","penguinplot.png"))
penplot<-image_read(here("Week_08","output","penguinplot.png")) #what you want in background should be read in first
out <- image_composite(penplot, penguin, offset = "+70+30") #to composite/put on top of each other, background goes first and layer on top goes next
out

# Read in a penguin gif
pengif<-image_read("https://media3.giphy.com/media/H4uE6w9G1uK4M/giphy.gif")
outgif <- image_composite(penplot, pengif, gravity = "center") #gravity allows you to move image/gif to different places on plot
animation <- image_animate(outgif, fps = 10, optimize = TRUE) #fps makes it low quality to not take a lot of space up on computer
animation
