## Write a script that:

#### Load Libraries ######
library(palmerpenguins)
library(tidyverse)
library(here)
library(ggthemes)

### Load data ######
# The data is part of the package and is called penguins
glimpse(penguins)

# calculates the mean and variance of body mass by species, island, and sex without any NAs
penguins %>% # use penguin dataframe
  drop_na(species, island, sex) %>% 
  group_by(species, island, sex) %>%
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE),
            variance_body_mass = var(body_mass_g, na.rm=TRUE))

# filters out (i.e. excludes) male penguins, then calculates the log body mass, 
# then selects only the columns for species, island, sex, and log body mass, 
# then use these data to make any plot. Make sure the plot has clean and clear 
# labels and follows best practices. Save the plot in the correct output folder.
penguins %>%
  filter(sex == "female") %>%
  drop_na(body_mass_g, species, island, sex) %>%
  mutate(log_mass = log(body_mass_g)) %>%
  select(species, island, sex, log_mass) %>%
  ggplot(mapping = aes(x = species, y = log_mass,
                       color = species)) +
  geom_violin(
    
  ) +
  labs(title = "Distribution of Log of Body Mass for Penguins", 
       subtitle = " Adelie, Chinstrap, and Gentoo Penguins",
       x = "Species", y = "Log of Mass",
       color = "Species",  #differentiate color by species
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  theme_dark() + #change theme to igray 
  theme(axis.title = element_text(size = 13), #change size of axis titles
        legend.key.size = unit(1.4, "line"), #extend length of line in legend
        title = element_text(size = 14, face = "bold"), #increase title size and make bold
        plot.subtitle = element_text(face = "plain"), #subtitle plain text instead of bold
        plot.caption = element_text(face = "plain")) +
  ggsave(here("Week_04", "Output", "Assignment 4a: Distribution of log of body mass for penguins.png"))   
