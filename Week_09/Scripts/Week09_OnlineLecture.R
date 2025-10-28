######### Today we're going to learn about factors ######################
### Created by: Ally Malilay #############
### Updated on: 2025-10-25 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
### Load data ######
#tuesdata <- tidytuesdayR::tt_load(2021, week = 7)
#income_mean<-tuesdata$income_mean
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')

##what is a factor??
  ##specialized version of a character. values that a factor takes is called 'levels'
  ## a level allows us to put data in a specific order in a particular way
  ## we use it for setting categorical data
  ##default is alphabetical but we can also re order it to be specific for our analyses
  # if a character is converted to a factor, it is then stored as an integer (1,2,3,4)

#let's turn a vector into a factor by using the function factor()
fruits<-factor(c("Apple", "Grape", "Banana"))
fruits #now, the order in which it will plot or summarize will now be in alphabeticl order

#if there is a typo in a column for everything that was supposed to be numbers, any other value
# will turn into NA
testdata <- c("a", "1", "3")
as.numeric(testdata)

testdata <- factor(testdata)
as.numeric(testdata)
 ###made up data, the "a" is now incorrect
  ##therefore, we should be careful and not use characters for factors
  ##only use factors when we need to
testdata

#read_csv allows us to read strings as characters (better) than read.csv which reads characters as factors
glimpse(starwars)

starwars %>%
  filter(!is.na(species)) %>% #remove NAs
  count(species, sort = TRUE)

star_counts<- starwars %>%
  filter(!is.na(species)) %>%
  mutate(species = fct_lump(species, n = 3)) %>% #converts data to a factor and then lumps together; lump species, anything less than 3 is lumped into other, anything 3+ is now a factor
  count(species) #alphabetical order

star_counts %>%
  ggplot(aes(x = species, y = n)) +
  geom_col() 

#VERSUS

star_counts %>%
  ggplot(aes(x = fct_reorder(species, n), y = n)) + #reorder, put in the order of n (counts of characters per species)
  geom_col() +
  labs( x = "Species")

########## make a line plot and reorder the legend to match the order of the lines #########
glimpse(income_mean)

total_income<-income_mean %>%
  group_by(year, income_quintile) %>%
  summarise(income_dollars_sum = sum(income_dollars)) %>%
  mutate(income_quintile = factor(income_quintile)) # make it a factor, question: Why can't we see this in the data as a separate column?

View(total_income)

total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, 
             color = fct_reorder2(income_quintile, year, income_dollars_sum)))+
  geom_line()+
  labs(color = "income quantile")

##### if we want to reorder levels in an explicit way that isn't alphabetical
x1 <- factor(c("Jan", "Mar", "Apr", "Dec"))
x1
x1 <- factor(c("Jan", "Mar", "Apr", "Dec"), levels = c("Jan", "Mar", "Apr", "Dec"))
x1

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor
  filter(n>3) # only keep species that have more than 3
starwars_clean
levels(starwars_clean$species) #all the levels are still there

### to drop all the levels that are not included in the dataframe
starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() %>% # drop extra levels########################## so no secret levels show up with NAs
  mutate(species = fct_recode(species, "Humanoid" = "Human")) ### RENAME or (recode) a level
