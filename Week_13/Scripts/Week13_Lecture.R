#set up libraries
library(tidyverse)
library(here)

#for loops 
#example of what I want my code to do
print(paste("The year is", 2000))

#put it in a for loop; needs to be a separate column each time
years <- c(2015:2021)
for (i in years){ #set up the for loop where i is the index
  print(paste("The year is",i))
  } #loop over i

#Pre-allocate space for the for loop
# empty matrix that is as long as the years vector
year_data<-tibble(year =  rep(NA, length(years)),  # column name for year
                  year_name = rep(NA, length(years))) # column name for the year name
year_data

#add the loop
for (i in 1:length(years)){ # set up the for loop where i is the index
  year_data$year_name[i]<-paste("The year is", years[i]) # loop over year name
  year_data$year[i]<-years[i] # loop over year
}
year_data

#use loops to read in multiple .csv files
testdata<-read_csv(here("Week_13", "data", "cond_data","011521_CT316_1pcal.csv"))
glimpse(testdata)

#list files in a directory
#point to the location on the computer of the folder
CondPath<-here("Week_13", "data", "cond_data")

#list all the files in that path with a specific pattern
#in this case we are looking for everything that has a .csv in the filename
#you can use regex to be more specific if you are looking for certain patterns in filenames
files <- dir(path = CondPath,pattern = ".csv")
files

# pre-allocate space for the loop
# make an empty dataframe that has one row for each file and 3 columns
cond_data<-tibble(filename =  rep(NA, length(files)),  # column name for year
                  mean_temp = rep(NA, length(files)), # column name for the mean temperature
                  mean_sal = rep(NA, length(files)) # column name for the mean salinity
) # column name for the year name
cond_data #we are now takking 4500 lines and turning it into 3 columns

raw_data<-read_csv(paste0(CondPath,"/",files[1])) # test by reading in the first file and see if it works
head(raw_data)

#test the code outside of the for loop, make sure it runs before you put it in
mean_temp<-mean(raw_data$Temperature, na.rm = TRUE) # calculate a mean
mean_temp

for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read_csv(paste0(CondPath,"/",files[i]))
  glimpse(raw_data)
}

for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read_csv(paste0(CondPath,"/",files[i]))
  #glimpse(raw_data)
  cond_data$filename[i]<-files[i]
  cond_data$mean_temp[i]<-mean(raw_data$Temperature, na.rm =TRUE)
  cond_data$mean_sal[i]<-mean(raw_data$Salinity, na.rm =TRUE)
} 
cond_data


1:10 # a vector from 1 to 10 (we are going to do this 10 times)
1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15) # calculate 15 random numbers based on a normal distribution in a list

1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15)  %>% # calculate 15 random numbers based on a normal distribution in a list 
  map_dbl(mean) # calculate the mean. It is now a vector which is type "double"

#make your own function
1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% # make your own function
  map_dbl(mean)

#use a formula when you want to change the arguments within the function
1:10 %>%
  map(~ rnorm(15, .x)) %>% # changes the arguments inside the function
  map_dbl(mean)

#bring files in using purrr instead of a for loop
files <- dir(path = CondPath,pattern = ".csv", full.names = TRUE)
#save the entire path name
files

#read in the data and paste it on top of each other
data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity,na.rm = TRUE))
data

# ---------------------------- Basic Linear Modeling ------------------------------------#
library(tidyverse)
library(here)
library(palmerpenguins)
library(broom)
library(performance) 
library(modelsummary)
library(tidymodels)
library(pandoc)

#does bill length drive bill depth? Does this slope change by species?
# example: mod<-lm(y~x, data = df) reads as y is a function of x
Peng_mod<-lm(bill_length_mm ~ bill_depth_mm*species, data = penguins)
Peng_mod
check_model(Peng_mod) # check assumptions of an lm model

anova(Peng_mod)
summary(Peng_mod) #gives you the effects size

# Tidy coefficients
coeffs<-tidy(Peng_mod) # just put tidy() around it
coeffs

# tidy r2, etc
results<-glance(Peng_mod) 
results

# tidy residuals, etc
resid_fitted<-augment(Peng_mod)
resid_fitted

# New model
Peng_mod_noX<-lm(bill_length_mm ~ bill_depth_mm, data = penguins)
#Make a list of models and name them
models<-list("Model with interaction" = Peng_mod,
             "Model with no interaction" = Peng_mod_noX)
#Save the results as a .docx
modelsummary(models, output = here("Week_13","output","table.docx"))


library(wesanderson)
modelplot(models) +
  labs(x = 'Coefficients', 
       y = 'Term names') +
  scale_color_manual(values = wes_palette('Darjeeling1'))

models<- penguins %>%
  ungroup()%>% # the penguin data are grouped so we need to ungroup them
  nest(.by = species) %>% # nest all the data by species
  mutate(fit = map(data, ~lm(bill_length_mm~body_mass_g, data = .)))

results <- models %>%
  mutate(coeffs = map(fit, tidy), # look at the coefficients
         modelresults = map(fit, glance)) %>% # R2 and others 
  select(species, coeffs, modelresults) %>% # only keep the results
  unnest() # put it back in a dataframe and specify which columns to unnest

results

linear_reg()
lm_mod<-linear_reg() %>%
  set_engine("lm") %>%
  fit(bill_length_mm ~ bill_depth_mm*species, data = penguins) %>%
  tidy() %>%
  ggplot()+
  geom_point(aes(x = term, y = estimate))+
  geom_errorbar(aes(x = term, ymin = estimate-std.error,
                    ymax = estimate+std.error), width = 0.1 )+
  coord_flip()

lm_mod