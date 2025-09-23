### Today we are going to practice tidy with biogeochemistry data from Hawaii ####
### Created by: Ally Malilay #############
### Updated on: 2025-09-21 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(ggbernie)
### Load data ######
ChemData<-read_csv(here("Week_04","data", "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)
head(ChemData)
tail(ChemData)

#instead of using drop_na(), use filter()
ChemData_clean <- ChemData %>%
  filter(complete.cases(.))
View(ChemData_clean)

#Use separate() to separate a character column into multiple columns
?separate
?unite
ChemData_clean <- ChemData %>% #pipe ChemData
  drop_na() %>% #filter out everything that isn't a complete row
  separate(col = Tide_time, #choose tide_time column
           into = c("Tide", "Time"), #question: how do I know when to use c() ?. initially just wrote Tide, Time, no quotations either
           sep = "_", #separate by _
           remove = FALSE) %>% #keep Tide_Time column
  unite(col = "Site_Zone",
        c(Site, Zone), #question: why not into = c(Site, Zone)?
        sep = "_",
        remove = FALSE)
head(ChemData_clean)
View(ChemData_clean)
#convert ChemData to long data
ChemData_long <- ChemData_clean %>% #pipe data in
  pivot_longer(cols = Temp_in:percent_sgd, #choose columns to pivot 
               names_to = "Variables", #names of new columns. question: I used 'Biogeochemistry Variables' instead of Variables and got errors using it line 46. Why?
               values_to = "Values") #name of new column holding all variable values
View(ChemData_long)

ChemData_long %>% 
  group_by(`Variables`, Site) %>% #group by everything we want
  summarise(Param_means = mean(Values, na.rm = TRUE), #get means
            Param_vars = var(Values, na.rm = TRUE)) #get variance
View(ChemData_long)
#calculate mean variance and SD for all variables by site and zone and tide
#question: is this correct for ALL variables?
ChemData_long %>%
  group_by(Variables, Site, Zone, Tide) %>%
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars = var(Values, na.rm = TRUE),
            Param_SD = sd(Values, na.rm = TRUE))

# make a boxplot of raw values on y and site on x, facet wrap as a function of variable 
ChemData_long %>%
  ggplot(aes(x= Site, y = Values)) +
  geom_boxplot() +
  facet_wrap(~Variables, scales = "free")

# convert to wide data
ChemData_wide <-ChemData_long %>%
  pivot_wider(names_from = Variables, # column with the names for the new columns
              values_from = Values) # column with the values


# Now make a clean data set so we can export the csv, same steps and annotations as above just all piped together
ChemData_clean<-ChemData %>%
  drop_na() %>%
  separate(col = Tide_time,
           into = c("Tide", "Time"),
           sep = "_",
           remove = FALSE) %>%
  pivot_longer(cols = Temp_in:percent_sgd,
               names_to = "Variables",
               values_to = "Values") %>%
  group_by(Variables, Site, Time) %>%
  summarise(mean_vals= mean(Values, na.rm=TRUE)) %>%   
  pivot_wider(names_from=Variables,
              values_from= mean_vals)%>% ###want to widen the data to make it look better
  write_csv(here("Week_04", "Output", "summary.csv"))      ###export as csv file
View(ChemData_clean)  

  