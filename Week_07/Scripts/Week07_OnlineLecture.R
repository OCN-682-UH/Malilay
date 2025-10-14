### Today we are going to learn how to make maps online, yay####
### Created by: Ally Malilay #############
### Updated on: 2025-10-09 ####################
#########################################################


#### Load Libraries ######
library(ggmap)
library(tidyverse)
library(here)
library(ggspatial)

### Read in data
ChemData <- read_csv(here("Week_07","Data","chemicaldata_maunalua.csv"))
glimpse(ChemData)

#get base maps
Oahu <- get_map("Oahu")

#view map
ggmap(Oahu)

#make a data frame of lat and long coordinates to make center of map
WP <- data.frame(lon = -157.7621, lat = 21.27427) #coords for Wailupe

#get base layer
Map1 <- get_map(WP, zoom = 17, #3 is continent level, 20 is singular building
                maptype = "satellite") #change map type 
ggmap(Map1)

#try using different source for maptype
Map2<-get_map(WP,zoom = 17, maptype = "stamen_watercolor", source = "stadia")
ggmap(Map2)

#plot points on a map

ggmap(Map1) +
  geom_point(data = ChemData, 
             aes(x = Long, y = Lat,
                 color = Salinity),
             size = 4) +
  scale_color_viridis_c() +
  annotation_scale(bar_cols = c("yellow","white"),
                   location = "bl") +
  annotation_north_arrow(location = "tr") +
  coord_sf(crs = 4326)

#find lat long
geocode("Moku o Lo'e")
