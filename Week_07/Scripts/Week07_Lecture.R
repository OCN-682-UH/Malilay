### Today we are going to learn how to make maps, yay####
### Created by: Ally Malilay #############
### Updated on: 2025-10-07 ####################
#########################################################


#### Load Libraries ######
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)
library(ggmap)

# read in data on population in CA by county
popdata <- read_csv(here("Week_07","Data","CApopdata.csv"))

#read in data on number of seastars at diff field sites
stars <- read_csv(here("Week_07","Data","stars.csv"))

# get data for the entire world
world<-map_data("world")

#get data for usa
usa <- map_data("usa")
head(usa)

#get data for italy
italy <- map_data("italy")
head(italy)

#get data for states
states <- map_data("state")
head(states)

#get data for counties
counties <- map_data("county")
head(counties)

ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, 
                   group = group,
                   fill = region),
               color = "black") +
  guides(fill = FALSE) +
  theme_linedraw() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_map(projection = "sinusoidal",
            xlim = c(-180, 180))
# use states dataset
CA_data <- states %>%
  filter(region == "california")

#make a plot of california that is super beautoiful
ggplot() +
  geom_polygon(data = CA_data,
               aes(x = long, y = lat)) +
  coord_map(projection = "gilbert") +
  theme_void() 

  
#make new map 
CApop_county<-popdata %>%
  select("subregion" = County, Population)  %>% # rename the county col
  inner_join(counties) %>%
  filter(region == "california") # some counties have same names in other states


ggplot()+
  geom_polygon(data = CApop_county, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = Population),  
               color = "black")+
  geom_point(data = stars, # add a point at all my sites 
             aes(x = long, 
                 y = lat,
                 size = star_no))+ 
  coord_map()+
  theme_void() +
  scale_fill_gradient(trans = "log10")+
  labs(size = "# stars/m2") 

#Save the plot
ggsave(here("Week_07","Output","CApop.pdf"))

bbox <- c(left = 24.61, bottom = 59.37, right = 24.94, top = 59.5)
get_stadiamap(bbox, zoom = 12, maptype = "stamen_toner_lite") %>% ggmap()

df = data.frame(
  lon = c(24.7408933, 24.7456935, 24.745414, 24.7455573, 24.74466, 24.7420517, 24.7406695, 24.7503795, 24.7451665, 24.7480215, 24.7465048, 24.7476525, 24.7461528, 24.745517, 24.7470942, 24.7456197, 24.7468268, 24.7433477, 24.7439016, 24.7456759, 24.7454717, 24.7492029, 24.7538602, 24.7421455, 24.7442811, 24.7499494, 24.7541156, 24.7496663, 24.7407915, 24.749436, 24.7496639, 24.7473202, 24.7500224, 24.7459907, 24.7435694, 24.737213, 24.7411354, 24.7457404, 24.744481, 24.7461543, 24.7498606, 24.7424696, 24.7478131, 24.7427942, 24.7414089, 24.7495252, 24.749662, 24.7457476, 24.7493961, 24.7483363, 24.7508932, 24.7456813, 24.7514706, 24.7467884, 24.7500806, 24.7381424, 24.7385634, 24.7482397, 24.7487323, 24.7515429, 24.7446615, 24.748973, 24.7485532, 24.7468977, 24.7454333, 24.7384107, 24.7487002, 24.7535016, 24.7389838, 24.7453955, 24.7448729, 24.7380715, 24.7503209),
  lat = c(59.4358885, 59.4383619, 59.4348787, 59.4307098, 59.4342793, 59.4386533, 59.4362213, 59.4369682, 59.4376537, 59.438031, 59.4370001, 59.4371395, 59.4388086, 59.4401727, 59.4391757, 59.4377618, 59.4367379, 59.4383531, 59.4376105, 59.4353492, 59.4379686, 59.4367582, 59.4338407, 59.43228, 59.435726, 59.4315892, 59.4293218, 59.4366903, 59.4360766, 59.4363518, 59.4375351, 59.4342403, 59.4409327, 59.4321987, 59.4333018, 59.4364968, 59.4384542, 59.4368014, 59.437154, 59.4377442, 59.4386843, 59.4388372, 59.435591, 59.4351632, 59.434102, 59.4359086, 59.4379891, 59.441055, 59.4362933, 59.4404203, 59.4414969, 59.4410772, 59.440026, 59.4367293, 59.4292313, 59.4412534, 59.4371417, 59.4357248, 59.4358888, 59.4353297, 59.4298636, 59.4320464, 59.4321256, 59.4313161, 59.4318567, 59.4440948, 59.4402673, 59.4335765, 59.4413692, 59.4364992, 59.4364868, 59.4425823, 59.4368323)
)

qmplot(lon, lat, data = df, maptype = "stamen_toner_lite", color = I("red"))


p <- ggmap(get_googlemap(center = c(lon = -122.335167, lat = 47.608013),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))
m <- get_map(location = c(lon = -157.8583, lat = 21.3069), zoom = 12, source = "google")
ggmap(m)
