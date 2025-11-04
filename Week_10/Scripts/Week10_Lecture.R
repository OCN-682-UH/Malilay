### Today we're going to learn how to use functions ###

##Load libraries
library(tidyverse)
library(ggplot2)

df <- tibble(
  a = rnorm(10), # draws 10 random values from a normal distribution
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10))
head(df)

#rescale every column individually
df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)))

#easy to make a mistake here and you could put an a instead of a b in a random place
df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)),
         b = (b-min(b, na.rm = TRUE))/(max(b, na.rm = TRUE)-min(b, na.rm = TRUE)),
         c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE)),
         d = (d-min(d, na.rm = TRUE))/(max(d, na.rm = TRUE)-min(d, na.rm = TRUE)))

#we can write a function
rescale01 <- function(x) {
  value<-(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  return(value)
}
df %>%
  mutate(a = rescale01(a),
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d))

df %>%
  mutate_all(rescale01)  # mutate every column with the rescale01 function

#----------------three key steps to creating a new function---------------------------
  #pick a name for the function
  #list the inputs, or arguments, to the function inside function. here we have one argument
      #if we had more the call would look like function(x,y,z)
  #place the code that has been developed in body of the function, a {block that follows the funct}
  #return tells us what values we want 

rescale01 <- function(x) { #function x is the thing being manipulated in the function
  value<-(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  return(value) #say what we want returned
} #close using {} so this is in the body of the function

# make a function to convert degrees fahrenheit to celsius
fahrenheit_to_celsius <- function(temp_F) { #can name it temp_F, does not have to be x
  temp_C <- (temp_F - 32) * 5 / 9 
  return(temp_C)
}

# convert celcius to kelvin
celcius_to_kelvin <- function(celsius) {
  kelvin <- celsius + 273.15
  return(kelvin)
}
# test it
celcius_to_kelvin(1)

library(palmerpenguins)
library(PNWColors) # for the PNW color palette 
# you may need to install the PNWColors library if you haven't used it yet
pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 

#------------------------- make ggplot function so you have the same theme for all graphs
ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = island))+
  geom_point()+
  geom_smooth(method = "lm")+ # add a linear model
  scale_color_manual("Island", values=pal)+   # use pretty colors and another example of how to manually change the legend title for colors
  theme_bw()

myplot<-function(data, x, y){ 
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+ #cannot be body_mass_gg for x = bc R cannot find this
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}

myplot(data = penguins, x = body_mass_g, y = bill_length_mm) #the x and y is specified here

#set data = penguins by putting data = penguins so that is the default data used
myplot<-function(data = penguins, x, y){
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}
myplot(x = body_mass_g, y = flipper_length_mm) #don't need to include data = penguins now bc of this

#you can now use it the same way you would use ggplot and use +
myplot(x = body_mass_g, y = flipper_length_mm)+
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)")

a <- 10
b <- 5

if (a > b) { # my question
  f <- 20 # if it is true give me answer 1
} else { # else give me answer 2 (else means if it is NOT TRUE)
  f <- 10
}
f

##------------apply if else to a plot---------------------------
myplot<-function(data = penguins, x, y, lines=TRUE ){ # add new argument for lines
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  if(lines==TRUE){
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      geom_smooth(method = "lm")+ # add a linear model
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
  }
  else{
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
  }
}


##with lines 
myplot(x = body_mass_g, y = flipper_length_mm)
#we do not want lines based on the above function
myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE)

library(emokid)
iamsad()
iamlesssad()
