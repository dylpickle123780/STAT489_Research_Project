#Import libraries
library(tidyverse)
library(readxl)
library(sf)
library(ggfortify)
library(zipcodeR)
library(ggplot2)

#import revised dataset
fmr_2024 = read_xlsx("./Data/fy2024_erap_fmrs_revised.xlsx")

#clean dataset and remove extraneous columns 
fmr_2024 <- fmr_2024 %>% rename(zipcode = `ZIP
Code`)

  #967-- and above removes Alaska and Hawaii
fmr_2024_cleaned <- fmr_2024 %>% 
  filter(fmr_2024$zipcode %in% zip_code_db$zipcode)  %>% 
  select(3,4,5,6,7,8) %>% mutate(zipcode = as.numeric(zipcode))

fmr_2024_cleaned <- fmr_2024_cleaned %>% filter(!between(zipcode,96700,96899), !between(zipcode,99500,99999))
#add lattitude and longitude data
coords <- data.frame(geocode_zip(fmr_2024_cleaned$zipcode))

coords <- coords %>% mutate(zipcode = as.numeric(zipcode))

fmr_2024_cleaned2 <- fmr_2024_cleaned %>% filter(fmr_2024_cleaned$zipcode %in% coords$zipcode)

fmr_2024_cleaned2 <- distinct(fmr_2024_cleaned2)

fmr_2024_cleaned2 <- fmr_2024_cleaned2[!duplicated(fmr_2024_cleaned2$zipcode),]

fmr_2024_cleaned2 <- fmr_2024_cleaned2 %>% mutate(lat = geocode_zip(zipcode)$lat, long = geocode_zip(zipcode)$lng)

#add us map as a file
US_sh_file = read_sf("./Data/Country_Shape_file/cb_2022_us_state_500k.shx")

#plot of Studio Apartments
ggplot(aes(),data=US_sh_file[-c(19,29,35,48,53,54,55),]) + #subtractions of geometry include territories and AK HI
  geom_sf()+
  geom_point(aes(x=long,y=lat,color=erap_fmr_br0),data = fmr_2024_cleaned2)+
  scale_color_gradientn(colours = terrain.colors(10))

ggplot(aes(),data=US_sh_file[-c(19,29,35,48,53,54,55),]) + #subtractions of geometry include territories and AK HI
  geom_sf()+
  geom_point(aes(x=long,y=lat,color=erap_fmr_br1),data = fmr_2024_cleaned2)+
  scale_color_gradientn(colours = terrain.colors(10))

ggplot(aes(),data=US_sh_file[-c(19,29,35,48,53,54,55),]) + #subtractions of geometry include territories and AK HI
  geom_sf()+
  geom_point(aes(x=long,y=lat,color=erap_fmr_br2),data = fmr_2024_cleaned2)+
  scale_color_gradientn(colours = terrain.colors(10))

ggplot(aes(),data=US_sh_file[-c(19,29,35,48,53,54,55),]) + #subtractions of geometry include territories and AK HI
  geom_sf()+
  geom_point(aes(x=long,y=lat,color=erap_fmr_br3),data = fmr_2024_cleaned2)+
  scale_color_gradientn(colours = terrain.colors(10))

ggplot(aes(),data=US_sh_file[-c(19,29,35,48,53,54,55),]) + #subtractions of geometry include territories and AK HI
  geom_sf()+
  geom_point(aes(x=long,y=lat,color=erap_fmr_br4),data = fmr_2024_cleaned2)+
  scale_color_gradientn(colours = terrain.colors(10))

