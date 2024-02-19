#EDA of datasets and covariates for apartments for rent classified

library(tidyverse)
library(readxl)
library(sf)
library(ggfortify)

#loading in datasets
apartments_100k = read.csv("./Data/apartments_for_rent_classified_100K.csv",sep=";")
fmr_2024 = read_xlsx("./Data/FMR2024_final_revised.xlsx")

#Cleaning data for 100k file
#fmr_2024 file already cleaned

#Removed ~40 data points where format did not include all values or included null values in variables needed

#set variables to proper type
apartments_100k_cleaned <- apartments_100k %>% 
  filter(currency=="USD", latitude!="null", longitude!="null", price!="null", state!="AK", state!="HI", bathrooms!="null", bedrooms!="null") %>% 
  select(price, latitude, longitude, state, bathrooms, bedrooms, square_feet) %>% 
  mutate(price = as.numeric(price), latitude = as.numeric(latitude), longitude = as.numeric(longitude),
         state = as.factor(state), bathrooms = as.numeric(bathrooms), bedrooms = as.numeric(bedrooms),
         square_feet = as.numeric(square_feet)) %>% 
  filter(price<5000)

#reading in US shapefile to underlay the data
US_sh_file = read_sf("./Data/Country_Shape_file/cb_2022_us_state_500k.shx")


#creating basic plot with just price and latitude longitude

ggplot(aes(),data=US_sh_file[-c(19,29,35,48,53,54,55),]) + #subtractions of geometry include territories and AK HI
  geom_sf()+
  geom_point(aes(x=longitude,y=latitude,color=price),data = apartments_100k_cleaned)+
  scale_color_gradientn(colours = terrain.colors(8))

#Basic lm model
rent_pricing_model = lm(price~.-state,data=apartments_100k_cleaned)

summary(rent_pricing_model)

#analyzing correlations 
variable_correlations <- cor(apartments_100k_cleaned[,-4])

