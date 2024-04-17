#Transforming data into areal data
library(tidyverse)
library(sf)
library(sp)
library(ggfortify)
library(covidcast)
load("../Data/Cleaned_100k_data.Rdata")

#Transforming into coordinates
apartment_point = SpatialPointsDataFrame(coords = apartments_100k_cleaned[,c(3,2)],
                                                 data = apartments_100k_cleaned[,-c(2,3,4)])
apartment_point =  st_as_sf(apartment_point)
apartment_point <- st_set_crs(apartment_point,4326)

#Loading County Shape
US_sh_file = st_read("../Data/cb_2022_us_county_5m/cb_2022_us_county_5m.shx")

#Remove Alaska Hawaii and territories
US_sh_file = US_sh_file %>% 
  filter(STUSPS!="AK",STUSPS!="HI",STUSPS!="PR",STUSPS!="VI",STUSPS!="GU",
         STUSPS!="AS",STUSPS!="MP") %>% 
  st_transform(4326) %>% 
  left_join(county_census, by = join_by(GEOID == FIPS))


#Join county data with point data
joined_data = st_join(US_sh_file,
                      apartment_point,
                      join = st_contains)
#Keep necessary variables 
joined_data = joined_data[,c(20:25)]


joined_data = joined_data %>% 
  group_by(geometry) %>% 
  summarise(Count = n(), bedrooms = mean(bedrooms), bathrooms = mean(bathrooms),
            square_feet = mean(square_feet), price = mean(price), population = mean(POPESTIMATE2019), .groups="drop") %>% 
  drop_na()


#save joined data for use in models
save(joined_data,file = "../Data/100k_shape_data.Rdata")
