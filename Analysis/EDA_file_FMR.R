#Import libraries
library(tidyverse)
library(readxl)
library(sf)
library(ggfortify)
library(zipcodeR)

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

#Linear Model Analysis
fmr_lm_model <- lm(fmr_2024_cleaned2$zipcode~., data = fmr_2024_cleaned2)
summary(fmr_lm_model)

#Correlations
variable_correlations <- cor (fmr_2024_cleaned2)



#import dataset
fmr_2019_county = read_xlsx("./Data/FY2019_4050_FMRs_rev2.xlsx")

fmr_2019_county_reduced = fmr_2019_county %>% 
  select(countyname, state_alpha, fmr_0, fmr_1, fmr_2, fmr_3, fmr_4)


US_sh_file = st_read("./Data/cb_2022_us_county_5m/cb_2022_us_county_5m.shx")

#Remove Alaska Hawaii and territories
US_sh_file = US_sh_file %>% 
  filter(STUSPS!="AK",STUSPS!="HI",STUSPS!="PR",STUSPS!="VI",STUSPS!="GU",
         STUSPS!="AS",STUSPS!="MP") %>% 
  st_transform(4326)

fmr_data = right_join(US_sh_file, fmr_2019_county_reduced, 
                           by = c("NAMELSAD" = "countyname", 
                                  "STUSPS" = "state_alpha" ))
ggplot(aes(),data=fmr_data) + #subtractions of geometry include territories and AK HI
  geom_sf(aes(fill = fmr_0))+
  scale_fill_gradientn(colours = terrain.colors(8))

save(fmr_data,file = "Data/FMR_shape_data.Rdata")








