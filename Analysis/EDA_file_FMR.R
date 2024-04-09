#Import libraries
library(tidyverse)
library(readxl)
library(sf)
library(ggfortify)

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

fmr_data$fmr_0 <- log(fmr_data$fmr_0)
fmr_data$fmr_1 <- log(fmr_data$fmr_1)
fmr_data$fmr_2 <- log(fmr_data$fmr_2)
fmr_data$fmr_3 <- log(fmr_data$fmr_3)
fmr_data$fmr_4 <- log(fmr_data$fmr_4)

fmr_data <- fmr_data %>% 
  rename(logFMR_0 = fmr_0, logFMR_1 = fmr_1, logFMR_2 = fmr_2, logFMR_3 = fmr_3, logFMR_4 = fmr_4)

fmr_data <- fmr_data %>% filter(!is.na(GEOID)) %>%
  select(logFMR_0, logFMR_1, logFMR_2, logFMR_3, logFMR_4, NAMELSAD, STUSPS, geometry)

fmr0_data <- fmr_data %>% select(logFMR_0, NAMELSAD, STUSPS, geometry)
fmr1_data <- fmr_data %>% select(logFMR_1, NAMELSAD, STUSPS, geometry)
fmr2_data <- fmr_data %>% select(logFMR_2, NAMELSAD, STUSPS, geometry)
fmr3_data <- fmr_data %>% select(logFMR_3, NAMELSAD, STUSPS, geometry)
fmr4_data <- fmr_data %>% select(logFMR_4, NAMELSAD, STUSPS, geometry)

save(fmr_data,file = "Data/FMR_shape_data.Rdata")
save(fmr0_data,file = "Data/FMR0_shape_data.Rdata")
save(fmr1_data,file = "Data/FMR1_shape_data.Rdata")
save(fmr2_data,file = "Data/FMR2_shape_data.Rdata")
save(fmr3_data,file = "Data/FMR3_shape_data.Rdata")
save(fmr4_data,file = "Data/FMR4_shape_data.Rdata")









