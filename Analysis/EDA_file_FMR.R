#Import libraries
library(tidyverse)
library(readxl)
library(sf)
library(ggfortify)

#import dataset
fmr_2019_county = read_xlsx("./Data/FY2019_4050_FMRs_rev2.xlsx")
fmr_2024_county = read_xlsx("./Data/FMR2024_final_revised.xlsx")

fmr_2019_county_reduced = fmr_2019_county %>% 
  select(fips2010,countyname, state_alpha, fmr_0, fmr_1, fmr_2, fmr_3, fmr_4)

fmr_2020_population = fmr_2024_county %>% select(fips2010, pop2020)

fmr_2019_county_reduced = left_join(fmr_2019_county_reduced, fmr_2020_population, by = "fips2010")


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
fmr_data$pop2020 <- log(fmr_data$pop2020+1)

fmr_data <- fmr_data %>% 
  rename(logFMR_0 = fmr_0, logFMR_1 = fmr_1, logFMR_2 = fmr_2, logFMR_3 = fmr_3, logFMR_4 = fmr_4, logPop2020 = pop2020)

hist(fmr_data$logPop2020)

fmr_data <- fmr_data %>% filter(!is.na(GEOID),!is.na(logPop2020)) %>%
  select(logFMR_0, logFMR_1, logFMR_2, logFMR_3, logFMR_4, logPop2020,NAMELSAD, STUSPS, geometry)

is.finite(fmr_data$logPop2020) %>% all()

save(fmr_data,file = "Data/FMR_shape_data.Rdata")










