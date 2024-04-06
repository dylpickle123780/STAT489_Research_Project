library(sf)
library(ggplot2)


load("./Data/100k_shape_data.Rdata")
load("./Data/FMR_shape_data.Rdata")

total_data <- joined_data %>% filter(joined_data$geometry %in% fmr_data$geometry)

fmr_data_reduced <- fmr_data %>% filter(fmr_data$geometry %in% joined_data$geometry)

total_data <- st_join(total_data,fmr_data_reduced)

total_data <- total_data %>% 
  group_by(geometry) %>% 
  summarise(Count = n(), bedrooms = mean(bedrooms), bathrooms = mean(bathrooms),
            square_feet = mean(square_feet), price = mean(price), fmr_0 = mean(fmr_0), fmr_1 = mean(fmr_1),fmr_2 = mean(fmr_2),fmr_3 = mean(fmr_3),fmr_4 = mean(fmr_4), .groups="drop") %>% 
  drop_na()
  
ggplot(aes(),data=total_data) + #subtractions of geometry include territories and AK HI
  geom_sf(aes(fill = fmr_0))+
  scale_fill_gradientn(colours = terrain.colors(8))

