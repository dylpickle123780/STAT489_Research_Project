library(sf)





total_data <- joined_data %>% filter(joined_data$geometry %in% fmr_data$geometry)

fmr_data_reduced <- fmr_data %>% filter(fmr_data$geometry %in% joined_data$geometry)

total_data <- st_join(total_data,fmr_data_reduced)
  
ggplot(aes(),data=total_data) + #subtractions of geometry include territories and AK HI
  geom_sf(aes(fill = base_rent))+
  scale_fill_gradientn(colours = terrain.colors(8))

ggplot(aes(),data=fmr_data_reduced) + #subtractions of geometry include territories and AK HI
  geom_sf(aes(fill = base_rent))+
  scale_fill_gradientn(colours = terrain.colors(8))