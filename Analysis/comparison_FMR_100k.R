library(sf)
library(ggplot2)
library(tidyverse)

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

total_data <- total_data %>% filter(bedrooms>=1)

bedrooms <- total_data$bedrooms

bedroom_col <- data.frame(bedrooms)

fmrs <- total_data[,c(7:11)] %>% st_drop_geometry()

calc_table <- cbind(bedroom_col,fmrs)

interpolate_fmr <- function(bedrooms, fmr_0, fmr_1, fmr_2, fmr_3, fmr_4) {
  fmr_values <- c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4)
  bedroom_counts <- 0:4
  approx(bedroom_counts, fmr_values, xout = bedrooms)$y
}

df <- calc_table %>% 
  rowwise() %>% 
  mutate(price_fmr = interpolate_fmr(bedrooms, fmr_0, fmr_1, fmr_2, fmr_3, fmr_4)) %>% 
  select(price_fmr)

total_data <- cbind(total_data,df)

price_delt_finder_raw <- function(price,price_fmr) {
  return (price-price_fmr)
}

df2 <- total_data %>% 
  rowwise() %>% 
  mutate(price_delt = price_delt_finder_raw(price,price_fmr)) %>% 
  select(price_delt) %>% st_drop_geometry()

avg_delt <- mean(df2$price_delt)

price_delt_finder_adj <- function(price_delt) {
  return (price_delt-avg_delt)
}

df2 <- df2 %>% 
  rowwise() %>% 
  mutate(price_delt_adj = price_delt_finder_adj(price_delt)) %>% 
  st_drop_geometry()

total_data <- cbind(total_data,df2)

ggplot(aes(),data=total_data) + #
  geom_sf(aes(fill = price_delt_adj))+
  scale_fill_gradientn(colours = terrain.colors(8))

save(total_data,file = "Data/Total_data.Rdata")







