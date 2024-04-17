set.seed("123780")
library(ggplot2)
library(sf)
library(sp)
library(tidyverse)

# Create a square boundary
square_bound = matrix(c(0,0,1,0,1,1,0,1,0,0),
                      ncol = 2, byrow = T) %>%
  list() %>% st_polygon()

num = c(1:500)
#Creating space for data
number_polys <- 500
voronoi_polys = st_sample(square_bound, number_polys) %>%
  do.call(c, .) %>% st_voronoi() %>%
  st_collection_extract() %>% st_intersection(square_bound) %>% st_sf() %>% 
  mutate(center = st_coordinates(st_centroid(geometry)))

voronoi_polys = voronoi_polys[order(voronoi_polys$center[,1]),1]


ggplot(voronoi_polys)+
  geom_sf(aes(fill = num))

#Generating data 

#each bedroom will increase rent by anywhere from 300 to 600
beta_bedrooms = seq(300,600,length.out = number_polys)
beta_bathrooms = seq(100,200,length.out = number_polys)
beta_square_footage = seq(0.5,1,length.out = number_polys)
beta_population = seq(0.005,0.01,length.out = number_polys)

voronoi_polys = voronoi_polys %>% 
  mutate(bedrooms = beta_bedrooms,
         bathrooms =  beta_bathrooms,
         square_footage = beta_square_footage,
         population = beta_population)

#Plot the Data
ggplot(voronoi_polys)+
  geom_sf(aes(fill = price),
          color = scales::alpha("black",alpha = 0.1))+
  scale_fill_gradientn(colours = terrain.colors(8))+
  theme(text = element_text(size = 20), 
        legend.position = "bottom")+
  labs(title = "Price")

#now to generate number of beds and baths and square footage in each area 
bedrooms_i = rpois(number_polys,2)
bathrooms_i = rpois(number_polys,2)
square_feet_i = rnorm(number_polys, mean = 500, sd = 60)
population_i = rbeta(number_polys,0.2,0.9)*1000000

#assume errors are ~N(0,20)
voronoi_polys = voronoi_polys %>% 
  mutate(price = bedrooms*bedrooms_i+bathrooms*bathrooms_i
         +square_footage*square_feet_i + population*population_i
         + rnorm(50,mean = 0,sd = 20),
         bed_val = bedrooms_i,
         bath_val = bathrooms_i,
         footage_val = square_feet_i,
         population_val = population_i)

#Create a random forest model on the spatial data
library(randomForest)
rf_model = randomForest(price ~ bedrooms + bathrooms + square_footage + population,
                        data = voronoi_polys)

#ggPlot the Random Forest Model
ggplot(voronoi_polys)+
  geom_sf(aes(fill = predict(rf_model, voronoi_polys)),
          color = scales::alpha("black",alpha = 0.1))+
  scale_fill_gradientn(colours = terrain.colors(8))+
  theme(text = element_text(size = 20), 
        legend.position = "bottom")+
  labs(title = "Predicted Values for Price by RF")


MSE = rf_model$mse

#Plot the MSE from within the RF model onto the Voronoi Polys
ggplot(voronoi_polys)+
  geom_sf(aes(fill = rf_model$MSE),
          color = scales::alpha("black",alpha = 0.1))+
  scale_fill_viridis_c(limits = c(0, 1000),
                       breaks = c(250, 500, 750))+
  theme(text = element_text(size = 20), 
        legend.position = "bottom")+
  labs(title = "Absolute Prediction Error of Price by RF")













