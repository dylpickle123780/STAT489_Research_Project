set.seed("123780")
library(ggplot2)
library(sf)
library(sp)
library(tidyverse)
library(raster)

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

voronoi_polys = voronoi_polys %>% 
  mutate(bedrooms = beta_bedrooms,
         bathrooms =  beta_bathrooms,
         square_footage = beta_square_footage)

#now to generate number of beds and baths and square footage in each area 
bedrooms_i = rpois(number_polys,2)
bathrooms_i = rpois(number_polys,2)
square_feet_i = rnorm(number_polys, mean = 500, sd = 60)

#assume errors are ~N(0,20)
voronoi_polys = voronoi_polys %>% 
  mutate(price = bedrooms*bedrooms_i+bathrooms*bathrooms_i
         +square_footage*square_feet_i 
         + rnorm(50,mean = 0,sd = 20),
         bed_val = bedrooms_i,
         bath_val = bathrooms_i,
         footage_val = square_feet_i)

#Using GWR
library(GWmodel)

#Turning to spatial data frame
model_data = voronoi_polys[,c(1,5,6,7,8)] %>% as_Spatial()

#calculate bandwidth
model_bandwidth = bw.gwr(model_data$price ~ ., data = model_data,
                         kernel = "exponential", parallel.method = "omp")

#run model with calculated bandwidth and return model
model_gwr = gwr.basic(model_data$price ~ ., data = model_data,
                      bw = model_bandwidth, kernel = "exponential", 
                      parallel.method = "omp")

model_results = model_gwr$SDF %>% as("sf")

model_results = model_results%>% 
  mutate("Predicted Price" = yhat) %>%
  mutate("Price Abs Err" = abs(yhat - y)) %>% 
  mutate("Bedroom coefficient" = bed_val)

#First plot Predicted Value 
#Second plot Absolute Error
#Third plot bedroom coefficient

MAE = mean(abs(model_results$residual))
MSE = mean(model_results$residual^2)
RMSE = sqrt(mean(model_results$residual^2))
