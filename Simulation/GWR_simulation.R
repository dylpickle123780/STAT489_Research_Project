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

#Generating data 

#each bedroom will increase rent by anywhere from 300 to 600
beta_bedrooms = (sin(3*voronoi_polys$center[,1]+3*voronoi_polys$center[,2])+1)*60
beta_bathrooms = (cos(5*voronoi_polys$center[,1])+cos(3*voronoi_polys$center[,2])+2)*60
beta_square_footage = (tan(1/2*voronoi_polys$center[,1]+1/2*voronoi_polys$center[,2])+1)/1000
beta_population = (cos(3*voronoi_polys$center[,1])+sin(5*voronoi_polys$center[,2])+2)/100000

voronoi_polys = voronoi_polys[,1]

voronoi_polys = voronoi_polys %>% 
  mutate(bedrooms = beta_bedrooms,
         bathrooms =  beta_bathrooms,
         square_footage = beta_square_footage,
         population = beta_population)

#now to generate number of beds and baths and square footage in each area 
bedrooms_i = rpois(number_polys,1)+1
bathrooms_i = rpois(number_polys,1)+1
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

#Using GWR
library(GWmodel)

#Turning to spatial data frame
simulation_model_data = voronoi_polys[,c(1,6,7,8,9,10)] %>% as_Spatial()

#calculate bandwidth
simulation_model_bandwidth = bw.gwr(simulation_model_data$price ~ ., data = simulation_model_data,
                         kernel = "exponential", parallel.method = "omp")

#run model with calculated bandwidth and return model
simulation_model_gwr = gwr.basic(simulation_model_data$price ~ ., data = simulation_model_data,
                      bw = simulation_model_bandwidth, kernel = "exponential", 
                      parallel.method = "omp")

simulation_results = simulation_model_gwr$SDF %>% as("sf")

simulation_results = simulation_results%>% 
  mutate("Predicted Price" = yhat) %>%
  mutate("Price Abs Err" = abs(yhat - y)) %>% 
  mutate("Bedroom coefficient" = bed_val) %>% 
  mutate("Bathrooms coefficient" = bath_val) %>% 
  mutate("Population coefficient" = population_val)

#First plot Predicted Value 
#Second plot Absolute Error
#Third plot bedroom coefficient

MAE_simulation = mean(abs(simulation_results$residual))
MSE_simulation = mean(simulation_results$residual^2)
RMSE_simulation = sqrt(mean(simulation_results$residual^2))


