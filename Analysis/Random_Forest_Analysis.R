#Load
library(tidyverse)
library(foreach)
library(doParallel)
library(doRNG)
library(sf)
library(tigris)
library(caret)
set.seed("123780")

load("../Data/100k_shape_data.Rdata")

#Turn multipolygon into centroid of each county for purpose of passing
#latitude and longitude into the random forest model to account for spatial effects
random_f_data = joined_data %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  mutate(longitude = st_coordinates(geometry)[,1], 
         latitude = st_coordinates(geometry)[,2],
         price = log(price),
         population = log(population),
         square_feet = log(square_feet)) %>% 
  st_drop_geometry()


# Create data split
## First, create training/testing split

rf_split = createDataPartition(random_f_data$price,
                               p = 0.8,
)[[1]]

apartment_train = random_f_data[rf_split,]
apartment_test = random_f_data[-rf_split,]

# Set up parallel training
compute_cluster = makeCluster(10)
registerDoParallel(compute_cluster)


####### Random Forest #########
source("../Functions/RF_implementation.R")

#Use number of parameters
rfGrid = expand.grid(
  mtry = c(1, 2, 3, 4, 5, 6)
)

random_f_model = run_rf_model(apartment_train, rfGrid)


# Random Forest predictions
model_predictions = rf_prediction(random_f_model, apartment_test)


root_mean = RMSE(model_predictions, apartment_test$price)
mean_absolute = MAE(model_predictions, apartment_test$price)
mean_squared = mean((model_predictions - apartment_test$price)^2)

comparison_metrics_RF = c(root_mean, mean_absolute, mean_squared)

stopCluster(compute_cluster)
