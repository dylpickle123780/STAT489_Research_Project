library(GWmodel)
library(tidyverse)
library(sf)
library(sp)
library(ggfortify)
library(ggpubr)
library(covidcast)
library(tigris)
library(caret)
load("../Data/100k_shape_data.Rdata")

#Turning to spatial
model_data = joined_data %>% 
  mutate(price = log(price),
         population = log(population),
         square_feet = log(square_feet))%>%
  select(-Count) %>%
  st_transform(4326) %>% 
  as_Spatial() 

#Selecting Bandwidth
model_bandwidth = bw.gwr(price ~ bedrooms + bathrooms + square_feet + population, data = model_data,
                         kernel = "exponential", parallel.method = "omp")

#running model
model_basic = gwr.basic(price ~ bedrooms + bathrooms + square_feet + population, data = model_data, kernel = "exponential",
                        bw = model_bandwidth, parallel.method = "omp")

#Extracting model coefficients
model_coefficients = cbind(model_basic$SDF$bedrooms, model_basic$SDF$bathrooms,
                           model_basic$SDF$square_feet, model_basic$SDF$population)

#multicollinearity diagnostics
multicollin_diagnostic = gwr.collin.diagno(price ~ bedrooms + bathrooms + square_feet + population, data = model_data, 
                                           kernel = "exponential",
                                           bw = model_bandwidth)

model_adjusted = gwr.t.adjust(model_basic)

#Source in functions for plotting
source("../Functions/GWR_implementation.R")


#Creating prediction data
test_data = counties()%>% 
  filter(STATEFP!="72",
         STATEFP!="02",
         STATEFP!="66",
         STATEFP!="78",
         STATEFP!="15",
         STATEFP!="60",
         STATEFP!="69")%>% 
  left_join(county_census, by = join_by(GEOID == FIPS)) %>% 
  select(geometry,POPESTIMATE2019)%>% 
  mutate(bedrooms = median(model_data$bedrooms),
         bathrooms = median(model_data$bathrooms),
         square_feet = mean(model_data$square_feet)) %>%
  rename(population = POPESTIMATE2019) %>% 
  as_Spatial()

#Run prediction model
gwr_bathrooms = gwr.predict(model_data$bathrooms ~ 1, model_data, test_data, 
                            model_bandwidth, kernel = "exponential")

gwr_bedrooms = gwr.predict(model_data$bedrooms ~ 1, model_data, test_data, 
                            model_bandwidth, kernel = "exponential")

gwr_sqft = gwr.predict(model_data$square_feet ~ 1, model_data, test_data, 
            model_bandwidth, kernel = "exponential")

gwr_pop = gwr.predict(model_data$population ~ 1, model_data, test_data, 
            model_bandwidth, kernel = "exponential")

test_data = test_data %>% 
  as("sf") %>% 
  mutate(population = gwr_pop$SDF$prediction,
         bathrooms = gwr_bathrooms$SDF$prediction,
         bedrooms = gwr_bedrooms$SDF$prediction,
         square_feet = gwr_sqft$SDF$prediction) %>% 
  as_Spatial()

gwr_predictions = gwr.predict(model_data$price ~ ., model_data, test_data, 
                              model_bandwidth, kernel = "exponential")

model_results = gwr_predictions$SDF %>% as("sf")

#FMR
load("../Data/FMR_shape_data.Rdata")

#Turning to spatial
fmr_data = fmr_data[,c(1:6,9)]

model_data_fmr = fmr_data %>%
  as_Spatial()

#Selecting Bandwidth
model_bandwidth_fmr = bw.gwr(model_data_fmr$logFMR_2 ~ ., data = model_data_fmr,
                             kernel = "exponential", parallel.method = "omp")

#Creating prediction data
test_data_fmr = counties() %>% 
  filter(STATEFP!="72",
         STATEFP!="02",
         STATEFP!="66",
         STATEFP!="78",
         STATEFP!="15",
         STATEFP!="60",
         STATEFP!="69") %>% 
  left_join(county_census, by = join_by(GEOID == FIPS)) %>% 
  select(geometry, POPESTIMATE2019) %>% 
  mutate(logFMR_0 = mean(fmr_data$logFMR_0),
         logFMR_1 = mean(fmr_data$logFMR_1),
         logFMR_3 = mean(fmr_data$logFMR_3),
         logFMR_4 = mean(fmr_data$logFMR_4),
         logPop2020 = gwr_pop$SDF$prediction) %>% 
  select(-POPESTIMATE2019) %>% 
  as_Spatial()

#running prediction model
gwr_fm0 = gwr.predict(model_data_fmr$logFMR_0 ~ 1, model_data_fmr, test_data_fmr, 
                            model_bandwidth, kernel = "exponential")

gwr_fm1 = gwr.predict(model_data_fmr$logFMR_1 ~ 1, model_data_fmr, test_data_fmr, 
                            model_bandwidth, kernel = "exponential")

gwr_fm3 = gwr.predict(model_data_fmr$logFMR_3 ~ 1, model_data_fmr, test_data_fmr, 
                            model_bandwidth, kernel = "exponential")

gwr_fm4 = gwr.predict(model_data_fmr$logFMR_4 ~ 1, model_data_fmr, test_data_fmr, 
                            model_bandwidth, kernel = "exponential")


test_data_fmr = test_data_fmr %>% 
  as("sf") %>% 
  mutate(logFMR_0 = gwr_fm0$SDF$prediction,
         logFMR_1 = gwr_fm1$SDF$prediction,
         logFMR_3 = gwr_fm3$SDF$prediction,
         logFMR_4 = gwr_fm4$SDF$prediction) %>% 
  as_Spatial()

gwr_predictions_fmr = gwr.predict(model_data_fmr$logFMR_2 ~ ., model_data_fmr, test_data_fmr, 
                                  model_bandwidth, kernel = "exponential")

model_results_fmr = gwr_predictions_fmr$SDF %>% as("sf")

model_results$diff = model_results$prediction-model_results_fmr$prediction

model_results$re_scale_diff = exp(model_results$prediction) - exp(model_results_fmr$prediction)

library(hexbin)

#coefficient plots
pl_1 = ggplot(model_results)+
  geom_sf(aes(fill = bedrooms_coef),
          color = scales::alpha("black",
                                alpha = 0.1),
          name = "Coefficient values")+
  scale_fill_viridis_c(option = "magma")

pl_2 = ggplot(model_results)+
  geom_sf(aes(fill = bathrooms_coef),
          color = scales::alpha("black",
                                alpha = 0.1))+
  scale_fill_viridis_c(option = "magma")

pl_3 = ggplot(model_results)+
  geom_sf(aes(fill = square_feet_coef),
          color = scales::alpha("black",
                                alpha = 0.1))+
  scale_fill_viridis_c(option = "magma")

pl_4 = ggplot(model_results)+
  geom_sf(aes(fill = population_coef),
          color = scales::alpha("black",
                                alpha = 0.1))+
  scale_fill_viridis_c(option = "magma")

#Significance tests for nonstationarity 
significance_test = gwr.montecarlo(price ~ ., data = model_data, kernel = "exponential", 
               bw =  model_bandwidth)

model_diag_results = model_adjusted$SDF %>% as("sf")

plot_significance = function(data = model_diag_results, par, coefficient){
  plot = ggplot(data)+
    geom_sf(aes(fill = ifelse(par <= 0.05, coefficient, NA)))+
    scale_fill_viridis_c(name = "P")
  return(plot)
}

pl_bedrooms = plot_significance(par = model_diag_results$bedrooms_p_fb, coefficient = model_coefficients[,1])

pl_bathrooms = plot_significance(par = model_diag_results$bathrooms_p_fb, coefficient = model_coefficients[,2])

pl_square_feet = plot_significance(par = model_diag_results$square_feet_p_fb, coefficient = model_coefficients[,3])

pl_population = plot_significance(par = model_diag_results$population_p_fb, coefficient = model_coefficients[,4])


#Out of sample test
set.seed("123780")

gwr_data = joined_data %>% 
  mutate(price = log(price),
         square_feet = log(square_feet),
         population = log(population))


# Create data split
## First, create training/testing split

rf_split = createDataPartition(gwr_data$price,
                               p = 0.8,
)[[1]]

apartment_train = gwr_data[rf_split,] %>% as_Spatial()
apartment_test = gwr_data[-rf_split,] %>% as_Spatial()

train_bw = bw.gwr(price ~ ., apartment_train, kernel = "exponential")

gwr_predictions_out = gwr.predict(price ~ ., apartment_train, apartment_test, kernel = "exponential",
                          bw = train_bw)

root_mean = RMSE(gwr_predictions_out$SDF$prediction, apartment_test$price)
mean_absolute = MAE(gwr_predictions_out$SDF$prediction, apartment_test$price)
mean_squared = mean((gwr_predictions_out$SDF$prediction - apartment_test$price)^2)

comparison_metrics_GWR = c(root_mean, mean_absolute, mean_squared)
