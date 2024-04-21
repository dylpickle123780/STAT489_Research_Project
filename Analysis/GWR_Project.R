library(GWmodel)
library(tidyverse)
library(sf)
library(sp)
library(ggfortify)
library(ggpubr)
library(covidcast)
library(tigris)

load("../Data/100k_shape_data.Rdata")

#Turning to spatial
model_data = joined_data %>% 
  mutate(price = log(price))%>% 
  st_transform(4326) %>% 
  as_Spatial() 

#Selecting Bandwidth
model_bandwidth = bw.gwr(price ~ Count + bedrooms + bathrooms + square_feet + population, data = model_data,
                         kernel = "exponential", parallel.method = "omp")

model_basic = gwr.basic(price ~ Count + bedrooms + bathrooms + square_feet + population, data = model_data, kernel = "exponential",
                        bw = model_bandwidth, parallel.method = "omp")

model_coefficients = cbind(model_basic$SDF$Count, model_basic$SDF$bedrooms, model_basic$SDF$bathrooms,
                           model_basic$SDF$square_feet, model_basic$SDF$population)

multicollin_diagnostic = gwr.collin.diagno(price ~ Count + bedrooms + bathrooms + square_feet + population, data = model_data, kernel = "exponential",
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
  mutate(Count = mean(model_data$Count),
         bedrooms = median(model_data$bedrooms),
         bathrooms = median(model_data$bathrooms),
         square_feet = mean(model_data$square_feet),
         POPESTIMATE2019 = mean(model_data$population)) %>%
  rename(population = POPESTIMATE2019) %>% 
  as_Spatial()

#Run prediction model
gwr_predictions = gwr.predict(model_data$price ~ ., model_data, test_data, 
                              model_bandwidth, kernel = "exponential")

model_results = gwr_predictions$SDF %>% as("sf")


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
  select(geometry) %>% 
  mutate(logFMR_0 = mean(fmr_data$logFMR_0),
         logFMR_1 = mean(fmr_data$logFMR_1),
         logFMR_3 = mean(fmr_data$logFMR_3),
         logFMR_4 = mean(fmr_data$logFMR_4),
         logPop2020 = mean(fmr_data$logPop2020)) %>% 
  as_Spatial()

#running prediction model
gwr_predictions_fmr = gwr.predict(model_data_fmr$logFMR_2 ~ ., model_data_fmr,
                                  test_data_fmr, model_bandwidth, 
                                  kernel = "exponential")

model_results_fmr = gwr_predictions_fmr$SDF %>% as("sf")

model_results$diff = model_results$prediction-model_results_fmr$prediction

model_results$re_scale_diff = exp(model_results$prediction) - exp(model_results_fmr$prediction)

#Significance tests for nonstationarity 
library(hexbin)

pl_1 = ggplot(model_results)+
  geom_sf(aes(fill = bedrooms_coef))+
  scale_fill_viridis_c(option = "magma")

pl_2 = ggplot(model_results)+
  geom_sf(aes(fill = bathrooms_coef))+
  scale_fill_viridis_c(option = "magma")

pl_3 = ggplot(model_results)+
  geom_sf(aes(fill = square_feet_coef))+
  scale_fill_viridis_c(option = "magma")

pl_4 = ggplot(model_results)+
  geom_sf(aes(fill = Count_coef))+
  scale_fill_viridis_c(option = "magma")

pl_5 = ggplot(model_results)+
  geom_sf(aes(fill = population_coef))+
  scale_fill_viridis_c(option = "magma")

gwr.montecarlo(price ~ ., data = model_data, kernel = "exponential", 
               bw =  model_bandwidth)

print(model_basic)

model_diag_results = model_adjusted$SDF %>% as("sf")

plot_significance = function(data = model_diag_results, par, coefficient){
  plot = ggplot(data)+
    geom_sf(aes(fill = ifelse(par <= 0.05, coefficient, NA)))+
    scale_fill_viridis_c(name = "P")
  return(plot)
}

pl_bedrooms = plot_significance(par = model_diag_results$bedrooms_p_fb, coefficient = model_coefficients[,4])

pl_bathrooms = plot_significance(par = model_diag_results$bathrooms_p_fb, coefficient = model_coefficients[,4])

pl_Count = plot_significance(par = model_diag_results$Count_p_fb, coefficient = model_coefficients[,4])

pl_square_feet = plot_significance(par = model_diag_results$square_feet_p_fb, coefficient = model_coefficients[,4])

pl_population = plot_significance(par = model_diag_results$population_p_fb, coefficient = model_coefficients[,4])


