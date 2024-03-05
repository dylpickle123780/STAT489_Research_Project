library(spatialreg); library(spdep); library(igraph); library(ggplot2); library(dplyr)
library(sf)

CAR_model_creation_from_data = function(data){
  #create spatial weights matrix
  W = poly2nb(data$geometry) %>% nb2listw(style = "W", zero.policy = TRUE)
  
  #create CAR model
  CAR_model = spautolm(price ~ bedrooms + bathrooms + square_feet, data = data, listw = W, family = "CAR")
  return(CAR_model)
}
model = CAR_model_creation_from_data(joined_data)


#Plot model onto the US has issues with entire country
plot_CAR_model_onto_US = function(model){
  joined_data %>% 
    mutate('model_result' = fitted.values(model)) %>%
    ggplot() + geom_sf(aes(fill = `model_result`), color = "white",
            color = scales::alpha("black",
                                  alpha = 0.1)) +
    scale_fill_gradientn(colors = terrain.colors(8)) +
    theme(text = element_text(size = 10), 
          legend.position = "bottom") +
    labs(title = "Predicted Values for Price by CAR Model", fill = "Predicted Price")
}
plot_CAR_model_onto_US(model)

#Plot residuals onto the US has issues with the entire country
plot_CAR_resids_onto_US = function(model){
  joined_data %>% 
    mutate('model_resids' = abs(residuals(model))) %>%
    ggplot() + geom_sf(aes(fill = `model_resids`), color = scales::alpha("black",
                                                                         alpha = 0.1)) +
    scale_fill_viridis_c(limits = c(0, 1000),
                         breaks = c(250, 500, 750)) +
    theme(text = element_text(size = 10), 
          legend.position = "bottom") +
    labs(title = "Residual Values for Price by CAR Model", fill = "Absolute Error in Price Prediction")
}





#Plot SQFT onto the US has issues with the entire country
plot_CAR_SQFT_onto_US = function(data){
  W = poly2nb(data$geometry) %>% nb2listw(style = "W", zero.policy = TRUE)
  model = spautolm(price ~ bedrooms, data = data, listw = W, family = "CAR")#, method = "SparseM", interval = 'optimize')

  
  joined_data %>% 
    mutate('model_result' = ) %>%
    ggplot() + geom_sf(aes(fill = `model_result`), color = "white",
                       color = scales::alpha("black",
                                             alpha = 0.1)) +
    scale_fill_gradientn(colors = terrain.colors(8)) +
    theme(text = element_text(size = 10), 
          legend.position = "bottom") +
    labs(title = "Predicted Values for Price by CAR Model", fill = "Predicted Price")
}
#bugged due to non symetrical weights

plot_CAR_SQFT_onto_US(joined_data)
