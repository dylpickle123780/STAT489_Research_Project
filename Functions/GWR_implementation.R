#GWR Implementation 

plotting_GWR_predicted = function(model){
  #turning to plotable data
  model_results = model$SDF %>% as("sf")
  
  model_results %>% 
    mutate("Predicted Price" = yhat) %>%
    ggplot() +
    geom_sf(aes(fill = `Predicted Price`),
            color = scales::alpha("black",
                                  alpha = 0.1)) +
    scale_fill_gradientn(colours = terrain.colors(8)) +
    theme(text = element_text(size = 20), 
          legend.position = "bottom") +
    labs(title = "Predicted Values for Price by GWR w/ exponential kernel")
}


plotting_GWR_residuals = function(model) {
  model_results = model$SDF %>% as("sf")
  
  model_results %>% 
    mutate("Price Abs Err" = abs(yhat - y)) %>%
    ggplot() +
    geom_sf(aes(fill = `Price Abs Err`),
            color = scales::alpha("black",
                                  alpha = 0.001)) +
    scale_fill_viridis_c(limits = c(0, 0.1),
                         breaks = c(0.01, 0.05, 0.1)) +
    theme(text = element_text(size = 20), 
          legend.position = "bottom") +
    labs(title = "Absolute Prediction Error of Price by GWR w/ \nexponential kernel")
}

plotting_GWR_bedrooms = function(model){
  model_results = model$SDF %>% as("sf")
  
  model_results %>% 
    mutate("Model variable" = bedrooms) %>%
    ggplot() +
    geom_sf(aes(fill = `bedrooms`),
            color = scales::alpha("black",
                                  alpha = 0.1)) +
    scale_fill_viridis_c(limits = c(-2000, 600),
                         breaks = c(-800, 0, 300)) +
    theme(text = element_text(size = 20), 
          legend.position = "bottom") +
    labs(title = "Beta Prediction by GWR w/ \nexponential kernel")
  
}