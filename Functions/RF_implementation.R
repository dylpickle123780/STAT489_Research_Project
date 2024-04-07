#split function
run_rf_model = function(data, rfGrid, folds = 5, ...){
  
  ## Set up cross validation using trainControl
  ## as well as seeds
  rf_ctrl = trainControl(
    method = "cv",
    number = folds,
    seeds = NULL
  )
  
  
  # train forest
  
  rf_train = train(
    x = (data %>% 
           select(-price_delt_adj) %>%
           st_drop_geometry()),
    y = data$price_delt_adj,
    trControl = rf_ctrl,
    tuneGrid = rfGrid,
    method = "rf"
  )
  #returns model 
  return(rf_train)
}

#returns vector of predictions
rf_prediction = function(model, test_data){
  
  rf_sacr_pred = test_data %>%
    st_drop_geometry() %>%
    select(!price_delt_adj) %>%
    predict(model, .)
  return(rf_sacr_pred)
}

plotting_rf_predicted = function(data_rf){
  #turning to plotable data
  
  data_rf %>% 
    mutate("Predicted Price" = predictions) %>%
    ggplot() +
    geom_sf(aes(fill = `Predicted Price`),
            color = scales::alpha("black",
                                  alpha = 0.1)) +
    scale_fill_gradientn(colours = terrain.colors(8)) +
    theme(text = element_text(size = 20), 
          legend.position = "bottom") +
    labs(title = "Predicted Values for Price by RF")
}


plotting_rf_residuals = function(data_rf) {
  
  data_rf %>% 
    mutate("Price Abs Err" = abs(predictions - price_delt_adj)) %>%
    ggplot() +
    geom_sf(aes(fill = `Price Abs Err`),
            color = scales::alpha("black",
                                  alpha = 0.1)) +
    scale_fill_viridis_c(limits = c(0, 1000),
                         breaks = c(250, 500, 750)) +
    theme(text = element_text(size = 20), 
          legend.position = "bottom") +
    labs(title = "Absolute Prediction Error of Price by RF")
}
