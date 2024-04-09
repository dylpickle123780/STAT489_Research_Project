library(tidyverse)
library(foreach)
library(doParallel)
library(doRNG)
library(microbenchmark)
library(sf)
library(tigris)
library(caret)
set.seed("123780")

run_XGB_model = function(data, xgbGrid, folds = 5, ...){
    data_ctrl = trainControl(
      method = "cv",
      number = 5,
      seeds = NULL
    )
    
    xgboost_train = train(
      x = data_train %>% 
        select(!logFMR_2),   
      y = data_train$logFMR_2,
      trControl = data_ctrl,
      tuneGrid = xgbGrid,
      method = "xgbTree",
      verbose = FALSE,
      verbosity = 0
    )
    
    return(xgboost_train)
}

xgb_prediction = function(model, test_data){
  xgb_sacr_pred = test_data %>% 
    select(!logFMR_2) %>% 
    predict(model, .)
  return(xgb_sacr_pred)
}


plotting_xgb_predicted = function(data_xgb, predictions){
  #turning to plotable data
  data_xgb %>% 
    mutate("Predicted Price" = predictions) %>%
    ggplot() +
    geom_sf(aes(fill = `Predicted Price`),
            color = scales::alpha("black",
                                  alpha = 0.1)) +
    scale_fill_gradientn(colours = terrain.colors(8)) +
    theme(text = element_text(size = 20), 
          legend.position = "bottom") +
    labs(title = "Predicted Values for Price by XGB")
  
}

plotting_xgb_resids = function(data_xgb, predictions){
  #turning to plotable data
  data_xgb %>% 
    mutate("Predicted Price" = abs(predictions-logFMR_2)) %>%
    ggplot() +
    geom_sf(aes(fill = `Predicted Price`),
            color = scales::alpha("black",
                                  alpha = 0.1)) +
    scale_fill_gradientn(colours = terrain.colors(8)) +
    theme(text = element_text(size = 20), 
          legend.position = "bottom") +
    labs(title = "Absolute Prediction Error of Price by RF")
  
}

