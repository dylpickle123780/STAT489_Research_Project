#GWR Implementation 

GWR_model = function(data, predictor_variable = price, lon_lat_cols, non_num_cols = state){
  library(GWmodel)
  
  #select only numeric data for gwr model
  data_numeric = data %>% 
    select(!non_num_cols)
    
  #create spatial points data frame using lat and long
  data_numeric = SpatialPointsDataFrame(coords = data_numeric[,lon_lat_cols],
                            data = data_numeric[,-lon_lat_cols])
  
  
  model_bandwidth = bw.gwr(predictor_variable ~ ., data = data_no_state,
                           kernel = "exponential", parallel.method = "omp")
  
  #run model with calculated bandwidth and return model
  model_gwr = gwr.basic(predictor_variable ~ ., data = data_no_state,
                        bw = model_bandwidth, kernel = "exponential", 
                        parallel.method = "omp")
  
  return(model_gwr)
}

