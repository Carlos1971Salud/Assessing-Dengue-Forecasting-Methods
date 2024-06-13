library(dplyr) # Load dplyr library for data manipulation
library(forecast) # Load forecast library for time series forecasting

predict_AR<-function(data, window_size){
  # Initialize an empty data frame to store predictions and related information
  prediction_table <- data.frame(Datetime = character(), Real_Cases = numeric(), 
                                 Predicted_Cases_Week1 = numeric(),
                                 Predicted_Cases_Week2 = numeric(),
                                 Predicted_Cases_Week3 = numeric(),
                                 Predicted_Cases_Week4 = numeric(),
                                 Predicted_Cases_Week8 = numeric(), 
                                 Predicted_Cases_Week12 = numeric())
  
  # Iterate through the data to move the window and make predictions
  for (i in 1:(nrow(data) - window_size)) {
    # Subset the data for the current window
    current_window <- data[i:(i + window_size - 1), ]
    
    # Extract the target variable for prediction
    target_variable_week1 <- data[i + window_size, "casos"]
    target_variable_week2 <- data[i + window_size + 1, "casos"]# Predict 2 weeks ahead
    target_variable_week3 <- data[i + window_size + 2, "casos"]# Predict 3 weeks ahead
    target_variable_week4 <- data[i + window_size + 3, "casos"]# Predict 4 weeks ahead
    target_variable_week8 <- data[i + window_size + 7, "casos"]# Predict 8 weeks ahead
    target_variable_week12 <- data[i + window_size + 11, "casos"]# Predict 12 weeks ahead
    
    
    # Perform differencing to make the time series stationary
    differenced_data <- diff(current_window$casos)
    
    # Fit autoregressive model (AR) using the differenced data
    ar_model <- arima(differenced_data, order = c(1, 0, 0))  # AR(1) model
    
    # Make predictions for the next week
    differenced_prediction <- predict(ar_model, n.ahead = 12)$pred
    
    # Cumulatively sum the differenced predictions to get predictions on the original scale
    current_prediction <- cumsum(c(current_window$casos[length(current_window$casos)], differenced_prediction))
    
    # Extract the prediction for the specified number of weeks ahead
    current_prediction_week1 <- current_prediction[2]
    current_prediction_week2 <- current_prediction[3]
    current_prediction_week3 <- current_prediction[4]
    current_prediction_week4 <- current_prediction[5]
    current_prediction_week8 <- current_prediction[9]
    current_prediction_week12 <- current_prediction[13]
    
    prediction_table <- rbind(prediction_table, data.frame(
      Datetime = as.Date(data[i + window_size, "datetime"]),
      Real_Cases = as.numeric(target_variable_week1),
      Predicted_Cases_Week1 = round(as.numeric(current_prediction_week1)),
      Predicted_Cases_Week2 = round(as.numeric(current_prediction_week2)),
      Predicted_Cases_Week3 = round(as.numeric(current_prediction_week3)),
      Predicted_Cases_Week4 = round(as.numeric(current_prediction_week4)),
      Predicted_Cases_Week8 = round(as.numeric(current_prediction_week8)),
      Predicted_Cases_Week12 = round(as.numeric(current_prediction_week12))
    ))
  }
  prediction_table <- prediction_table %>% mutate(Predicted_Cases_Week2 = lag(Predicted_Cases_Week2),
                                                  Predicted_Cases_Week3 = lag(Predicted_Cases_Week3, 2),
                                                  Predicted_Cases_Week4 = lag(Predicted_Cases_Week4, 3),
                                                  Predicted_Cases_Week8 = lag(Predicted_Cases_Week8, 7), 
                                                  Predicted_Cases_Week12 = lag(Predicted_Cases_Week12, 11)
                                                  )
  return(prediction_table)
}



predict_MA<-function(data, window_size){
  prediction_table <- data.frame(Datetime = character(), Real_Cases = numeric(), 
                                 Predicted_Cases_Week1 = numeric(), 
                                 Predicted_Cases_Week2 = numeric(), 
                                 Predicted_Cases_Week3 = numeric(), 
                                 Predicted_Cases_Week4 = numeric(), 
                                 Predicted_Cases_Week8 = numeric(), 
                                 Predicted_Cases_Week12 = numeric()
                                 )
  
  # Iterate through the data to move the window and make predictions
  for (i in 1:(nrow(data) - window_size)) {
    # Subset the data for the current window
    current_window <- data[i:(i + window_size - 1), ]
    
    # Extract the target variable for prediction
    target_variable_week1 <- data[i + window_size, "casos"]
    target_variable_week2 <- data[i + window_size + 1, "casos"]# Predict 2 weeks ahead
    target_variable_week3 <- data[i + window_size + 2, "casos"]# Predict 3 weeks ahead
    target_variable_week4 <- data[i + window_size + 3, "casos"]# Predict 4 weeks ahead
    target_variable_week8 <- data[i + window_size + 7, "casos"]# Predict 8 weeks ahead
    target_variable_week12 <- data[i + window_size + 11, "casos"]# Predict 12 weeks ahead
    
    # Perform differencing to make the time series stationary
    differenced_data <- diff(current_window$casos)
    
    # Fit Moving Average (MA) model using the differenced data
    ma_model <- arima(differenced_data, order = c(0, 0, 1))  # MA(1) model
    
    # Make predictions for the next week
    # ma_prediction <- predict(ma_model, n.ahead = 1)$pred
    
    # Make predictions for the next week
    ma_prediction <- predict(ma_model, n.ahead = 12)$pred
    
    # Cumulatively sum the predictions to get predictions on the original scale
    current_prediction <- cumsum(c(current_window$casos[length(current_window$casos)], ma_prediction))
    
    # Extract the prediction for the specified number of weeks ahead
    current_prediction_week1 <- current_prediction[2]
    current_prediction_week2 <- current_prediction[3]
    current_prediction_week3 <- current_prediction[4]
    current_prediction_week4 <- current_prediction[5]
    current_prediction_week8 <- current_prediction[9]
    current_prediction_week12 <- current_prediction[13]
    
    prediction_table <- rbind(prediction_table, data.frame(
      Datetime = as.Date(data[i + window_size, "datetime"]),
      Real_Cases = as.numeric(target_variable_week1),
      Predicted_Cases_Week1 = round(as.numeric(current_prediction_week1)),
      Predicted_Cases_Week2 = round(as.numeric(current_prediction_week2)),
      Predicted_Cases_Week3 = round(as.numeric(current_prediction_week3)),
      Predicted_Cases_Week4 = round(as.numeric(current_prediction_week4)),
      Predicted_Cases_Week8 = round(as.numeric(current_prediction_week8)),
      Predicted_Cases_Week12 = round(as.numeric(current_prediction_week12))
    ))
  }
  prediction_table <- prediction_table %>% mutate(Predicted_Cases_Week2 = lag(Predicted_Cases_Week2),
                                                  Predicted_Cases_Week3 = lag(Predicted_Cases_Week3, 2),
                                                  Predicted_Cases_Week4 = lag(Predicted_Cases_Week4, 3),
                                                  Predicted_Cases_Week8 = lag(Predicted_Cases_Week8, 7), 
                                                  Predicted_Cases_Week12 = lag(Predicted_Cases_Week12, 11)
  )
  return(prediction_table)
}


predict_ARIMA<-function(data, window_size){
  # Initialize an empty data frame to store predictions and related information
  prediction_table <- data.frame(Datetime = character(), Real_Cases = numeric(), 
                                 Predicted_Cases_Week1 = numeric(),
                                 Predicted_Cases_Week2 = numeric(),
                                 Predicted_Cases_Week3 = numeric(),
                                 Predicted_Cases_Week4 = numeric(),
                                 Predicted_Cases_Week8 = numeric(), 
                                 Predicted_Cases_Week12 = numeric())
  
  # Iterate through the data to move the window and make predictions
  for (i in 1:(nrow(data) - window_size )) { # Adjusted to accommodate predicting 3 weeks ahead
    # Subset the data for the current window
    current_window <- data[i:(i + window_size - 1), ]
    
    # Extract the target variable for prediction
    target_variable_week1 <- data[i + window_size, "casos"]
    target_variable_week2 <- data[i + window_size + 1, "casos"]# Predict 2 weeks ahead
    target_variable_week3 <- data[i + window_size + 2, "casos"]# Predict 3 weeks ahead
    target_variable_week4 <- data[i + window_size + 3, "casos"]# Predict 4 weeks ahead
    target_variable_week8 <- data[i + window_size + 7, "casos"]# Predict 8 weeks ahead
    target_variable_week12 <- data[i + window_size + 11, "casos"]# Predict 12 weeks ahead
    
    # Fit ARIMA model using auto.arima
    arima_model <- auto.arima(current_window$casos,seasonal = FALSE)
    
    # Make predictions for the next week
    arima_prediction <- forecast(arima_model, h = 12)  # Predict 12 weeks ahead
    
    # Extract the predicted values
    current_prediction_week1 <- arima_prediction$mean[1]
    current_prediction_week2 <- arima_prediction$mean[2]
    current_prediction_week3 <- arima_prediction$mean[3]
    current_prediction_week4 <-  arima_prediction$mean[4]
    current_prediction_week8 <-  arima_prediction$mean[8]
    current_prediction_week12 <-  arima_prediction$mean[12]
    
    
    prediction_table <- rbind(prediction_table, data.frame(
      Datetime = as.Date(data[i + window_size, "datetime"]),
      Real_Cases = as.numeric(target_variable_week1),
      Predicted_Cases_Week1 = round(as.numeric(current_prediction_week1)),
      Predicted_Cases_Week2 = round(as.numeric(current_prediction_week2)),
      Predicted_Cases_Week3 = round(as.numeric(current_prediction_week3)),
      Predicted_Cases_Week4 = round(as.numeric(current_prediction_week4)),
      Predicted_Cases_Week8 = round(as.numeric(current_prediction_week8)),
      Predicted_Cases_Week12 = round(as.numeric(current_prediction_week12))
    ))
  }
  prediction_table <- prediction_table %>% mutate(Predicted_Cases_Week2 = lag(Predicted_Cases_Week2),
                                                  Predicted_Cases_Week3 = lag(Predicted_Cases_Week3, 2),
                                                  Predicted_Cases_Week4 = lag(Predicted_Cases_Week4, 3),
                                                  Predicted_Cases_Week8 = lag(Predicted_Cases_Week8, 7), 
                                                  Predicted_Cases_Week12 = lag(Predicted_Cases_Week12, 11)
  )
  return(prediction_table)
}

predict_ETS<-function(data, window_size){
  # Initialize an empty data frame to store predictions and related information
  prediction_table <- data.frame(Datetime = character(), Real_Cases = numeric(), 
                                 Predicted_Cases_Week1 = numeric(),
                                 Predicted_Cases_Week2 = numeric(),
                                 Predicted_Cases_Week3 = numeric(),
                                 Predicted_Cases_Week4 = numeric(),
                                 Predicted_Cases_Week8 = numeric(), 
                                 Predicted_Cases_Week12 = numeric())
  
  # Iterate through the data to move the window and make predictions
  for (i in 1:(nrow(data) - window_size)) {
    # Subset the data for the current window
    current_window <- data[i:(i + window_size - 1), ]
    
    # Extract the target variable for prediction
    target_variable_week1 <- data[i + window_size, "casos"]
    target_variable_week2 <- data[i + window_size + 1, "casos"]# Predict 2 weeks ahead
    target_variable_week3 <- data[i + window_size + 2, "casos"]# Predict 3 weeks ahead
    target_variable_week4 <- data[i + window_size + 3, "casos"]# Predict 4 weeks ahead
    target_variable_week8 <- data[i + window_size + 7, "casos"]# Predict 8 weeks ahead
    target_variable_week12 <- data[i + window_size + 11, "casos"]# Predict 12 weeks ahead
    
    # Perform differencing to make the time series stationary
    differenced_data <- diff(current_window$casos)
    
    # Fit ETS model using the differenced data
    ets_model <- ets(differenced_data)
    
    # Make predictions for the next week
    forecast_values <- forecast(ets_model, h = 12)
    differenced_prediction <- forecast_values$mean
    
    # Cumulatively sum the differenced predictions to get predictions on the original scale
    current_prediction <- cumsum(c(current_window$casos[length(current_window$casos)], differenced_prediction))
    
    # Extract the prediction for the specified number of weeks ahead
    current_prediction_week1 <- current_prediction[2]
    current_prediction_week2 <- current_prediction[3]
    current_prediction_week3 <- current_prediction[4]
    current_prediction_week4 <- current_prediction[5]
    current_prediction_week8 <- current_prediction[9]
    current_prediction_week12 <- current_prediction[13]
    
    prediction_table <- rbind(prediction_table, data.frame(
      Datetime = as.Date(data[i + window_size, "datetime"]),
      Real_Cases = as.numeric(target_variable_week1),
      Predicted_Cases_Week1 = round(as.numeric(current_prediction_week1)),
      Predicted_Cases_Week2 = round(as.numeric(current_prediction_week2)),
      Predicted_Cases_Week3 = round(as.numeric(current_prediction_week3)),
      Predicted_Cases_Week4 = round(as.numeric(current_prediction_week4)),
      Predicted_Cases_Week8 = round(as.numeric(current_prediction_week8)),
      Predicted_Cases_Week12 = round(as.numeric(current_prediction_week12))
    ))
  }
  prediction_table <- prediction_table %>% mutate(Predicted_Cases_Week2 = lag(Predicted_Cases_Week2),
                                                  Predicted_Cases_Week3 = lag(Predicted_Cases_Week3, 2),
                                                  Predicted_Cases_Week4 = lag(Predicted_Cases_Week4, 3),
                                                  Predicted_Cases_Week8 = lag(Predicted_Cases_Week8, 7), 
                                                  Predicted_Cases_Week12 = lag(Predicted_Cases_Week12, 11)
  )
  return(prediction_table)
}





