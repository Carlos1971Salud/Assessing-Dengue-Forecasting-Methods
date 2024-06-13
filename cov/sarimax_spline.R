library(forecast)
library(splines)
library(dplyr)

predict_sarimax_with_splines <- function(data, window_size, degree = 3) {
  prediction_table <- data.frame(Datetime = character(), Real_Cases = numeric(),
                                 Predicted_Cases_Week1 = numeric(),
                                 Predicted_Cases_Week2 = numeric(),
                                 Predicted_Cases_Week3 = numeric(),
                                 Predicted_Cases_Week4 = numeric(),
                                 Predicted_Cases_Week8 = numeric(),
                                 Predicted_Cases_Week12 = numeric())
  
  for (i in 1:(nrow(data) - window_size)) {
    current_window <- data[i:(i + window_size - 1), ]
    
    # Generate spline variables for temperature and humidity in the current window
    temp_spline <- ns(current_window$tempmed, df = degree)
    hum_spline <- ns(current_window$umidmed, df = degree)
    xreg_splines <- cbind(temp_spline, hum_spline)
    
    # Fit SARIMAX model with spline variables as exogenous regressors
    sarimax_model <- auto.arima(current_window$casos, xreg = xreg_splines)
    
    # Function to create spline variables for future exogenous data
    create_future_splines <- function(future_data) {
      temp_spline <- ns(future_data$tempmed, df = degree)
      hum_spline <- ns(future_data$umidmed, df = degree)
      return(cbind(temp_spline, hum_spline))
    }
    
    # Initialize a list to store predictions for different weeks
    predictions_list <- list()
    
    # Predictions for 1, 2, 3, 4, 8, and 12 weeks ahead
    for (weeks_ahead in c(1, 2, 3, 4, 8, 12)) {
      next_data <- data[(i + window_size):(i + window_size + weeks_ahead - 1), ]
      next_xreg_splines <- create_future_splines(next_data)
      predictions <- forecast(sarimax_model, h = weeks_ahead, xreg = next_xreg_splines)
      predictions_list[[as.character(weeks_ahead)]] <- predictions$mean[weeks_ahead]
    }
    
    # Store the information in the prediction_table
    prediction_table <- rbind(prediction_table, data.frame(
      Datetime = as.Date(data[i + window_size, "datetime"]),
      Real_Cases = as.numeric(data[i + window_size, "casos"]),
      Predicted_Cases_Week1 = as.numeric(predictions_list[["1"]]),
      Predicted_Cases_Week2 = as.numeric(predictions_list[["2"]]),
      Predicted_Cases_Week3 = as.numeric(predictions_list[["3"]]),
      Predicted_Cases_Week4 = as.numeric(predictions_list[["4"]]),
      Predicted_Cases_Week8 = as.numeric(predictions_list[["8"]]),
      Predicted_Cases_Week12 = as.numeric(predictions_list[["12"]])
    ))
  }
  
  # Adjusting predictions for lag to align with prediction weeks
  prediction_table <- prediction_table %>% 
    mutate(Predicted_Cases_Week2 = lag(Predicted_Cases_Week2),
           Predicted_Cases_Week3 = lag(Predicted_Cases_Week3, 2),
           Predicted_Cases_Week4 = lag(Predicted_Cases_Week4, 3),
           Predicted_Cases_Week8 = lag(Predicted_Cases_Week8, 7), 
           Predicted_Cases_Week12 = lag(Predicted_Cases_Week12, 11))
  
  return(prediction_table)
}


