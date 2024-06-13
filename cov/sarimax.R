# Load necessary libraries
library(forecast)


# Define the function to predict using SARIMAX model
predict_sarimax <- function(data, window_size) {
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
    
    # Fit SARIMAX model using the differenced data and exogenous variables (umidmed and tempmed)
    sarimax_model <- auto.arima(current_window$casos, xreg = as.matrix(current_window[, c("umidmed", "tempmed")]))
    
    # Make predictions for the next 1, 2, and 3 weeks
    predictions_1W <- forecast(sarimax_model, h = 1, xreg = as.matrix(data[i + window_size, c("umidmed", "tempmed")]))
    exog_2w <- as.matrix(data[(i + window_size):(i + window_size + 1), c("umidmed", "tempmed")])
    predictions_2W <- forecast(sarimax_model, h = 2, xreg = exog_2w)

    exog_3w <- as.matrix(data[(i + window_size):(i + window_size + 2), c("umidmed", "tempmed")])
    predictions_3W <- forecast(sarimax_model, h = 3, xreg = exog_3w)
    
    exog_4w <- as.matrix(data[(i + window_size):(i + window_size + 3), c("umidmed", "tempmed")])
    predictions_4W <- forecast(sarimax_model, h = 4, xreg = exog_4w)
    
    exog_8w <- as.matrix(data[(i + window_size):(i + window_size + 7), c("umidmed", "tempmed")])
    predictions_8W <- forecast(sarimax_model, h = 8, xreg = exog_8w)
    
    exog_12w <- as.matrix(data[(i + window_size):(i + window_size + 11), c("umidmed", "tempmed")])
    predictions_12W <- forecast(sarimax_model, h = 12, xreg = exog_12w)
    
    # Extract the predicted values
    pred_1W <- predictions_1W$mean[1]
    pred_2W <- predictions_2W$mean[2] # Note: index 2 for 2 weeks ahead
    pred_3W <- predictions_3W$mean[3] # Note: index 3 for 3 weeks ahead
    pred_4W <- predictions_4W$mean[4]
    pred_8W <- predictions_8W$mean[8] 
    pred_12W <- predictions_12W$mean[12] 

    # Store the information in the data frame
    prediction_table <- rbind(prediction_table, data.frame(
      Datetime = as.Date(data[i + window_size, "datetime"]),
      Real_Cases = as.numeric(data[i + window_size, "casos"]),
      Predicted_Cases_Week1 = as.numeric(pred_1W),
      Predicted_Cases_Week2 = as.numeric(pred_2W),
      Predicted_Cases_Week3 = as.numeric(pred_3W),
      Predicted_Cases_Week4 = as.numeric(pred_4W),
      Predicted_Cases_Week8 = as.numeric(pred_8W),
      Predicted_Cases_Week12 = as.numeric(pred_12W)
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