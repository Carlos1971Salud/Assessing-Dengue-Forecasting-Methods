# Function to add lagged variables to the dataset
library(forecast)
library(dplyr)

# Function to add lagged variables to the dataset, replacing NAs caused by lagging with 0
add_lagged_variables <- function(data, max_lag) {
  lagged_data <- data
  for (lag in 1:max_lag) {
    # Create lagged variables for temperature and humidity
    lagged_data <- lagged_data %>%
      mutate(!!paste0("tempmed_lag", lag) := dplyr::lag(tempmed, lag, default = 0),
             !!paste0("umidmed_lag", lag) := dplyr::lag(umidmed, lag, default = 0))
  }
  return(lagged_data)
}

predict_sarimax_with_lags <- function(data, window_size, max_lag) {
  # Ensure data has lagged variables
  data_with_lags <- add_lagged_variables(data, max_lag)
  
  # Initialize the prediction table
  prediction_table <- data.frame(Datetime = character(), Real_Cases = numeric(),
                                 Predicted_Cases_Week1 = numeric(),
                                 Predicted_Cases_Week2 = numeric(),
                                 Predicted_Cases_Week3 = numeric(),
                                 Predicted_Cases_Week4 = numeric(),
                                 Predicted_Cases_Week8 = numeric(),
                                 Predicted_Cases_Week12 = numeric())
  
  for (i in 1:(nrow(data_with_lags) - window_size)) {
    current_window <- data_with_lags[i:(i + window_size - 1), ]
    
    # Selecting lagged and original exogenous variables
    exog_vars <- c("tempmed", "umidmed", grep("tempmed_lag|umidmed_lag", names(current_window), value = TRUE))
    xreg <- as.matrix(current_window[, exog_vars, drop = FALSE])
    
    sarimax_model <- auto.arima(current_window$casos, xreg = xreg)
    
    # Make predictions for the specified weeks ahead
    predictions_list <- setNames(vector("list", 6), c("1", "2", "3", "4", "8", "12"))
    for (weeks_ahead in c(1, 2, 3, 4, 8, 12)) {
      if (i + window_size - 1 + weeks_ahead <= nrow(data_with_lags)) {
        future_xreg <- as.matrix(data_with_lags[(i + window_size - 1 + weeks_ahead), exog_vars, drop = FALSE])
        prediction <- forecast(sarimax_model, h = 1, xreg = future_xreg)
        predictions_list[[as.character(weeks_ahead)]] <- prediction$mean
      } else {
        predictions_list[[as.character(weeks_ahead)]] <- NA
      }
    }
    
    # Append the prediction for this window to the prediction table
    prediction_table <- rbind(prediction_table, data.frame(
      Datetime = as.Date(data_with_lags[i + window_size, "datetime"]),
      Real_Cases = as.numeric(data_with_lags[i + window_size, "casos"]),
      Predicted_Cases_Week1 = as.numeric(predictions_list[["1"]]),
      Predicted_Cases_Week2 = as.numeric(predictions_list[["2"]]),
      Predicted_Cases_Week3 = as.numeric(predictions_list[["3"]]),
      Predicted_Cases_Week4 = as.numeric(predictions_list[["4"]]),
      Predicted_Cases_Week8 = as.numeric(predictions_list[["8"]]),
      Predicted_Cases_Week12 = as.numeric(predictions_list[["12"]])
    ))
  }
  
  prediction_table <- prediction_table %>% mutate(Predicted_Cases_Week2 = lag(Predicted_Cases_Week2),
                                                  Predicted_Cases_Week3 = lag(Predicted_Cases_Week3, 2),
                                                  Predicted_Cases_Week4 = lag(Predicted_Cases_Week4, 3),
                                                  Predicted_Cases_Week8 = lag(Predicted_Cases_Week8, 7), 
                                                  Predicted_Cases_Week12 = lag(Predicted_Cases_Week12, 11))
  
  return(prediction_table)
}





