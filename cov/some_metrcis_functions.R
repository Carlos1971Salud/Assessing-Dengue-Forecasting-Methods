# Assuming prediction_table has columns Predicted_Cases_Week1 and Real_Cases

rmse<-function(prediction_table){
  # Calculate the squared differences
  squared_errors_1 <- (prediction_table$Predicted_Cases_Week1 - prediction_table$Real_Cases)^2
  squared_errors_2 <- (prediction_table$Predicted_Cases_Week2 - prediction_table$Real_Cases)^2
  squared_errors_3 <- (prediction_table$Predicted_Cases_Week3 - prediction_table$Real_Cases)^2
  squared_errors_4 <- (prediction_table$Predicted_Cases_Week4 - prediction_table$Real_Cases)^2
  squared_errors_8 <- (prediction_table$Predicted_Cases_Week8 - prediction_table$Real_Cases)^2
  squared_errors_12 <- (prediction_table$Predicted_Cases_Week12 - prediction_table$Real_Cases)^2
  
  # Calculate the mean of the squared differences
  mean_squared_error_1 <- mean(squared_errors_1, na.rm = TRUE)
  mean_squared_error_2 <- mean(squared_errors_2, na.rm = TRUE)
  mean_squared_error_3 <- mean(squared_errors_3, na.rm = TRUE)
  mean_squared_error_4 <- mean(squared_errors_4, na.rm = TRUE)
  mean_squared_error_8 <- mean(squared_errors_8, na.rm = TRUE)
  mean_squared_error_12 <- mean(squared_errors_12, na.rm = TRUE)
  
  
  # Calculate the square root of the mean squared error (RMSE)
  rmse_1 <- sqrt(mean_squared_error_1)
  rmse_2 <- sqrt(mean_squared_error_2)
  rmse_3 <- sqrt(mean_squared_error_3)
  rmse_4 <- sqrt(mean_squared_error_4)
  rmse_8 <- sqrt(mean_squared_error_8)
  rmse_12 <- sqrt(mean_squared_error_12)
  
  # Put all RMSE values into a list
  rmse_list <- list(rmse_1 = rmse_1, rmse_2 = rmse_2, rmse_3 = rmse_3, rmse_4 = rmse_4, rmse_8 = rmse_8,rmse_12 = rmse_12)
  
  # Print the RMSE
  return(rmse_list)
}


mape <- function(prediction_table) {
  # Calculate percentage errors, avoiding division by zero
  percent_errors_1 <- abs((prediction_table$Predicted_Cases_Week1 - prediction_table$Real_Cases) / prediction_table$Real_Cases) * 100
  percent_errors_2 <- abs((prediction_table$Predicted_Cases_Week2 - prediction_table$Real_Cases) / prediction_table$Real_Cases) * 100
  percent_errors_3 <- abs((prediction_table$Predicted_Cases_Week3 - prediction_table$Real_Cases) / prediction_table$Real_Cases) * 100
  percent_errors_4 <- abs((prediction_table$Predicted_Cases_Week4 - prediction_table$Real_Cases) / prediction_table$Real_Cases) * 100
  percent_errors_8 <- abs((prediction_table$Predicted_Cases_Week8 - prediction_table$Real_Cases) / prediction_table$Real_Cases) * 100
  percent_errors_12 <- abs((prediction_table$Predicted_Cases_Week12 - prediction_table$Real_Cases) / prediction_table$Real_Cases) * 100
  
  
  # Calculate the mean of percentage errors
  mape_1 <- mean(percent_errors_1, na.rm = TRUE)
  mape_2 <- mean(percent_errors_2, na.rm = TRUE)
  mape_3 <- mean(percent_errors_3, na.rm = TRUE)
  mape_4 <- mean(percent_errors_4, na.rm = TRUE)
  mape_8 <- mean(percent_errors_8, na.rm = TRUE)
  mape_12 <- mean(percent_errors_12, na.rm = TRUE)
  
  
  # Put all MAPE values into a list
  mape_list <- list(mape_1 = mape_1, mape_2 = mape_2, mape_3 = mape_3, mape_4 = mape_4, mape_8 = mape_8,mape_12 = mape_12)
  
  # Return the list of MAPE values
  return(mape_list)
}

mae <- function(prediction_table) {
  # Calculate absolute differences
  abs_errors_1 <- abs(prediction_table$Predicted_Cases_Week1 - prediction_table$Real_Cases)
  abs_errors_2 <- abs(prediction_table$Predicted_Cases_Week2 - prediction_table$Real_Cases)
  abs_errors_3 <- abs(prediction_table$Predicted_Cases_Week3 - prediction_table$Real_Cases)
  abs_errors_4 <- abs(prediction_table$Predicted_Cases_Week4 - prediction_table$Real_Cases)
  abs_errors_8 <- abs(prediction_table$Predicted_Cases_Week8 - prediction_table$Real_Cases)
  abs_errors_12 <- abs(prediction_table$Predicted_Cases_Week12 - prediction_table$Real_Cases)
  
  
  # Calculate the mean of absolute differences
  mae_1 <- mean(abs_errors_1, na.rm = TRUE)
  mae_2 <- mean(abs_errors_2, na.rm = TRUE)
  mae_3 <- mean(abs_errors_3, na.rm = TRUE)
  mae_4 <- mean(abs_errors_4, na.rm = TRUE)
  mae_8 <- mean(abs_errors_8, na.rm = TRUE)
  mae_12 <- mean(abs_errors_12, na.rm = TRUE)
  
  
  # Put all MAE values into a list
  mae_list <- list(mae_1 = mae_1, mae_2 = mae_2, mae_3 = mae_3, mae_4 = mae_4, mae_8 = mae_8,mae_12 = mae_12)
  
  # Return the list of MAE values
  return(mae_list)
}

smape <- function(prediction_table) {
  # Calculate absolute percentage errors, avoiding division by zero
  abs_percent_errors_1 <- 200 * abs(prediction_table$Predicted_Cases_Week1 - prediction_table$Real_Cases) / (abs(prediction_table$Predicted_Cases_Week1) + abs(prediction_table$Real_Cases))
  abs_percent_errors_2 <- 200 * abs(prediction_table$Predicted_Cases_Week2 - prediction_table$Real_Cases) / (abs(prediction_table$Predicted_Cases_Week2) + abs(prediction_table$Real_Cases))
  abs_percent_errors_3 <- 200 * abs(prediction_table$Predicted_Cases_Week3 - prediction_table$Real_Cases) / (abs(prediction_table$Predicted_Cases_Week3) + abs(prediction_table$Real_Cases))
  abs_percent_errors_4 <- 200 * abs(prediction_table$Predicted_Cases_Week4 - prediction_table$Real_Cases) / (abs(prediction_table$Predicted_Cases_Week4) + abs(prediction_table$Real_Cases))
  abs_percent_errors_8 <- 200 * abs(prediction_table$Predicted_Cases_Week8 - prediction_table$Real_Cases) / (abs(prediction_table$Predicted_Cases_Week8) + abs(prediction_table$Real_Cases))
  abs_percent_errors_12 <- 200 * abs(prediction_table$Predicted_Cases_Week12 - prediction_table$Real_Cases) / (abs(prediction_table$Predicted_Cases_Week12) + abs(prediction_table$Real_Cases))
  
  # Calculate the mean of absolute percentage errors
  smape_1 <- mean(abs_percent_errors_1, na.rm = TRUE)
  smape_2 <- mean(abs_percent_errors_2, na.rm = TRUE)
  smape_3 <- mean(abs_percent_errors_3, na.rm = TRUE)
  smape_4 <- mean(abs_percent_errors_4, na.rm = TRUE)
  smape_8 <- mean(abs_percent_errors_8, na.rm = TRUE)
  smape_12 <- mean(abs_percent_errors_12, na.rm = TRUE)
  
  # Put all SMAPE values into a list
  smape_list <- list(smape_1 = smape_1, smape_2 = smape_2, smape_3 = smape_3, smape_4 = smape_4, smape_8 = smape_8, smape_12 = smape_12)
  
  # Return the list of SMAPE values
  return(smape_list)
}



combine_metrics <- function(prediction_result) {
  
  rmse_list <- rmse(prediction_result) 
  mae_list <- mae(prediction_result)
  mape_list <- mape(prediction_result)
  smape_list <- smape(prediction_result)
  
  combined_table <- data.frame(
    RMSE = unlist(rmse_list),
    MAPE = unlist(mape_list),
    SMAPE = unlist(smape_list),
    MAE = unlist(mae_list),
    Week = rep(c(1, 2, 3, 4, 8, 12))
  )
  rownames(combined_table) <- paste0("Week", 1:nrow(combined_table))  # Setting row names
  
# Print the combined table
  return(combined_table)
}


