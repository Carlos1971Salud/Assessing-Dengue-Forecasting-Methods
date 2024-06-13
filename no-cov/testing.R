#library(tidyverse)
data <- read.csv("data.csv")

start_index <- 201601
end_index <- 202352

filtered_data<-filter_data(data,start_index,end_index)
#plot_dengue_cases(filtered_data)

data<-filtered_data

# Set the window size
window_size <- 6 * 52 + 1

ar_prediction_result <- predict_AR(data, window_size)
ma_prediction_result <- predict_MA(data, window_size)
arima_prediction_result <- predict_ARIMA(data, window_size)
ets_prediction_result <- predict_ETS(data, window_size)




# calculate the RMSE, MAE ane MAPE
#print(rmse(ar_prediction_result))
#print(mae(ar_prediction_result))
#print(mape(ar_prediction_result))

print(combine_metrics(ar_prediction_result))
#write.csv(combine_metrics(ar_prediction_result), file = "ar_results.csv", row.names = FALSE)


#print(rmse(ma_prediction_result))
#print(mae(ma_prediction_result))
#print(mape(ma_prediction_result))
#write.csv(combine_metrics(ma_prediction_result), file = "ma_results.csv", row.names = FALSE)



#print(rmse(arima_prediction_result))
#print(mae(arima_prediction_result))
#print(mape(arima_prediction_result))
#write.csv(combine_metrics(arima_prediction_result), file = "arima_results.csv", row.names = FALSE)
print(combine_metrics(arima_prediction_result))



#print(rmse(ets_prediction_result))
#print(mae(ets_prediction_result))
#print(mape(ets_prediction_result))
#write.csv(combine_metrics(ets_prediction_result), file = "ets_results.csv", row.names = FALSE)







