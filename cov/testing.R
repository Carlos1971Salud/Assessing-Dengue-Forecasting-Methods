data <- read.csv("data_with_covarites.csv")

sarimax_prediction_result <- predict_sarimax(data, window_size)
#write.csv(sarimax_prediction_result, file = "sarimax_results_table.csv", row.names = FALSE)


print(combine_metrics(sarimax_prediction_result))
#write.csv(combine_metrics(sarimax_prediction_result), file = "sarimax_results.csv", row.names = FALSE)


spline_result <-  predict_sarimax_with_splines(data, window_size = 313, degree = 3)
#write.csv(spline_result, file = "sarimax_spline_result_table.csv", row.names = FALSE)


print(combine_metrics(spline_result))
#write.csv(combine_metrics(spline_result), file = "spline_result.csv", row.names = FALSE)

spline_lag_result <- predict_sarimax_with_lags(data, window_size = 313,max_lag = 12)
#write.csv(spline_lag_result, file = "lag12_result_table.csv", row.names = FALSE)


print(combine_metrics(spline_lag_result ))
#write.csv(combine_metrics(spline_lag_result ), file = "lag12_result.csv", row.names = FALSE)
