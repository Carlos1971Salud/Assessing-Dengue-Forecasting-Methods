# Assessing-Dengue-Forecasting-Methods
codes for the paper *Assessing Dengue Forecasting Methods: A Comparative Study of Statistical Models and Machine Learning Techniques in Rio de Janeiro, Brazil* [pre-print version]( https://medrxiv.org/cgi/content/short/2024.06.12.24308827v1)


There are 2 parts of the models: first is using the cases itself (no-cov); the other is including covariates (cov).

## no-cov

The data is in `data.csv`, only including time and the dengue cases.

Main function is `testing.R`.

All the models are in the `predict_functions.R`.

Using `ar_prediction_result <- predict_AR(data, window_size)` can get a table of results including the real cases and predicting cases.

Then using `print(combine_metrics(ar_prediction_result))` you can get a table of all 3 metrics of the model: MAE, MAPE, and RMSE.



## Cov

The data is stored in `data_with_covarites.csv`, including time, cases, humidity and temperature.

The main function is `testing.R`, you can call `sarimax_prediction_result <- predict_sarimax(data, window_size)` to get the same result table of  real cases and predicting cases, the same as the no-cov.

Then using the same function `print(combine_metrics(sarimax_prediction_result))` you can get the metrics of MAE, MAPE, and RMSE.
