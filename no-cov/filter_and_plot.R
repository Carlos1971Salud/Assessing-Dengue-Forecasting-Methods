data <- read.csv("data.csv")
head(data)


# Assuming data is a data frame with a column 'SE'
start_index <- 201601
end_index <- 202352

filter_data<-function(data,start_index,end_index){
  # Convert the string to a datetime object
  data$datetime <- as.Date(data$data_iniSE, format="%Y/%m/%d")
  
  # Filter the data
  filtered_data <- subset(data, SE >= start_index & SE <= end_index)
  filtered_data$week <- seq_len(nrow(filtered_data))
  
  return(filtered_data)
}



# Plotting
plot_dengue_cases <- function(data) {
  plot(data$datetime, data$casos, type = "l", lwd = 2,
       main = "Number of Dengue Cases per week in Rio de Janeiro",
       xlab = "time (week)", ylab = "Dengue Cases")
}

# Example usage:
# Assuming your data is stored in a data frame called 'your_data'
# Call the function with your data to generate the plot
plot_dengue_cases(filtered_data)
