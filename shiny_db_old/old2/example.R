# Load the gamm4 package
library(gamm4)
library(ggplot2)

# Simulate a time series dataset
set.seed(123)
n <- 100  # Number of time steps
time <- 1:n
rain <- rnorm(n, mean = 0, sd = 2)
# Simulate lake level using an autoregressive process
lake_level <- numeric(n)
lake_level[1] <- rnorm(1, mean = 100, sd = 5)
for (i in 2:n) {
  lake_level[i] <- 0.7 * lake_level[i - 1] + rnorm(1, mean = 0, sd = 2)
}
your_data <- data.frame(time = time, lake_level = lake_level, rain = rain)

# Split the data into training and test datasets
train_ratio <- 0.8  # 80% for training, 20% for testing
n_train <- round(n * train_ratio)
train_data <- your_data[1:n_train, ]
test_data <- your_data[(n_train + 1):n, ]

# Fit a GAM with autoregression using gamm4
# The 'ar()' term specifies an autoregressive structure
gam_model <- gamm4(lake_level ~ s(time, k = 10) + s(rain) + ar(time), data = train_data)

# Initialize vectors to store predicted lake level values
n_test <- n - n_train
predicted_lake_level <- numeric(n_test)

# Perform one-step-ahead predictions on the test data
for (i in 1:n_test) {
  # Create a data frame with the current 'time' and 'rain' values
  current_data <- data.frame(
    time = time[n_train + i],  # Assuming time increments by 1
    rain = test_data$rain[n_train + i]
  )
  
  # Predict 'lake_level' for the current time step
  predicted_lake_level[i] <- predict(gam_model, newdata = current_data)
}

# Create a data frame for the test data and predictions
results <- data.frame(
  Time = time[(n_train + 1):n],
  ObservedLakeLevel = test_data$lake_level,
  PredictedLakeLevel = predicted_lake_level
)

# Create a ggplot scatter plot to visualize the results
ggplot(results, aes(x = Time)) +
  geom_line(aes(y = ObservedLakeLevel, color = "Observed")) +
  geom_line(aes(y = PredictedLakeLevel, color = "Predicted"), linetype = "dashed") +
  scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red")) +
  labs(x = "Time", y = "Lake Level") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("Observed vs. Predicted Lake Level")
