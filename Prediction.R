library(tidyverse)

# Create a data frame with historical data
data <- data.frame(
  Year = c(2018, 2019, 2020, 2021, 2022),  # Years
  Region1 = c(10, 15, 18, 21, 25),  # Region 1 data
  Region2 = c(8, 11, 13, 16, 19),   # Region 2 data
  Region3 = c(20, 22, 25, 28, 32),  # Region 3 data
  Region4 = c(5, 7, 9, 12, 14),     # Region 4 data
  Region5 = c(30, 35, 40, 45, 50)   # Region 5 data
)

# Fit linear regression models for each region
model_region1 <- lm(Region1 ~ Year, data = data)
model_region2 <- lm(Region2 ~ Year, data = data)
model_region3 <- lm(Region3 ~ Year, data = data)
model_region4 <- lm(Region4 ~ Year, data = data)
model_region5 <- lm(Region5 ~ Year, data = data)


# Create a data frame with the years for prediction
new_years <- data.frame(Year = c(2023, 2024, 2025, 2026, 2027))

# Make predictions for each region
predictions_region1 <- predict(model_region1, newdata = new_years)
predictions_region2 <- predict(model_region2, newdata = new_years)
predictions_region3 <- predict(model_region3, newdata = new_years)
predictions_region4 <- predict(model_region4, newdata = new_years)
predictions_region5 <- predict(model_region5, newdata = new_years)

# Print predicted values
predicted_values <- data.frame(
  Year = new_years$Year,
  Region1 = predictions_region1,
  Region2 = predictions_region2,
  Region3 = predictions_region3,
  Region4 = predictions_region4,
  Region5 = predictions_region5
)
print(predicted_values)



