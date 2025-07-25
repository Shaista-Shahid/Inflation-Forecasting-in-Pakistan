# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(patchwork)

# Load data and rename columns
df <- read_excel("C:/Users/faiza/Downloads/Data for Inflation Forecasting in Pakistan.xlsx")
colnames(df) <- c("Year", "Inflation", "ExchangeRate", "GDPGrowth", 
                  "Unemployment", "BroadMoney", "Exports", 
                  "Imports", "OilRents", "Remittances")

# View first few rows
head(df)

# Summary statistics
summary(df$BroadMoney)
summary(df$Inflation)
summary(df$ExchangeRate)
summary(df$GDPGrowth)
summary(df$Unemployment)
summary(df$Exports)
summary(df$Imports)
summary(df$OilRents)
summary(df$Remittances)

# Normalize the data
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

df_normalized <- df %>%
  mutate(across(c(Inflation, ExchangeRate, GDPGrowth, Unemployment, BroadMoney, 
                  Exports, Imports, OilRents, Remittances), normalize))

head(df_normalized)

# Pivot to long format for boxplot
df_long <- df_normalized %>%
  pivot_longer(cols = c(Inflation, ExchangeRate, GDPGrowth, Unemployment, BroadMoney, 
                        Exports, Imports, OilRents, Remittances),
               names_to = "Variable", values_to = "Value")

#boxplot
ggplot(df_long, aes(x = Value, y = Variable, fill = Variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Horizontal Box and Whisker Plots for Normalized Variables", 
       x = "Normalized Value", y = "Variables") +
  theme(axis.text.y = element_text(size = 10))





# Select only numeric (normalized) variables for scatter plot matrix
df_numeric <- df_normalized[, c("Inflation", "ExchangeRate", "GDPGrowth", "Unemployment", 
                                "BroadMoney", "Exports", "Imports", "OilRents", "Remittances")]

# Scatter plot matrix
ggpairs(df_numeric) +
  theme_minimal()


independent_vars <- c("ExchangeRate", "GDPGrowth", "Unemployment", "BroadMoney",
                      "Exports", "Imports", "OilRents", "Remittances")

# Generate individual scatter plots (tidy evaluation method)
plot_list <- lapply(independent_vars, function(var) {
  ggplot(df_numeric, aes(x = .data[[var]], y = Inflation)) +
    geom_point(color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    labs(title = paste("Inflation vs", var), x = var, y = "Inflation") +
    theme_minimal()
})

# Combine and display the plots in a grid
wrap_plots(plot_list, ncol = 3) +
  plot_annotation(title = "Scatter Plots of Inflation vs Independent Variables")


#ARIMA
# Enhanced time series plot
library(ggplot2)
library(tibble)

# Convert to a dataframe for ggplot
inflation_ts <- ts(df$Inflation, start = min(df$Year), frequency = 1)
fit <- auto.arima(inflation_ts)

inflation_df <- tibble(
  Year = as.numeric(time(inflation_ts)),
  Inflation = as.numeric(inflation_ts)
)

ggplot(inflation_df, aes(x = Year, y = Inflation)) +
  geom_line(color = "darkred", size = 1.2) +
  theme_minimal() +
  labs(title = "Inflation Time Series",
       x = "Year",
       y = "Inflation Rate")


library(forecast)

# Forecast the next 5 years (adjust h as needed, e.g., h = 5 for 5 years if yearly data)
forecast_horizon <- 5
inflation_forecast <- forecast(fit, h = forecast_horizon)


#R^2
SSE <- sum(residuals(fit)^2)  # Sum of squared errors
SST <- sum((inflation_ts - mean(inflation_ts))^2)  # Total sum of squares

r_squared <- 1 - SSE/SST
r_squared

#RIDGE
install.packages("glmnet")
library(glmnet)
# Response variable (Inflation)
y <- df$Inflation  

# Predictor variables (excluding Inflation and Year)
x <- as.matrix(df[, c("ExchangeRate", "GDPGrowth", "Unemployment", 
                      "BroadMoney", "Exports", "Imports", 
                      "OilRents", "Remittances")])

x_scaled <- scale(x)
# Perform Ridge regression with 10-fold cross-validation
cv_ridge <- cv.glmnet(x_scaled, y, alpha = 0)

# Best lambda value (penalty parameter)
best_lambda <- cv_ridge$lambda.min

# Fit model with best lambda
ridge_model <- glmnet(x_scaled, y, alpha = 0, lambda = best_lambda)

# Coefficients
coef(ridge_model)

# Predict using the fitted model
predictions <- predict(ridge_model, newx = x_scaled)

# Calculate R-squared manually
SSE <- sum((y - predictions)^2)
SST <- sum((y - mean(y))^2)
r_squared_ridge <- 1 - SSE/SST

r_squared_ridge

plot(cv_ridge)
title("Ridge Regression Cross-Validation", line = 2.5)

#Lasso
library(glmnet)
# x_scaled: matrix of standardized independent variables
# y: numeric vector of inflation values
#Fit the Lasso model (alpha = 1)
lasso_model <- glmnet(x_scaled, y, alpha = 1)

#Cross-validation to choose best lambda
cv_lasso <- cv.glmnet(x_scaled, y, alpha = 1)
plot(cv_lasso)
best_lambda <- cv_lasso$lambda.min


# Fit the final model using the best lambda
lasso_final <- glmnet(x_scaled, y, alpha = 1, lambda = best_lambda)
coef(lasso_final)

#Evaluate model performance
predictions_lasso <- predict(lasso_final, newx = x_scaled)
SSE <- sum((y - predictions_lasso)^2)
SST <- sum((y - mean(y))^2)
r_squared_lasso <- 1 - SSE/SST
r_squared_lasso

#Elastic net
set.seed(123)

# Elastic Net with alpha = 0.5 (equal mix of Lasso and Ridge)
elastic_net_cv <- cv.glmnet(x_scaled, y, alpha = 0.5)

# Get best lambda
best_lambda_en <- elastic_net_cv$lambda.min

#Fit the final Elastic Net model
elastic_net_model <- glmnet(x_scaled, y, alpha = 0.5, lambda = best_lambda_en)

#view thw coefficients
coef(elastic_net_model)

#predictions of r^2
predictions_en <- predict(elastic_net_model, newx = x_scaled)

SSE_en <- sum((y - predictions_en)^2)
SST_en <- sum((y - mean(y))^2)
r_squared_en <- 1 - SSE_en/SST_en

r_squared_en

#all values
# Store R^2 values in a data frame
r_squared_values <- data.frame(
  Model = c("ARIMA", "Ridge", "Lasso", "Elastic Net"),
  R_squared = c(r_squared, r_squared_ridge, r_squared_lasso, r_squared_en)
)

# Print R^2 values
print(r_squared_values)





library(patchwork)
library(ggplot2)

# Combine all plots together
plot_arima <- ggplot(forecast_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Forecast, color = "Forecast"), size = 1, linetype = "dashed") +
  labs(title = "ARIMA: Actual vs Forecasted", x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Forecast" = "blue")) +
  theme_minimal()
library(ggplot2)

plot_ridge <- ggplot(ridge_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted_Ridge, color = "Predicted (Ridge)"), size = 1, linetype = "dashed") +
  labs(title = "Ridge: Actual vs Predicted", x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Predicted (Ridge)" = "blue")) +
  theme_minimal()
library(ggplot2)


plot_lasso <- ggplot(lasso_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted_Lasso, color = "Predicted (Lasso)"), size = 1, linetype = "dashed") +
  labs(title = "Lasso: Actual vs Predicted", x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Predicted (Lasso)" = "blue")) +
  theme_minimal()

plot_elastic_net <- ggplot(elastic_net_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted_ElasticNet, color = "Predicted (Elastic Net)"), size = 1, linetype = "dashed") +
  labs(title = "Elastic Net: Actual vs Predicted", x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Predicted (Elastic Net)" = "purple")) +
  theme_minimal()

# Display all plots together
plot_arima + plot_ridge + plot_lasso + plot_elastic_net + plot_layout(ncol = 2)


#comparison of actual vs arima
# Forecast using ARIMA model
inflation_forecast <- forecast(fit, h = forecast_horizon)

# Check the forecast object to see if the forecasted values are available
print(inflation_forecast)

# Create the forecast_data with both actual and forecasted values
forecast_data <- data.frame(
  Year = c(time(inflation_ts), seq(max(time(inflation_ts)) + 1, by = 1, length.out = forecast_horizon)),
  Actual = c(as.numeric(inflation_ts), rep(NA, forecast_horizon)),  # Actual values (existing data)
  Forecast = c(rep(NA, length(inflation_ts)), inflation_forecast$mean)  # Forecasted values (new predictions)
)

# Check the head of forecast_data again
head(forecast_data)
# Plotting Actual vs Forecasted Values for ARIMA
ggplot(forecast_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Forecast, color = "Forecast"), size = 1, linetype = "dashed") +
  labs(title = "ARIMA: Actual vs Forecasted Inflation",
       x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Forecast" = "purple")) +
  theme_minimal()


# Check the predictions from the Ridge model
head(predictions_ridge)

# Ensure predictions_ridge is a numeric vector
predictions_ridge <- as.numeric(predictions_ridge)

# Create the ridge_data data frame
ridge_data <- data.frame(
  Year = df$Year,
  Actual = df$Inflation,
  Predicted_Ridge = predictions_ridge
)

# View the first few rows of ridge_data to ensure it's correct
head(ridge_data)

# Plot Actual vs Predicted (Ridge) Inflation
ggplot(ridge_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted_Ridge, color = "Predicted (Ridge)"), size = 1, linetype = "dashed") +
  labs(title = "Ridge Model: Actual vs Predicted Inflation",
       x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Predicted (Ridge)" = "purple")) +
  theme_minimal()



#lasso vs actual
# Make predictions for the Lasso model
predictions_lasso <- predict(lasso_final, newx = x_scaled)

# Ensure predictions_lasso is a numeric vector
predictions_lasso <- as.numeric(predictions_lasso)

# Create the lasso_data data frame
lasso_data <- data.frame(
  Year = df$Year,
  Actual = df$Inflation,
  Predicted_Lasso = predictions_lasso
)

# View the first few rows of lasso_data to ensure it's correct
head(lasso_data)
# Plot Actual vs Predicted (Lasso) Inflation
ggplot(lasso_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted_Lasso, color = "Predicted (Lasso)"), size = 1, linetype = "dashed") +
  labs(title = "Lasso Model: Actual vs Predicted Inflation",
       x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Predicted (Lasso)" = "purple")) +
  theme_minimal()



# Make predictions for the Elastic Net model
predictions_en <- predict(elastic_net_model, newx = x_scaled)

# Ensure predictions_en is a numeric vector
predictions_en <- as.numeric(predictions_en)

# Create the elastic_net_data data frame

elastic_net_data <- data.frame(
  Year = df$Year,
  Actual = df$Inflation,
  Predicted_ElasticNet = predictions_en
)
# View the first few rows of elastic_net_data to ensure it's correct
head(elastic_net_data)

# Plot Actual vs Predicted (Elastic Net) Inflation
ggplot(elastic_net_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted_ElasticNet, color = "Predicted (Elastic Net)"), size = 1, linetype = "dashed") +
  labs(title = "Elastic Net Model: Actual vs Predicted Inflation",
       x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Predicted (Elastic Net)" = "purple")) +
  theme_minimal()



# Combine the ARIMA, Ridge, Lasso, and Elastic Net plots
arima_plot <- ggplot(forecast_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Forecast, color = "Forecast"), size = 1, linetype = "dashed") +
  labs(title = "ARIMA: Actual vs Forecasted Inflation",
       x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Forecast" = "purple")) +
  theme_minimal()

ridge_plot <- ggplot(ridge_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted_Ridge, color = "Predicted (Ridge)"), size = 1, linetype = "dashed") +
  labs(title = "Ridge Model: Actual vs Predicted Inflation",
       x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Predicted (Ridge)" = "purple")) +
  theme_minimal()

lasso_plot <- ggplot(lasso_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted_Lasso, color = "Predicted (Lasso)"), size = 1, linetype = "dashed") +
  labs(title = "Lasso Model: Actual vs Predicted Inflation",
       x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Predicted (Lasso)" = "purple")) +
  theme_minimal()

elastic_net_plot <- ggplot(elastic_net_data, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted_ElasticNet, color = "Predicted (Elastic Net)"), size = 1, linetype = "dashed") +
  labs(title = "Elastic Net Model: Actual vs Predicted Inflation",
       x = "Year", y = "Inflation Rate") +
  scale_color_manual(values = c("Actual" = "darkred", "Predicted (Elastic Net)" = "purple")) +
  theme_minimal()

# Combine all the plots into one grid
combined_plots <- arima_plot + ridge_plot + lasso_plot + elastic_net_plot + plot_layout(ncol = 2)

# Print the combined plots
print(combined_plots)
ggplot(r_squared_values, aes(x = reorder(Model, -R_squared), y = R_squared, fill = Model)) +
  geom_col() +
  labs(title = "R² Comparison Across Models", x = "Model", y = "R-squared") +
  theme_minimal()


#ridge coefficient
as.matrix(coef(ridge_model))

#lasso coeffucients
as.matrix(coef(lasso_final))

#elastic net coefficients
as.matrix(coef(elastic_net_model))


#arima model summary
summary(fit)  # 'fit' is your ARIMA model





library(glmnet)
library(caret)  # for k-fold splitting
library(Metrics)  # for RMSE
set.seed(123)

# Create folds
folds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)


ridge_rmse <- c()

#ridge cv
for (i in 1:length(folds)) {
  test_indices <- folds[[i]]
  train_x <- x_scaled[-test_indices, ]
  train_y <- y[-test_indices]
  test_x <- x_scaled[test_indices, ]
  test_y <- y[test_indices]
  
  # Fit Ridge model (alpha = 0)
  model <- cv.glmnet(train_x, train_y, alpha = 0)
  pred <- predict(model, newx = test_x, s = model$lambda.min)
  
  # Calculate RMSE
  rmse_fold <- rmse(test_y, pred)
  ridge_rmse <- c(ridge_rmse, rmse_fold)
}

mean_ridge_rmse <- mean(ridge_rmse)
print(paste("Ridge 10-fold CV RMSE:", round(mean_ridge_rmse, 4)))



#lasso
lasso_rmse <- c()



for (i in 1:length(folds)) {
  test_indices <- folds[[i]]
  train_x <- x_scaled[-test_indices, ]
  train_y <- y[-test_indices]
  test_x <- x_scaled[test_indices, ]
  test_y <- y[test_indices]
  
  # Fit Lasso model (alpha = 1)
  model <- cv.glmnet(train_x, train_y, alpha = 1)
  pred <- predict(model, newx = test_x, s = model$lambda.min)
  
  # Calculate RMSE
  rmse_fold <- rmse(test_y, pred)
  lasso_rmse <- c(lasso_rmse, rmse_fold)
}

mean_lasso_rmse <- mean(lasso_rmse)
print(paste("Lasso 10-fold CV RMSE:", round(mean_lasso_rmse, 4)))


#elastic
elastic_net_rmse <- c()

for (i in 1:length(folds)) {
  test_indices <- folds[[i]]
  train_x <- x_scaled[-test_indices, ]
  train_y <- y[-test_indices]
  test_x <- x_scaled[test_indices, ]
  test_y <- y[test_indices]
  
  # Fit Elastic Net model (alpha = 0.5)
  model <- cv.glmnet(train_x, train_y, alpha = 0.5)
  pred <- predict(model, newx = test_x, s = model$lambda.min)
  
  # Calculate RMSE
  rmse_fold <- rmse(test_y, pred)
  elastic_net_rmse <- c(elastic_net_rmse, rmse_fold)
}

mean_en_rmse <- mean(elastic_net_rmse)
print(paste("Elastic Net 10-fold CV RMSE:", round(mean_en_rmse, 4)))



#summary cv
cv_results <- data.frame(
  Model = c("Ridge", "Lasso", "Elastic Net"),
  RMSE = c(mean_ridge_rmse, mean_lasso_rmse, mean_en_rmse)
)

library(Metrics)

# Function to calculate 10-fold CV MSE for a given alpha (0 = Ridge, 1 = Lasso, 0.5 = Elastic Net)
get_cv_mse <- function(x_scaled, y, alpha_value) {
  set.seed(123)
  folds <- createFolds(y, k = 10)
  mse_values <- c()
  
  for (i in 1:length(folds)) {
    test_idx <- folds[[i]]
    train_x <- x_scaled[-test_idx, ]
    train_y <- y[-test_idx]
    test_x <- x_scaled[test_idx, ]
    test_y <- y[test_idx]
    
    model <- cv.glmnet(train_x, train_y, alpha = alpha_value)
    pred <- predict(model, newx = test_x, s = model$lambda.min)
    
    mse_fold <- mse(test_y, pred)  # Mean Squared Error
    mse_values <- c(mse_values, mse_fold)
  }
  
  return(mean(mse_values))
}

# Compute MSE for Ridge (alpha = 0), Lasso (alpha = 1), Elastic Net (alpha = 0.5)
mse_ridge <- get_cv_mse(x_scaled, y, alpha_value = 0)
mse_lasso <- get_cv_mse(x_scaled, y, alpha_value = 1)
mse_elastic <- get_cv_mse(x_scaled, y, alpha_value = 0.5)

# Create a summary table
mse_results <- data.frame(
  Model = c("Ridge", "Lasso", "Elastic Net"),
  MSE = c(mse_ridge, mse_lasso, mse_elastic)
)

# Display the MSE results
print(mse_results)


library(Metrics)
library(caret)
library(glmnet)
library(forecast)

# Function to calculate 10-fold CV MSE for a given alpha (0 = Ridge, 1 = Lasso, 0.5 = Elastic Net)
get_cv_mse <- function(x_scaled, y, alpha_value) {
  set.seed(123)
  folds <- createFolds(y, k = 10)
  mse_values <- c()
  
  for (i in 1:length(folds)) {
    test_idx <- folds[[i]]
    train_x <- x_scaled[-test_idx, ]
    train_y <- y[-test_idx]
    test_x <- x_scaled[test_idx, ]
    test_y <- y[test_idx]
    
    model <- cv.glmnet(train_x, train_y, alpha = alpha_value)
    pred <- predict(model, newx = test_x, s = model$lambda.min)
    
    mse_fold <- mse(test_y, pred)  # Mean Squared Error
    mse_values <- c(mse_values, mse_fold)
  }
  
  return(mean(mse_values))
}

# Function to calculate ARIMA MSE
get_arima_mse <- function(y) {
  set.seed(123)
  folds <- createFolds(y, k = 10)
  mse_values <- c()
  
  for (i in 1:length(folds)) {
    test_idx <- folds[[i]]
    train_y <- y[-test_idx]
    test_y <- y[test_idx]
    
    # Fit ARIMA model on the training set
    arima_model <- auto.arima(train_y)
    
    # Forecast on the test set
    forecast_values <- forecast(arima_model, h = length(test_y))$mean
    
    # Calculate MSE for ARIMA
    mse_fold <- mse(test_y, forecast_values)
    mse_values <- c(mse_values, mse_fold)
  }
  
  return(mean(mse_values))
}

# Compute MSE for Ridge (alpha = 0), Lasso (alpha = 1), Elastic Net (alpha = 0.5)
mse_ridge <- get_cv_mse(x_scaled, y, alpha_value = 0)
mse_lasso <- get_cv_mse(x_scaled, y, alpha_value = 1)
mse_elastic <- get_cv_mse(x_scaled, y, alpha_value = 0.5)

# Compute MSE for ARIMA
mse_arima <- get_arima_mse(y)

# Create a summary table
mse_results <- data.frame(
  Model = c("Ridge", "Lasso", "Elastic Net", "ARIMA"),
  MSE = c(mse_ridge, mse_lasso, mse_elastic, mse_arima)
)
# Display the MSE results
print(mse_results)
df$GDPGrowth
sum(df$GDPGrowth)
        