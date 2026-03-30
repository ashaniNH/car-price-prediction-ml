attach(trainingModel)
attach(testingModel)
library(dplyr)
trainingModel <- trainingModel %>%
  mutate_if(is.character, as.factor)

testingModel <- testingModel %>%
  mutate_if(is.character, as.factor)

trainingModel$Doors <- factor(trainingModel$Doors)
testingModel$Doors <- factor(testingModel$Doors)
trainingModel$Turbo <- factor(trainingModel$Turbo)
testingModel$Turbo <- factor(testingModel$Turbo)
str(trainingModel)
str(testingModel)

cylinderTrain <- table(trainingModel$Cylinders)
cylinderTest <- table(testingModel$Cylinders)
common_levels_c <- union(levels(trainingModel$Cylinders), levels(testingModel$Cylinders))
trainingModel$Cylinders <- factor(trainingModel$Cylinders, levels = common_levels_c)
testingModel$Cylinders <- factor(testingModel$Cylinders, levels = common_levels_c)
levels(testingModel$Cylinders)
levels(trainingModel$Cylinders)

fuelTrain <- table(trainingModel$Fuel_type)
fuelTest <- table(testingModel$Fuel_type)

common_levels_c <- union(levels(trainingModel$Fuel_type), levels(testingModel$Fuel_type))
trainingModel$Fuel_type <- factor(trainingModel$Fuel_type, levels = common_levels_c)
testingModel$Fuel_type <- factor(testingModel$Fuel_type, levels = common_levels_c)
levels(testingModel$Fuel_type)
levels(trainingModel$Fuel_type)
str(trainingModel)
str(testingModel)
colSums(is.na(trainingModel))
colSums(is.na(testingModel))

# Load necessary libraries
library(caret)
library(dplyr)
# Load necessary libraries
library(caret)
library(dplyr)

# Step 1: One-Hot Encoding for Categorical Variables
# Identify categorical variables
categorical_vars <- names(trainingModel)[sapply(trainingModel, is.factor) | sapply(trainingModel, is.character)]

# Create dummy variables for categorical predictors
dummy <- dummyVars(~ ., data = trainingModel[, categorical_vars], fullRank = TRUE)

# Apply one-hot encoding to training and testing data
training_encoded_categorical <- predict(dummy, newdata = trainingModel)
testing_encoded_categorical <- predict(dummy, newdata = testingModel)

# Step 2: Standardize Numerical Variables
# Identify numerical variables (excluding the target variable "price_sqrt")
numeric_vars <- names(trainingModel)[sapply(trainingModel, is.numeric)]
numeric_vars <- numeric_vars[!numeric_vars %in% c("price_sqrt")]

# Standardize numerical variables
preprocess_params <- preProcess(trainingModel[, numeric_vars, drop = FALSE], method = c("center", "scale"))
training_encoded_numeric <- predict(preprocess_params, trainingModel[, numeric_vars, drop = FALSE])
testing_encoded_numeric <- predict(preprocess_params, testingModel[, numeric_vars, drop = FALSE])

# Step 3: Combine One-Hot Encoded Categorical and Standardized Numerical Variables
# Combine categorical and numerical variables for training data
training_encoded <- cbind(training_encoded_categorical, training_encoded_numeric)
training_encoded$price_sqrt <- trainingModel$price_sqrt  # Add target variable

# Combine categorical and numerical variables for testing data
testing_encoded <- cbind(testing_encoded_categorical, testing_encoded_numeric)
testing_encoded$price_sqrt <- testingModel$price_sqrt  # Add target variable

# Ensure proper column names
colnames(training_encoded) <- make.names(colnames(training_encoded))
colnames(testing_encoded) <- make.names(colnames(testing_encoded))

# Check the final datasets
str(training_encoded)
str(testing_encoded)
####################################
     # Linear Regression Model #
####################################
# Load necessary libraries
#install.packages("Metrics")
library(dplyr)       # For data manipulation
library(Metrics)    # For calculating MSE
# Fit the linear regression model
linear_model <- lm(price_sqrt ~ ., data = training_encoded)
# View the model summary
summary(linear_model)
# Predict on training data
train_predictions <- predict(linear_model, newdata = training_encoded)

# Predict on testing data
test_predictions <- predict(linear_model, newdata = testing_encoded)
# Calculate training MSE
train_mse <- mse(training_encoded$price_sqrt, train_predictions)

# Calculate testing MSE
test_mse <- mse(testing_encoded$price_sqrt, test_predictions)

# Print MSE values
cat("Training MSE:", train_mse, "\n")
cat("Testing MSE:", test_mse, "\n")
# Calculate training R²
train_rsq <- 1 - sum((training_encoded$price_sqrt - train_predictions)^2) / sum((training_encoded$price_sqrt - mean(training_encoded$price_sqrt))^2)

# Calculate testing R²
test_rsq <- 1 - sum((testing_encoded$price_sqrt - test_predictions)^2) / sum((testing_encoded$price_sqrt - mean(testing_encoded$price_sqrt))^2)

# Print R² values
cat("Training R²:", train_rsq, "\n")
cat("Testing R²:", test_rsq, "\n")
###################################
      # forward selection #
###################################

# Install and load the leaps package
install.packages("leaps")
library(leaps)
# List of original categorical variables in your dataset
original_vars <- c("Airbags", "Category", "Gear_box_type", "Turbo","Color","Cylinders","Doors","Drive_wheels","Fuel_type","Leather_interior","Manufacturer","Model","Wheel")  # Add all categorical variables here

# Define the evaluate_model function
evaluate_model <- function(model, test_data) {
  # Make predictions
  predictions <- predict(model, newdata = test_data)
  
  # Calculate MSE
  mse_value <- mean((test_data$price_sqrt - predictions)^2)
  
  # Calculate R²
  rsq_value <- 1 - sum((test_data$price_sqrt - predictions)^2) / sum((test_data$price_sqrt - mean(test_data$price_sqrt))^2)
  
  # Return results as a list
  return(list(mse = mse_value, rsq = rsq_value))
}

# Forward selection
forward_model <- regsubsets(price_sqrt ~ ., data = trainingModel, method = "forward")
best_forward <- which.min(summary(forward_model)$bic)
selected_vars_forward <- names(coef(forward_model, id = best_forward))[-1]  # Exclude intercept

# Clean variable names
selected_vars_forward_clean <- unique(sapply(selected_vars_forward, function(x) {
  for (var in original_vars) {
    if (startsWith(x, var)) {
      return(var)
    }
  }
  return(NA)
}))
selected_vars_forward_clean <- na.omit(selected_vars_forward_clean)

# Fit the final model
final_forward_model <- lm(price_sqrt ~ ., data = trainingModel[, c("price_sqrt", selected_vars_forward_clean)])

# Evaluate the model
forward_results <- evaluate_model(final_forward_model, testingModel)

# Extract training predictions
train_predictions <- predict(final_forward_model, newdata = trainingModel)

# Extract residuals (actual - predicted)
train_residuals <- trainingModel$price_sqrt - train_predictions

# Calculate training MSE
train_mse <- mean(train_residuals^2)

# Calculate total sum of squares (SST)
sst <- sum((trainingModel$price_sqrt - mean(trainingModel$price_sqrt))^2)

# Calculate sum of squared residuals (SSR)
ssr <- sum(train_residuals^2)

# Calculate training R-squared
train_rsq <- 1 - (ssr / sst)

# Print training MSE and R-squared
cat("Training MSE:", train_mse, "\n")
cat("Training R²:", train_rsq, "\n")
cat("Forward Selection - Testing MSE:", forward_results$mse, "\n")
cat("Forward Selection - Testing R²:", forward_results$rsq, "\n")

###################################
      # backward selection #
###################################
# Install and load the leaps package
#install.packages("leaps")
library(leaps)

# List of original categorical variables in your dataset
original_vars <- c("Airbags", "Category", "Gear_box_type", "Turbo", "Color", "Cylinders", "Doors", "Drive_wheels", "Fuel_type", "Leather_interior", "Manufacturer", "Model", "Wheel")  # Add all categorical variables here

# Define the evaluate_model function
evaluate_model <- function(model, test_data) {
  # Make predictions
  predictions <- predict(model, newdata = test_data)
  
  # Calculate MSE
  mse_value <- mean((test_data$price_sqrt - predictions)^2)
  
  # Calculate R²
  rsq_value <- 1 - sum((test_data$price_sqrt - predictions)^2) / sum((test_data$price_sqrt - mean(test_data$price_sqrt))^2)
  
  # Return results as a list
  return(list(mse = mse_value, rsq = rsq_value))
}

# Backward selection
backward_model <- regsubsets(price_sqrt ~ ., data = trainingModel, method = "backward")

# Summarize the best model at each step
summary(backward_model)

# Get the best model based on a specific criterion (e.g., BIC)
best_backward <- which.min(summary(backward_model)$bic)

# Extract the selected predictors
selected_vars_backward <- names(coef(backward_model, id = best_backward))[-1]  # Exclude intercept

# Clean variable names
selected_vars_backward_clean <- unique(sapply(selected_vars_backward, function(x) {
  for (var in original_vars) {
    if (startsWith(x, var)) {
      return(var)
    }
  }
  return(NA)
}))
selected_vars_backward_clean <- na.omit(selected_vars_backward_clean)

# Fit the final model using the cleaned variable names
final_backward_model <- lm(price_sqrt ~ ., data = trainingModel[, c("price_sqrt", selected_vars_backward_clean)])

# Evaluate the model on the testing dataset
backward_results <- evaluate_model(final_backward_model, testingModel)

# Extract training predictions
train_predictions <- predict(final_backward_model, newdata = trainingModel)

# Extract residuals (actual - predicted)
train_residuals <- trainingModel$price_sqrt - train_predictions

# Calculate training MSE
train_mse <- mean(train_residuals^2)

# Calculate total sum of squares (SST)
sst <- sum((trainingModel$price_sqrt - mean(trainingModel$price_sqrt))^2)

# Calculate sum of squared residuals (SSR)
ssr <- sum(train_residuals^2)

# Calculate training R-squared
train_rsq <- 1 - (ssr / sst)

# Print training MSE and R-squared
cat("Training MSE:", train_mse, "\n")
cat("Training R²:", train_rsq, "\n")

# Print testing MSE and R-squared
cat("Backward Selection - Testing MSE:", backward_results$mse, "\n")
cat("Backward Selection - Testing R²:", backward_results$rsq, "\n")


# Compare selected predictors from forward and backward selection
cat("Forward Selection Selected Predictors:", selected_vars_forward_clean, "\n")
cat("Backward Selection Selected Predictors:", selected_vars_backward_clean, "\n")

# Check if the selected predictors are identical
if (identical(selected_vars_forward_clean, selected_vars_backward_clean)) {
  cat("Forward and Backward Selection selected the same predictors.\n")
} else {
  cat("Forward and Backward Selection selected different predictors.\n")
}
#################################
 # best subset selection #
################################
# Install and load the leaps package
#install.packages("leaps")
library(leaps)

# List of original categorical variables in your dataset
original_vars <- c("Airbags", "Category", "Gear_box_type", "Turbo", "Color", "Cylinders", "Doors", "Drive_wheels", "Fuel_type", "Leather_interior", "Manufacturer", "Model", "Wheel")  # Add all categorical variables here

# Define the evaluate_model function
evaluate_model <- function(model, test_data) {
  # Make predictions
  predictions <- predict(model, newdata = test_data)
  
  # Calculate MSE
  mse_value <- mean((test_data$price_sqrt - predictions)^2)
  
  # Calculate R²
  rsq_value <- 1 - sum((test_data$price_sqrt - predictions)^2) / sum((test_data$price_sqrt - mean(test_data$price_sqrt))^2)
  
  # Return results as a list
  return(list(mse = mse_value, rsq = rsq_value))
}

# Best subset selection with really.big = TRUE
best_subset_model <- regsubsets(price_sqrt ~ ., data = trainingModel, method = "exhaustive", really.big = TRUE)

# Summarize the best model at each step
summary(best_subset_model)

# Get the best model based on BIC
best_subset <- which.min(summary(best_subset_model)$bic)

# Extract the selected predictors
selected_vars_best_subset <- names(coef(best_subset_model, id = best_subset))[-1]  # Exclude intercept

# Clean variable names
selected_vars_best_subset_clean <- unique(sapply(selected_vars_best_subset, function(x) {
  for (var in original_vars) {
    if (startsWith(x, var)) {
      return(var)
    }
  }
  return(NA)
}))
selected_vars_best_subset_clean <- na.omit(selected_vars_best_subset_clean)

# Fit the final model using the cleaned variable names
final_best_subset_model <- lm(price_sqrt ~ ., data = trainingModel[, c("price_sqrt", selected_vars_best_subset_clean)])

# Evaluate the model on the testing dataset
best_subset_results <- evaluate_model(final_best_subset_model, testingModel)

# Extract training predictions
train_predictions <- predict(final_best_subset_model, newdata = trainingModel)

# Extract residuals (actual - predicted)
train_residuals <- trainingModel$price_sqrt - train_predictions

# Calculate training MSE
train_mse <- mean(train_residuals^2)

# Calculate total sum of squares (SST)
sst <- sum((trainingModel$price_sqrt - mean(trainingModel$price_sqrt))^2)

# Calculate sum of squared residuals (SSR)
ssr <- sum(train_residuals^2)

# Calculate training R-squared
train_rsq <- 1 - (ssr / sst)

# Print training MSE and R-squared
cat("Training MSE:", train_mse, "\n")
cat("Training R²:", train_rsq, "\n")

# Print testing MSE and R-squared
cat("Best Subset Selection - Testing MSE:", best_subset_results$mse, "\n")
cat("Best Subset Selection - Testing R²:", best_subset_results$rsq, "\n")



















# Load libraries
library(dplyr)
library(forcats)
library(caret)
library(Metrics)

# Step 1: Group rare levels
trainingModel_2 <- trainingModel %>%
  mutate(Manufacturer_Grouped = fct_lump(Manufacturer_Grouped, n = 10))  # Keep top 10 levels
testingModel_2 <- testingModel %>%
  mutate(Manufacturer_Grouped = fct_lump(Manufacturer_Grouped, n = 10))

# Step 2: Target encoding
target_encode <- function(train_data, test_data, cat_var, target_var) {
  encode_map <- train_data %>%
    group_by(!!sym(cat_var)) %>%
    summarise(target_mean = mean(!!sym(target_var)))
  
  train_data <- train_data %>%
    left_join(encode_map, by = cat_var) %>%
    mutate(!!paste0(cat_var, "_encoded") := target_mean) %>%
    select(-target_mean)
  
  test_data <- test_data %>%
    left_join(encode_map, by = cat_var) %>%
    mutate(!!paste0(cat_var, "_encoded") := target_mean) %>%
    select(-target_mean)
  
  return(list(train = train_data, test = test_data))
}

encoded_data <- target_encode(trainingModel_2, testingModel_2, "Manufacturer_Grouped", "LogPrice")
trainingModel_2 <- encoded_data$train
testingModel_2 <- encoded_data$test

# Step 3: Remove original categorical variable
trainingModel_2 <- trainingModel_2 %>%
  select(-Manufacturer_Grouped)
testingModel_2 <- testingModel_2 %>%
  select(-Manufacturer_Grouped)

# Step 4: Fit the model
linear_model <- lm(LogPrice ~ ., data = trainingModel_2)
summary(linear_model)

# Step 5: Make predictions and evaluate
train_predictions <- predict(linear_model, newdata = trainingModel_2)
test_predictions <- predict(linear_model, newdata = testingModel_2)

train_mse <- mse(trainingModel_2$LogPrice, train_predictions)
test_mse <- mse(testingModel_2$LogPrice, test_predictions)
train_rsq <- 1 - sum((trainingModel_2$LogPrice - train_predictions)^2) / sum((trainingModel_2$LogPrice - mean(trainingModel_2$LogPrice))^2)
test_rsq <- 1 - sum((testingModel_2$LogPrice - test_predictions)^2) / sum((testingModel_2$LogPrice - mean(testingModel_2$LogPrice))^2)

cat("Training MSE:", train_mse, "\n")
cat("Testing MSE:", test_mse, "\n")
cat("Training R²:", train_rsq, "\n")
cat("Testing R²:", test_rsq, "\n")


##########################################
       # Add interaction terms #
##########################################
# Load libraries
library(dplyr)
library(Metrics)

# Step 1: Add interaction terms to the model formula
# Example interactions:
# - Engine_volume * Turbo
# - Car_Age * Mileage_km
# - Fuel_type * Engine_volume

# Define the formula with interaction terms
formula_with_interactions <- LogPrice ~ . + Engine_volume * Turbo + Car_Age * Mileage_km + Fuel_type * Engine_volume

# Step 2: Fit the linear regression model with interaction terms
linear_model_interaction <- lm(formula_with_interactions, data = trainingModel)

# Step 3: View the model summary
summary(linear_model_interaction)

# Step 4: Make predictions on training and testing data
train_predictions_interaction <- predict(linear_model_interaction, newdata = trainingModel)
test_predictions_interaction <- predict(linear_model_interaction, newdata = testingModel)

# Step 5: Calculate MSE and R² for the model with interactions
train_mse_interaction <- mse(trainingModel$LogPrice, train_predictions_interaction)
test_mse_interaction <- mse(testingModel$LogPrice, test_predictions_interaction)
train_rsq_interaction <- 1 - sum((trainingModel$LogPrice - train_predictions_interaction)^2) / sum((trainingModel$LogPrice - mean(trainingModel$LogPrice))^2)
test_rsq_interaction <- 1 - sum((testingModel$LogPrice - test_predictions_interaction)^2) / sum((testingModel$LogPrice - mean(testingModel$LogPrice))^2)

# Step 6: Print results
cat("Training MSE (with interactions):", train_mse_interaction, "\n")
cat("Testing MSE (with interactions):", test_mse_interaction, "\n")
cat("Training R² (with interactions):", train_rsq_interaction, "\n")
cat("Testing R² (with interactions):", test_rsq_interaction, "\n")

# Original model (without interactions)
linear_model <- lm(LogPrice ~ ., data = trainingModel)

# Make predictions
train_predictions <- predict(linear_model, newdata = trainingModel)
test_predictions <- predict(linear_model, newdata = testingModel)

# Calculate MSE and R²
train_mse <- mse(trainingModel$LogPrice, train_predictions)
test_mse <- mse(testingModel$LogPrice, test_predictions)
train_rsq <- 1 - sum((trainingModel$LogPrice - train_predictions)^2) / sum((trainingModel$LogPrice - mean(trainingModel$LogPrice))^2)
test_rsq <- 1 - sum((testingModel$LogPrice - test_predictions)^2) / sum((testingModel$LogPrice - mean(testingModel$LogPrice))^2)

# Print results
cat("Training MSE (original):", train_mse, "\n")
cat("Testing MSE (original):", test_mse, "\n")
cat("Training R² (original):", train_rsq, "\n")
cat("Testing R² (original):", test_rsq, "\n")
str(trainingModel)

