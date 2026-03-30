attach(trainingModel)
attach(testingModel)
####################################
      # Ridge Regression #
####################################
# Load necessary libraries
library(glmnet)
library(dplyr)
str(trainingModel)
# Convert 'Doors' numerical variable into a factor in the dataset
trainingModel$Doors <- factor(trainingModel$Doors)
testingModel$Doors <- factor(testingModel$Doors)
trainingModel$Turbo <- factor(trainingModel$Turbo)
testingModel$Turbo <- factor(testingModel$Turbo)
# Check the structure to confirm the change
str(trainingModel$Doors)
str(testingModel$Doors)
str(trainingModel$Turbo)
str(testingModel$Turbo)
# Assuming your target variable is "Price_sqrt" and categorical variables are already identified
# Define the numeric columns
numeric_cols <- c("Car_Age", "Engine_volume", "Mileage_sqrt")  # Replace with actual numeric column names

# 1. Standardize the numerical variables (both training and testing datasets)
x_train_numeric <- scale(trainingModel[numeric_cols])  # Standardize training data
x_test_numeric <- scale(testingModel[numeric_cols])    # Standardize testing data

# 2. Create dummy variables for categorical predictors (ensure consistency)
categorical_cols <- c("Color","Category", "Leather_interior", "Fuel_type", "Turbo", "Cylinders", 
                      "Gear_box_type", "Drive_wheels", "Doors", "Airbags", "Manufacturer_Grouped","Manufacturer_Model_Grouped","Wheel")

# One-hot encode categorical variables
x_train_categorical <- model.matrix(~ . - 1, data = trainingModel[categorical_cols])
x_test_categorical <- model.matrix(~ . - 1, data = testingModel[categorical_cols])

# Ensure dummy variables are the same and ordered (using intersect to match columns)
common_cols <- intersect(colnames(x_train_categorical), colnames(x_test_categorical))
x_train_categorical <- x_train_categorical[, common_cols]
x_test_categorical <- x_test_categorical[, common_cols]

# 3. Combine standardized numeric and one-hot encoded categorical variables
x_train <- cbind(x_train_numeric, x_train_categorical)
x_test <- cbind(x_test_numeric, x_test_categorical)

# Check dimensions to ensure they match
dim(x_train)
dim(x_test)
# Fit the Ridge Regression model
ridge_model <- cv.glmnet(x = x_train, y = trainingModel$price_sqrt, alpha = 0)

# Make predictions on the testing data
test_predictions_ridge <- predict(ridge_model, newx = x_test)

# View the predicted values
head(test_predictions_ridge)

# 1. Predict on the training data
train_predictions <- predict(ridge_model, newx = x_train)

# 2. Calculate training MSE
train_mse <- mean((train_predictions - trainingModel$price_sqrt)^2)

# 3. Calculate training R-squared
train_residuals <- train_predictions - trainingModel$price_sqrt
train_tss <- sum((trainingModel$price_sqrt - mean(trainingModel$price_sqrt))^2)
train_r_squared <- 1 - sum(train_residuals^2) / train_tss

# 4. Predict on the testing data
test_predictions <- predict(ridge_model, newx = x_test)

# 5. Calculate testing MSE
test_mse <- mean((test_predictions - testingModel$price_sqrt)^2)

# 6. Calculate testing R-squared
test_residuals <- test_predictions - testingModel$price_sqrt
test_tss <- sum((testingModel$price_sqrt - mean(testingModel$price_sqrt))^2)
test_r_squared <- 1 - sum(test_residuals^2) / test_tss

# Print the results
cat("Training MSE: ", train_mse, "\n")
cat("Testing MSE: ", test_mse, "\n")
cat("Training R-squared: ", train_r_squared, "\n")
cat("Testing R-squared: ", test_r_squared, "\n")

