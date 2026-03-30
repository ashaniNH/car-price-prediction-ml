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
library(xgboost)
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

# Step 3: Prepare Data for XGBoost
train_x <- as.matrix(training_encoded[, -which(names(training_encoded) == "price_sqrt")])
train_y <- trainingModel$price_sqrt

test_x <- as.matrix(testing_encoded[, -which(names(testing_encoded) == "price_sqrt")])
test_y <- testingModel$price_sqrt

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)



# Load necessary libraries
library(caret)
library(xgboost)
library(dplyr)

# Step 1: Define the Grid of Hyperparameters
xgb_grid <- expand.grid(
  nrounds = c(100, 200),  # Number of boosting rounds
  max_depth = c(3, 6),    # Maximum depth of a tree
  eta = c(0.01, 0.1),     # Learning rate
  gamma = c(0, 0.2),      # Minimum loss reduction to make a split
  colsample_bytree = c(0.7, 1),  # Fraction of features to use for each tree
  min_child_weight = c(1, 4),    # Minimum sum of instance weights in a child node
  subsample = c(0.75, 1)  # Fraction of training data to use for each tree
)

# Step 2: Set Up Control Parameters for Grid Search
ctrl <- trainControl(
  method = "cv",          # Cross-validation
  number = 5,             # Number of folds
  verboseIter = TRUE,     # Print progress
  returnData = FALSE,
  returnResamp = "final"
)

# Step 3: Train the Model Using Grid Search
set.seed(123)  # For reproducibility
xgb_tune <- train(
  x = train_x,            # Training features (matrix)
  y = train_y,            # Training target variable
  method = "xgbTree",     # Use XGBoost
  trControl = ctrl,       # Control parameters
  tuneGrid = xgb_grid,    # Grid of hyperparameters
  metric = "RMSE"         # Evaluation metric
)

# Step 4: View the Best Hyperparameters
print(xgb_tune$bestTune)

# Step 5: Train the Final Model with the Best Hyperparameters
best_params <- xgb_tune$bestTune
xgb_model <- xgboost(
  data = dtrain,
  params = list(
    objective = "reg:squarederror",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample
  ),
  nrounds = best_params$nrounds,
  verbose = 1
)

# Step 6: Evaluate the Model on Training Data
train_predictions <- predict(xgb_model, dtrain)
train_rmse <- sqrt(mean((train_y - train_predictions)^2))
cat("Training RMSE:", train_rmse, "\n")

# Step 7: Evaluate the Model on Testing Data
test_predictions <- predict(xgb_model, dtest)
test_rmse <- sqrt(mean((test_y - test_predictions)^2))
cat("Testing RMSE:", test_rmse, "\n")

# Step 6: Evaluate on Training Data
train_predictions <- predict(xgb_model, dtrain)
train_mse <- mean((train_y - train_predictions)^2)
train_rsq <- 1 - sum((train_y - train_predictions)^2) / sum((train_y - mean(train_y))^2)

cat("Training MSE:", train_mse, "\n")
cat("Training R²:", train_rsq, "\n")

# Step 7: Evaluate on Testing Data
test_predictions <- predict(xgb_model, dtest)
test_mse <- mean((test_y - test_predictions)^2)
test_rsq <- 1 - sum((test_y - test_predictions)^2) / sum((test_y - mean(test_y))^2)

cat("Testing MSE:", test_mse, "\n")
cat("Testing R²:", test_rsq, "\n")
#Training MSE: 591.4032 
#Training R²: 0.8430053 
#Testing MSE: 830.4554 
#Testing R²: 0.7533462 
