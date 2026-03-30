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
# Load necessary library
library(ggplot2)

# Create the box plot
ggplot(cleaned_data, aes(y = Mileage_sqrt)) +
  geom_boxplot(fill = "#8b0000", color = "black", alpha = 0.7) +  # Customize box plot colors
  labs(
    title = "Box Plot of Mileage_sqrt",
    y = "Mileage_sqrt"
  ) +
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    axis.title.y = element_text(size = 12, face = "bold")   # Customize y-axis label
  )

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


# Step 4: Prepare Data for XGBoost
library(xgboost)

train_x <- as.matrix(training_encoded[, -which(names(training_encoded) == "price_sqrt")])
train_y <- trainingModel$price_sqrt

test_x <- as.matrix(testing_encoded[, -which(names(testing_encoded) == "price_sqrt")])
test_y <- testingModel$price_sqrt

# Step 5: Fit the XGBoost Model
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "rmse"
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10
)

# Step 6: Evaluate the Model
test_predictions <- predict(xgb_model, dtest)
test_mse <- mean((test_y - test_predictions)^2)
test_rsq <- 1 - sum((test_y - test_predictions)^2) / sum((test_y - mean(test_y))^2)

cat("Testing MSE:", test_mse, "\n")
cat("Testing R²:", test_rsq, "\n")
# Step 6: Evaluate the Model on Training Data
train_predictions <- predict(xgb_model, dtrain)
train_mse <- mean((train_y - train_predictions)^2)
train_rsq <- 1 - sum((train_y - train_predictions)^2) / sum((train_y - mean(train_y))^2)

cat("Training MSE:", train_mse, "\n")
cat("Training R²:", train_rsq, "\n")

# Step 6: Evaluate the Model on Testing Data
test_predictions <- predict(xgb_model, dtest)
test_mse <- mean((test_y - test_predictions)^2)
test_rsq <- 1 - sum((test_y - test_predictions)^2) / sum((test_y - mean(test_y))^2)


cat("Training MSE:", train_mse, "\n")
cat("Training R²:", train_rsq, "\n")
cat("Testing MSE:", test_mse, "\n")
cat("Testing R²:", test_rsq, "\n")

