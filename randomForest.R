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
####################################
         # random Forest #
####################################
# Load the randomForest package
#install.packages("randomForest")  # Install if not already installed
library(randomForest)

# Fit the default Random Forest model
set.seed(123)  # For reproducibility

rf_model <- randomForest(
  price_sqrt ~ .,           # Predict price_sqrt using all other variables
  data = trainingModel,     # Training data
  ntree = 200,              # Number of trees (default is 500)
  mtry = sqrt(ncol(trainingModel) - 1),  # Default mtry for regression
  importance = TRUE         # Compute feature importance
)
  
  # View the model summary
  print(rf_model)
  
  # Make predictions on the training set
  train_predictions <- predict(rf_model, newdata = trainingModel)
  
  # Calculate training MSE and R-squared
  train_mse <- mean((trainingModel$price_sqrt - train_predictions)^2)
  train_rsquared <- cor(trainingModel$price_sqrt, train_predictions)^2
  
  # Make predictions on the testing set
  test_predictions <- predict(rf_model, newdata = testingModel)
  
  # Calculate testing MSE and R-squared
  test_mse <- mean((testingModel$price_sqrt - test_predictions)^2)
  test_rsquared <- cor(testingModel$price_sqrt, test_predictions)^2
  
  # Print the results
  print(paste("Training MSE:", train_mse))
  print(paste("Training R-squared:", train_rsquared))
  print(paste("Testing MSE:", test_mse))
  print(paste("Testing R-squared:", test_rsquared))
  
  # Extract and plot feature importance
  importance_scores <- importance(rf_model)
  varImpPlot(rf_model)
  