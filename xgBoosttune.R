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

# Set seed for reproducibility
set.seed(123)

# Step 1: One-Hot Encoding for Categorical Variables
categorical_vars <- names(trainingModel)[sapply(trainingModel, is.factor) | sapply(trainingModel, is.character)]

dummy <- dummyVars(~ ., data = trainingModel[, categorical_vars], fullRank = TRUE)
training_encoded <- predict(dummy, newdata = trainingModel)
testing_encoded <- predict(dummy, newdata = testingModel)

# Add numeric variables back
numeric_vars <- names(trainingModel)[sapply(trainingModel, is.numeric)]
training_encoded <- cbind(training_encoded, trainingModel[, numeric_vars])
testing_encoded <- cbind(testing_encoded, testingModel[, numeric_vars])

# Ensure proper column names
colnames(training_encoded) <- make.names(colnames(training_encoded))
colnames(testing_encoded) <- make.names(colnames(testing_encoded))

# Step 2: Standardize Numeric Variables
numeric_vars_encoded <- names(training_encoded)[sapply(training_encoded, is.numeric)]
numeric_vars_encoded <- numeric_vars_encoded[!numeric_vars_encoded %in% c("price_sqrt")]

preprocess_params <- preProcess(training_encoded[, numeric_vars_encoded, drop = FALSE], method = c("center", "scale"))
training_encoded[, numeric_vars_encoded] <- predict(preprocess_params, training_encoded[, numeric_vars_encoded, drop = FALSE])
testing_encoded[, numeric_vars_encoded] <- predict(preprocess_params, testing_encoded[, numeric_vars_encoded, drop = FALSE])

# Step 3: Prepare Data for XGBoost
train_x <- as.matrix(training_encoded[, -which(names(training_encoded) == "price_sqrt")])
train_y <- trainingModel$price_sqrt

test_x <- as.matrix(testing_encoded[, -which(names(testing_encoded) == "price_sqrt")])
test_y <- testingModel$price_sqrt

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

# Step 4: Hyperparameter Tuning via Cross-Validation
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.03,  # Lower learning rate for better accuracy
  max_depth = 6,  # Prevent overfitting
  min_child_weight = 4,  # Avoids overfitting by requiring larger splits
  subsample = 0.75,  # Reduce to prevent overfitting
  colsample_bytree = 0.7,  # Reduce feature selection
  lambda = 5,  # Stronger L2 regularization
  alpha = 2,  # Stronger L1 regularization
  gamma = 0.2,  # Avoids unnecessary splits
  eval_metric = "rmse"
)

cv_model <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,  # Higher to allow early stopping to find the best
  nfold = 5,
  early_stopping_rounds = 20,  # Stops training when improvement stops
  nthread = parallel::detectCores() - 1,
  verbose = TRUE
)

best_nrounds <- cv_model$best_iteration
cat("Optimal nrounds:", best_nrounds, "\n")

# Step 5: Train Final XGBoost Model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  nthread = parallel::detectCores() - 1,
  verbose = TRUE
)

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


# Load necessary libraries
library(xgboost)
library(ggplot2)

# Extract feature importance
importance_matrix <- xgb.importance(
  feature_names = colnames(train_x),  # Names of the features
  model = xgb_model                   # Trained XGBoost model
)

# Convert importance matrix to a data frame
importance_df <- as.data.frame(importance_matrix)

# Sort by Gain (importance) in descending order
importance_df <- importance_df[order(-importance_df$Gain), ]

# Calculate cumulative importance
importance_df$Cumulative_Gain <- cumsum(importance_df$Gain)

# Filter features contributing to the top 90% of importance
threshold <- 0.90  # Adjust this threshold as needed
filtered_importance_df <- importance_df[importance_df$Cumulative_Gain <= threshold, ]

# Calculate percentage importance
filtered_importance_df$Percentage <- filtered_importance_df$Gain / sum(filtered_importance_df$Gain) * 100

# Create the plot
ggplot(filtered_importance_df, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
  geom_bar(stat = "identity", width = 0.7) +  # Bars with color gradient
  scale_fill_gradient(low = "#ffcccc", high = "#8b0000") +  # Light to dark red gradient
  geom_text(
    aes(label = paste0(round(Percentage, 1), "%")),  # Display percentages
    hjust = -0.1, size = 4, color = "black"
  ) +
  labs(
    title = "Feature Importance Plot (Top 90%)",
    x = "Features",
    y = "Importance (Gain)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center title
    axis.title.x = element_text(size = 12, face = "bold"),  # X-axis label
    axis.title.y = element_text(size = 12, face = "bold")   # Y-axis label
  ) +
  coord_flip()  # Flip axes for horizontal bars

# Install and load the pdp package
#install.packages("pdp")
library(pdp)

# Create a PDP for a specific feature (e.g., "Engine_volume")
pdp_engine <- partial(
  xgb_model,                        # Trained XGBoost model
  pred.var = "Engine_volume",       # Feature to analyze
  train = train_x,                  # Training data (matrix format)
  type = "regression"               # Type of model (regression)
)

# Plot the PDP
plotPartial(pdp_engine, xlab = "Engine Volume", ylab = "Predicted LogPrice", main = "Partial Dependence Plot for Engine Volume")


# Create a PDP for a specific feature (e.g., "Car_Age")
pdp_engine <- partial(
  xgb_model,                        # Trained XGBoost model
  pred.var = "Car_Age",       # Feature to analyze
  train = train_x,                  # Training data (matrix format)
  type = "regression"               # Type of model (regression)
)

# Plot the PDP
plotPartial(pdp_engine, xlab = "Car_Age", ylab = "Predicted LogPrice", main = "Partial Dependence Plot for Car Age")

# Create a PDP for a specific feature (e.g., "Mileage_sqrt")
pdp_engine <- partial(
  xgb_model,                        # Trained XGBoost model
  pred.var = "Mileage_sqrt",       # Feature to analyze
  train = train_x,                  # Training data (matrix format)
  type = "regression"               # Type of model (regression)
)

# Plot the PDP
plotPartial(pdp_engine, xlab = "Mileage_sqrt", ylab = "Predicted LogPrice", main = "Partial Dependence Plot for Mileage Square Root")




# Create an interaction plot for two features (e.g., "Engine_volume" and "Mileage_km")
pdp_interaction <- partial(
  xgb_model,                        # Trained XGBoost model
  pred.var = c("Engine_volume", "Mileage_km"),  # Features to analyze
  train = train_x,                  # Training data (matrix format)
  type = "regression"               # Type of model (regression)
)

# Plot the interaction
plotPartial(pdp_interaction, levelplot = TRUE, colorkey = TRUE, main = "Interaction Plot: Engine Volume vs Mileage")