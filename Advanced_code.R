# Load Training and Testing Data
# Assuming trainingSet_clean and testingSet_clean are saved as CSV files
trainingSet_clean <- read.csv("trainingSet_clean.csv", stringsAsFactors = FALSE)
testingSet_clean <- read.csv("testingSet_clean.csv", stringsAsFactors = FALSE)


# Find manufacturer counts
manufacturer_counts <- table(trainingSet_clean$Manufacturer)

# Define a threshold (e.g., keep only manufacturers with > 100 occurrences)
threshold <- 100

# Create a new grouped variable
trainingSet_clean$Manufacturer_Grouped <- ifelse(trainingSet_clean$Manufacturer %in% names(manufacturer_counts[manufacturer_counts >= threshold]),
                                                 trainingSet_clean$Manufacturer, "Other")

testingSet_clean$Manufacturer_Grouped <- ifelse(testingSet_clean$Manufacturer %in% names(manufacturer_counts[manufacturer_counts >= threshold]),
                                                testingSet_clean$Manufacturer, "Other")

# Convert back to factor
trainingSet_clean$Manufacturer_Grouped <- as.factor(trainingSet_clean$Manufacturer_Grouped)
testingSet_clean$Manufacturer_Grouped <- as.factor(testingSet_clean$Manufacturer_Grouped)
manufacturer_counts
table(testingSet_clean$Manufacturer_Grouped)

# Confirm Data Structure
str(trainingSet_clean)
str(testingSet_clean)
dim(trainingSet_clean)
dim(testingSet_clean)

# Compute percentiles
lower_bound <- quantile(testingSet_clean$LogPrice, 0.05, na.rm = TRUE)
upper_bound <- quantile(testingSet_clean$LogPrice, 0.95, na.rm = TRUE)

# Apply capping (Winsorization)
testingSet_clean$LogPrice[testingSet_clean$LogPrice < lower_bound] <- lower_bound
testingSet_clean$LogPrice[testingSet_clean$LogPrice > upper_bound] <- upper_bound


# Compute percentiles
lower_bound <- quantile(testingSet_clean$Mileage.km., 0.05, na.rm = TRUE)
upper_bound <- quantile(testingSet_clean$Mileage.km., 0.95, na.rm = TRUE)

# Apply capping (Winsorization)
testingSet_clean$Mileage.km.[testingSet_clean$Mileage.km. < lower_bound] <- lower_bound
testingSet_clean$Mileage.km.[testingSet_clean$Mileage.km. > upper_bound] <- upper_bound





#feature engineering
trainingSet_clean$Car_Age <- 2024 - trainingSet_clean$Prod..year
testingSet_clean$Car_Age <- 2024 - testingSet_clean$Prod..year 

trainingSet_clean$Mileage_per_Year <- trainingSet_clean$Mileage.km / trainingSet_clean$Car_Age
trainingSet_clean$Mileage_per_Year[is.infinite(trainingSet_clean$Mileage_per_Year)] <- NA  # Handle divide by zero

testingSet_clean$Mileage_per_Year <- testingSet_clean$Mileage.km / testingSet_clean$Car_Age
testingSet_clean$Mileage_per_Year[is.infinite(testingSet_clean$Mileage_per_Year)] <- NA  # Handle divide by zero



trainingSet_clean$Turbo <- ifelse(trainingSet_clean$Turbo == 1, 1, 0)
trainingSet_clean$Leather.interior <- ifelse(trainingSet_clean$Leather.interior == "Yes", 1, 0)

testingSet_clean$Turbo <- ifelse(testingSet_clean$Turbo == 1, 1, 0)
testingSet_clean$Leather.interior <- ifelse(testingSet_clean$Leather.interior == "Yes", 1, 0)

colnames(trainingSet_clean)
str(trainingSet_clean)
#library(dplyr)
#trainingSet_clean <-trainingSet_clean %>% select(-c(ID, Manufacturer_Model, Wheel, Color))  # Drop unnecessary columns
#testingSet_clean <-testingSet_clean %>% select(-c(ID, Manufacturer_Model, Wheel, Color))  # Drop unnecessary columns
trainingSet_clean <- trainingSet_clean[, !colnames(trainingSet_clean) %in% c("ID", "Manufacturer_Model", "Wheel", "Color","Manufacturer")]
testingSet_clean <- testingSet_clean[, !colnames(testingSet_clean) %in% c("ID", "Manufacturer_Model", "Wheel", "Color","Manufacturer")]

str(trainingSet_clean)

library(caret)
library(dplyr)
trainingSet_clean <- trainingSet_clean %>% mutate_if(is.character, as.factor)
testingSet_clean <- testingSet_clean %>% mutate_if(is.character, as.factor)

# Ensure test set has the same factor levels as the training set
for (col in names(trainingSet_clean)) {
  if (is.factor(trainingSet_clean[[col]])) {
    testingSet_clean[[col]] <- factor(testingSet_clean[[col]], levels = levels(trainingSet_clean[[col]]))
  }
}

trainingSet_clean <- model.matrix(~ . - 1, data = trainingSet_clean) %>% as.data.frame()
testingSet_clean <- model.matrix(~ . - 1, data = testingSet_clean) %>% as.data.frame()

colnames(trainingSet_clean)

str(trainingSet_clean)

# Visualize outliers
boxplot(trainingSet_clean$Mileage.km, main = "Mileage Outliers", horizontal = TRUE)
boxplot(testingSet_clean$Mileage.km, main = "Mileage Outliers", horizontal = TRUE)
boxplot(trainingSet_clean$Car_Age, main = "Mileage Outliers", horizontal = TRUE)
boxplot(trainingSet_clean$LogPrice, main = "LogPrice Outliers", horizontal = TRUE)
boxplot(testingSet_clean$LogPrice, main = "LogPrice Outliers", horizontal = TRUE)
boxplot(trainingSet_clean$Engine.volume, main = "Engine_Volume Outliers", horizontal = TRUE)


# Compute percentiles
lower_bound <- quantile(trainingSet_clean$Engine.volume, 0.05, na.rm = TRUE)
upper_bound <- quantile(trainingSet_clean$Engine.volume, 0.95, na.rm = TRUE)

# Apply capping (Winsorization)
trainingSet_clean$Engine.volume[trainingSet_clean$Engine.volume < lower_bound] <- lower_bound
trainingSet_clean$Engine.volume[trainingSet_clean$Engine.volume > upper_bound] <- upper_bound

# Compute percentiles
lower_bound <- quantile(testingSet_clean$Engine.volume, 0.05, na.rm = TRUE)
upper_bound <- quantile(testingSet_clean$Engine.volume, 0.95, na.rm = TRUE)

# Apply capping (Winsorization)
testingSet_clean$Engine.volume[testingSet_clean$Engine.volume < lower_bound] <- lower_bound
testingSet_clean$Engine.volume[testingSet_clean$Engine.volume > upper_bound] <- upper_bound


# Remove the variables from training and testing sets
trainingSet_clean <- trainingSet_clean[, !colnames(trainingSet_clean) %in% c("Mileage.km.", "Prod..year")]
testingSet_clean <- testingSet_clean[, !colnames(testingSet_clean) %in% c("Mileage.km.", "Prod..year")]

# Identify missing columns in each dataset
missing_in_test <- setdiff(colnames(trainingSet_clean), colnames(testingSet_clean))
missing_in_train <- setdiff(colnames(testingSet_clean), colnames(trainingSet_clean))


# Add missing columns to test set (fill with 0 for categorical variables)
for (col in missing_in_test) {
  testingSet_clean[[col]] <- 0  # Assume 0 means category was absent
}

# Add missing columns to training set (fill with 0)
for (col in missing_in_train) {
  trainingSet_clean[[col]] <- 0
}

# Ensure both datasets have the same column order
trainingSet_clean <- trainingSet_clean[, order(names(trainingSet_clean))]
testingSet_clean <- testingSet_clean[, order(names(testingSet_clean))]

colnames(trainingSet_clean)
colnames(testingSet_clean)
str(trainingSet_clean)

dim(trainingSet_clean)
dim(testingSet_clean)




# Scale numerical columns (excluding target variable and categorical variables)
# Apply Z-Score Standardization for SVM, PCA, Linear Models
# Standardization function
standardize <- function(x, mean_val, sd_val) { 
  (x - mean_val) / sd_val 
}

# Explicitly define TRUE numerical variables (continuous features)
true_numeric_cols <- c("Airbags", "Car_Age", "Cylinders", "Engine.volume", 
                       "LogPrice", "Mileage_per_Year")

# Identify all other columns (one-hot encoded variables, categorical)
all_other_cols <- setdiff(names(trainingSet_clean), true_numeric_cols)

# Ensure categorical one-hot encoded variables remain unchanged
#trainingSet_clean[, all_other_cols] <- lapply(trainingSet_clean[, all_other_cols], as.factor)
# Compute mean and standard deviation from training set (only for continuous numerical columns)
train_means <- sapply(trainingSet_clean[, true_numeric_cols, drop=FALSE], mean, na.rm = TRUE)
train_sds <- sapply(trainingSet_clean[, true_numeric_cols, drop=FALSE], sd, na.rm = TRUE)

# Apply standardization to training set (ONLY continuous numerical features)
trainingSet_standardized <- trainingSet_clean
trainingSet_standardized[, true_numeric_cols] <- scale(trainingSet_clean[, true_numeric_cols])


print(train_means)
print(train_sds)

# Apply same standardization to test set using training means and standard deviations
testingSet_standardized <- testingSet_clean
testingSet_standardized[, true_numeric_cols] <- scale(testingSet_clean[, true_numeric_cols], 
                                                      center = train_means, 
                                                      scale = train_sds)

dim(trainingSet_standardized)
dim(testingSet_standardized)



# Save Standardized Datasets
write.csv(trainingSet_standardized, "trainingSet_standardized.csv", row.names = FALSE)
write.csv(testingSet_standardized, "testingSet_standardized.csv", row.names = FALSE)

colnames(trainingSet_standardized)[colnames(trainingSet_standardized) == "CategoryGoods wagon"] <- "CategoryGoodswagon"
colnames(testingSet_standardized)[colnames(testingSet_standardized) == "CategoryGoods wagon"] <- "CategoryGoodswagon"
colnames(trainingSet_standardized)[colnames(trainingSet_standardized) == "Fuel.typePlug-in Hybrid"] <- "Fuel.typePluginHybrid"
colnames(testingSet_standardized)[colnames(testingSet_standardized) == "Fuel.typePlug-in Hybrid"] <- "Fuel.typePluginHybrid"
colnames(trainingSet_standardized)[colnames(trainingSet_standardized) == "Manufacturer_GroupedMERCEDES-BENZ"] <- "Manufacturer_GroupedMERCEDES_BENZ"
colnames(testingSet_standardized)[colnames(testingSet_standardized) == "Manufacturer_GroupedMERCEDES-BENZ"] <- "Manufacturer_GroupedMERCEDES_BENZ"


# Load necessary package
library(MASS)

# Define the full model (all features) and the null model (intercept only)
full_model <- lm(LogPrice ~ ., data = trainingSet_standardized)
null_model <- lm(LogPrice ~ 1, data = trainingSet_standardized)

# Perform Forward Selection
forward_model <- step(null_model, 
                      scope = list(lower = null_model, upper = full_model), 
                      direction = "forward")

# View selected features
summary(forward_model)

# Load necessary package
library(MASS)

# Define the full model with all predictors
full_model <- lm(LogPrice ~ ., data = trainingSet_standardized)

# Perform Backward Selection using stepwise regression
backward_model <- step(full_model, direction = "backward")

# View summary of the final selected model
summary(backward_model)

# Load necessary package
#step-wise-selection
library(MASS)

# Define the full model with all predictors
full_model <- lm(LogPrice ~ ., data = trainingSet_standardized)

# Define the null model (intercept only)
null_model <- lm(LogPrice ~ 1, data = trainingSet_standardized)

# Perform Stepwise Selection (both Forward & Backward)
stepwise_model <- step(null_model, 
                       scope = list(lower = null_model, upper = full_model), 
                       direction = "both")

# View the final selected model
summary(stepwise_model)




# Compare R-squared values
r2_forward  <- summary(forward_model)$r.squared
r2_backward <- summary(backward_model)$r.squared
r2_stepwise <- summary(stepwise_model)$r.squared

cat("Forward Selection R²:", r2_forward, "\n")
cat("Backward Selection R²:", r2_backward, "\n")
cat("Stepwise Selection R²:", r2_stepwise, "\n")

# Compare Adjusted R-squared values
adj_r2_forward  <- summary(forward_model)$adj.r.squared
adj_r2_backward <- summary(backward_model)$adj.r.squared
adj_r2_stepwise <- summary(stepwise_model)$adj.r.squared

cat("Forward Selection Adjusted R²:", adj_r2_forward, "\n")
cat("Backward Selection Adjusted R²:", adj_r2_backward, "\n")
cat("Stepwise Selection Adjusted R²:", adj_r2_stepwise, "\n")

# Compare Residual Standard Errors (RSE)
rse_forward  <- summary(forward_model)$sigma
rse_backward <- summary(backward_model)$sigma
rse_stepwise <- summary(stepwise_model)$sigma

cat("Forward Selection RSE:", rse_forward, "\n")
cat("Backward Selection RSE:", rse_backward, "\n")
cat("Stepwise Selection RSE:", rse_stepwise, "\n")



best_model <- forward_model

# Print a summary of the final model
summary(best_model)

# --- For the Training Set ---
# Obtain predictions on the training set
train_preds <- predict(best_model, newdata = trainingSet_standardized)
actual_train <- trainingSet_standardized$LogPrice

# Calculate Mean Squared Error (MSE) for training set
mse_train <- mean((actual_train - train_preds)^2)

# Calculate R² for training set manually:
# R² = 1 - (SS_residual / SS_total)
ss_total_train <- sum((actual_train - mean(actual_train))^2)
ss_res_train <- sum((actual_train - train_preds)^2)
r2_train <- 1 - (ss_res_train / ss_total_train)

cat("Training set MSE:", mse_train, "\n")
cat("Training set R²:", r2_train, "\n")

# --- For the Test Set ---
# Ensure you have a test set named testSet_standardized
test_preds <- predict(best_model, newdata = testingSet_standardized)
actual_test <- testingSet_standardized$LogPrice

# Calculate Mean Squared Error (MSE) for test set
mse_test <- mean((actual_test - test_preds)^2)

# Calculate R² for test set manually:
ss_total_test <- sum((actual_test - mean(actual_test))^2)
ss_res_test <- sum((actual_test - test_preds)^2)
r2_test <- 1 - (ss_res_test / ss_total_test)

cat("Test set MSE:", mse_test, "\n")
cat("Test set R²:", r2_test, "\n")


# Plot common diagnostic charts for linear models:
par(mfrow = c(2, 2))
plot(best_model)
par(mfrow = c(1, 1))

# Shapiro-Wilk test for normality of residuals
shapiro.test(resid(best_model))
#we cann't do this because the sample size is large

#install.packages("nortest")
library(nortest)

residuals_all <- resid(best_model)

# Anderson-Darling test
ad.test(residuals_all)
#reject the null hypothesis. Residuals are not normally distributed.

# Install/load the 'car' package if needed
# install.packages("car")
library(car)

# Variance Inflation Factor (VIF)
vif_values <- vif(best_model)
print(vif_values)

# install.packages("lmtest")  # if not already installed
library(lmtest)

bptest(best_model)
#A p-value < 0.05 indicates evidence of heteroscedasticity (non-constant variance)


# Predict on test set
test_preds_forward <- predict(forward_model, newdata = testingSet_standardized)

# Compute Testing Mean Squared Error (MSE)
mse_test_forward <- mean((testingSet_standardized$LogPrice - test_preds_forward)^2)
cat("📌 Testing MSE - Forward Model:", mse_test_forward, "\n")

# Compute R² for the test set
sst_test <- sum((testingSet_standardized$LogPrice - mean(testingSet_standardized$LogPrice))^2)  # Total Sum of Squares
sse_test <- sum((testingSet_standardized$LogPrice - test_preds_forward)^2)  # Sum of Squared Errors
r2_test_forward <- 1 - (sse_test / sst_test)

cat("📌 Testing R² - Forward Model:", r2_test_forward, "\n")

# Load necessary library
#install.packages("glmnet")  # Install if not installed
library(glmnet)

# Set seed for reproducibility
set.seed(123)

# Define X (independent variables) and y (dependent variable)
X <- model.matrix(LogPrice ~ ., data=trainingSet_standardized)[,-1]  # Remove intercept column
y <- trainingSet_standardized$LogPrice

# Define X_test and y_test
X_test <- model.matrix(LogPrice ~ ., data=testingSet_standardized)[,-1]
y_test <- testingSet_standardized$LogPrice

# Function to compute Mean Squared Error (MSE)
compute_mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# Function to compute R² (coefficient of determination)
compute_r2 <- function(actual, predicted) {
  1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
}

### **1️⃣ Lasso Regression (L1 Regularization)**
lasso_model <- cv.glmnet(X, y, alpha=1, standardize=TRUE)
best_lambda_lasso <- lasso_model$lambda.min

# Predict on training and testing data
train_preds_lasso <- predict(lasso_model, newx = X, s = best_lambda_lasso)
test_preds_lasso <- predict(lasso_model, newx = X_test, s = best_lambda_lasso)

# Compute MSE and R²
mse_train_lasso <- compute_mse(y, train_preds_lasso)
mse_test_lasso <- compute_mse(y_test, test_preds_lasso)
r2_train_lasso <- compute_r2(y, train_preds_lasso)
r2_test_lasso <- compute_r2(y_test, test_preds_lasso)

### **2️⃣ Ridge Regression (L2 Regularization)**
ridge_model <- cv.glmnet(X, y, alpha=0, standardize=TRUE)
best_lambda_ridge <- ridge_model$lambda.min

# Predict on training and testing data
train_preds_ridge <- predict(ridge_model, newx = X, s = best_lambda_ridge)
test_preds_ridge <- predict(ridge_model, newx = X_test, s = best_lambda_ridge)

# Compute MSE and R²
mse_train_ridge <- compute_mse(y, train_preds_ridge)
mse_test_ridge <- compute_mse(y_test, test_preds_ridge)
r2_train_ridge <- compute_r2(y, train_preds_ridge)
r2_test_ridge <- compute_r2(y_test, test_preds_ridge)

### **3️⃣ Elastic Net (L1 + L2 Regularization)**
elastic_net_model <- cv.glmnet(X, y, alpha=0.5, standardize=TRUE)
best_lambda_elastic <- elastic_net_model$lambda.min

# Predict on training and testing data
train_preds_elastic <- predict(elastic_net_model, newx = X, s = best_lambda_elastic)
test_preds_elastic <- predict(elastic_net_model, newx = X_test, s = best_lambda_elastic)

# Compute MSE and R²
mse_train_elastic <- compute_mse(y, train_preds_elastic)
mse_test_elastic <- compute_mse(y_test, test_preds_elastic)
r2_train_elastic <- compute_r2(y, train_preds_elastic)
r2_test_elastic <- compute_r2(y_test, test_preds_elastic)

# Print MSE and R² Results
cat("\n📌 Mean Squared Error (MSE) and R² Comparison:\n")
results <- data.frame(
  Model = c("Lasso", "Ridge", "Elastic Net"),
  Training_MSE = c(mse_train_lasso, mse_train_ridge, mse_train_elastic),
  Testing_MSE = c(mse_test_lasso, mse_test_ridge, mse_test_elastic),
  Training_R2 = c(r2_train_lasso, r2_train_ridge, r2_train_elastic),
  Testing_R2 = c(r2_test_lasso, r2_test_ridge, r2_test_elastic)
)
print(results)


# Install necessary packages (Run once if not installed)
#install.packages(c("randomForest", "xgboost"))

# Load libraries
library(randomForest)
library(xgboost)

# Set seed for reproducibility
set.seed(123)

# Define X (independent variables) and y (dependent variable)
X <- model.matrix(LogPrice ~ ., data=trainingSet_standardized)[,-1]  # Remove intercept column
y <- trainingSet_standardized$LogPrice

# Define X_test and y_test
X_test <- model.matrix(LogPrice ~ ., data=testingSet_standardized)[,-1]
y_test <- testingSet_standardized$LogPrice

# Function to compute Mean Squared Error (MSE)
compute_mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# Function to compute R²
compute_r2 <- function(actual, predicted) {
  1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
}

### **1️⃣ Train Random Forest Model**
rf_model <- randomForest(X, y, ntree=500, mtry=sqrt(ncol(X)), importance=TRUE)

# Predict on training & test sets
train_preds_rf <- predict(rf_model, X)
test_preds_rf <- predict(rf_model, X_test)

# Compute MSE & R²
mse_train_rf <- compute_mse(y, train_preds_rf)
mse_test_rf <- compute_mse(y_test, test_preds_rf)
r2_train_rf <- compute_r2(y, train_preds_rf)
r2_test_rf <- compute_r2(y_test, test_preds_rf)

### **2️⃣ Train XGBoost Model**
# Convert to DMatrix (optimized for XGBoost)
dtrain <- xgb.DMatrix(data = X, label = y)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Define XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # Regression problem
  eta = 0.3,  # Learning rate
  max_depth = 3,  # Tree depth
  subsample = 0.8,  # Sample fraction per tree
  colsample_bytree = 0.8  # Feature sampling per tree
)

# Train XGBoost model
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 200, verbose = 0)

# Predict on training & test sets
train_preds_xgb <- predict(xgb_model, dtrain)
test_preds_xgb <- predict(xgb_model, dtest)

# Compute MSE & R²
mse_train_xgb <- compute_mse(y, train_preds_xgb)
mse_test_xgb <- compute_mse(y_test, test_preds_xgb)
r2_train_xgb <- compute_r2(y, train_preds_xgb)
r2_test_xgb <- compute_r2(y_test, test_preds_xgb)

# Print Results
cat("\n📌 Performance Comparison (Random Forest & XGBoost):\n")
results <- data.frame(
  Model = c("Random Forest", "XGBoost"),
  Training_MSE = c(mse_train_rf, mse_train_xgb),
  Testing_MSE = c(mse_test_rf, mse_test_xgb),
  Training_R2 = c(r2_train_rf, r2_train_xgb),
  Testing_R2 = c(r2_test_rf, r2_test_xgb)
)
print(results)

### **3️⃣ Feature Importance (Random Forest)**
cat("\n📌 Feature Importance from Random Forest:\n")
importance_rf <- importance(rf_model)
importance_rf_df <- data.frame(Feature = rownames(importance_rf), Importance = importance_rf[,1])
importance_rf_df <- importance_rf_df[order(-importance_rf_df$Importance),]  # Sort by importance
print(head(importance_rf_df, 10))  # Show top 10 important features

### **4️⃣ Feature Importance (XGBoost)**
cat("\n📌 Feature Importance from XGBoost:\n")
importance_xgb <- xgb.importance(model = xgb_model)
print(head(importance_xgb, 10))  # Show top 10 important features


# Install necessary packages if not installed
#install.packages(c("ggplot2"))

# Load libraries
library(ggplot2)

### **1️⃣ Feature Importance for Random Forest**
rf_importance <- importance(rf_model)  # Extract importance scores
rf_importance_df <- data.frame(
  Feature = rownames(rf_importance),
  Importance = rf_importance[, 1]  # Use MeanDecreaseGini score
)

# Sort in descending order
rf_importance_df <- rf_importance_df[order(-rf_importance_df$Importance), ]

# Plot Feature Importance for Random Forest
ggplot(rf_importance_df[1:10, ], aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("Feature Importance (Random Forest)") +
  xlab("Feature") +
  ylab("Importance") +
  theme_minimal()

### **2️⃣ Feature Importance for XGBoost**
xgb_importance <- xgb.importance(model = xgb_model)  # Extract importance scores

# Convert to dataframe
xgb_importance_df <- data.frame(
  Feature = xgb_importance$Feature,
  Importance = xgb_importance$Gain  # Use Gain score (how much each feature contributes)
)

# Sort in descending order
xgb_importance_df <- xgb_importance_df[order(-xgb_importance_df$Importance), ]

# Plot Feature Importance for XGBoost
ggplot(xgb_importance_df[1:10, ], aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  ggtitle("Feature Importance (XGBoost)") +
  xlab("Feature") +
  ylab("Importance") +
  theme_minimal()


# Identify low-importance features (present in neither model's top 10)
low_importance_features <- c("CategoryJeep", "Manufacturer_GroupedTOYOTA", "Fuel.typeHybrid")

# Remove these features from both training and test sets
X_reduced <- X[, !(colnames(X) %in% low_importance_features)]
X_test_reduced <- X_test[, !(colnames(X_test) %in% low_importance_features)]

### **1️⃣ Retrain Random Forest Model with Reduced Features**
rf_model_reduced <- randomForest(X_reduced, y, ntree=500, mtry=sqrt(ncol(X_reduced)), importance=TRUE)

# Predict on reduced dataset
train_preds_rf_reduced <- predict(rf_model_reduced, X_reduced)
test_preds_rf_reduced <- predict(rf_model_reduced, X_test_reduced)

# Compute MSE & R²
mse_train_rf_reduced <- compute_mse(y, train_preds_rf_reduced)
mse_test_rf_reduced <- compute_mse(y_test, test_preds_rf_reduced)
r2_train_rf_reduced <- compute_r2(y, train_preds_rf_reduced)
r2_test_rf_reduced <- compute_r2(y_test, test_preds_rf_reduced)

### **2️⃣ Retrain XGBoost Model with Reduced Features**
dtrain_reduced <- xgb.DMatrix(data = X_reduced, label = y)
dtest_reduced <- xgb.DMatrix(data = X_test_reduced, label = y_test)

# Train XGBoost with reduced features
xgb_model_reduced <- xgb.train(params = params, data = dtrain_reduced, nrounds = 200, verbose = 0)

# Predict on reduced dataset
train_preds_xgb_reduced <- predict(xgb_model_reduced, dtrain_reduced)
test_preds_xgb_reduced <- predict(xgb_model_reduced, dtest_reduced)

# Compute MSE & R²
mse_train_xgb_reduced <- compute_mse(y, train_preds_xgb_reduced)
mse_test_xgb_reduced <- compute_mse(y_test, test_preds_xgb_reduced)
r2_train_xgb_reduced <- compute_r2(y, train_preds_xgb_reduced)
r2_test_xgb_reduced <- compute_r2(y_test, test_preds_xgb_reduced)

# Print Performance Comparison
cat("\n📌 Model Performance Before & After Feature Selection:\n")
results <- data.frame(
  Model = c("Random Forest (Before)", "Random Forest (After)", 
            "XGBoost (Before)", "XGBoost (After)"),
  Testing_MSE = c(mse_test_rf, mse_test_rf_reduced, mse_test_xgb, mse_test_xgb_reduced),
  Testing_R2 = c(r2_test_rf, r2_test_rf_reduced, r2_test_xgb, r2_test_xgb_reduced)
)
print(results)

# Save the trained Random Forest model
saveRDS(rf_model_reduced, file = "final_random_forest_model.rds")


# Install required packages if not installed
#install.packages("pdp")

# Load necessary libraries
library(pdp)
library(ggplot2)
library(randomForest)

# Load the final trained Random Forest model
rf_model_final <- readRDS("final_random_forest_model.rds")

# Define top important variables based on feature importance
top_variables <- c("Car_Age", "Mileage_per_Year", "Gear.box.typeTiptronic", "Airbags")

# Create Partial Dependence Plots for each important variable
for (var in top_variables) {
  pd <- partial(rf_model_final, pred.var = var, train = X_reduced)
  
  # Convert to data frame
  pd_df <- data.frame(x = pd[[var]], y = pd$yhat)
  
  # Plot PDP using ggplot2
  p <- ggplot(pd_df, aes(x = x, y = y)) +
    geom_line(color = "blue", size = 1.2) +
    geom_point(color = "red", size = 2) +
    ggtitle(paste("Partial Dependence Plot for", var)) +
    xlab(var) +
    ylab("Predicted Log Price") +
    theme_minimal()
  
  print(p)
}

# Check correlation
cor(trainingSet_standardized$Airbags, trainingSet_standardized$Car_Age, use = "complete.obs")

library(ggplot2)

# Histogram of Airbags distribution
ggplot(trainingSet_standardized, aes(x = Airbags)) +
  geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
  ggtitle("Distribution of Airbags in the Dataset") +
  xlab("Number of Airbags") +
  ylab("Count") +
  theme_minimal()

# Boxplot of Log Price by Airbags & Manufacturer
ggplot(trainingSet_standardized, aes(x = as.factor(Airbags), y = LogPrice, fill = "Manufacturer_Grouped")) +
  geom_boxplot() +
  ggtitle("Impact of Airbags on Log Price by Manufacturer") +
  xlab("Number of Airbags") +
  ylab("Log Price") +
  theme_minimal()



