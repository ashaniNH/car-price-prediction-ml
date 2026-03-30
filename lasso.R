attach(trainingModel)
attach(testingModel)

str(trainingModel)
str(testingModel)


# Load necessary library
library(caret)

# Convert categorical variables to factors
trainingModel[] <- lapply(trainingModel, function(x) if(is.character(x) || is.factor(x)) as.factor(x) else x)
testingModel[] <- lapply(testingModel, function(x) if(is.character(x) || is.factor(x)) as.factor(x) else x)


# Ensure that testingModel has the same factor levels as trainingModel
factor_cols <- sapply(trainingModel, is.factor)

for (col in names(factor_cols[factor_cols])) {
  testingModel[[col]] <- factor(testingModel[[col]], levels = levels(trainingModel[[col]]))
}

# Identify numerical columns
num_cols <- sapply(trainingModel, is.numeric)

# Compute mean and standard deviation from training data
train_means <- sapply(trainingModel[, num_cols], mean, na.rm = TRUE)
train_sds <- sapply(trainingModel[, num_cols], sd, na.rm = TRUE)

print(any(is.na(train_means)))  # Should be FALSE
print(any(is.na(train_sds)))    # Should be FALSE


# Standardize numerical variables in training set
trainingModel[, num_cols] <- scale(trainingModel[, num_cols], center = train_means, scale = train_sds)

# Standardize numerical variables in testing set using training statistics
testingModel[, num_cols] <- scale(testingModel[, num_cols], center = train_means, scale = train_sds)

# Check structure after transformation
str(trainingModel)
str(testingModel)

dim(trainingModel)
dim(testingModel)

####################################
# Lasso #
####################################
# Load necessary library
#install.packages("glmnet")  # Install if not installed



library(glmnet)

# Set seed for reproducibility
set.seed(123)

# Define X (independent variables) and y (dependent variable)
X <- model.matrix(price_sqrt ~ ., data=trainingModel)[,-1]  # Remove intercept column
y <- trainingModel$price_sqrt



# Define X_test and y_test
X_test <- model.matrix(price_sqrt ~ ., data=testingModel)[,colnames(X)]
y_test <- testingModel$price_sqrt

# Function to compute Mean Squared Error (MSE)
compute_mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# Function to compute R² (coefficient of determination)
compute_r2 <- function(actual, predicted) {
  1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
}

### **1️⃣ Lasso Regression (L1 Regularization)**
lasso_model <- cv.glmnet(X, y, alpha=1, standardize=FALSE)
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
