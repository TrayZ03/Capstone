# Model Tuning

####### Setup #########
# source constants
PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment
source(file.path(PROJ_PATH, "constants.R"))

# load libraries 
library(tidyverse)
library(glmnet)
library(boot)
library(car)
library(randomForest)
library(caret)

# load flight delay dataset as CSV [TZ]
data <- read_csv(MODEL_DATA_FILE) %>%
  select(!c("CARRIER", "CARRIER_NAME"))  %>%
  select(!c("YEAR", "MONTH")) %>% # omit time features
  select(!c("MISHAND_PASS_RATIO", "MISHAND_FLIGHTS_RATIO", "AVG_MISHAND_RATIO")) # omit features derived from target

# Prepare the data
y <- data$MISHANDLED_BAGGAGE
X <- model.matrix(MISHANDLED_BAGGAGE ~ ., data)[, -1]  # Create a model matrix without the intercept

######## Elastic Net Tuning ##########
# Fit the Elastic Net regression model
set.seed(123)  # For reproducibility
elastic_net_model <- cv.glmnet(
  X, y, 
  alpha = 0.5,  # Elastic Net (0 = Ridge, 1 = Lasso)
  nfolds = 10   # Number of folds for cross-validation
)

# Set up a grid of alpha and lambda values 
alpha_values <- seq(0, 1, by = 0.1)  # Range of alpha values from 0 (Ridge) to 1 (Lasso)
lambda_values <- 10^seq(-3, 3, length = 100)  # Range of lambda values on a log scale

# Set variables to store results
best_alpha <- NULL
best_lambda <- NULL
best_model <- NULL
lowest_cv_error <- Inf # Start with infinite

# Grid search 
for (alpha in alpha_values) {
  set.seed(123)  # For reproducibility
  cv_model <- cv.glmnet(X, y, alpha = alpha, lambda = lambda_values, nfolds = 10)
  
  # If this model has a lower cross-validation error, save it as the best model
  if (min(cv_model$cvm) < lowest_cv_error) {
    lowest_cv_error <- min(cv_model$cvm)
    best_alpha <- alpha
    best_lambda <- cv_model$lambda.min
    best_model <- cv_model
  }
}

# Print the best alpha and lambda
print(paste("Best alpha: ", best_alpha)) # The best alpha model chosen by this method (0) corresponds to ridge regression, not elastic net (between 0 and 1)
print(paste("Best lambda: ", best_lambda))

# Fit the final model with the best lambda
final_model <- glmnet(X, y, alpha = best_alpha, lambda = best_lambda)

# Print the model summary
print(final_model)

# Print the coefficients of the final model
print(coef(final_model))

# Make predictions
predictions <- predict(final_model, newx = X)

# Calculate R-squared
SST <- sum((y - mean(y))^2)
SSE <- sum((predictions - y)^2)
R_squared <- 1 - SSE/SST

# Calculate Mean Squared Error (MSE)
MSE <- mean((predictions - y)^2)

# Print useful regression statistics
cat("Best lambda: ", best_lambda, "\n")
cat("Coefficients:\n")
print(coef(final_model))
cat("R-squared: ", R_squared, "\n")
cat("Mean Squared Error: ", MSE, "\n")

# Extract coefficients
coefficients <- as.matrix(coef(final_model))
selected_features <- rownames(coefficients)[coefficients != 0]
selected_features <- selected_features[selected_features != "(Intercept)"]

# Create a new data frame with only the selected features
selected_data <- data %>%
  select(MISHANDLED_BAGGAGE, all_of(selected_features))

######### Linear Model Tuning ##########
# Fit a linear model using the selected features
linear_model <- lm(MISHANDLED_BAGGAGE ~ ., data = selected_data)

# Check for multicollinearity (shown by high VIF values)
vif_values <- vif(linear_model)
cat("VIF Values:\n")
print(vif_values)

# Remove Q3_DEP_DELAY and NUM_FLIGHTS from linear model - both had very high VIF values. Since PASSENGERS is an important variable to consider, it will stay in the model.
linear_model_refined <- lm(MISHANDLED_BAGGAGE ~ . - NUM_FLIGHTS - Q3_DEP_DEL, data = selected_data)

# Get the summary of the first linear model, including p-values
summary_stats <- summary(linear_model)
cat("Linear Model Summary:\n")
print(summary_stats)

# Plot diagnostic plots
par(mfrow = c(2, 2))  # Set up plot layout
plot(linear_model)

# Get the summary of the refined linear model, incl. p-values
summary_stats2 <- summary(linear_model_refined)
cat("Linear Model Summary:/n")
print(summary_stats2)

# Plot diagnostic plots
plot(linear_model_refined)

# Perform k-fold cross-validation
set.seed(123)  # For reproducibility
cv_error <- cv.glm(selected_data, linear_model, K = 10)

# Print the cross-validation error
cat("Cross-Validation Error:\n")
print(cv_error$delta)

######## Random Forest - Fitting & Evaluating ########
set.seed(123) # for reproducibility

# build preliminary random forest model
random_forest_model <- randomForest(
  x = X,
  y = y,
  ntree = 100,           # Number of trees (this is a preliminary choice)
  mtry = sqrt(ncol(X)),  # Number of variables randomly sampled at each split
  importance = TRUE      # To compute variable importance
)

# print random forest model 
print(random_forest_model)

# Print the MSE for each tree
cat("Mean Squared Error (MSE) for each tree:\n")
print(random_forest_model$mse)

# Compute the average MSE across all trees
avg_mse <- mean(random_forest_model$mse)
cat("Average OOB Mean Squared Error Estimate:\n")
print(avg_mse)

# Print the R-squared values for each tree
cat("R-Squared Values for each tree:\n")
print(random_forest_model$rsq)

# Print the variable importance
cat("Variable Importance:\n")
print(importance(random_forest_model))

# Plot the variable importance
varImpPlot(random_forest_model)

####### Random Forest - Tuning ##########
# A hyperparameter grid search will be used to evaluate the ideal values for this model.
# Define a parameter grid
param_grid <- expand.grid(
  mtry = c(2, 3, 4, 5)     # Number of variables to try at each split
)

# Define cross-validation control
ctrl <- trainControl(
  method = "cv",
  number = 5,
  search = "grid"
)

# Train the Random Forest model using grid search
rf_model <- train(
  MISHANDLED_BAGGAGE ~ ., 
  data = selected_data, 
  method = "rf", 
  tuneGrid = param_grid, 
  trControl = ctrl
)

# Print the best hyperparameters
cat("Best Hyperparameters:\n")
print(rf_model$bestTune)

# Print a summary of the results
cat("Model Summary:\n")
print(rf_model$results)

# Extract best hyperparameters from the tuned model
best_params <- rf_model$bestTune
print(best_params)

# Use the best hyperparameters to build the final Random Forest model
final_rf_model <- randomForest(
  x = selected_data %>% select(-MISHANDLED_BAGGAGE),  # Exclude the target variable from features
  y = selected_data$MISHANDLED_BAGGAGE,  # Target variable
  ntree = 100,              # Number of trees
  mtry = best_params$mtry,                 # Number of variables to try at each split
  nodesize = 10,         # Minimum size of terminal nodes
  importance = TRUE                       # Compute variable importance
)

# Print the final model summary
print(final_rf_model)

# This tuned version of the model returns a higher percent of variance explained and a lower mean of squared residuals.
