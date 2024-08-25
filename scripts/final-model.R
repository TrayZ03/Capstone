# !/usr/local/bin/Rscript

###################################### Setup ###################################

# set paths

# PROJ_PATH <- "C:\Users\trace\OneDrive\Documents\Capstone\Capstone-Shared-Repo" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment

# source constants
source(file.path(PROJ_PATH, "constants.R"))
source(file.path(PROJ_PATH, "helpers.R"))

# Load necessary libraries
library(tidyverse)
library(caret)
library(glmnet)
library(ranger)  

# Set seed for reproducibility
set.seed(123)

################################# Prepare Data #################################

# Load standardized modeling dataset as CSV [TZ]
final_model_data_std <- read_csv(INITIAL_MODEL_DATA_FILE) %>%
  rename_all(~ gsub("[ .]", "_", .)) # clean up column names

# Load unstandardized preprocessed flight dataset as CSV [TZ]
final_model_data <- read_csv(PREPROCESSED_DATA_FILE)

# One-hot encode carrier
final_model_data <- one_hot_encode_carrier(final_model_data) %>%
  rename_all(~ gsub("[ .]", "_", .)) # clean up column names

# Final cleanup
final_model_data <- final_model_data %>%
  select(all_of(colnames(final_model_data_std))) %>% # Select correct features
  mutate(MAX_DEP_DEL_HRS = MAX_DEP_DEL/60) %>% # Rescale max departure delay to hrs.
  select(!MAX_DEP_DEL) # drop old minutes version

# Drop proportion of carrier delay (VIF > 10) and Alaska Airlines (p > 0.01)
final_model_data_std <- final_model_data_std %>%
  select(!c(PROP_CARRIER_DEL, !!sym("CARRIER_NAMEAlaska_Airlines_Inc_")))
final_model_data <- final_model_data %>%
  select(!c(PROP_CARRIER_DEL, !!sym("CARRIER_NAMEAlaska_Airlines_Inc_")))

# Save final model data sets
save_data(final_model_data, FINAL_MODEL_DATA_FILE, "FINAL_MODEL_FEATURES")
save_data(final_model_data_std, FINAL_MODEL_DATA_STD_FILE, "FINAL_MODEL_STD_FEATURES")


######################### Fit Final Linear Model ################################

linear_model_final <- lm(AVG_MISHAND_RATIO ~ ., data = final_model_data)

# Get the summary of the linear model, including p-values
summary_stats <- summary(linear_model_final)
cat("Final Linear Model Summary:\n")
print(summary_stats)

# Show updated diagnostic plots
diagnostic_plots(linear_model_final, file.path(IMAGES_PATH, "final-linear-model-diagnostic-plots.png"))

################################# Evaluation ##################################

# Prepare the data for glmnet and random forest (requires matrix input for predictors)
x <- model.matrix(AVG_MISHAND_RATIO ~ . - 1, data = final_model_data)
y <- final_model_data$AVG_MISHAND_RATIO

##################### Elastic Net Model with Alpha Tuning #####################

# Set up a grid of alpha and lambda values
alpha_values <- seq(0, 1, by = 0.1)  # Range of alpha values from 0 (Ridge) to 1 (Lasso)
lambda_values <- 10^seq(-3, 3, length = 100)  # Range of lambda values on a log scale

# Initialize variables to store results
best_alpha <- NULL
best_lambda <- NULL
best_model <- NULL
lowest_cv_error <- Inf  # Initialize with infinity

# Set seed for reproducibility
set.seed(123)

# Perform grid search over alpha values
for (alpha in alpha_values) {
  # Perform cross-validation with glmnet
  cv_model <- cv.glmnet(x, y, alpha = alpha, lambda = lambda_values, nfolds = 10)
  
  # Track the model with the lowest cross-validation error
  if (min(cv_model$cvm) < lowest_cv_error) {
    lowest_cv_error <- min(cv_model$cvm)
    best_alpha <- alpha
    best_lambda <- cv_model$lambda.min
    best_model <- cv_model
  }
}

# Output the best alpha and lambda values found
cat("Best alpha:", best_alpha, "\n")
cat("Best lambda:", best_lambda, "\n")

# Fit the final model using the best alpha and lambda
elastic_net_model <- glmnet(x, y, alpha = best_alpha, lambda = best_lambda)

# Summarize the final model
print(elastic_net_model)

# Make predictions on the entire data using the best lambda
elastic_net_predictions <- predict(elastic_net_model, newx = x, s = "lambda.min")

# Calculate evaluation metrics for the Elastic Net model
elastic_net_rmse <- sqrt(mean((y - elastic_net_predictions)^2))
elastic_net_r2 <- 1 - sum((y - elastic_net_predictions)^2) / sum((y - mean(y))^2)
elastic_net_mae <- mean(abs(y - elastic_net_predictions))

# Output the evaluation metrics
cat("Elastic Net Model Summary:\n")
cat("Best Lambda (Shrinkage Parameter):", best_lambda, "\n")

# Extract the coefficients for interpretation
coef_estimates <- coef(elastic_net_model, s = "lambda.min")

# Convert sparse matrix of coefficients to a regular matrix
coef_matrix <- as.matrix(coef_estimates)

# Filter out zero coefficients for clarity
non_zero_coefs <- coef_matrix[coef_matrix != 0, , drop = FALSE]

# Print the non-zero coefficients
cat("Non-Zero Estimates:\n")
print(non_zero_coefs)

##################### Random Forest Model with ranger #####################

# Fit the Random Forest model using ranger
random_forest_model <- ranger(
  formula = AVG_MISHAND_RATIO ~ ., 
  data = final_model_data,
  mtry = 3,
  num.trees = 200,
  importance = 'impurity'
)

# Predict on the entire data using the Random Forest model
rf_predictions <- predict(random_forest_model, data = final_model_data)$predictions

# Calculate metrics for the Random Forest model
rf_rmse <- sqrt(mean((y - rf_predictions)^2))
rf_r2 <- 1 - sum((y - rf_predictions)^2) / sum((y - mean(y))^2)
rf_mae <- mean(abs(y - rf_predictions))

######################### Cross-Validation for Linear Model #########################

# Define cross-validation settings
train_control <- trainControl(method = "cv", number = 10) # 10-fold cross-validation

# Perform cross-validation
lm_cv_model <- train(
  AVG_MISHAND_RATIO ~ ., 
  data = final_model_data,
  method = "lm",
  trControl = train_control,
  metric = "RMSE"
)

# Calculate predictions on the final model data
lm_predictions <- predict(lm_cv_model, final_model_data)

# Compute the RMSE, R-squared, and MAE using postResample
cv_metrics <- postResample(pred = lm_predictions, obs = y)

cv_rmse <- cv_metrics["RMSE"]
cv_r2 <- cv_metrics["Rsquared"]
cv_mae <- cv_metrics["MAE"]

##################### Evaluation Results #####################

# Create a tibble with the results from all models
results <- tibble(
  Metric = c("RMSE", "R-Squared", "MAE"),
  Elastic_Net = c(elastic_net_rmse, elastic_net_r2, elastic_net_mae),
  Random_Forest = c(rf_rmse, rf_r2, rf_mae),
  Linear_Model_CV = c(cv_rmse, cv_r2, cv_mae)  # Add cross-validated metrics for the linear model
)

# Print the results
print(results)