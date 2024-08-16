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

##################### Elastic Net Model #####################

# Set up a grid of alpha and lambda values [JW]
alpha_values <- seq(0, 1, by = 0.1)  # Range of alpha values from 0 (Ridge) to 1 (Lasso)
lambda_values <- 10^seq(-3, 3, length = 100)  # Range of lambda values on a log scale

# Set variables to store results [JW]
best_alpha <- NULL
best_lambda <- NULL
best_model <- NULL
lowest_cv_error <- Inf # Start with infinite

# set seed for reproducibility
set.seed(123) # [JW, TZ]

# Grid search [JW]
for (alpha in alpha_values) {
  cv_model <- cv.glmnet(x, y, alpha = alpha, lambda = lambda_values, nfolds = 10)
  
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
elastic_net_model <- glmnet(x, y, alpha = best_alpha, lambda = best_lambda)

# Print the model summary
print(elastic_net_model)

# Predict on the entire data using the best lambda from CV
elastic_net_predictions <- predict(elastic_net_model, newx = x, s = "lambda.min")

# Calculate metrics for the Elastic Net model
elastic_net_rmse <- sqrt(mean((y - elastic_net_predictions)^2))
elastic_net_r2 <- 1 - sum((y - elastic_net_predictions)^2) / sum((y - mean(y))^2)
elastic_net_mae <- mean(abs(y - elastic_net_predictions))

# Print the summary of the Elastic Net model
cat("Elastic Net Model Summary:\n")
cat("Best Lambda (Shrinkage Parameter):", best_lambda, "\n")

# Extract and sparse matrix of coefficients
coef_estimates <- coef(elastic_net_model, s = "lambda.min")

# Convert to a regular matrix
coef_matrix <- as.matrix(coef_estimates)

# Filter out the zero coefficients for a cleaner output
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

##################### Results #####################

# Create a tibble with the results from both models
results <- tibble(
  Metric = c("RMSE", "R-Squared", "MAE"),
  Elastic_Net = c(elastic_net_rmse, elastic_net_r2, elastic_net_mae),
  Random_Forest = c(rf_rmse, rf_r2, rf_mae)
)

# Print the results
print(results)