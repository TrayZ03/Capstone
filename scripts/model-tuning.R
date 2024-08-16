# !/usr/local/bin/Rscript

# Model Tuning

####### Setup #########

# source constants

# PROJ_PATH <- "C:\Users\trace\OneDrive\Documents\Capstone\Capstone-Shared-Repo" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment

source(file.path(PROJ_PATH, "constants.R"))
source(file.path(PROJ_PATH, "helpers.R"))

# load libraries 
library(tidyverse)
library(glmnet)
library(boot)
library(car)
library(randomForest)
library(FactoMineR)
library(caret)
library(factoextra)


# load flight delay dataset as CSV [TZ]
model_tuning_data <- read_csv(INITIAL_MODEL_DATA_FILE) # this data is pre-scaled in data-processing.R and one-hot encoded

# Prepare the data [JW]
y <- model_tuning_data$AVG_MISHAND_RATIO
X <- model.matrix(AVG_MISHAND_RATIO ~ ., model_tuning_data)[, -1]  # Create a model matrix without the intercept


######## PCA Tuning ##########

# Perform PCA on the standardized data
set.seed(123)  # For reproducibility
pca_results <- PCA(X, ncp = ncol(X), graph = FALSE)

# Scree plot to determine the number of components to retain by examining the cumulative variance
scree_plot <- fviz_eig(pca_results, addlabels = TRUE, ylim = c(0, 100))
ggsave(filename = file.path(IMAGES_PATH, "pca-scree-plot.png"), plot = scree_plot, width = 8, height = 6)

# Use only the number of components returned by PCA
num_components <- 2:ncol(pca_results$ind$coord)

# Perform a grid search to tune the number of components
lowest_cv_error_pca <- Inf
best_num_components <- NULL
best_pca_model <- NULL

for (n_comp in num_components) {
  # Check that n_comp does not exceed the available components
  if (n_comp <= ncol(pca_results$ind$coord)) {
    X_pca <- pca_results$ind$coord[, 1:n_comp, drop = FALSE]  # Ensure X_pca is a matrix

    # Ensure X_pca is not NULL and has valid dimensions before proceeding
    if (!is.null(X_pca) && !is.na(ncol(X_pca)) && ncol(X_pca) > 0 && nrow(X_pca) > 0) {
      cv_model_pca <- cv.glmnet(X_pca, y, alpha = 0.5, nfolds = 10)

      if (min(cv_model_pca$cvm) < lowest_cv_error_pca) {
        lowest_cv_error_pca <- min(cv_model_pca$cvm)
        best_num_components <- n_comp
        best_pca_model <- cv_model_pca
      }
    }
  }
}

# Print the best number of components
print(paste("Best number of PCA components: ", best_num_components))

######## Elastic Net Tuning ##########
# Fit the Elastic Net regression model [JW]
set.seed(123)  # For reproducibility
elastic_net_model <- cv.glmnet(
  X, y, 
  alpha = 0.5,  # Elastic Net (0 = Ridge, 1 = Lasso)
  nfolds = 10   # Number of folds for cross-validation
)

# Plot the cross-validation curve for Elastic Net
elastic_net_plot <- plot(elastic_net_model)
ggsave(filename = file.path(IMAGES_PATH, "elastic-net-cv-plot.png"), plot = elastic_net_plot, width = 8, height = 6)

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

# Plot the coefficients
coef_plot <- plot(final_model, "lambda", label = TRUE)

ggsave(filename = file.path(IMAGES_PATH, "elastic-net-coefficients-plot.png"), plot = coef_plot, width = 8, height = 6)


########################### Final Linear Model ################################

linear_model_final <- lm(AVG_MISHAND_RATIO ~ ., data = model_tuning_data)

# Get the summary of the linear model, including p-values
summary_stats <- summary(linear_model_final)
cat("Final Linear Model Summary:\n")
print(summary_stats)

# show updated diagnostic plots
diagnostic_plots(linear_model_final, file.path(IMAGES_PATH, "final-linear-model-diagnostic-plots.png"))

# identify collinearity with aliasing which prevented VIF
print(alias(linear_model_final))

# Check for multicollinearity (shown by high VIF values) [JW]
vif_values <- vif(linear_model_final)
cat("VIF Values:\n")
print(vif_values)

# Convert VIF values to a data frame for plotting [JW]
vif_df <- as.data.frame(vif_values) %>%
  rownames_to_column(var = "Variable")

# Plot the VIF values [JW]
vif_plot <- ggplot(vif_df, aes(x = reorder(Variable, vif_values), y =  vif_values)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "VIF Values for Final Linear Model", x = "Variable", y = "VIF") +
  theme_minimal()

# print the VIF plot
print(vif_plot)

# Save the VIF plot to a file [JW]
ggsave(filename = file.path(IMAGES_PATH, "final-linear-model-vif-values-plot.png"), plot = vif_plot, width = 8, height = 6)

# Consistently returns NaN for CV despite all best practice resolution attempts.
# # Perform k-fold cross-validation
# set.seed(123)  # For reproducibility
# cv_error <- cv.glm(selected_data, linear_model_final, K = 10)
# 
# # Print the cross-validation error
# cat("Cross-Validation Error:\n")
# print(cv_error$delta)

####### Random Forest - Tuning ##########
# A hyperparameter grid search will be used to evaluate the ideal values for this model.

# Define a parameter grid for mtry
param_grid <- expand.grid(
  mtry = c(2, 3, 4, 5, 6, 7, 8)     # Number of variables to try at each split
)

# Define cross-validation control
ctrl <- trainControl(
  method = "cv",
  number = 5,
  search = "grid"
)

# Train the Random Forest model using grid search for mtry
rf_model <- train(
  AVG_MISHAND_RATIO ~ ., 
  data = model_tuning_data, 
  method = "rf", 
  tuneGrid = param_grid, 
  trControl = ctrl
)

# Print the best hyperparameters
cat("Best mtry:\n")
print(rf_model$bestTune)

# Plot the results of the grid search for mtry
rf_mtry_plot <- ggplot(rf_model$results, aes(x = factor(mtry), y = RMSE)) +
  geom_line() +
  geom_point() +
  labs(title = "Grid Search Results for Random Forest (mtry)",
       x = "mtry",
       y = "RMSE") +
  theme_minimal()

ggsave(filename = file.path(IMAGES_PATH, "random-forest-grid-search-mtry.png"), plot = rf_mtry_plot, width = 8, height = 6)

# Now, evaluate the best number of trees with the best mtry
ntree_values <- c(50, 100, 200, 500)
ntree_results <- sapply(ntree_values, function(ntree) {
  rf_temp <- randomForest(
    x = model_tuning_data %>% select(-AVG_MISHAND_RATIO), 
    y = model_tuning_data$AVG_MISHAND_RATIO,
    ntree = ntree,
    mtry = rf_model$bestTune$mtry,
    nodesize = 10,
    importance = TRUE
  )
  return(mean(rf_temp$mse))  # Use MSE to evaluate performance
})

# Convert the ntree results to a data frame
ntree_df <- data.frame(ntree = ntree_values, MSE = ntree_results)

# Plot the results for different ntree values
ntree_plot <- ggplot(ntree_df, aes(x = ntree, y = MSE)) +
  geom_line() +
  geom_point() +
  labs(title = "Random Forest Performance with Different ntree Values",
       x = "Number of Trees (ntree)",
       y = "Mean Squared Error (MSE)") +
  theme_minimal()

ggsave(filename = file.path(IMAGES_PATH, "random-forest-ntree-tuning.png"), plot = ntree_plot, width = 8, height = 6)

# Select the best ntree
best_ntree <- ntree_df$ntree[which.min(ntree_df$MSE)]
cat("Best ntree:\n")
print(best_ntree)

# Use the best hyperparameters to build the final Random Forest model
final_rf_model <- randomForest(
  x = model_tuning_data %>% select(-AVG_MISHAND_RATIO),  # Exclude the target variable from features
  y = model_tuning_data$AVG_MISHAND_RATIO,  # Target variable
  ntree = best_ntree,              # Number of trees
  mtry = rf_model$bestTune$mtry,                 # Number of variables to try at each split
  nodesize = 10,         # Minimum size of terminal nodes
  importance = TRUE                       # Compute variable importance
)

# Print the final model summary
print(final_rf_model)

# Save the final model if needed
saveRDS(final_rf_model, file = file.path(DATA_PATH, "final_rf_model.rds"))

# Plot and save final random forest model's variable importance
final_var_importance_plot <- varImpPlot(final_rf_model, main = "Final Model Variable Importance in Random Forest")

# Plot and save final random forest model's variable importance
png(filename = file.path(IMAGES_PATH, "final-random-forest-variable-importance.png"), width = 800, height = 600)
varImpPlot(final_rf_model, main = "Variable Importance for Tuned Random Forest")
dev.off()