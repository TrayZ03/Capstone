
# source constants
PROJ_PATH <- "C:\\Users\\trace\\OneDrive\\Documents\\Capstone\\Capstone-Repo-Shared" # change project path for local environment
source(file.path(PROJ_PATH, "constants.R"))

# load libraries 
library(tidyverse)
library(glmnet)

# load flight delay dataset as CSV [TZ]
data <- read_csv(MODEL_DATA_FILE) %>%
  select(!c("CARRIER", "CARRIER_NAME"))  %>%
  select(!c("YEAR", "MONTH")) %>% # omit time features
  select(!c("MISHAND_PASS_RATIO", "MISHAND_FLIGHTS_RATIO", "AVG_MISHAND_RATIO")) # omit features derived from target


# Assuming your tibble is named data and has a target variable MISHANDLED_BAGGAGE
# Prepare the data
y <- data$MISHANDLED_BAGGAGE
X <- model.matrix(MISHANDLED_BAGGAGE ~ ., data)[, -1]  # Create a model matrix without the intercept

# Fit the Elastic Net regression model
set.seed(123)  # For reproducibility
elastic_net_model <- cv.glmnet(
  X, y, 
  alpha = 0.5,  # Elastic Net (0 = Ridge, 1 = Lasso)
  nfolds = 10   # Number of folds for cross-validation
)

# Get the best lambda value
best_lambda <- elastic_net_model$lambda.min

# Print the best lambda
print(paste("Best lambda: ", best_lambda))

# Fit the final model with the best lambda
final_model <- glmnet(X, y, alpha = 0.5, lambda = best_lambda)

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

# Fit a linear model using the selected features
linear_model <- lm(MISHANDLED_BAGGAGE ~ ., data = selected_data)

# Get the summary of the linear model, including p-values
summary_stats <- summary(linear_model)
cat("Linear Model Summary:\n")
print(summary_stats)

# plot diagnostic plots
plot(linear_model)
