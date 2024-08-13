# !/usr/local/bin/Rscript

# source constants

PROJ_PATH <- "C:/Users/trace/OneDrive/Documents/Capstone/Capstone-Repo-Shared" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment

source(file.path(PROJ_PATH, "constants.R"))
source(file.path(PROJ_PATH, "helpers.R"))

# load libraries 
library(tidyverse)
library(glmnet)
library(car)
library(boot)

# load flight delay dataset as CSV [TZ]
selected_data <- read_csv(SELECTED_DATA_FILE) 

######### One-Hot Encode CARRIER_NAME ##########

# Use model.matrix to one-hot encode the CARRIER_NAME column
carrier_dummies <- model.matrix(~ CARRIER_NAME - 1, data = selected_data)
carrier_dummies <- as.data.frame(carrier_dummies)

# Combine the one-hot encoded columns with the rest of the data
selected_data <- cbind(selected_data %>% select(-CARRIER_NAME), carrier_dummies)

######### Full Linear Regression Model ########## 

# Fit an updated linear model using the selected features
linear_model <- lm(AVG_MISHAND_RATIO ~ ., data = selected_data )

# Get the summary of the linear model, including p-values
summary_stats <- summary(linear_model)
cat("Full Linear Model Summary:\n")
print(summary_stats)

diagnostic_plots(linear_model, file.path(IMAGES_PATH, "full-linear-model-diagnostic-plots.png"))


######### Updated Linear Regression Model ########## 

# drop HIGH_UTIL_RATIO, CARRIER_NAMEVirgin America which are perfectly collinear with the intercept and prevents VIF values,
selected_data  <- selected_data  %>%
  select(!c("HIGH_UTIL_RATIO", "CARRIER_NAMEVirgin America"))

# Fit an updated linear model using the selected features
linear_model_updated <- lm(AVG_MISHAND_RATIO ~ ., data = selected_data )

# Get the summary of the linear model, including p-values
summary_stats <- summary(linear_model_updated)
cat("Updated Linear Model Summary:\n")
print(summary_stats)

# show updated diagnostic plots
diagnostic_plots(linear_model_updated, file.path(IMAGES_PATH, "updated-linear-model-diagnostic-plots.png"))

# identify collinearity with aliasing which prevented VIF
print(alias(linear_model_updated))

# Check for multicollinearity (shown by high VIF values) [JW]
vif_values <- vif(linear_model_updated)
cat("VIF Values:\n")
print(vif_values)

# Convert VIF values to a data frame for plotting [JW]
vif_df <- as.data.frame(vif_values) %>%
  rownames_to_column(var = "Variable")

# Plot the VIF values [JW]
vif_plot <- ggplot(vif_df, aes(x = reorder(Variable, vif_values), y =  vif_values)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "VIF Values for Linear Model", x = "Variable", y = "VIF") +
  theme_minimal()

# print the VIF plot
print(vif_plot)

# Save the VIF plot to a file [JW]
ggsave(filename = file.path(IMAGES_PATH, "vif-values-plot.png"), plot = vif_plot, width = 8, height = 6)


######### Refine Linear Model with Feature Selection ########## 

# drop features with low significance and high multicollinearity
DROP_FEATURES <- c("PASSENGERS", "NUM_FLIGHTS", "PROP_DELAYED", "PROP_WEATHER_DEL",
                   "CARRIER_NAMEAmerican Airlines Inc.", "CARRIER_NAMEDelta Air Lines Inc.",
                   "CARRIER_NAMEEnvoy Air", "CARRIER_NAMEHawaiian Airlines Inc.", 
                   "CARRIER_NAMESouthwest Airlines Co.", "CARRIER_NAMEUnited Air Lines Inc.",
                   "MIN_DEP_DEL", "Q1_DEP_DEL", "MED_DEP_DEL", "Q3_DEP_DEL"
                   )

selected_data <- selected_data  %>%
  select(-all_of(DROP_FEATURES))

# save updated selected data
save_selected_data(selected_data)

# fit refined model
linear_model_refined <- lm(AVG_MISHAND_RATIO ~ ., data = selected_data)

# Get the summary of the linear model, including p-values
summary_stats <- summary(linear_model_refined)
cat("Refined Linear Model Summary:\n")
print(summary_stats)

# show updated diagnostic plots
diagnostic_plots(linear_model_refined, file.path(IMAGES_PATH, "refined-linear-model-diagnostic-plots.png"))


######### Elastic Net Regression Model ########## 

# Prepare the data [JW, TZ]
y <- selected_data$AVG_MISHAND_RATIO
X <- model.matrix(AVG_MISHAND_RATIO ~ ., selected_data)[, -1]  # Create a model matrix without the intercept


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

# Save selected data for modeling
save_selected_data(selected_data)


