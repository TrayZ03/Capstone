# !/usr/local/bin/Rscript

# PROJ_PATH <- "C:/Users/trace/OneDrive/Documents/Capstone/Capstone-Repo-Shared" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment

source(file.path(PROJ_PATH, "constants.R"))

# Plot diagnostic plots for the linear model
diagnostic_plots <- function(model, filename) {
  par(mfrow = c(2, 2))
  plot(model)
  dev.copy(png, filename)
  dev.off()
}

# Save selected data for modeling 

save_selected_data <- function(selected_data) {
  # Get the final model column names as a vector
  SELECTED_FEATURES <- colnames(selected_data)
  
  # Save the SELECTED_FEATURES vector to the constants.R file only if it doesn't exist
  constants_file <- file.path(PROJ_PATH, "constants.R")
  
  # Check if SELECTED_FEATURES already exists in constants.R
  if (!any(grepl("SELECTED_FEATURES", readLines(constants_file)))) {
    write(paste("SELECTED_FEATURES <- c(", paste0('"', SELECTED_FEATURES, '"', collapse = ", "), ")"), 
          file = constants_file, append = TRUE)
  }
  
  # Write the merged tibble to a CSV file
  write_csv(selected_data, SELECTED_DATA_FILE)
  
  return(SELECTED_FEATURES)
}

# Function to calculate MSE for cross-validation
cv_mse <- function(model, data, indices) {
  train_data <- data[indices, ]  # Training data
  test_data <- data[-indices, ]  # Testing data
  
  # Fit the model on the training data
  fit <- lm(formula(model), data = train_data)
  
  # Predict on the test data
  predictions <- predict(fit, newdata = test_data)
  
  # Calculate Mean Squared Error
  mse <- mean((test_data$mpg - predictions)^2)
  return(mse)
}
