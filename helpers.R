# !/usr/local/bin/Rscript

PROJ_PATH <- "/cloud/project"
# PROJ_PATH <- "C:\\Users\\trace\\OneDrive\\Documents\\Capstone\\Capstone_Shared_Repo" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment

source(file.path(PROJ_PATH, "constants.R"))


# Plot diagnostic plots for the linear model
diagnostic_plots <- function(model, filename, title = NULL) {
  # Set up a 2x2 plotting layout
  par(mfrow = c(2, 2))
  
  # Plot the model diagnostics
  plot(model)
  
  # Add the title if provided
  if (!is.null(title)) {
    mtext(title, outer = TRUE, cex = 1.5)
  }
  
  # Save the plot to a file
  dev.copy(png, filename)
  dev.off()
}


# standardize data
standardize_data <- function(tb) {
  numeric_cols <- tb %>% select(where(is.numeric)) %>% colnames()
  
  tb[numeric_cols] <- scale(tb[numeric_cols])
  
  return(tb)
}

# one-hot encode data

one_hot_encode_carrier <- function(tb){
  # Use model.matrix to one-hot encode the CARRIER_NAME column
  carrier_dummies <- model.matrix(~ CARRIER_NAME - 1, data = tb)
  carrier_dummies <- as.data.frame(carrier_dummies)
  
  # Combine the one-hot encoded columns with the rest of the data
  tb <- cbind(tb %>% select(-CARRIER_NAME), carrier_dummies)
  
  return(tb)
}

# Save selected data for modeling 
save_data <- function(data, DATA_FILE, FEATURES_NAME) {
  # Get the final model column names as a vector
  FEATURES <- colnames(data)
  
  # Save the SELECTED_FEATURES vector to the constants.R file only if it doesn't exist
  constants_file <- file.path(PROJ_PATH, "constants.R")
  
  # Check if SELECTED_FEATURES already exists in constants.R
  if (!any(grepl(FEATURES_NAME, readLines(constants_file)))) {
    # Create a string to write to constants.R
    features_string <- paste0(FEATURES_NAME, " <- c(", paste0('"', FEATURES, '"', collapse = ", "), ")")
    
    # Write the string to constants.R
    write(features_string, file = constants_file, append = TRUE)
  }
  
  # Write the merged tibble to a CSV file
  write_csv(data, DATA_FILE)
  
  return(FEATURES)
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
