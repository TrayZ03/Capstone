# !/usr/local/bin/Rscript

# source constants
PROJ_PATH <- "C:\\Users\\trace\\OneDrive\\Documents\\Capstone\\Capstone-Repo-Shared" # change project path for local environment
source(file.path(PROJ_PATH, "constants.R"))


# Set working directory
if (dir.exists(PROJ_PATH)) {
  setwd(PROJ_PATH)
} else {
  stop("Project path does not exist.")
}

# Libraries
library(readr)
library(dplyr)

# Initialize an empty list to store the tibbles
tibble_list <- list()

# Generate file names
for (year in YEAR_RANGE) {
  for (month in MONTH_RANGE) {
    # Uncomment if using zip files for monthly data
    # Construct the zip file name and csv file name
    # zip_file <- file.path(DATA_PATH, paste0(ZIP_PREFIX, year, '_', month, '.zip'))
    # file_list <- unzip(zip_file, list = TRUE)
    # csv_file <- file_list[[1]][1]
    
    # if using .csvs for monthly data
    csv_file <- file.path(DATA_PATH, paste0(ZIP_PREFIX, year, '_', month, '.csv'))
    
    # Uncomment if using zip files for monthly data
    # # Check if the zip file exists
    # if (file.exists(zip_file)) {
    #   # Read the CSV file directly from the ZIP archive into a tibble
    #   tibble_data <- tryCatch(
    #     {
    #       read_csv(unz(zip_file, csv_file))
    #     },
    #     error = function(e) {
    #       message(paste("Error reading:", csv_file))
    #       return(NULL)
    #     }
    #   )
      
      # Select ONTIME_FEATURES
      
      tibble_data <- read_csv(csv_file) %>%
        select(all_of(ONTIME_FEATURES))
      
      # Add the tibble to the list if it was successfully read
      if (!is.null(tibble_data)) {
        key <- paste0(year, '_', month)
        tibble_list[[key]] <- tibble_data
      } else {
      message(paste(".csv file does not exist:", csv_file))
    }
  }
}

# remove last tibble before merge to save memory
rm(tibble_data)

# Merge all tibbles into a single tibble
merged_tibble <- bind_rows(tibble_list, .id = "source") %>%
  select(all_of(ONTIME_FEATURES))

# remove tibble list
rm(tibble_list)

# Define the output CSV file and zip file paths
output_csv <- file.path(DATA_PATH, "On_Time_Marketing_Carrier_On_Time_Performance_2016-2018.csv")
output_zip <- file.path(DATA_PATH, "On_Time_Marketing_Carrier_On_Time_Performance_2016-2018.zip")

# Write the merged tibble to a CSV file
write_csv(merged_tibble, output_csv)

# Zip the CSV file
zip(output_zip, output_csv)

# Optionally, remove the CSV file after zipping
# file.remove(output_csv)
