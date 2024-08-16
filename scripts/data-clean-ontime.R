# !/usr/local/bin/Rscript

# source constants
# PROJ_PATH <- "C:\Users\trace\OneDrive\Documents\Capstone\Capstone-Shared-Repo" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment

source(file.path(PROJ_PATH, "constants.R"))
source(file.path(PROJ_PATH, "helpers.R"))

# Set working directory
if (dir.exists(PROJ_PATH)) {
  setwd(PROJ_PATH)
} else {
  stop("Project path does not exist.")
}

# Libraries
library(readr)
library(dplyr)
library(tidyr)

# load on-time data
tibble_clean <- read_csv(ONTIME_DATA_FILE) %>% 
  select(all_of(ONTIME_FEATURES))

# small sample for testing
if (TESTING) {
  tibble_clean <- tibble_clean %>% 
    sample_n(1000)
}

# Replace NA values with 0 using mutate_at
tibble_clean <- tibble_clean %>%
  mutate_at(vars(CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay), ~ replace_na(., 0))

tibble_clean <- tibble_clean %>% 
  filter(if_all(c(DepDelay, ArrDelay), ~ !is.na(.))) %>% # drop flights with missing delay times
  rename_with(~ toupper(.)) %>% # make column names uppercase
  rename(CARRIER = REPORTING_AIRLINE) # rename for consistency with mishandled data

# Define the output CSV file and zip file paths
output_csv <- file.path(DATA_PATH, "Cleaned_On_Time_Marketing_Carrier_On_Time_Performance_2016-2018.csv")
output_zip <- file.path(DATA_PATH, "Cleaned_On_Time_Marketing_Carrier_On_Time_Performance_2016-2018.zip")

# Write the merged tibble to a CSV file
write_csv(tibble_clean, output_csv)

# Zip the CSV file
zip(output_zip, output_csv)

# Optionally, remove the CSV file after zipping
# file.remove(output_csv)
