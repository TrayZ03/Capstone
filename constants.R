# Set local project and data path

# Comment out unused path
# PROJ_PATH <- "C:\\Users\\trace\\OneDrive\\Documents\\Capstone\\Capstone-Repo-Shared" # Tracey's local environment
PROJ_PATH <- "/Users/jessweeks/Documents/Capstone" # Jess' local environment

ZIP_PREFIX <- 'On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_'
YEAR_RANGE <- 2016:2018
MONTH_RANGE <- 1:12

# directory paths
DATA_PATH <- file.path(PROJ_PATH, "data")
IMAGES_PATH <- file.path(PROJ_PATH, "images")
SCRIPTS_PATH <- file.path(PROJ_PATH, "scripts")

# on-time data constants
ONTIME_DATA_FILE <- file.path(DATA_PATH, "On_Time_Marketing_Carrier_On_Time_Performance_2016-2018.csv")
CLEAN_ONTIME_DATA_FILE <- file.path(DATA_PATH, "Cleaned_On_Time_Marketing_Carrier_On_Time_Performance_2016-2018.csv")
ONTIME_FEATURES <- c("Year", "Month", "Reporting_Airline", "DepDelay", "ArrDelay", "Cancelled", 
                     "Diverted", "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay",
                     "LateAircraftDelay")

# baggage data constants
BAGGAGE_DATA_FILE <- file.path(DATA_PATH, "Commercial_Aviation_-_Mishandled_Baggage_and_Mishandled_Wheelchairs_and_Scooter_20240717.csv")
BAGGAGE_FEATURES <- c("YEAR", "MONTH", "CARRIER", "PASSENGERS", "MISHANDLED_BAGGAGE", "CARRIER_NAME")

# mode data constants
MODEL_DATA_FILE <- file.path(DATA_PATH, "Model_Data.csv")
SELECTED_FEATURES <- c("PASSENGERS", "NUM_FLIGHTS", "PROP_DELAYED", "PROP_CARRIER_DEL", "PROP_WEATHER_DEL", 
                    "PROP_LATEAIRCRAFT_DEL", "MIN_DEP_DEL", "Q1_DEP_DEL", "MED_DEP_DEL", "Q3_DEP_DEL", "MAX_DEP_DEL")

# for testing on small datasets, set to false for final run
TESTING <- FALSE 
