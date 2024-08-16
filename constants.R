# Set local project and data path

# PROJ_PATH <- "C:\Users\trace\OneDrive\Documents\Capstone\Capstone-Shared-Repo" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment

# directory paths
DATA_PATH <- file.path(PROJ_PATH, "data")
IMAGES_PATH <- file.path(PROJ_PATH, "images")
SCRIPTS_PATH <- file.path(PROJ_PATH, "scripts")

# on-time data file
ONTIME_DATA_FILE <- file.path(DATA_PATH, "On_Time_Marketing_Carrier_On_Time_Performance_2016-2018.csv")

# clean data file
CLEAN_ONTIME_DATA_FILE <- file.path(DATA_PATH, "Cleaned_On_Time_Marketing_Carrier_On_Time_Performance_2016-2018.csv")

# baggage data file
BAGGAGE_DATA_FILE <- file.path(DATA_PATH, "Commercial_Aviation_-_Mishandled_Baggage_and_Mishandled_Wheelchairs_and_Scooter_20240717.csv")

# merged data - first output of data-preprocessing.R
FLIGHT_DATA_FILE <- file.path(DATA_PATH, "flight-data.csv")

# preprocessed data - second output of data-preprocessing.R
PREPROCESSED_DATA_FILE <- file.path(DATA_PATH, "preprocessed-data.csv")

# initial model data (standardized) - output of intial-model.R
INITIAL_MODEL_DATA_FILE <- file.path(DATA_PATH, "initial-model-data.csv")

# final model data, standardized and unstandardized - output of final-model.R
FINAL_MODEL_DATA_FILE <- file.path(DATA_PATH, "final-model-data.csv")
FINAL_MODEL_DATA_STD_FILE <- file.path(DATA_PATH, "final-model-standardized-data.csv")
  
# for data-clean-ontime.R
ZIP_PREFIX <- 'On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_'
YEAR_RANGE <- 2016:2018
MONTH_RANGE <- 1:12

# intermediate feature sets
ONTIME_FEATURES <- c("Year", "Month", "Reporting_Airline", "DepDelay", "ArrDelay", "Cancelled", 
                     "Diverted", "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay",
                     "LateAircraftDelay")
BAGGAGE_FEATURES <- c("YEAR", "MONTH", "CARRIER", "PASSENGERS", "MISHANDLED_BAGGAGE", "CARRIER_NAME")
FLIGHT_FEATURES <- c("YEAR", "MONTH", "CARRIER", "PASSENGERS", "MISHANDLED_BAGGAGE", "CARRIER_NAME", "NUM_FLIGHTS", "PROP_DELAYED", "PROP_CARRIER_DEL", "PROP_WEATHER_DEL", "PROP_LATEAIRCRAFT_DEL", "MIN_DEP_DEL", "Q1_DEP_DEL", "MED_DEP_DEL", "Q3_DEP_DEL", "MAX_DEP_DEL", "MISHAND_PASS_RATIO", "MISHAND_FLIGHTS_RATIO", "AIRCRAFT_UTIL_RATIO", "HIGH_UTIL_RATIO", "AVG_MISHAND_RATIO")
PREPROCESSED_FEATURES <- c("PASSENGERS", "CARRIER_NAME", "NUM_FLIGHTS", "PROP_DELAYED", "PROP_CARRIER_DEL", "PROP_WEATHER_DEL", "PROP_LATEAIRCRAFT_DEL", "MIN_DEP_DEL", "Q1_DEP_DEL", "MED_DEP_DEL", "Q3_DEP_DEL", "MAX_DEP_DEL", "AIRCRAFT_UTIL_RATIO", "HIGH_UTIL_RATIO", "AVG_MISHAND_RATIO")
INITIAL_MODEL_FEATURES <- c("PROP_CARRIER_DEL", "PROP_LATEAIRCRAFT_DEL", "MAX_DEP_DEL", "AIRCRAFT_UTIL_RATIO", "AVG_MISHAND_RATIO", "CARRIER_NAMEAlaska Airlines Inc.", "CARRIER_NAMEExpressJet Airlines Inc.", "CARRIER_NAMEFrontier Airlines Inc.", "CARRIER_NAMEJetBlue Airways", "CARRIER_NAMESkyWest Airlines Inc.", "CARRIER_NAMESpirit Air Lines")
FINAL_MODEL_FEATURES <- c("PROP_CARRIER_DEL", "PROP_LATEAIRCRAFT_DEL", "MAX_DEP_DEL", "AIRCRAFT_UTIL_RATIO", "AVG_MISHAND_RATIO", "CARRIER_NAMEAlaska Airlines Inc.", "CARRIER_NAMEExpressJet Airlines Inc.", "CARRIER_NAMEFrontier Airlines Inc.", "CARRIER_NAMEJetBlue Airways", "CARRIER_NAMESkyWest Airlines Inc.", "CARRIER_NAMESpirit Air Lines")
FINAL_MODEL_STD_FEATURES <- c("PROP_CARRIER_DEL", "PROP_LATEAIRCRAFT_DEL", "MAX_DEP_DEL", "AIRCRAFT_UTIL_RATIO", "AVG_MISHAND_RATIO", "CARRIER_NAMEAlaska Airlines Inc.", "CARRIER_NAMEExpressJet Airlines Inc.", "CARRIER_NAMEFrontier Airlines Inc.", "CARRIER_NAMEJetBlue Airways", "CARRIER_NAMESkyWest Airlines Inc.", "CARRIER_NAMESpirit Air Lines")
