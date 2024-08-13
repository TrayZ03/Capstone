# !/usr/local/bin/Rscript

# Data Preprocessing 
# Team Alpha: Jess and Tracey

########## Initial Setup ###########

PROJ_PATH <- "C:/Users/trace/OneDrive/Documents/Capstone/Capstone-Repo-Shared" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment

source(file.path(PROJ_PATH, "constants.R"))
source(file.path(PROJ_PATH, "helpers.R"))

# load libraries 
library(tidyverse)

# load flight delay dataset as CSV [JW]
ontime_data <- read_csv(CLEAN_ONTIME_DATA_FILE)

# load mishandled baggage data as CSV [JW]
mishandled_data <- read_csv(BAGGAGE_DATA_FILE)

########## On-Time Data Preprocessing ##########

ontime_data <- select(ontime_data, -ARRDELAY) %>% # drop ARRDELAY column [JW]
  na.omit(ontime_data) # drop na values

# pre-aggregation feature engineering [TZ]
ontime_data <- ontime_data %>% 
  mutate(DELAYED = ifelse((DEPDELAY > 0), 1, 0),
         IS_CARRIER_DEL = ifelse(((CARRIERDELAY > 0) & DELAYED), 1, 0),
         IS_WEATHER_DEL = ifelse(((WEATHERDELAY > 0) & DELAYED), 1, 0),
         IS_NAS_DEL = ifelse(((NASDELAY > 0) & DELAYED), 1, 0),
         IS_SECURITY_DEL = ifelse(((SECURITYDELAY > 0) & DELAYED), 1, 0),
         IS_LATEAIRCRAFT_DEL = ifelse(((LATEAIRCRAFTDELAY > 0) & DELAYED), 1, 0))

# aggregate using mean for CANCELLED, DIVERTED & aggregate using median for DELAY types [JW, TZ]
ontime_aggregated <- ontime_data %>%
  group_by(YEAR, MONTH, CARRIER) %>%
  summarize(NUM_FLIGHTS = n(),
            PROP_DELAYED = mean(DELAYED),
            PROP_CARRIER_DEL = mean(IS_CARRIER_DEL),
            PROP_WEATHER_DEL = mean(IS_WEATHER_DEL),
            PROP_LATEAIRCRAFT_DEL = mean(IS_LATEAIRCRAFT_DEL),
            MIN_DEP_DEL = min(DEPDELAY),
            Q1_DEP_DEL = quantile(DEPDELAY, 0.25),
            MED_DEP_DEL = median(DEPDELAY),
            Q3_DEP_DEL = quantile(DEPDELAY, 0.75),
            MAX_DEP_DEL = max(DEPDELAY)
  )


########## Mishandled Data Preprocessing ###########

mishandled_data <- mishandled_data %>%  # [TZ]
  select(all_of(BAGGAGE_FEATURES)) %>%
  filter(YEAR <= 2018)

########## Mishandled / Ontime Data Merge ##########

# merge MISHANDLED/ONTIME by YEAR/MONTH/CARRIER [JW, TZ]
flight_data <- inner_join(mishandled_data, ontime_aggregated, 
                          by = c("YEAR", "MONTH", "CARRIER"))

########## Final Feature Engineering ########## 

# filter out for positive passenger and number of flights
flight_data <- flight_data %>%
  filter((PASSENGERS > 0) & (NUM_FLIGHTS > 0))

# create ratio features [TZ]
flight_data <- flight_data %>%
  mutate(MISHAND_PASS_RATIO = MISHANDLED_BAGGAGE/PASSENGERS,
         MISHAND_FLIGHTS_RATIO = MISHANDLED_BAGGAGE/NUM_FLIGHTS,
         AIRCRAFT_UTIL_RATIO =  NUM_FLIGHTS/PASSENGERS)

# create derivative features
flight_data <- flight_data %>%
  mutate(HIGH_UTIL_RATIO = AIRCRAFT_UTIL_RATIO >= 0.015, # threshold value determined by scatter plot in secondary eda
         AVG_MISHAND_RATIO = (MISHAND_PASS_RATIO + MISHAND_FLIGHTS_RATIO)/2
         )

# Write the merged tibble to a CSV file
write_csv(flight_data, FLIGHT_DATA_FILE)


###################### Create Selected Dataset for Modeling ####################

########## Final Feature Selection ########## 

# drop columns
selected_data <- flight_data %>%
  select(!c("CARRIER"))  %>% # redundant, use CARRIER_NAME
  select(!c("YEAR", "MONTH")) %>% # drop time information
  select(!c("MISHAND_PASS_RATIO", "MISHAND_FLIGHTS_RATIO", "MISHANDLED_BAGGAGE")) # omit features derived from target

########## Standardize the data ########## 

# Standardizing numeric columns [TZ]
numeric_cols <- selected_data %>% select(where(is.numeric)) %>% colnames()

selected_data[numeric_cols] <- scale(selected_data[numeric_cols])

########## Final Checks and Cleanups ########## 

# Identify constant columns
constant_columns <- sapply(selected_data, function(x) length(unique(x)) == 1)

# Identify columns with all zeros
zero_columns <- sapply(selected_data, function(x) all(x == 0))

# Combine the results
problematic_columns <- which(constant_columns | zero_columns)

# remove outliers
selected_data <- selected_data %>%
  slice(-c(7, 31, 43))

# Save selected data for modeling
save_selected_data(selected_data)
