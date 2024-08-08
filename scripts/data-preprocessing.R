# Data Preprocessing 
# Team Alpha: Jess and Tracey

########## Initial Setup ###########

# source constants
PROJ_PATH <- "C:\\Users\\trace\\OneDrive\\Documents\\Capstone\\Capstone-Repo-Shared" # change project path for local environment
source(file.path(PROJ_PATH, "constants.R"))

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
model_data <- inner_join(mishandled_data, ontime_aggregated, by = c("YEAR", "MONTH", "CARRIER"))


########## Final Feature Engineering ########## 

# create final features [TZ]
model_data <- model_data %>%
  mutate(MISHAND_PASS_RATIO = MISHANDLED_BAGGAGE/PASSENGERS,
         MISHAND_FLIGHTS_RATIO = MISHANDLED_BAGGAGE/NUM_FLIGHTS,
         AVG_MISHAND_RATIO = (MISHAND_PASS_RATIO + MISHAND_FLIGHTS_RATIO)/2)


# Write the merged tibble to a CSV file
write_csv(model_data, MODEL_DATA_FILE)

