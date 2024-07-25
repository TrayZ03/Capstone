# Exploratory Data Analysis
# Team Alpha: Jess and Tracey

# download necessary libraries
library(readr)
library(tidyverse)
library(car)

######### Prepping flight delay data ############
# load flight delay dataset as CSV 
ontime_data <- read_csv("~/Downloads/ontime_data_clean.csv")
View(ontime_data) # 26.5mil x 16

# check for missing values 
sum(is.na(ontime_data))

# 2 missing - omit them
ontime_data %>% na.omit(ontime_data)

# prelim summary statistics
summary(ontime_data)


######### Prepping mishandled baggage data ############
# load mishandled baggage data as CSV
mishandled_data <- read_csv("~/Downloads/Commercial_Aviation_-_Mishandled_Baggage_and_Mishandled_Wheelchairs_and_Scooter_20240717.csv")
View(mishandled_data) # 1083 x 15

# check for missing values
sum(is.na(mishandled_data$PASSENGERS)) # 757 missing
sum(is.na(mishandled_data$ENPLANED_BAGGAGE)) # 326 missing
sum(is.na(mishandled_data$MISHANDLED_WCHR_SCTR)) # 326 missing
sum(is.na(mishandled_data$ENPLANED_WCHR_SCTR)) # 326
sum(is.na(mishandled_data$FORM_TYPE)) # 326
sum(is.na(mishandled_data$MKT_CARRIER_FLAG)) # 328

# mishandling summary statistics
summary(mishandled_data)

# dealing with missing values - check the distribution of each variable with NAs before & after imputation
## NEXT STEP: use KNN (or another appropriate method) to fill in the missing values

# distribution of passenger variable
ggplot(mishandled_data, aes(PASSENGERS)) +
  geom_histogram(color = "black", fill = "#0099F8") +
  ggtitle("Passenger Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of enplaned_baggage variable
ggplot(mishandled_data, aes(ENPLANED_BAGGAGE)) +
  geom_histogram(color = "black", fill = "#0099F8") +
  ggtitle("Enplaned Baggage Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of mishandled_wchr_sctr
ggplot(mishandled_data, aes(MISHANDLED_WCHR_SCTR)) +
  geom_histogram(color = "black", fill = "#0099F8") +
  ggtitle("Mishandled Wheelchair/Scooter Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of enplaned_wchr_sctr
ggplot(mishandled_data, aes(ENPLANED_WCHR_SCTR)) +
  geom_histogram(color = "black", fill = "#0099F8") +
  ggtitle("Enplaned Wheelchair/Scooter Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of form_type
ggplot(mishandled_data, aes(FORM_TYPE)) +
  geom_histogram(color = "black", fill = "#0099F8", stat = "count") +
  ggtitle("Form Type Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of mkt_carrier_flag
ggplot(mishandled_data, aes(MKT_CARRIER_FLAG)) +
  geom_histogram(color = "black", fill = "#0099F8", stat = "count") +
  ggtitle("Market Carrier Flag Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

######### Aggregating delay data and merging datasets ############
## move forward with merging datasets:
# standardize names for columns to merge on: year, month, carrier
mishandled_data %>%
  rename(Year = YEAR,
         Month = MONTH,
         Carrier = CARRIER) %>%
head()

ontime_data %>% 
  rename(Carrier = Marketing_Airline_Network) %>%
  head()

# merge the two datasets based on the above columns
### merged_data <- merge(ontime_data, mishandled_data, by = c("Year", "Month", "Carrier")) - nope, try again
# since the two datasets do not have matching rows that uniquely identify one another, and ontime_data has so many observations, we will aggregate first before performing the merge. 
# this will make it so we are analyzing the monthly numbers for delay times, rather than flight-by-flight. May be a temporary solution

# aggregate the ontime_data dataframe based on year, month, carrier
agg_ontime_data <- ontime_data %>%
  group_by(Year, Month, Marketing_Airline_Network) %>%
  summarize(Flight_Count = n(), 
            Avg_DepDelay = mean(DepDelay, na.rm = T),
            Avg_ArrDelay = mean(ArrDelay, na.rm = T),
            Avg_Distance = mean(Distance, na.rm = T),
            Avg_AirTime = mean(AirTime, na.rm = T))

View(agg_ontime_data)

# merge aggregated ontime data with mishandled data - month-level initial analysis purposes
merged_agg_data <- merge(agg_ontime_data, mishandled_data,
                         by.x = c("Year", "Month", "Marketing_Airline_Network"),
                         by.y = c("YEAR", "MONTH", "CARRIER"))
View(merged_agg_data)
summary(merged_agg_data)

# correlation matrix of numeric variables
# create numeric subset of merged data first
numeric_data_subset <- merged_agg_data %>%
  select_if(c())

head(numeric_data_subset)

######### Preliminary analysis of monthly merged data #############
# make a correlation matrix of the numeric variables
correlation_matrix <- cor(numeric_data_subset)
print(correlation_matrix)

# look into collinearity and other potential issues
# variables of interest: mishandled_baggage/avg_depdelay, mishandled_baggage/avg_arrdelay
linear_model <- lm(data = numeric_data_subset, formula = as.formula(paste("~ .")))

# REVISIT AFTER FILLING NA VALUES
vif_values <- vif(linear_model)


print(linear_model)