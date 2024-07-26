# Exploratory Data Analysis
# Team Alpha: Jess and Tracey

# download necessary libraries
library(readr)
library(tidyverse)
library(car)
library(mice)

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
## NEXT STEP: decide on a missing value imputation method appropriate for the dataset. Too many missing values to use KNN
## Have tried: KNN, mice (linear regression, ridge regression, predictive mean matching, classification and regression trees)

# First check the variance of each predictor variable - ones with variance close to 0 can be removed
variances <- apply(mishandled_data, 2, var, na.rm = TRUE)

# Display the variances - carrier and carrier_id are redundant, can be removed
variances

# remove columns unique_carrier and unique_carrier name, contain the same info as carrier and carrier_id
mishandled_data <- mishandled_data[, -c(7, 8)]

# set the imputation methods for each variable
methods <- make.method(mishandled_data)

# do not include form type in the imputation
methods["FORM_TYPE"] <- "none"

# set classification and regression trees as the method for other variables
methods[names(methods) != "FORM_TYPE"] <- "cart"

# current method: trying the mice package and classification and regression trees to impute values
imputed_data_mice <- mice(mishandled_data, method = methods, m = 1)

# Complete the dataset by filling in the missing values
mishandled_data_comp <- complete(imputed_data_mice)

# Check the first few rows of the completed data
head(mishandled_data_comp)
sum(is.na(mishandled_data_comp))

####### mishandled_baggage variable graphs pre- and post-imputation ############
# distribution of passenger variable before imputation
ggplot(mishandled_data, aes(PASSENGERS)) +
  geom_histogram(color = "black", fill = "#0099F8") +
  ggtitle("Passenger Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of passenger variable after imputation
ggplot(mishandled_data_comp, aes(PASSENGERS)) +
  geom_histogram(color = "black", fill = "#8A9A5B") +
  ggtitle("Post-Imputation Passenger Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of enplaned_baggage variable
ggplot(mishandled_data, aes(ENPLANED_BAGGAGE)) +
  geom_histogram(color = "black", fill = "#0099F8") +
  ggtitle("Enplaned Baggage Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of enplaned_baggage variable after imputation
ggplot(mishandled_data_comp, aes(ENPLANED_BAGGAGE)) +
  geom_histogram(color = "black", fill = "#8A9A5B") +
  ggtitle("Post-Imputation Enplaned Baggage Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of mishandled_wchr_sctr
ggplot(mishandled_data, aes(MISHANDLED_WCHR_SCTR)) +
  geom_histogram(color = "black", fill = "#0099F8") +
  ggtitle("Mishandled Wheelchair/Scooter Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of mishandled_wchr_sctr variable after imputation
ggplot(mishandled_data_comp, aes(MISHANDLED_WCHR_SCTR)) +
  geom_histogram(color = "black", fill = "#8A9A5B") +
  ggtitle("Post-Imputation Mishandled Wheelchair/Scooter Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of enplaned_wchr_sctr
ggplot(mishandled_data, aes(ENPLANED_WCHR_SCTR)) +
  geom_histogram(color = "black", fill = "#0099F8") +
  ggtitle("Enplaned Wheelchair/Scooter Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of enplaned_wchr_sctr variable after imputation
ggplot(mishandled_data_comp, aes(ENPLANED_WCHR_SCTR)) +
  geom_histogram(color = "black", fill = "#8A9A5B") +
  ggtitle("Post-Imputation Enplaned Wheelchair/Scooter Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of form_type
ggplot(mishandled_data, aes(FORM_TYPE)) +
  geom_histogram(color = "black", fill = "#0099F8", stat = "count") +
  ggtitle("Form Type Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of form_type variable after imputation
ggplot(mishandled_data_comp, aes(FORM_TYPE)) +
  geom_histogram(color = "black", fill = "#8A9A5B") +
  ggtitle("Post-Imputation Form Type Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))
## Imputation caused issues with form_type - remove it from the list of variables to undergo this method of imputation, find an alternative

# distribution of mkt_carrier_flag
ggplot(mishandled_data, aes(MKT_CARRIER_FLAG)) +
  geom_histogram(color = "black", fill = "#0099F8", stat = "count") +
  ggtitle("Market Carrier Flag Variable Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# distribution of mkt_carrier_flag variable after imputation
ggplot(mishandled_data_comp, aes(MKT_CARRIER_FLAG)) +
  geom_histogram(color = "black", fill = "#8A9A5B") +
  ggtitle("Post-Imputation Market Carrier Flag Distribution") + 
  theme_classic() +
  theme(plot.title = element_text(size = 18))
# Edit this graph later - poor formatting but gets point across

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

head(numeric_data_subset) # shows that there are still issues with ENPLANED_BAGGAGE, MISHANDLED_WCHR_SCTR, and ENPLANED_WCHR_SCTR

######### Preliminary analysis of monthly merged data #############
# make a correlation matrix of the numeric variables
correlation_matrix <- cor(numeric_data_subset)
print(correlation_matrix) # after messing around with imputation - there are a lot of NAs on this correlation matrix

# Next steps: examine multicollinearity, create an unsupervised learning model, continue exploring interesting patterns in the data
# variables of interest: mishandled_baggage/avg_depdelay, mishandled_baggage/avg_arrdelay
linear_model <- lm(data = numeric_data_subset, formula = as.formula(paste("~ .")))

# REVISIT AFTER FILLING NA VALUES
vif_values <- vif(linear_model)


print(linear_model)
