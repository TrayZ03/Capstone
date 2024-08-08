# Exploratory Data Analysis
# Team Alpha: Jess and Tracey

# source constants
PROJ_PATH <- "C:\\Users\\trace\\OneDrive\\Documents\\Capstone\\Capstone-Repo-Shared" # change project path for local environment
source(file.path(PROJ_PATH, "constants.R"))


# download necessary libraries
library(tidyverse)

######### Prepping flight delay data ############
# load flight delay dataset as CSV 
ontime_data <- read_csv(CLEAN_ONTIME_DATA_FILE) %>% 
  select(all_of(ONTIME_FEATURES)) %>% # select proposed features

# check for missing values 
ontime_missing <- ontime_data %>%
  summarize(across(everything(), ~ sum(is.na(.)), .names = "missing_{col}"))

print(summary(ontime_data))

######### Prepping mishandled baggage data ############
# load mishandled baggage data as CSV
mishandled_data <- read_csv(BAGGAGE_DATA_FILE) %>% 
  select(BAGGAGE_FEATURES) %>% # select proposed features
  rename_with(~ str_to_upper(.)) # convert columns to upper case

# check for missing values
mishandled_missing <- mishandled_data %>%
  summarize(across(everything(), ~ sum(is.na(.)), .names = "missing_{col}"))

# Visual inspection reveals PASSENGERS is completely missing for years 2019-2021, 
# these should be dropped in cleaning and preprocessing. Here's code to prove it
mishandled_missing_by_year <- mishandled_data %>%
  group_by(YEAR) %>%
  summarize(across(everything(), ~ sum(is.na(.)), .names = "missing_{col}"))

print(summary(mishandled_data))


############################## Summary Statistics ##############################

# # Drop years 2019-2021 for both datasets
# mishandled_data <- mishandled_data %>%
#   filter(YEAR <= 2018)
# ontime_data <- ontime_data %>% 
#   filter(YEAR <= 2018)


###################### Plot Distributions of Departure Delay ######################

# full histogram of departure delay
depdelay_histplot <- ontime_data %>%
  ggplot(aes(x=DEPDELAY)) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Histogram of Departure Delay",
       x = "Departure Delay [Minutes]",
       y = "Count") +
  theme_minimal()
print(depdelay_histplot)
ggsave(file.path(IMAGES_PATH, "depdelay_histplot.png"), 
       plot = depdelay_histplot, width = 10, height = 6)

# histogram of departure delay bounded betweem min and max
depdelay_neg_histplot <- ontime_data %>%
  filter((min(DEPDELAY) < DEPDELAY) & (DEPDELAY < max(DEPDELAY))) %>%
  ggplot(aes(x=DEPDELAY)) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Bounded Histogram of Negative Departure Delay Values",
       x = "Departure Delay [Minutes]",
       y = "Count") +
  theme_minimal()
print(depdelay_neg_histplot)
ggsave(file.path(IMAGES_PATH, "depdelay_bdd_histplot.png"), 
       plot = depdelay_neg_histplot, width = 10, height = 6)


###################### Plot Distributions of Arrival Delay ######################

# full histogram of arrival delay
arrdelay_histplot <- ontime_data %>%
  ggplot(aes(x=ARRDELAY)) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Histogram of Arrival Delay",
       x = "Arrival Delay [Minutes]",
       y = "Count") +
  theme_minimal()
print(arrdelay_histplot)
ggsave(file.path(IMAGES_PATH, "arrdelay_histplot.png"), 
       plot = arrdelay_histplot, width = 10, height = 6)

# histogram of arrival delay bounded betweem min and max
arrdelay_neg_histplot <- ontime_data %>%
  filter((min(ARRDELAY) < ARRDELAY) & (ARRDELAY < max(ARRDELAY))) %>%
  ggplot(aes(x=ARRDELAY)) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Bounded Histogram of Arrival Delay Values",
       x = "Arrival Delay [Minutes]",
       y = "Count") +
  theme_minimal()
print(arrdelay_neg_histplot)
ggsave(file.path(IMAGES_PATH, "arrdelay_bdd_histplot.png"), 
       plot = arrdelay_neg_histplot, width = 10, height = 6)



###################### Correlation Matrix for On-Time Data #####################

# Use correlation matrix since dataset is way too big for scatterplot

# Select only relevant columns
ontime_numeric_data <- ontime_data %>% 
  select(c("CARRIERDELAY", "WEATHERDELAY", "NASDELAY",
           "LATEAIRCRAFTDELAY", "SECURITYDELAY", "ARRDELAY", "DEPDELAY"))

# Generate the correlation matrix
ontime_correlation_matrix <- cor(ontime_numeric_data, use = "complete.obs")

# Print the correlation matrix
print(ontime_correlation_matrix)


###################### Scatterplot for Baggage Data #########################

passengers_mishandled_scatterplot <- mishandled_data %>%
  ggplot(aes(x=PASSENGERS, y=MISHANDLED_BAGGAGE)) +
  geom_point() +
  labs(title = "Passengers vs. Mishandled Baggage",
       x = "Passengers",
       y = "Mishandled Baggage") +
  theme_minimal()
print(passengers_mishandled_scatterplot)
ggsave(file.path(IMAGES_PATH, "passengers_mishandled_scatterplot.png"), 
       plot=passengers_mishandled_scatterplot, width = 10, height = 6)


####################### Additional Informative Plots ###########################

########## Plot Average Mishandled Baggage by Carrier Over all Years ###########

avg_mishandled_by_carrier_plot <- mishandled_data %>%
  group_by(CARRIER) %>%
  summarize(AVG_MISHANDLED_BAGGAGE = mean(MISHANDLED_BAGGAGE)) %>%
  ggplot(aes(x=CARRIER, y=AVG_MISHANDLED_BAGGAGE, fill=CARRIER)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Mishandled Baggage by Airline Carrier",
       x = "Carrier",
       y = "Average Mishandled Baggage") +
  theme_minimal()
print(avg_mishandled_by_carrier_plot)
ggsave(file.path(IMAGES_PATH, "avg_mishandled_by_carrier_plot.png"), 
       plot=avg_mishandled_by_carrier_plot, width = 10, height = 6)


########## Plot Average Passengers by Carrier Over Years ###########

avg_passengers_by_carrier_plot <- mishandled_data %>%
  group_by(CARRIER) %>%
  summarize(AVG_PASSENGERS = mean(PASSENGERS)) %>%
  ggplot(aes(x=CARRIER, y=AVG_PASSENGERS, fill=CARRIER)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Passengers by Airline Carrier 2016-2019",
       x = "Carrier",
       y = "Average Passengers") +
  theme_minimal()
ggsave(file.path(IMAGES_PATH, "avg_passengers_by_carrier_plot.png"), 
       plot=avg_passengers_by_carrier_plot, width = 10, height = 6)


###### Box-and-Whisker Plot of Arrival Delay by Canceled vs. Not Canceled ######

arrdelay_canceled_boxplot <- ggplot(ontime_data, 
                                    aes(x = as.factor(CANCELLED), y = ARRDELAY, 
                                        fill = as.factor(CANCELLED))) +
  geom_boxplot() +
  labs(title = "Box Plot of Arrival Delays by Cancellation Status",
       x = "Cancelled",
       y = "Arrival Delay",
       fill = "Cancelled") +
  theme_minimal()
print(arrdelay_canceled_boxplot)
ggsave(file.path(IMAGES_PATH, "arrdelay_canceled_boxplot.png"), 
       plot=arrdelay_canceled_boxplot, width = 10, height = 6)

