# !/usr/local/bin/Rscript

# source constants

# PROJ_PATH <- "C:\Users\trace\OneDrive\Documents\Capstone\Capstone-Shared-Repo" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment

source(file.path(PROJ_PATH, "constants.R"))
source(file.path(PROJ_PATH, "helpers.R"))

# load libraries 
library(tidyverse)
library(glmnet)

# load flight delay dataset as CSV [TZ]
flight_data <- read_csv(FLIGHT_DATA_FILE) %>%
  mutate(MAX_DEP_DEL_HRS = MAX_DEP_DEL/60)
  


####################### HISTOGRAM OF AVG_MISHAND_RATIO ##############################

# Create the histogram for AVG_MISHAND_RATIO [TZ]
histogram_plot <- ggplot(flight_data, aes(x = AVG_MISHAND_RATIO)) +
  geom_histogram(bins = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of AVG_MISHAND_RATIO",
       x = "Average Mishandled Ratio",
       y = "Frequency") +
  theme_minimal()

# Print the histogram [TZ]
print(histogram_plot)

# Save the histogram to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "histogram-avgmishand.png"), 
       plot = histogram_plot, width = 8, height = 6)

####################### BAR CHART OF AVG_MISHAND_RATIO COLORED BY CARRIER ##############################

# Aggregate the data by the average of AVG_MISHAND_RATIO for each carrier
aggregated_data <- flight_data %>%
  group_by(CARRIER, CARRIER_NAME) %>%
  summarise(AVG_MISHAND_RATIO_MEAN = mean(AVG_MISHAND_RATIO, na.rm = TRUE)) %>%
  ungroup()  # Ungroup to avoid issues with reordering

# Reorder the CARRIER factor based on AVG_MISHAND_RATIO_MEAN in descending order
aggregated_data <- aggregated_data %>%
  mutate(CARRIER = fct_reorder(CARRIER, AVG_MISHAND_RATIO_MEAN, .desc = TRUE))

# Create the bar chart with the average AVG_MISHAND_RATIO colored by CARRIER_NAME [TZ]
bar_chart <- ggplot(aggregated_data, 
                    aes(x = CARRIER, y = AVG_MISHAND_RATIO_MEAN, fill = CARRIER_NAME)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Mean of Average Mishandled Ratio by Carrier",
       x = "Carrier",
       y = "Mean Average Mishandled Ratio",
       fill = "Carrier Name") +
  theme_minimal()

# Print the bar chart [TZ]
print(bar_chart)

# Save the bar chart to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "bar-chart-avgmishand-by-carrier.png"), 
       plot = bar_chart, width = 8, height = 6)


####################### HISTOGRAM OF PROP_LATEAIRCRAFT_DEL ##############################

# Create the histogram for PROP_LATEAIRCRAFT_DEL [TZ]
histogram_plot <- ggplot(flight_data, aes(x = PROP_LATEAIRCRAFT_DEL)) +
  geom_histogram(bins = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of PROP_LATEAIRCRAFT_DEL",
       x = "Proportion of Late Aircraft Delays",
       y = "Frequency") +
  theme_minimal()

# Print the histogram [TZ]
print(histogram_plot)

# Save the histogram to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "histogram-prop-lateaircraft-del.png"), 
       plot = histogram_plot, width = 8, height = 6)


####################### BAR CHART OF PROP_LATEAIRCRAFT_DEL COLORED BY CARRIER ##############################

# Aggregate the data by the average of PROP_LATEAIRCRAFT_DEL for each carrier
aggregated_data <- flight_data %>%
  group_by(CARRIER, CARRIER_NAME) %>%
  summarise(PROP_LATEAIRCRAFT_DEL_MEAN = mean(PROP_LATEAIRCRAFT_DEL, na.rm = TRUE)) %>%
  ungroup()  # Ungroup to avoid issues with reordering

# Reorder the CARRIER factor based on PROP_LATEAIRCRAFT_DEL_MEAN in descending order
aggregated_data <- aggregated_data %>%
  mutate(CARRIER = fct_reorder(CARRIER, PROP_LATEAIRCRAFT_DEL_MEAN, .desc = TRUE))

# Create the bar chart with the average AVG_MISHAND_RATIO colored by CARRIER_NAME [TZ]
bar_chart <- ggplot(aggregated_data, 
                    aes(x = CARRIER, y = PROP_LATEAIRCRAFT_DEL_MEAN, fill = CARRIER_NAME)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Mean Proportion of Late Aircraft Delays by Carrier",
       x = "Carrier",
       y = "Mean Proportion of Late Aircraft Delays",
       fill = "Carrier Name") +
  theme_minimal()

# Print the bar chart [TZ]
print(bar_chart)

# Save the bar chart to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "bar-chart-prop-lateaircraft-delay-by-carrier.png"), 
       plot = bar_chart, width = 8, height = 6)

####################### HISTOGRAM OF AIRCRAFT_UTIL_RATIO ##############################

# Create the histogram for AIRCRAFT_UTIL_RATIO [TZ]
histogram_plot <- ggplot(flight_data, aes(x = AIRCRAFT_UTIL_RATIO)) +
  geom_histogram(bins = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of AIRCRAFT_UTIL_RATIO",
       x = "Aircraft Utilization Ratio",
       y = "Frequency") +
  theme_minimal()

# Print the histogram [TZ]
print(histogram_plot)

# Save the histogram to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "histogram-aircraft-util-ratio.png"), 
       plot = histogram_plot, width = 8, height = 6)

####################### BAR CHART OF AIRCRAFT_UTIL_RATIO COLORED BY CARRIER ##############################

# Aggregate the data by the average of AIRCRAFT_UTIL_RATIO for each carrier
aggregated_data <- flight_data %>%
  group_by(CARRIER, CARRIER_NAME) %>%
  summarise(AIRCRAFT_UTIL_RATIO_MEAN = mean(AIRCRAFT_UTIL_RATIO, na.rm = TRUE)) %>%
  ungroup()  # Ungroup to avoid issues with reordering

# Reorder the CARRIER factor based on AIRCRAFT_UTIL_RATIO_MEAN in descending order
aggregated_data <- aggregated_data %>%
  mutate(CARRIER = fct_reorder(CARRIER, AIRCRAFT_UTIL_RATIO_MEAN, .desc = TRUE))

# Create the bar chart with the average AVG_MISHAND_RATIO colored by CARRIER_NAME [TZ]
bar_chart <- ggplot(aggregated_data, 
                    aes(x = CARRIER, y = AIRCRAFT_UTIL_RATIO_MEAN, fill = CARRIER_NAME)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Mean Aircraft Utilization Ratio by Carrier",
       x = "Carrier",
       y = "Mean Aircraft Utilization Ratio",
       fill = "Carrier Name") +
  theme_minimal()

# Print the bar chart [TZ]
print(bar_chart)

# Save the bar chart to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "bar-chart-aircraft-util-ratio-by-carrier.png"), 
       plot = bar_chart, width = 8, height = 6)

####################### HISTOGRAM OF MAX_DEP_DEL_HRS ##############################

# Create the histogram for MAX_DEP_DEL_HRS [TZ]
histogram_plot <- ggplot(flight_data %>% mutate(MAX_DEP_DEL_HRS = MAX_DEP_DEL/60), 
                         aes(x = MAX_DEP_DEL_HRS)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of MAX_DEP_DEL_HRS",
       x = "Maximum Departure Delay (Hours)",
       y = "Frequency") +
  theme_minimal()

# Print the histogram [TZ]
print(histogram_plot)

# Save the histogram to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "histogram-maxdepdelhrs.png"), 
       plot = histogram_plot, width = 8, height = 6)

####################### BAR CHART OF MAX_DEP_DEL_HRS COLORED BY CARRIER ##############################

# Aggregate the data by the average of MAX_DEP_DEL_HRS for each carrier
aggregated_data <- flight_data %>%
  mutate(MAX_DEP_DEL_HRS = MAX_DEP_DEL/60) %>%
  group_by(CARRIER, CARRIER_NAME) %>%
  summarise(MAX_DEP_DEL_HRS_MEAN = mean(MAX_DEP_DEL_HRS, na.rm = TRUE)) %>%
  ungroup()  # Ungroup to avoid issues with reordering

# Reorder the CARRIER factor based on MAX_DEP_DEL_HRS_MEAN in descending order
aggregated_data <- aggregated_data %>%
  mutate(CARRIER = fct_reorder(CARRIER, MAX_DEP_DEL_HRS_MEAN, .desc = TRUE))

# Create the bar chart with the average MAX_DEP_DEL_HRS colored by CARRIER_NAME [TZ]
bar_chart <- ggplot(aggregated_data, 
                    aes(x = CARRIER, y = MAX_DEP_DEL_HRS_MEAN, fill = CARRIER_NAME)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Mean Average Maximum Departure Delay by Carrier",
       x = "Carrier",
       y = "Mean Average Maximum Departure Delay (Hours)",
       fill = "Carrier Name") +
  theme_minimal()

# Print the bar chart [TZ]
print(bar_chart)

# Save the bar chart to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "bar-chart-maxdepdelhrs-by-carrier.png"), 
       plot = bar_chart, width = 8, height = 6)

####################### SCATTERPLOT OF AVG_MISHAND_RATIO VS AIRCRAFT_UTIL_RATIO by CARRIER ##############################

# Create the scatter plot with CARRIER for axis labels and CARRIER_NAME for the legend [TZ]
scatter_plot <- ggplot(flight_data, 
                       aes(x = AIRCRAFT_UTIL_RATIO, y = AVG_MISHAND_RATIO, color = CARRIER_NAME)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of AVG_MISHAND_RATIO vs AIRCRAFT_UTIL_RATIO by CARRIER",
       x = "Aircraft Utilization Ratio",
       y = "Average Mishandled Ratio",
       color = "Carrier Name") +
  theme_minimal()

# Print the scatter plot [TZ]
print(scatter_plot)

# Save the scatter plot to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "scatter-plot-avgmishand-vs-aircraftutil.png"), 
       plot = scatter_plot, width = 8, height = 6)


####################### SCATTERPLOT OF PROP_LATEAIRCRAFT_DEL VS AVG_MISHAND_RATIO by CARRIER ##############################

# Create the scatter plot with CARRIER for axis labels and CARRIER_NAME for the legend [TZ]
scatter_plot <- ggplot(flight_data, 
                       aes(x = PROP_LATEAIRCRAFT_DEL, y = AVG_MISHAND_RATIO, color = CARRIER_NAME)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of AVG_MISHAND_RATIO vs PROP_LATEAIRCRAFT_DEL by CARRIER",
       x = "Proportion of Late Aircraft Delayed Flights",
       y = "Average Mishandled Ratio",
       color = "Carrier Name") +
  theme_minimal()

# Print the scatter plot [TZ]
print(scatter_plot)

# Save the scatter plot to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "scatter-plot-avgmishand-vs-proplateaircraftdelay.png"), 
       plot = scatter_plot, width = 8, height = 6)


####################### BAR CHART OF TOTAL_FLIGHTS COLORED BY CARRIER ##############################

# Aggregate the number of flights by carrier
aggregated_data <- flight_data %>%
  group_by(CARRIER, CARRIER_NAME) %>%
  summarise(TOTAL_FLIGHTS = sum(NUM_FLIGHTS, na.rm = TRUE)) %>%
  ungroup()  # Ungroup to avoid issues with reordering

# Reorder the CARRIER factor based on TOTAL_FLIGHTS in descending order
aggregated_data <- aggregated_data %>%
  mutate(CARRIER = fct_reorder(CARRIER, TOTAL_FLIGHTS, .desc = TRUE))

# Create the bar chart with aggregated number of flights colored by CARRIER_NAME [TZ]
bar_chart_num_flights <- ggplot(aggregated_data, 
                                aes(x = CARRIER, y = TOTAL_FLIGHTS, fill = CARRIER_NAME)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Total Flights by Carrier",
       x = "Carrier",
       y = "Total Number of Flights",
       fill = "Carrier Name") +
  theme_minimal()

# Print the bar chart [TZ]
print(bar_chart_num_flights)

# Save the bar chart to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "bar-chart-total-flights-by-carrier.png"), 
       plot = bar_chart_num_flights, width = 8, height = 6)

####################### SCATTERPLOT OF MAX_DEP_DEL_HRS VS AVG_MISHAND_RATIO by CARRIER ##############################

# Create the scatter plot with CARRIER for axis labels and CARRIER_NAME for the legend [TZ]
scatter_plot <- ggplot(flight_data, 
                       aes(x = MAX_DEP_DEL_HRS, y = AVG_MISHAND_RATIO, color = CARRIER_NAME)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of AVG_MISHAND_RATIO vs MAX_DEP_DEL_HRS by CARRIER",
       x = "Maximum Departure Delay (Hrs)",
       y = "Average Mishandled Ratio",
       color = "Carrier Name") +
  theme_minimal()

# Print the scatter plot [TZ]
print(scatter_plot)

# Save the scatter plot to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "scatter-plot-avgmishand-vs-maxdepdelhrs.png"), 
       plot = scatter_plot, width = 8, height = 6)

####################### SCATTERPLOT OF PROP_LATEAIRCRAFT_DEL VS MAX_DEP_DEL_HRS by CARRIER ##############################

# Create the scatter plot with CARRIER for axis labels and CARRIER_NAME for the legend [TZ]
scatter_plot <- ggplot(flight_data, 
                       aes(x = PROP_LATEAIRCRAFT_DEL, y = MAX_DEP_DEL_HRS, color = CARRIER_NAME)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of MAX_DEP_DEL_HRS vs PROP_LATEAIRCRAFT_DEL by CARRIER",
       x = "Proportion of Late Aircraft Delayed Flights",
       color = "Carrier Name") +
  theme_minimal()

# Print the scatter plot [TZ]
print(scatter_plot)

# Save the scatter plot to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "scatter-plot-proplateaircraft-vs-maxdepdelhrs.png"), 
       plot = scatter_plot, width = 8, height = 6)