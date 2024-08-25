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
flight_data <- read_csv(FLIGHT_DATA_FILE) 

####################### PASSENGERS VS NUM_FLIGHTS ##############################

# Create the scatter plot with a dotted line [TZ]
scatter_plot <- ggplot(cbind(flight_data), 
                       aes(x = NUM_FLIGHTS, y = PASSENGERS, color = CARRIER_NAME)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 0.8, intercept = -0.2, linetype = "dotted", color = "red") +
  labs(title = "Scatter Plot of PASSENGERS vs NUM_FLIGHTS by CARRIER_NAME",
       x = "Number of Flights",
       y = "Number of Passengers",
       color = "Carrier") +
  theme_minimal()

# print the scatter plot [TZ]
print(scatter_plot)

# Save the scatter plot to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "scatter-plot-passengers-vs-numflights.png"), 
       plot = scatter_plot, width = 8, height = 6)



####################### AVG_MISHAND_RATIO VS AIRCRAFT_UTIL_RATIO by CARRIER_NAME ##############################


# Create the scatter plot with a dotted line [TZ]
scatter_plot <- ggplot(flight_data, 
                       aes(x = AIRCRAFT_UTIL_RATIO, y = AVG_MISHAND_RATIO, color = CARRIER_NAME)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of AVG_MISHAND_RATIO vs AIRCRAFT_UTIL_RATIO by CARRIER_NAME",
       x = "Aircraft Utilization Ratio",
       y = "Average Mishandled Ratio",
       color = "Carrier") +
  theme_minimal()

# Print the scatter plot [TZ]
print(scatter_plot)

# Save the scatter plot to a file [TZ]
ggsave(filename = file.path(IMAGES_PATH, "scatter-plot-avgmishand-vs-aircraftutil.png"), 
       plot = scatter_plot, width = 8, height = 6)