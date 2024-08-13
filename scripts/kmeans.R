# !/usr/local/bin/Rscript

# source constants

PROJ_PATH <- "C:/Users/trace/OneDrive/Documents/Capstone/Capstone-Repo-Shared" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment


source(file.path(PROJ_PATH, "constants.R"))
source(file.path(PROJ_PATH, "helpers.R"))

# load libraries 
library(tidyverse)
library(cluster)

# load data
flight_data <- read_csv(FLIGHT_DATA_FILE) %>%
  select(where(is.numeric)) # k-means works only on numeric features

# Perform PCA
pca_result <- prcomp(flight_data, scale. = TRUE)

# Convert PCA results to a tibble for easier plotting
pca_scores <- as_tibble(pca_result$x)

########################## k means for all PCA components ######################

### Within and between cluster sum of squares [WCSS/BCSS]
##
#

# Function to calculate WCSS and BCSS
calculate_wcss_bcss <- function(k) {
  kmeans_result <- kmeans(pca_scores, centers = k, nstart = 25)
  wcss <- kmeans_result$tot.withinss
  bcss <- sum(kmeans_result$betweenss)
  return(tibble(k = k, WCSS = wcss, BCSS = bcss))
}

# Perform k-means clustering for k in the range 2:10 and calculate WCSS and BCSS
wcss_bcss <- map_dfr(2:20, calculate_wcss_bcss)

# Convert k to numeric
wcss_bcss <- wcss_bcss %>%
  mutate(k = as.numeric(k))

# Plot WCSS and BCSS
ggplot(wcss_bcss) +
  geom_point(aes(x = k, y = WCSS), color = 'blue') +
  geom_line(aes(x = k, y = WCSS), color = 'blue', linetype = "dashed") +
  geom_point(aes(x = k, y = BCSS), color = 'red') +
  geom_line(aes(x = k, y = BCSS), color = 'red', linetype = "dashed") +
  labs(title = "k-Means WCSS and BCSS",
       x = "Number of Clusters (k)",
       y = "Sum of Squares",
       color = "Metric") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "BCSS")) +
  theme_minimal()



### Silhouette Scores
##
#

# Function to calculate average silhouette width for a given k
calculate_silhouette <- function(k) {
  kmeans_result <- kmeans(pca_scores, centers = k, nstart = 25)
  silhouette_result <- silhouette(kmeans_result$cluster, dist(pca_scores))
  mean(silhouette_result[, 3])
}

# Calculate silhouette scores for k in the range 2:10
silhouette_scores <- map_dbl(2:20, calculate_silhouette)

# Create a tibble for plotting
silhouette_df <- tibble(
  k = 2:20,
  silhouette_score = silhouette_scores
)

# Plot the silhouette scores
ggplot(silhouette_df, aes(x = k, y = silhouette_score)) +
  geom_point() +
  geom_line() +
  labs(title = "k-Means Silhouette Scores",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Score") +
  theme_minimal()

########################## k means for PCA reduced data ########################

#### Cumulative variance explained indicates using the first twelve PCA components
pca_scores_red <- pca_scores[,1:12]

### Within and between cluster sum of squares [WCSS/BCSS]
##
#

# Function to calculate WCSS and BCSS
calculate_wcss_bcss_red <- function(k) {
  kmeans_result <- kmeans(pca_scores_red, centers = k, nstart = 25)
  wcss <- kmeans_result$tot.withinss
  bcss <- sum(kmeans_result$betweenss)
  return(tibble(k = k, WCSS = wcss, BCSS = bcss))
}

# Perform k-means clustering for k in the range 2:10 and calculate WCSS and BCSS
wcss_bcss_red <- map_dfr(2:20, calculate_wcss_bcss_red)

# Convert k to numeric
wcss_bcss_red <- wcss_bcss_red %>%
  mutate(k = as.numeric(k))

# Plot WCSS and BCSS for reduced data
ggplot(wcss_bcss_red) +
  geom_point(aes(x = k, y = WCSS), color = 'blue') +
  geom_line(aes(x = k, y = WCSS), color = 'blue', linetype = "dashed") +
  geom_point(aes(x = k, y = BCSS), color = 'red') +
  geom_line(aes(x = k, y = BCSS), color = 'red', linetype = "dashed") +
  labs(title = "k-Means WCSS and BCSS for PCA Reduced Data",
       x = "Number of Clusters (k)",
       y = "Sum of Squares",
       color = "Metric") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "BCSS")) +
  theme_minimal()


### Silhouette Scores
##
#

# Function to calculate average silhouette width for a given k
calculate_silhouette_red <- function(k) {
  kmeans_result <- kmeans(pca_scores_red, centers = k, nstart = 25)
  silhouette_result <- silhouette(kmeans_result$cluster, dist(pca_scores))
  mean(silhouette_result[, 3])
}

# Calculate silhouette scores for k in the range 2:10
silhouette_scores_red <- map_dbl(2:20, calculate_silhouette_red)

# Create a tibble for plotting
silhouette_df_red <- tibble(
  k = 2:20,
  silhouette_score = silhouette_scores_red
)

# Plot the silhouette scores
ggplot(silhouette_df_red, aes(x = k, y = silhouette_scores_red)) +
  geom_point() +
  geom_line() +
  labs(title = "k-Means Silhouette Scores for PCA Reduced Data",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Score") +
  theme_minimal()


