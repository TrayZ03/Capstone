# source constants
PROJ_PATH <- "C:\\Users\\trace\\OneDrive\\Documents\\Capstone\\Capstone-Repo-Shared" # change project path for local environment
source(file.path(PROJ_PATH, "constants.R"))

# load libraries 
library(tidyverse)

# load data
data <- read_csv(MODEL_DATA_FILE) %>%
  select(SELECTED_FEATURES) # select model specific features

# Perform PCA
pca_result <- prcomp(data, scale. = TRUE)

# Print the PCA results
summary(pca_result)
pca_result$rotation  # Loadings
pca_result$x  # Scores

# Visualize the PCA results (optional)
# Convert PCA results to a tibble for easier plotting
pca_scores <- as_tibble(pca_result$x)

# Calculate the cumulative proportion of variance explained
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance_explained <- cumsum(variance_explained)

# Create a data frame for plotting
variance_df <- tibble(
  Principal_Component = 1:length(cumulative_variance_explained),
  Cumulative_Variance_Explained = cumulative_variance_explained,
  Cumulative_Variance_Explained_Diff = Cumulative_Variance_Explained - lag(Cumulative_Variance_Explained)
)

# Plot the cumulative proportion of variance
ggplot(variance_df, aes(x = Principal_Component, y = Cumulative_Variance_Explained)) +
  geom_point() +
  geom_line() +
  labs(title = "PCA Cumulative Variance Explained",
       x = "Number of Principal Components",
       y = "Cumulative Proportion of Variance Explained") +
  theme_minimal()

# Plot the difference of cumulative proportion of variance explained
ggplot(variance_df, aes(x = Principal_Component, y =  Cumulative_Variance_Explained_Diff)) +
  geom_point() +
  geom_line() +
  labs(title = "PCA Differences of Cumulative Variance Explained",
       x = "Number of Principal Components",
       y = "Cumulative Variance Explained Difference") +
  theme_minimal()

# Plot the first two principal components
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(title = "PCA First Two Components", x = "PC1", y = "PC2") +
  theme_minimal()
