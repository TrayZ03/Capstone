# source constants
PROJ_PATH <- "C:\\Users\\trace\\OneDrive\\Documents\\Capstone\\Capstone-Repo-Shared" # change project path for local environment
source(file.path(PROJ_PATH, "constants.R"))

# merge monthly on-time flight performance data files - uncomment if starting with monthly ontime .csvs
# source(file.path(SCRIPTS_PATH, "data-merge-csvs-ontime.R"))

# preliminary cleaning on-time flight performance data files
source(file.path(SCRIPTS_PATH, "data-clean-ontime.R"))

# final preprocessing and merging of on-time and baggage data files - includes
# feature selection and engineering, creates model data
source(file.path(SCRIPTS_PATH, "data-clean-ontime.R"))

# unsupervised methods on model data
source(file.path(SCRIPTS_PATH, "pca.R"))
source(file.path(SCRIPTS_PATH, "kmeans.R"))

# initial elastic net model
source(file.path(SCRIPTS_PATH, "initial-model.R"))
