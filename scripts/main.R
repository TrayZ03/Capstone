# !/usr/local/bin/Rscript

# source constants

# PROJ_PATH <- "C:\Users\trace\OneDrive\Documents\Capstone\Capstone-Shared-Repo" # change project path for local environment
# PROJ_PATH <- "/Users/jessweeks/Documents/Capstone/Capstone_Shared_Repo/Capstone-main" # change project path for local environment

# project constants, especially paths
source(file.path(PROJ_PATH, "constants.R"))

# helper functions
source(file.path(PROJ_PATH, "helpers.R"))

# merge monthly on-time flight performance data files
# source(file.path(SCRIPTS_PATH, "data-merge-csvs-ontime.R"))

# preliminary EDA
# source(file.path(SCRIPTS_PATH, "prelim-eda.R"))

# preliminary cleaning on-time flight performance data files
# source(file.path(SCRIPTS_PATH, "data-clean-ontime.R"))

# final pre-processing, feature selection and engineering
source(file.path(SCRIPTS_PATH, "data-preprocessing.R"))

# additional post-processing EDA
# source(file.path(SCRIPTS_PATH, "secondary-eda.R"))

# unsupervised methods on model data
# source(file.path(SCRIPTS_PATH, "pca.R"))
# source(file.path(SCRIPTS_PATH, "kmeans.R"))

# initial regression models
source(file.path(SCRIPTS_PATH, "initial-model.R"))

# tuning
source(file.path(SCRIPTS_PATH, "model-tuning.R"))

# fit final models and eval
source(file.path(SCRIPTS_PATH, "final-model.R"))
