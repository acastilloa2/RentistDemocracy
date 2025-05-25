# ============================================================
# Master Script: Rentist Democracy Replication
# Author: Alejandro Castillo Ardila
# Date: May 2025
# Description: This script runs all steps of the replication
# in the correct order: data cleaning and analysis.
# ============================================================

# Clear environment
rm(list = ls())

# Set working directory to project root if needed
# setwd("path/to/RentistDemocracy")

# Run scripts in order
source("Scripts/01_ANH_Cleaning.R")
source("Scripts/02_IICA_Cleaning.R")
source("Scripts/03_DataCleaning.R")
source("Scripts/04_RegressionModels.R")

message("✅ Replication complete.")
