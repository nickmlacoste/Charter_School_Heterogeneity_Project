# Intro --------------------------
"
This is the master file for running the analysis. This requires all build code to have been 
executed already!
"

rm(list = ls())

# Calls causal forest training script
source(file.path("C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/Charter_School_Heterogeneity_Project/analysis/code/causal_forest_analysis.R"))

cat("Causal Forest Models Trained. \n")

# Calls script to make supplementary tables
source(file.path("C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/Charter_School_Heterogeneity_Project/analysis/code/post_ML_analysis.R"))

cat("Analysis Complete.\n")