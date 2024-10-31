# Intro -----------------------------

"
This script imports the causal forest estimates of the CATEs and does analysis.
"

rm(list = ls())

library(tidyverse)
library(haven)
library(grf)
library(xtable)
library(knitr)
library(kableExtra)
library(plm)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/data"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/Charter_School_Heterogeneity_Project/analysis/output"
cf_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/large_output"

# Import graduation rate data
charter_afgr2 <- read.csv(file.path(data_path, "afgr_post_ML.csv"))
charter_seda_math <- read.csv(file.path(data_path, "math_post_ML.csv"))
charter_seda_ela <- read.csv(file.path(data_path, "ela_post_ML.csv"))



















