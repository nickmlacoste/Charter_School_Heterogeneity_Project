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

# Import data with predictions
charter_afgr2 <- read.csv(file.path(data_path, "afgr_post_ML.csv"))
charter_seda_math <- read.csv(file.path(data_path, "math_post_ML.csv"))
charter_seda_ela <- read.csv(file.path(data_path, "ela_post_ML.csv"))

# # Import causal forests
# cf_model_afgr <- readRDS(file.path(cf_output_path, "cf_model_afgr.rda"))
# cf_model_math <- readRDS(file.path(cf_output_path, "cf_model_math.rda"))
# cf_model_ela <- readRDS(file.path(cf_output_path, "cf_model_ela.rda"))

# pull-in ATEs from causal forests ---------------------

ATEs <- read.csv(file.path(data_path, "cf_ATE_estimates.csv"))

# Distribution of treatment effects plots (distrct x year estimates) -------------------

# Graduation Rates
plot <- ggplot(charter_afgr2, aes(x = predictions, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of Estimated Treatment Effects -- Graduation Rates", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-0.5, 1) +  
  geom_vline(aes(xintercept = ATEs$ATE[1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs$ATE[1], y = 20000, label = paste("ATE =", round(ATEs$ATE[1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "/figures/cate_dist_afgr.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# Math scores
plot <- ggplot(charter_seda_math, aes(x = predictions, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of Estimated Treatment Effects -- Math", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-2, 2) +  
  geom_vline(aes(xintercept = ATEs$ATE[2]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs$ATE[2], y = 20000, label = paste("ATE =", round(ATEs$ATE[2], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "/figures/cate_dist_math.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# ELA scores
plot <- ggplot(charter_seda_ela, aes(x = predictions, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of Estimated Treatment Effects -- ELA", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-1.5, 1.5) +  
  geom_vline(aes(xintercept = ATEs$ATE[3]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs$ATE[3], y = 20000, label = paste("ATE =", round(ATEs$ATE[3], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "/figures/cate_dist_ela.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)


























